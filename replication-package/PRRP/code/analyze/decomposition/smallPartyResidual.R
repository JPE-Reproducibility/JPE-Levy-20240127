# This script investigates new parties in wave 6 (2017-2020):
# it imputes their (non-existing) residuals for wave 4 (2005-2009),
# and simulates their counterfactual vote share in wave 4 .
# Uses the decomposition function to simulate shares

source("code/util/commonVars.R")
source("code/model/decompose_delta.R")
source("code/model/func_nuclear_and_pgd.R")
source("code/analyze/party_positions/manifesto_general_functions.R")
source("code/analyze/priorities_and_model/func_plot_weight.R")

library(ggplot2)
library(dplyr)
library(patchwork)



# Decide how to impute valence in wave 4: ---------------------------------

# set to 1 to plugin the valence of wave 6
# set to 0 to impute using the linear model
plugin_wave6 <- 0

# Use the shared decomposition helper with party-level residual outputs.
source("code/model/func_nuclear_and_pgd.R", local = TRUE)

strt=1
final=3
k = 5 # the number of dimentions to which the data will be reduced
quad = F # controls whether quadratic (squared) terms are included in the decomposition.
dec.order = c("Z", "resid", "X", "param")
# first component - counterfactual support when changing only the first component, holding all others constant
c1 <- decomposition(X.list, Z.list, Y.list,
                    phi.list, FE.list, party.id.list,
                    X.country, Z.country, phi,
                    W.list = W.list, k = k, quad = quad,
                    X = 1,
                    param = 1,
                    Z = 1,
                    resid = "*",
                    return.party.level = TRUE)


# Build party-level residual data with country, family, and PRRP labels.
build_party_residual_df <- function(c1, wave_position, wave_number, wave_parties, residual_name, Z.all, party_dat) {
  z.sub <- subset(Z.all, BB_Wave_num == wave_number,
                  select = c("manifesto_country_name", "country",
                             "Y_manifesto_party_id", "manifesto_party_name",
                             "parfam"))

  data.frame(
    residual = unname(c1$vel.new[[wave_position]]),
    Y_manifesto_party_id = names(c1$vel.new[[wave_position]])
  ) %>%
    filter(Y_manifesto_party_id %in% wave_parties) %>%
    mutate(Y_manifesto_party_id = as.numeric(Y_manifesto_party_id)) %>%
    merge(z.sub, by = "Y_manifesto_party_id", all = FALSE) %>%
    rename(!!residual_name := residual) %>%
    mutate(country = as.factor(country),
           parfam = as.numeric(as.character(parfam))) %>%
    dplyr::left_join(party_dat %>% dplyr::select(Y_manifesto_party_id, Y_FR_Pop),
                     by = "Y_manifesto_party_id") %>%
    add_parfam()
}

# Create wave-4 residual values for parties that only appear in wave 6.
impute_new_party_residuals <- function(df2, new_parties, lm1, plugin_wave6) {
  df3 <- subset(df2, Y_manifesto_party_id %in% new_parties)

  if (plugin_wave6 == 1) {
    df3 <- df3 %>% rename(zeta4 = zeta6)
  } else {
    df3$zeta4 <- predict(lm1, newdata = df3)
  }

  df3 %>% mutate(Y_manifesto_party_id = as.character(Y_manifesto_party_id))
}


# create a df to store residual values at the party level by wave
colnames(c1$res) <- paste0("w", 1:ncol(c1$res), ".1")

# Predict zeta (vel.new) for new parties  --------------------

# Find parties that have -Inf in wave 4 and not in 6
# instead of -inf, predict their value based on parfam & country using OLS

# keep party.dat to identify PRRP parties later
party.dat <- party.dat %>%
  dplyr::mutate(manifestoKey = rownames(party.dat))

party.dat$Y_manifesto_party_id <- as.numeric(party.dat$manifestoKey)

# find out which parties existed in wave 6 but not in 4
wave4Parties <- as.vector(party.id.list[[1]])
wave6Parties <- as.vector(party.id.list[[3]])
newParties <- wave6Parties[!wave6Parties %in% wave4Parties]

# Build wave-4 residual data for observed parties.
df1 <- build_party_residual_df(c1, 1, 4, wave4Parties, "zeta4", Z.all, party.dat)

# estimate linear model (will help in predicting zeta for new parties in wave 4)
lm1 <- lm(zeta4 ~ as.factor(country) + as.factor(parfam), data = df1)
# lm1 <- lm(zeta4 ~ as.factor(country):as.factor(parfam), data = df1)

# Build wave-6 residual data for observed parties.
df2 <- build_party_residual_df(c1, 3, 6, wave6Parties, "zeta6", Z.all, party.dat)



# create the new version of vel.new2 (list of zeta values in wave 4, wave 6)
vel.new2 <- list(
  zeta4 = setNames(df1$zeta4, df1$Y_manifesto_party_id), # Keys as Y_manifesto_party_id, values as zeta4
  zeta6 = setNames(df2$zeta6, df2$Y_manifesto_party_id)  # Keys as Y_manifesto_party_id, values as zeta6
)
# Simulate shares using the new version for vel.new -----------------------

# adjust input sizes

# remove wave 5 from X,Z,phi lists and convert to matrices
X.list <- X.list[-2]
X.list <- lapply(X.list, as.matrix)
Z.list <- Z.list[-2]
Z.list <- lapply(Z.list, as.matrix)
Z.country <- Z.country[-2]
phi.list <- phi.list[-2]
phi.list <- lapply(phi.list, as.matrix)
party.id.list <- party.id.list[-2]

beta.list = c1$beta.list[-2]
X.group.list = c1$X.group.list[-2]
W.list = W.list[-2]

# ensure all parties are available
# those with vel.new == -inf in wave 6 should still have vel.new == -inf in wave 6
# those with vel.new == -inf in wave 4 (but not in 6), should have a predicted finite value
EP <- expand.parties(Z.list, vel.new2, Z.country, party.id.list)
Z.new <- EP[[1]]
vel.final <- EP[[2]]
Z.group <- EP[[3]]

# Build predicted or plugged-in wave-4 residuals for wave-6 entrants.
subset_newParites <- vel.final[[1]][names(vel.final[[1]]) %in% newParties]
df3 <- impute_new_party_residuals(df2, newParties, lm1, plugin_wave6)

# plug in the zeta4 predictions into new party zetas in vel.final[[1]]
newPartyIndices <- as.character(names(subset_newParites))
for (i in newPartyIndices) { # Go over every new party
  party_name <- i  # Get the party name
  # Find the corresponding row in df3
  matching_row <- which(df3$Y_manifesto_party_id == party_name)
  if (length(matching_row) == 1) {
    # Replace the value in vel.final[[1]] with the prediction
    vel.final[[1]][i] <- df3$zeta4[matching_row]
  } else {
    warning(paste("No matching prediction for party:", party_name))
  }
}

# the following function simulates total votes for specific party, not vote share
shrs4 <- simulate.shares(
  X.list[[1]], Z.new[[1]], phi.list[[1]],
  beta.list[[1]], vel.final[[1]],
  X.group.list[[1]], Z.group, W.list[[1]])


# Plot the counterfactual votes for wave 4 with predicted residua --------

# create a data frame of simulated shares
df_shrs4 <- as.data.frame(shrs4)
df_shrs4 <- df_shrs4 %>%
  mutate(manifestoKey = as.character(rownames(df_shrs4))) %>%
  rename(totalVote = shrs4)

df_shrs4 <- df_shrs4 %>%
  dplyr::mutate(newParty = ifelse(as.character(manifestoKey) %in% newParties, 1, 0))

# add binary indicator for radical right (Y_FR_Pop)
df_shrs4 <- df_shrs4 %>%
  left_join(select(party.dat, manifestoKey, Y_FR_Pop, BB_Country), by = "manifestoKey")

# add a column to denote vote share (rather than total vote)
df_shrs4 <- df_shrs4 %>%
  group_by(BB_Country) %>% # for each country
  mutate(shrs4 = totalVote / sum(totalVote)) %>% # vote share out of the country's total vote
  ungroup()


# create histogram for vote shares ----------------------------------------


# 1. Histogram for newParty == 1 and Y_FR_Pop == 1
newprrp <- subset(df_shrs4, Y_FR_Pop == 1 & newParty == 1) %>%
  mutate(group = "New Party - PRRP")
newprrpPlot <- ggplot(newprrp, aes(x = shrs4)) +
  geom_histogram(binwidth = 0.01, fill = "skyblue", color = "black") +
  labs(title = "Histogram of vote share (newParty == 1 & Y_FR_Pop == 1)",
       x = "vote share", y = "Frequency") +
  theme_minimal()

# 2. Histogram for newParty == 1 and Y_FR_Pop == 0
new_other <- subset(df_shrs4, Y_FR_Pop == 0 & newParty == 1) %>%
  mutate(group = "New Party - Other")
new_otherPlot <- ggplot(new_other, aes(x = shrs4)) +
  geom_histogram(binwidth = 0.01, fill = "skyblue", color = "black") +
  labs(title = "Histogram of vote share (newParty == 1 & Y_FR_Pop == 0)",
       x = "vote share", y = "Frequency") +
  theme_minimal()

# 3. Histogram for newParty == 0
oldParty <- subset(df_shrs4, newParty == 0) %>%
  mutate(group = "Old Party")
oldPartyPlot <- ggplot(oldParty, aes(x = shrs4)) +
  geom_histogram(binwidth = 0.01, fill = "skyblue", color = "black") +
  labs(title = "Histogram of vote share (newParty == 0)",
       x = "vote share", y = "Frequency") +
  theme_minimal()

# Create a single plot with faceted histograms
combined_data <- bind_rows(newprrp, new_other, oldParty)

facetedPlot <- ggplot(combined_data, aes(x = shrs4, fill = group)) +
  geom_histogram(binwidth = 0.02, color = "black", alpha = 0.7) +
  scale_fill_manual(values = c(
    "New Party - PRRP" = "skyblue",
    "New Party - Other" = "lightgreen",
    "Old Party" = "lightcoral"
  )) +
  labs(title = "Histograms of Simulated Shares by Group",
       x = "Share", y = "Frequency", fill = "Party Type") +
  theme_minimal() +
  theme(legend.position = "top") +
  facet_wrap(~ group, ncol = 1, scales = "free_y")


# combine newprrp and new_other together on the same axes

# to ensure same axes limits:
x_limits <- c(0, max(c(newprrp$shrs4, new_other$shrs4, oldParty$shrs4), na.rm = TRUE))

y_limits <- c(0, 20)

newprrp <- newprrp %>% filter(!is.na(shrs4))
new_other <- new_other %>% filter(!is.na(shrs4))
oldParty <- oldParty %>% filter(!is.na(shrs4))

# create a combined histogram with new prrp/other on the same axes
combined_overlay <- ggplot() +
  geom_histogram(data = new_other, aes(x = shrs4, fill = "New Party - Other"),
                 binwidth = 0.015, color = "black", alpha = 1, position = "identity") +  # Red bars at the back
  geom_histogram(data = newprrp, aes(x = shrs4, fill = "New Party - PRRP"),
                 binwidth = 0.015, color = "black", alpha = 1, position = "identity") +  # Blue bars in front
  scale_fill_manual(values = c(
    "New Party - PRRP" = "skyblue",
    "New Party - Other" = "lightgreen"
  )) +
  labs(title = "Overlayed Histogram of Vote Share for New Parties",
       x = "Vote Share", y = "Frequency", fill = "Party Type") +
  xlim(x_limits) +
  ylim(y_limits) +
  theme_minimal() +
  theme(legend.position = "top")

# Facet plot for oldParty
old_party_facet <- ggplot(oldParty, aes(x = shrs4)) +
  geom_histogram(binwidth = 0.015, fill = "lightcoral", color = "black") +
  labs(title = "Histogram of Vote Share for Old Parties",
       x = "Vote Share", y = "Frequency") +
  xlim(x_limits) +
  ylim(y_limits) +
  theme_minimal()

# Combine the plots using patchwork

final_plot <- combined_overlay / old_party_facet +
  plot_layout(heights = c(1, 1))  # Adjust the relative heights


# create density plot for vote shares -------------------------------------

# Density plot for newprrp and new_other overlaid
combined_density <- ggplot() +
  geom_density(data = new_other, aes(x = shrs4, fill = "New Party - Other"),
               color = "black", alpha = 0.6, adjust = 1.5) +  # Green in the back
  geom_density(data = newprrp, aes(x = shrs4, fill = "New Party - PRRP"),
               color = "black", alpha = 0.6, adjust = 1.5) +  # Blue in the front
  scale_fill_manual(values = c(
    "New Party - PRRP" = "skyblue",
    "New Party - Other" = "lightgreen"
  )) +
  labs(title = "Overlayed Density Plot of Vote Share for New Parties",
       x = "Vote Share", y = "Density", fill = "Party Type") +
  xlim(x_limits) +
  theme_minimal() +
  theme(legend.position = "top")

# Density plot for oldParty
old_party_density <- ggplot(oldParty, aes(x = shrs4)) +
  geom_density(fill = "lightcoral", color = "black", alpha = 0.6, adjust = 1.5) +
  labs(title = "Density Plot of Vote Share for Old Parties",
       x = "Vote Share", y = "Density") +
  xlim(x_limits) +
  theme_minimal()

# Combine the density plots using patchwork
final_density_plot <- combined_density / old_party_density +
  plot_layout(heights = c(1, 1))  # Keep relative heights similar



# Check if some party families have small representation --------------------

#add party family information to combined data (data after vote share evaluation)
# Get unique parties from combined_data
parties <- unique(combined_data$manifestoKey)

# Subset Z.all to include only relevant rows
tempZ <- subset(Z.all, Y_manifesto_party_id %in% parties, select = c("parfam", "BB_Country", "Y_manifesto_party_id"))
tempZ <- tempZ %>%
  rename("manifestoKey" = "Y_manifesto_party_id") %>%
  distinct()


# Merge combined_data with tempZ by Y_manifesto_party_id
combined_data2 <- merge(combined_data, tempZ, by = "manifestoKey", all.x = TRUE)

# count the appearances of each parfam value within combined data
# Provide the count of each unique combined_data$parfam value
table(combined_data2$parfam)

table(df1$parfam)
# is the election threshold correlated with new parties' valence? ---------

# Create a data frame with election threshold data (from ChatGPT 4)
threshold_data <- data.frame(
  BB_Country = c("Austria", "Bulgaria", "Croatia", "Denmark", "Estonia", "Finland", "France", "Germany",
                 "Hungary", "Iceland", "Italy", "Netherlands", "Norway", "Poland", "Portugal", "Spain",
                 "Sweden", "Switzerland", "UK", "Greece", "USA", "Czech Republic", "Lithuania",
                 "Slovakia", "Slovenia"),
  election_threshold = c(4, 4, 5, 2, 5, NA, NA, 5, 5, 5, 3, 0.67, 4, 5, NA, 3, 4, NA, NA, 3, NA, 5, 5, 5, 4)
)

# Filter the dataset with vote shares to include only new parties
newParty_data <- combined_data[combined_data$newParty == 1, ]

# merge threshold data with new party data
newParty_data <- merge(newParty_data, threshold_data, by = "BB_Country")


# Calculate the correlation between residuals and election thresholdsgetw
correlation <- cor(newParty_data$shrs4, newParty_data$election_threshold, use = "complete.obs")

# Print the correlation result
print(paste("Correlation between VOTE SHARE for new parties and election thresholds:", correlation))



# Plot each new party (y axis) and their vote share (x axis) --------------

# 1. Convert `Y_manifesto_party_id` to character & merge party names
Z.all <- Z.all %>%
  mutate(Y_manifesto_party_id = as.character(Y_manifesto_party_id))  # Convert numeric to character

newParty_data <- newParty_data %>%
  left_join(Z.all[, c("Y_manifesto_party_id", "manifesto_party_name")],
            by = c("manifestoKey" = "Y_manifesto_party_id"))

# arrange data by largest share to smallest
newParty_data <- newParty_data %>%
  arrange(desc(shrs4))

# ensure no identical rows within the data
newParty_data <- newParty_data %>%
  distinct(manifestoKey, manifesto_party_name, shrs4, Y_FR_Pop, .keep_all = TRUE)


# create the bar plot
newPartyPlot = ggplot(newParty_data, aes(x = shrs4,
                                         y = factor(manifesto_party_name, levels = rev(unique(manifesto_party_name))),
                                         fill = factor(Y_FR_Pop))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("0" = "blue", "1" = "red")) +
  labs(x = "Share (shrs4)", y = "Party Name",
       title = "Party Shares by Manifesto Party Name",
       fill = "Far Right") +
  theme_minimal()



# new- plot 25th, 50th, 75th percentile for each party family -------------


# join df1$BBF_parfam to df_shrs4 based on df_shrs4$manifestoKey and df1$Y_manifesto_party_id
# ensure compatibility of manifestoKey and Y_manifesto_party_id
df_shrs4$manifestoKey <- as.numeric(df_shrs4$manifestoKey)

# join
df_shrs4 <- df_shrs4 %>%
  left_join(df2[, c("Y_manifesto_party_id", "BBF_parfam")],
            by = c("manifestoKey" = "Y_manifesto_party_id"))



# Prepare a dataframe compatible with the plotting function

# Filter for new parties only
df_plot <- df_shrs4 %>%
  filter(newParty == 1)

# select relevant columns
df_plot <- df_plot %>%
  dplyr::mutate(variable = "shrs4", value = shrs4, BB_Weight_CW = 1) %>%
  dplyr::select(BBF_parfam, variable, value, BB_Weight_CW)


# Now call the function with the correct data
p <- plot_errbar_parfam_static(df_plot, list_var = c("shrs4"))
p <- p + theme(strip.text.x = element_blank())
p <- p + xlab("Counterfactual Support 2005-2009 (p25, median, p75)")
#p <- p + labs(title = "Residual Imputed By Party Family")

##### XXX- plot PRRP vs all other parties -----------------------------

# Filter df_plot for non-NA values in BBF_parfam
df_plot <- df_plot[!is.na(df_plot$BBF_parfam), ]

# plugin BBF_parfam = "Non-PRRP" for any BBF_parfam != "PRRP"
df_plot$BBF_parfam <- ifelse(df_plot$BBF_parfam == "PRRP", "PRRP", "Non-PRRP")

# Calculate the percentile summary directly so this paper figure can be styled
# without changing the shared plot helper used by other outputs.
df_plot_summary <- df_plot %>%
  group_by(BBF_parfam) %>%
  summarise(
    median = as.numeric(Hmisc::wtd.quantile(value, BB_Weight_CW, 0.5)),
    pc75 = as.numeric(Hmisc::wtd.quantile(value, BB_Weight_CW, 0.75)),
    pc25 = as.numeric(Hmisc::wtd.quantile(value, BB_Weight_CW, 0.25)),
    .groups = "drop"
  ) %>%
  mutate(BBF_parfam = factor(BBF_parfam, levels = c("Non-PRRP", "PRRP")))

p <- ggplot(df_plot_summary, aes(y = BBF_parfam, color = BBF_parfam)) +
  geom_vline(xintercept = 0, linetype = "dashed",
             color = "black", linewidth = 0.8) +
  geom_boxplot(aes(xlower = pc25, xupper = pc75, xmiddle = median,
                   x = median, xmin = pc25, xmax = pc75),
               outlier.shape = NA, stat = "identity",
               width = 0.22, linewidth = 1.1, show.legend = FALSE) +
  scale_color_manual(values = c("PRRP" = "red", "Non-PRRP" = "black")) +
  scale_x_continuous(
    limits = c(-0.01, 0.20),
    breaks = seq(0, 0.20, 0.05),
    labels = function(x) sprintf("%.2f", x),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(x = "Counterfactual Support 2005-2009\n(p25, median, p75)", y = NULL) +
  theme_classic(base_size = 11.2) +
  theme(legend.position = "none",
        axis.title.x = element_text(size = 11.2),
        axis.text = element_text(size = 10, color = "black"),
        axis.line = element_line(linewidth = 0.9),
        axis.ticks = element_line(linewidth = 0.8),
        axis.ticks.length = grid::unit(0.14, "cm"),
        plot.margin = ggplot2::margin(2, 10, 2, 4))


# Save the plot
if (plugin_wave6 == 0){
  ggsave("output/figures/partyResidual/errbar_prrpShrs_vs_all.pdf",
         plot = p, width = figureWidth * 0.8, height = 8.0 * 0.8,
         units = "cm", dpi = 300)
}

# # add perceniles to df_shrs4
# percentiles <- df_shrs4 %>%
#   group_by(BBF_parfam) %>%
#   summarise(
#     p25 = quantile(shrs4, 0.25, na.rm = TRUE),
#     p50 = quantile(shrs4, 0.50, na.rm = TRUE),
#     p75 = quantile(shrs4, 0.75, na.rm = TRUE)
#   ) %>%
#   ungroup()
