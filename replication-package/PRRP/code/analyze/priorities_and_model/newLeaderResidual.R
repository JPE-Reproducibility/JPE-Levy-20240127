source("code/util/commonVars.R")
source("code/model/decompose_delta.R")
source("code/model/func_nuclear_and_pgd.R")
source("code/analyze/party_positions/manifesto_general_functions.R")

library(ggplot2)
library(dplyr)

# Build residual changes for one wave pair and attach leader-change status.
build_residual_pair <- function(res_df, leader_flags, waves) {
  dat_wave <- leader_flags %>%
    filter(BB_Wave_num == as.numeric(waves[2])) %>%
    select(manifestoKey, newLeader)

  res_df %>%
    select(all_of(c(waves[1], waves[2], "manifestoKey"))) %>%
    mutate(manifestoKey = as.numeric(manifestoKey)) %>%
    left_join(dat_wave, by = "manifestoKey") %>%
    mutate(diff = .data[[waves[2]]] - .data[[waves[1]]]) %>%
    filter(is.finite(diff), !is.na(newLeader)) %>%
    mutate(group = ifelse(newLeader == 1, "New Leader", "Same Leader"))
}

# Plot histogram and density summaries for one wave-pair residual change.
plot_residual_change_distribution <- function(combined_data, output_file) {
  newLead <- subset(combined_data, group == "New Leader")
  oldLead <- subset(combined_data, group == "Same Leader")

  mean_new_leader <- mean(abs(newLead$diff), na.rm = TRUE)
  sd_new_leader <- sd(newLead$diff, na.rm = TRUE)
  mean_same_leader <- mean(abs(oldLead$diff), na.rm = TRUE)
  sd_same_leader <- sd(oldLead$diff, na.rm = TRUE)

  facetedPlot <- ggplot(combined_data, aes(x = diff, fill = group)) +
    geom_histogram(binwidth = 0.1, color = "black", alpha = 0.7) +
    scale_fill_manual(values = c("Same Leader" = "skyblue", "New Leader" = "lightcoral")) +
    labs(
      title = "",
      x = "Difference in Valence (diff)",
      y = "Frequency",
      fill = paste0(
        "New Leader (Mean Abs. Diff): ", round(mean_new_leader, 2), " SD: ", round(sd_new_leader,2), "\n",
        "Same Leader (Mean Abs. Diff): ", round(mean_same_leader, 2), " SD: ", round(sd_same_leader,2)
      )
    ) +
    theme_minimal() +
    theme(legend.position = "top") +
    facet_wrap(~ group, ncol = 1, scales = "fixed")

  output_file_density <- sub(".pdf", "_density.pdf", output_file)
  legend_new <- paste0("New Leader:\nMean Absolute Diff: ", round(mean(abs(newLead$diff), na.rm = TRUE), 2),
                       " (SD: ", round(sd(newLead$diff, na.rm = TRUE), 2), ")")
  legend_old <- paste0("Same Leader:\nMean Absolute Diff: ", round(mean(abs(oldLead$diff), na.rm = TRUE), 2),
                       " (SD: ", round(sd(oldLead$diff, na.rm = TRUE), 2), ")")

  combined_data$group_label <- ifelse(combined_data$group == "New Leader", legend_new, legend_old)

  densityPlot <- ggplot(combined_data, aes(x = diff, color = group_label, linetype = group_label)) +
    geom_density(linewidth = 1.6, key_glyph = ggplot2::draw_key_path) +
    scale_color_manual(values = setNames(c("#216869", "#b2182b"), c(legend_new, legend_old))) +
    scale_linetype_manual(values = setNames(c("dashed", "solid"), c(legend_new, legend_old))) +
    scale_x_continuous(expand = ggplot2::expansion(mult = c(0.01, 0.01))) +
    scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.03))) +
    labs(title = "", x = "Change in Residual", y = "Density", color = "", linetype = "") +
    theme_minimal(base_size = 13) +
    theme(
      legend.position = "top",
      legend.title = element_blank(),
      legend.text = element_text(size = 8.5, lineheight = 0.9),
      legend.key.width = grid::unit(1.1, "cm"),
      legend.key.height = grid::unit(0.35, "cm"),
      legend.spacing.x = grid::unit(0.25, "cm"),
      legend.margin = ggplot2::margin(0, 0, 2, 0),
      axis.title = element_text(size = 13),
      axis.text = element_text(size = 11),
      axis.line = element_line(colour = "black", linewidth = 1),
      axis.ticks = element_line(colour = "black", linewidth = 1),
      panel.border = element_blank(),
      panel.grid.minor = element_blank(),
      plot.margin = ggplot2::margin(2, 4, 2, 4, "pt")
    ) +
    geom_vline(xintercept = 0, color = "black", linetype = "solid", linewidth = 1.2)

  ggsave(densityPlot, file = output_file_density, height = 10.5, width = figureWidth, units = "cm")
}

dat <- readRDS(file.path(cleandata_path, "leaders", "leader_changes.rds"))

# run decomposition returning values at the party level -------------------
# Reload the shared decomposition helper into this script environment.
source("code/model/func_nuclear_and_pgd.R", local = TRUE)

strt=1
final=3
k = 5
quad = F
dec.order = c("Z", "resid", "X", "param")
c1 <- decomposition(X.list, Z.list, Y.list,
                    phi.list, FE.list, party.id.list,
                    X.country, Z.country, phi,
                    W.list = W.list, k = k, quad = quad,
                    X = 1,
                    param = 1,
                    Z = 1,
                    resid = "*",
                    return.party.level = TRUE)

res_df <- as.data.frame(c1$vel.new)
colnames(res_df) <- seq_len(ncol(res_df)) + 3
res_df <- res_df %>% mutate(manifestoKey = rownames(res_df))

wave_pairs <- list(
  list(waves = c("4", "6"), output = "output/figures/newLeaderResidual/waves46.pdf")
)

### PLOT differences in valence by new/same leader status for each wave pair
for (pair in wave_pairs) {
  # Report the wave pair currently being processed.
  print(paste("Processing waves:", pair$waves[1], "and", pair$waves[2]))
  waves <- pair$waves
  output_file <- pair$output

  # Build the wave-pair residual-change data and save both summary plots.
  combined_data <- build_residual_pair(res_df, dat, waves)
  plot_residual_change_distribution(combined_data, output_file)
}
