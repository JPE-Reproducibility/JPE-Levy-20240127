# Match party-leader observations to the nearest WVS wave and flag leader changes.
prepare_new_leader_flags <- function(wvs, leader_data) {
  leader_data <- leader_data %>%
    rowwise() %>%
    mutate(
      BB_Wave_num = {
        diffs <- abs(wvs$manifestoYear - manifestoYear)
        min_indices <- which(diffs == min(diffs))
        selected_index <- min_indices[which.max(wvs$manifestoYear[min_indices])]
        wvs$BB_Wave_num[selected_index]
      }
    ) %>%
    ungroup()

  leader_data <- leader_data %>% subset(select=c("country", "party_name", "year", "BB_Wave_num",
                                                 "manifestoKey", "manifestoYear", "leader"))

  leader_data %>%
    group_by(manifestoYear) %>%
    summarise(
      manifestoKey = dplyr::first(manifestoKey),
      BB_Wave_num = dplyr::first(BB_Wave_num),
      leader = paste(leader, collapse = ","),
      .groups = "drop"
    ) %>%
    arrange(manifestoKey, BB_Wave_num) %>%
    group_by(manifestoKey) %>%
    mutate(newLeader = lag(leader) != leader) %>%
    ungroup()
}

