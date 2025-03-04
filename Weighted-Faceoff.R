### Loading necessary packages ###

suppressMessages(library(hockeyR))
suppressMessages(library(tidyverse))
suppressMessages(library(sportyR))
suppressMessages(library(dplyr))
suppressMessages(library(gt))
suppressMessages(library(ggplot2))

### Loading dataset and filtering ###

pbp <- load_pbp('2023-24') 
str(pbp)
pbp_faceoffs <- pbp %>% filter(event_type == "FACEOFF") 
ls(pbp_faceoffs)


### Calculating normal faceoff win percentage for each player ###

player_faceoff_percentage <- pbp_faceoffs %>%
  group_by(event_player_1_name) %>%  
  summarize(
    faceoff_wins = sum(event_player_1_type == "Winner", na.rm = TRUE),
    .groups = "drop"
  ) %>%
  full_join( 
    pbp_faceoffs %>%
      group_by(event_player_2_name) %>%
      summarize(
        faceoff_losses = sum(event_player_2_type == "Loser", na.rm = TRUE),
        .groups = "drop"
      ),
    by = c("event_player_1_name" = "event_player_2_name")
  ) %>%
  mutate(
    total_faceoffs = faceoff_wins + faceoff_losses, 
    win_percentage = (faceoff_wins / total_faceoffs) * 100  
  ) %>%
  arrange(desc(win_percentage)) %>% 
  select(event_player_1_name, win_percentage, total_faceoffs) 

### Calculate the median total faceoffs ###

median_total_faceoffs <- median(player_faceoff_percentage$total_faceoffs, na.rm = TRUE)

print(paste("Median Total Faceoffs:", median_total_faceoffs)) 


### Set the cutoff using median and filter ###

cutoff <- 72

filtered_player_faceoff_percentage <- player_faceoff_percentage %>%
  filter(total_faceoffs >= cutoff)

print(filtered_player_faceoff_percentage, width = Inf)


### Creating weighting system ###

pbp_faceoffs <- pbp_faceoffs %>%
  mutate(
    # Zone Score
    zone_score = case_when(
      zoneCode == "O" ~ 3,  # Offensive zone
      zoneCode == "N" ~ 1,  # Neutral zone
      zoneCode == "D" ~ 3,  # Defensive zone
      TRUE ~ 0 
    ),
    # Score Differential Score
    score_diff = abs(home_score - away_score), # Absolute difference
    score_diff_score = case_when(
      score_diff >= 3 ~ 1, # 3+ score game
      score_diff == 2 ~ 2, # 2 score game
      score_diff == 1 ~ 3, # 1 score game
      score_diff == 0 ~ 3  # Tie game
    ),
    # Period Score
    period_score = case_when(
      period == 1 ~ 1, # 1st period
      period == 2 ~ 2, # 2nd period
      period == 3 ~ 3, # 3rd period
      TRUE ~ 4         # Overtime
    ),
    # Time Remaining Score
    time_remaining_seconds = (period -1 ) * 1200 + period_seconds, #Seconds left in regulation
    time_remaining_score = case_when(
      time_remaining_seconds <= 300 ~ 3, # Last 5 min
      time_remaining_seconds <= 600 ~ 2, # Last 10 min
      time_remaining_seconds <= 900 ~ 1, # Last 15 min
      TRUE ~ 1
    ),
    # Strength State Score
    strength_score = ifelse(strength_code == "EV", 1, 3),  # Even strength least important
    
### Calculating Faceoff Importance Metric ###

    faceoff_importance = (.3 * zone_score) + (.2 * score_diff_score) + (.2 * period_score) + (.1 * time_remaining_score) + (.2 * strength_score)
  )


### Calculate weighted faceoff win percentage ###

weighted_player_faceoff_percentage <- pbp_faceoffs %>%
  group_by(event_player_1_name) %>% 
  summarize(
    weighted_wins = sum(ifelse(event_player_1_type == "Winner", faceoff_importance, 0), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  full_join(
    pbp_faceoffs %>%
      group_by(event_player_2_name) %>%
      summarize(
        weighted_losses = sum(ifelse(event_player_2_type == "Loser", faceoff_importance, 0), na.rm = TRUE),
        .groups = "drop"
      ),
    by = c("event_player_1_name" = "event_player_2_name")
  ) %>%
  mutate(
    total_weighted = coalesce(weighted_wins, 0) + coalesce(weighted_losses, 0),
    weighted_win_percentage = (weighted_wins / total_weighted) * 100
  ) %>%
  arrange(desc(weighted_win_percentage)) %>%
  select(event_player_1_name, weighted_win_percentage, total_weighted)

### Filtering based on cutoff for weighted faceoff ###

filtered_weighted_player_faceoff_percentage <- weighted_player_faceoff_percentage %>%
  filter(total_weighted >= 145)

print(filtered_weighted_player_faceoff_percentage, width = Inf)


### Comparison Table ~ not final table ###

comparison_df <- filtered_player_faceoff_percentage %>%  
  select(event_player_1_name, win_percentage) %>% 
  left_join(
    filtered_weighted_player_faceoff_percentage %>%
      select(event_player_1_name, weighted_win_percentage),  
    by = "event_player_1_name"
  ) %>%
  arrange(desc(win_percentage)) %>%
  mutate(rank = row_number()) %>% 
  arrange(desc(weighted_win_percentage)) %>% 
  mutate(weighted_rank = row_number()) 

comparison_df <- comparison_df %>%
  drop_na(weighted_win_percentage)

print(comparison_df, width = Inf)

### Calculate the rank difference between NFP & WFP ###

comparison_df <- comparison_df %>%
  mutate(rank_diff = rank - weighted_rank)

print(comparison_df, width = Inf)

### Print players with the biggest rank changes ###

biggest_movers <- comparison_df %>%
  arrange(desc(abs(rank_diff))) %>%
  head(20) 

print(biggest_movers, width=Inf)


### Merge the two dataframes ###

merged_df <- left_join(
  filtered_weighted_player_faceoff_percentage, 
  filtered_player_faceoff_percentage, 
  by = "event_player_1_name"
)

### Calculate importance_factor ###

merged_df_final <- merged_df %>%
  mutate(
    importance_factor = total_weighted / total_faceoffs
  )

### Most important faceoffs ranks ###

merged_df_final_desc <- merged_df_final %>%
  arrange(
    desc(importance_factor)
  )

print(merged_df_final_desc, width = Inf)


### Finding average difference in WFP - NFP ###

average_diff <- merged_df_final %>%
  summarize(avg_diff = mean(weighted_win_percentage - win_percentage, na.rm = TRUE))

print(average_diff)


### TABLES USED IN ARTICLE ###


### Table for Weighted Faceoff ###

merged_df_final %>%
  mutate(
    faceoff_percentage_diff = weighted_win_percentage - win_percentage
  ) %>%
  arrange(desc(weighted_win_percentage)) %>%  
  slice_head(n = 10) %>%  
  gt() %>%
  tab_header(
    title = "Top 10 Players by Weighted Faceoff Percentage",
    subtitle = "Comparison of Weighted vs. Regular Faceoff Performance"
  ) %>%
  fmt_number(
    columns = c(weighted_win_percentage, win_percentage, faceoff_percentage_diff, importance_factor),
    decimals = 2
  ) %>%
  cols_label(
    event_player_1_name = "Player",
    weighted_win_percentage = "Weighted Win %",
    win_percentage = "Regular Win %",
    faceoff_percentage_diff = "Diff (Weighted % - Regular %)",
    total_weighted = "Total Weighted",
    total_faceoffs = "Total Faceoffs",
    importance_factor = "Importance Factor"
  ) %>%
  tab_options(
    table.font.size = "small",
    column_labels.font.weight = "bold"
  )


### Table for Difference in Faceoff Percentages ###

merged_df_final %>%
  mutate(
    faceoff_percentage_diff = weighted_win_percentage - win_percentage 
  ) %>%
  arrange(desc(faceoff_percentage_diff)) %>%  
  slice_head(n = 15) %>%  
  gt() %>%
  tab_header(
    title = "Top 15 Players by Difference in Faceoff Percentage",
    subtitle = "Comparison of Weighted vs. Regular Faceoff Performance"
  ) %>%
  fmt_number(
    columns = c(weighted_win_percentage, win_percentage, faceoff_percentage_diff, importance_factor),
    decimals = 2
  ) %>%
  cols_label(
    event_player_1_name = "Player",
    weighted_win_percentage = "Weighted Win %",
    win_percentage = "Regular Win %",
    faceoff_percentage_diff = "Diff (Weighted % - Regular %)",
    total_weighted = "Total Weighted",
    total_faceoffs = "Total Faceoffs",
    importance_factor = "Importance Factor"
  ) %>%
  tab_options(
    table.font.size = "small",
    column_labels.font.weight = "bold"
  )

### Table for Difference in Faceoff Percentages ###

merged_df_final %>%
  mutate(
    faceoff_percentage_diff = weighted_win_percentage - win_percentage
  ) %>%
  arrange(faceoff_percentage_diff) %>%  
  slice_head(n = 15) %>%  
  gt() %>%
  tab_header(
    title = "Bottom 15 Players by Difference in Faceoff Percentage",
    subtitle = "Comparison of Weighted vs. Regular Faceoff Performance"
  ) %>%
  fmt_number(
    columns = c(weighted_win_percentage, win_percentage, faceoff_percentage_diff, importance_factor),
    decimals = 2
  ) %>%
  cols_label(
    event_player_1_name = "Player",
    weighted_win_percentage = "Weighted Win %",
    win_percentage = "Regular Win %",
    faceoff_percentage_diff = "Diff (Weighted % - Regular %)",
    total_weighted = "Total Weighted",
    total_faceoffs = "Total Faceoffs",
    importance_factor = "Importance Factor"
  ) %>%
  tab_options(
    table.font.size = "small",
    column_labels.font.weight = "bold"
  )


### Table for Importance Factor ###

merged_df_final %>%
  mutate(
    faceoff_percentage_diff = weighted_win_percentage - win_percentage 
  ) %>%
  arrange(desc(importance_factor)) %>%  
  slice_head(n = 10) %>%  
  gt() %>%
  tab_header(
    title = "Top 10 Players by Faceoff Importance",
    subtitle = "Comparison of Weighted vs. Regular Faceoff Performance"
  ) %>%
  fmt_number(
    columns = c(weighted_win_percentage, win_percentage, faceoff_percentage_diff, importance_factor),
    decimals = 2
  ) %>%
  cols_label(
    event_player_1_name = "Player",
    weighted_win_percentage = "Weighted Win %",
    win_percentage = "Regular Win %",
    faceoff_percentage_diff = "Diff (Weighted % - Regular %)",
    total_weighted = "Total Weighted",
    total_faceoffs = "Total Faceoffs",
    importance_factor = "Importance Factor"
  ) %>%
  tab_options(
    table.font.size = "small",
    column_labels.font.weight = "bold"
  )

### VISUALS USED IN ARTILCE ###


### Bar Graph Code ###

ggplot(biggest_movers, aes(x = reorder(event_player_1_name, rank_diff), y = rank_diff, fill = rank_diff > 0)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(
    values = c("green", "red"), 
    labels = c("Improved in rank", "Dropped in rank"),
    limits = c(TRUE, FALSE)  
  ) +
  coord_flip() +  
  labs(title = "Biggest Rank Movers (Rank of Weighted vs. Normal Faceoff %)", x = "Player", y = "Rank Difference") +
  theme_minimal() +
  theme(legend.title = element_blank())

### Scatterplot Graph Code ###

ggplot(merged_df_final, aes(x = total_faceoffs, y = weighted_win_percentage, size = importance_factor, color = importance_factor)) +
  geom_point(alpha = 0.8) +
  geom_smooth(method = "lm", color = "black", linetype = "dashed") + 
  scale_color_gradient(low = "blue", high = "red") + 
  scale_size_continuous(guide = "none") +  
  labs(title = "Weighted Faceoff % vs. Total Faceoffs", x = "Total Faceoffs Taken", y = "Weighted Faceoff %") +
  theme_minimal()

### END OF CODE FOR 2023-2024 SEASON ###


### 2022-2023 SEASON CODE (Repetitive) ###

pbp_22 <- load_pbp('2022-23')  
pbp_faceoffs_22 <- pbp_22 %>% filter(event_type == "FACEOFF")  

pbp_faceoffs_22 <- pbp_faceoffs_22 %>%
  mutate(
    zoneCode = case_when(
      x > 25  ~ "O",  
      x < -25 ~ "D",  
      TRUE    ~ "N"  
    )
  )

player_faceoff_percentage_22 <- pbp_faceoffs_22 %>%
  group_by(event_player_1_name) %>%
  summarize(
    faceoff_wins = sum(event_player_1_type == "Winner", na.rm = TRUE),
    .groups = "drop"
  ) %>%
  full_join(
    pbp_faceoffs_22 %>%
      group_by(event_player_2_name) %>%
      summarize(
        faceoff_losses = sum(event_player_2_type == "Loser", na.rm = TRUE),
        .groups = "drop"
      ),
    by = c("event_player_1_name" = "event_player_2_name")
  ) %>%
  mutate(
    total_faceoffs = faceoff_wins + faceoff_losses,
    win_percentage = (faceoff_wins / total_faceoffs) * 100
  ) %>%
  arrange(desc(win_percentage)) %>%
  select(event_player_1_name, win_percentage, total_faceoffs)

filtered_player_faceoff_percentage_22 <- player_faceoff_percentage_22 %>%
  filter(total_faceoffs >= cutoff)


pbp_faceoffs_22 <- pbp_faceoffs_22 %>%
  mutate(
    zone_score = case_when(
      zoneCode == "O" ~ 3,
      zoneCode == "N" ~ 1,
      zoneCode == "D" ~ 3,
      TRUE ~ 0
    ),
    score_diff = abs(home_score - away_score),
    score_diff_score = case_when(
      score_diff >= 3 ~ 1,
      score_diff == 2 ~ 2,
      score_diff == 1 ~ 3,
      score_diff == 0 ~ 3
    ),
    period_score = case_when(
      period == 1 ~ 1,
      period == 2 ~ 2,
      period == 3 ~ 3,
      TRUE ~ 4
    ),
    time_remaining_seconds = (period -1 ) * 1200 + period_seconds,
    time_remaining_score = case_when(
      time_remaining_seconds <= 300 ~ 3,
      time_remaining_seconds <= 600 ~ 2,
      time_remaining_seconds <= 900 ~ 1,
      TRUE ~ 1
    ),
    strength_score = ifelse(strength_code == "EV", 1, 3),
    
    faceoff_importance = (.3 * zone_score) + (.2 * score_diff_score) + 
      (.2 * period_score) + (.1 * time_remaining_score) + 
      (.2 * strength_score)
  )

weighted_player_faceoff_percentage_22 <- pbp_faceoffs_22 %>%
  group_by(event_player_1_name) %>%
  summarize(
    weighted_wins = sum(ifelse(event_player_1_type == "Winner", faceoff_importance, 0), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  full_join(
    pbp_faceoffs_22 %>%
      group_by(event_player_2_name) %>%
      summarize(
        weighted_losses = sum(ifelse(event_player_2_type == "Loser", faceoff_importance, 0), na.rm = TRUE),
        .groups = "drop"
      ),
    by = c("event_player_1_name" = "event_player_2_name")
  ) %>%
  mutate(
    total_weighted = coalesce(weighted_wins, 0) + coalesce(weighted_losses, 0),
    weighted_win_percentage = (weighted_wins / total_weighted) * 100
  ) %>%
  arrange(desc(weighted_win_percentage)) %>%
  select(event_player_1_name, weighted_win_percentage, total_weighted)

filtered_weighted_player_faceoff_percentage_22 <- weighted_player_faceoff_percentage_22 %>%
  filter(total_weighted >= 145)

comparison_df_22 <- filtered_player_faceoff_percentage_22 %>%
  select(event_player_1_name, win_percentage) %>%
  left_join(
    filtered_weighted_player_faceoff_percentage_22 %>%
      select(event_player_1_name, weighted_win_percentage),
    by = "event_player_1_name"
  ) %>%
  arrange(desc(win_percentage)) %>%
  mutate(rank = row_number()) %>%
  arrange(desc(weighted_win_percentage)) %>%
  mutate(weighted_rank = row_number())

comparison_df_22 <- comparison_df_22 %>%
  mutate(rank_diff = rank - weighted_rank)

biggest_movers_22 <- comparison_df_22 %>%
  arrange(desc(abs(rank_diff))) %>%
  head(20)

print(biggest_movers_22, width = Inf)

merged_df_22 <- left_join(
  filtered_weighted_player_faceoff_percentage_22,
  filtered_player_faceoff_percentage_22,
  by = "event_player_1_name"
) %>%
  mutate(importance_factor = total_weighted / total_faceoffs)

merged_df_final_desc_22 <- merged_df_22 %>%
  arrange(desc(importance_factor))

print(merged_df_final_desc_22, width = Inf)

average_diff_22 <- merged_df_22 %>%
  summarize(avg_diff = mean(weighted_win_percentage - win_percentage, na.rm = TRUE))

print(average_diff_22)

comparison_df_22 <- comparison_df_22 %>%
  mutate(event_player_1_name = str_replace_all(event_player_1_name, "\\.", " "))


comparison_df_22 <- comparison_df_22 %>%
  mutate(diff = weighted_win_percentage - win_percentage)

output_columns <- c("event_player_1_name", "weighted_win_percentage", "win_percentage", "diff")


biggest_positive_movers_22 <- comparison_df_22 %>%
  arrange(desc(diff)) %>%
  head(15) %>%
  select(all_of(output_columns))

print(biggest_positive_movers_22, width = Inf)

biggest_negative_movers_22 <- comparison_df_22 %>%
  arrange(diff) %>%
  head(15) %>%
  select(all_of(output_columns))

print(biggest_negative_movers_22, width = Inf)


### 2021-2022 SEASON CODE (Repetitive) ###

pbp_21_22 <- load_pbp("2021-22") 
str(pbp_21_22) 

pbp_faceoffs_21_22 <- pbp_21_22 %>% filter(event_type == "FACEOFF")

pbp_faceoffs_21_22 <- pbp_faceoffs_21_22 %>%
  mutate(
    zoneCode = case_when(
      x < -25 ~ "D",  # Defensive zone
      x > 25 ~ "O",   # Offensive zone
      TRUE ~ "N"      # Neutral zone
    )
  )

player_faceoff_percentage_21_22 <- pbp_faceoffs_21_22 %>%
  group_by(event_player_1_name) %>%
  summarize(
    faceoff_wins = sum(event_player_1_type == "Winner", na.rm = TRUE),
    .groups = "drop"
  ) %>%
  full_join(
    pbp_faceoffs_21_22 %>%
      group_by(event_player_2_name) %>%
      summarize(
        faceoff_losses = sum(event_player_2_type == "Loser", na.rm = TRUE),
        .groups = "drop"
      ),
    by = c("event_player_1_name" = "event_player_2_name")
  ) %>%
  mutate(
    total_faceoffs = faceoff_wins + faceoff_losses,
    win_percentage = (faceoff_wins / total_faceoffs) * 100
  ) %>%
  filter(total_faceoffs >= 72) %>%  
  select(event_player_1_name, win_percentage, total_faceoffs)


pbp_faceoffs_21_22 <- pbp_faceoffs_21_22 %>%
  mutate(
    zone_score = case_when(
      zoneCode == "O" ~ 3,
      zoneCode == "N" ~ 1,
      zoneCode == "D" ~ 3,
      TRUE ~ 0
    ),
    score_diff = abs(home_score - away_score),
    score_diff_score = case_when(
      score_diff >= 3 ~ 1,
      score_diff == 2 ~ 2,
      score_diff == 1 ~ 3,
      score_diff == 0 ~ 3
    ),
    period_score = case_when(
      period == 1 ~ 1,
      period == 2 ~ 2,
      period == 3 ~ 3,
      TRUE ~ 4
    ),
    time_remaining_seconds = (period - 1) * 1200 + period_seconds,
    time_remaining_score = case_when(
      time_remaining_seconds <= 300 ~ 3,
      time_remaining_seconds <= 600 ~ 2,
      time_remaining_seconds <= 900 ~ 1,
      TRUE ~ 1
    ),
    strength_score = ifelse(strength_code == "EV", 1, 3),
    faceoff_importance = (.3 * zone_score) + (.2 * score_diff_score) + 
      (.2 * period_score) + (.1 * time_remaining_score) + 
      (.2 * strength_score)
  )

weighted_player_faceoff_percentage_21_22 <- pbp_faceoffs_21_22 %>%
  group_by(event_player_1_name) %>%
  summarize(
    weighted_wins = sum(ifelse(event_player_1_type == "Winner", faceoff_importance, 0), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  full_join(
    pbp_faceoffs_21_22 %>%
      group_by(event_player_2_name) %>%
      summarize(
        weighted_losses = sum(ifelse(event_player_2_type == "Loser", faceoff_importance, 0), na.rm = TRUE),
        .groups = "drop"
      ),
    by = c("event_player_1_name" = "event_player_2_name")
  ) %>%
  mutate(
    total_weighted = coalesce(weighted_wins, 0) + coalesce(weighted_losses, 0),
    weighted_win_percentage = (weighted_wins / total_weighted) * 100
  ) %>%
  filter(total_weighted >= 145) %>%  
  select(event_player_1_name, weighted_win_percentage, total_weighted)

merged_df_21_22 <- left_join(
  weighted_player_faceoff_percentage_21_22, 
  player_faceoff_percentage_21_22, 
  by = "event_player_1_name"
) %>%
  mutate(
    importance_factor = total_weighted / total_faceoffs,
    rank_diff = weighted_win_percentage - win_percentage,
    event_player_1_name = str_replace_all(event_player_1_name, "\\.", " ")
  )

top_positive_movers_21_22 <- merged_df_21_22 %>%
  arrange(desc(rank_diff)) %>%
  select(event_player_1_name, weighted_win_percentage, win_percentage, rank_diff) %>%
  head(15)

print(top_positive_movers_21_22, width = Inf)

top_negative_movers_21_22 <- merged_df_21_22 %>%
  arrange(rank_diff) %>%
  select(event_player_1_name, weighted_win_percentage, win_percentage, rank_diff) %>%
  head(15)

print(top_negative_movers_21_22, width = Inf)
