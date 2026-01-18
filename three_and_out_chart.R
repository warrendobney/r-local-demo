library(dplyr)
library(ggplot2)
library(nflfastR)
library(scales)
library(tidyr)

season <- 2025
team <- "PHI"

# Load drive-level data for the season.
drives <- nflfastR::load_drives(seasons = season)

team_drives <- drives %>%
  filter(posteam == team) %>%
  mutate(
    three_and_out = plays == 3 & drive_result == "Punt",
    opponent = if_else(posteam == home_team, away_team, home_team),
    game_label = paste0("W", week, " vs ", opponent)
  )

game_summary <- team_drives %>%
  group_by(game_id, week, game_label) %>%
  summarise(
    total_possessions = n(),
    three_and_outs = sum(three_and_out, na.rm = TRUE),
    pct_three_and_out = three_and_outs / total_possessions,
    pct_not_three_and_out = 1 - pct_three_and_out,
    .groups = "drop"
  ) %>%
  arrange(week)

plot_data <- game_summary %>%
  pivot_longer(
    cols = c(pct_three_and_out, pct_not_three_and_out),
    names_to = "result",
    values_to = "percentage"
  ) %>%
  mutate(
    result = recode(
      result,
      pct_three_and_out = "3 and out",
      pct_not_three_and_out = "Not 3 and out"
    ),
    game_label = factor(game_label, levels = unique(game_label))
  )

plot <- ggplot(plot_data, aes(x = game_label, y = percentage, fill = result)) +
  geom_col(width = 0.7) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_fill_manual(values = c("3 and out" = "green4", "Not 3 and out" = "steelblue")) +
  labs(
    title = "Eagles 2025 Offensive Possessions Ending in 3 and Out",
    x = "Game",
    y = "Percentage of possessions",
    fill = NULL
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(plot)

ggsave(
  filename = "eagles_2025_three_and_outs.png",
  plot = plot,
  width = 10,
  height = 6,
  dpi = 300
)
