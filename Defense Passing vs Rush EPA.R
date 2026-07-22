

# ============================================================
# Defensive Pass vs Rush EPA by Week — Win/Loss shaded
# ============================================================

chi_defense_split <- chi_defense |>
  group_by(week) |>
  summarise(
    pass_epa  = mean(epa[play_type == "pass"], na.rm = TRUE),
    rush_epa  = mean(epa[play_type == "run"], na.rm = TRUE),
    result    = first(result),
    home_team = first(home_team),
    away_team = first(away_team),
    .groups   = "drop"
  ) |>
  mutate(outcome = case_when(
    home_team == "CHI" & result > 0 ~ "Win",
    home_team == "CHI" & result < 0 ~ "Loss",
    away_team == "CHI" & result < 0 ~ "Win",
    away_team == "CHI" & result > 0 ~ "Loss",
    TRUE ~ "Tie"
  )) |>
  pivot_longer(cols = c(pass_epa, rush_epa),
               names_to  = "epa_type",
               values_to = "epa")

ggplot(chi_defense_split, aes(x = week, y = epa, color = epa_type)) +
  geom_rect(aes(xmin = week - 0.4, xmax = week + 0.4,
                ymin = -Inf, ymax = Inf, fill = outcome),
            alpha = 0.15, inherit.aes = FALSE) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  scale_color_manual(values = c("pass_epa" = "#0B162A", "rush_epa" = "#C83803")) +
  scale_fill_manual(values = c("Win" = "green", "Loss" = "red", "Tie" = "gray")) +
  scale_x_continuous(breaks = unique(chi_defense_split$week)) +
  labs(
    title    = "Bears Defense EPA Allowed per Play by Week",
    subtitle = "Green shading = Win, Red shading = Loss | Lower is better for defense",
    x        = "Week",
    y        = "Mean EPA Allowed",
    color    = "Play Type",
    fill     = "Outcome"
  ) +
  theme_minimal()