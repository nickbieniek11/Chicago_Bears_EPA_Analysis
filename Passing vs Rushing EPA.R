

# ============================================================
# Offensive Pass vs Rush EPA by Week — Win/Loss shaded
# ============================================================

chi_offense_split <- chi_offense |>
  group_by(week) |>
  summarise(
    pass_epa = mean(epa[play_type == "pass"], na.rm = TRUE),
    rush_epa = mean(epa[play_type == "run"], na.rm = TRUE),
    result   = first(result),
    home     = first(home_team),
    .groups  = "drop"
  ) |>
  mutate(outcome = case_when(
    home == "CHI" & result > 0 ~ "Win",
    home == "CHI" & result < 0 ~ "Loss",
    home != "CHI" & result < 0 ~ "Win",
    home != "CHI" & result > 0 ~ "Loss",
    TRUE ~ "Tie"
  )) |>
  pivot_longer(cols = c(pass_epa, rush_epa),
               names_to  = "epa_type",
               values_to = "epa")

ggplot(chi_offense_split, aes(x = week, y = epa, color = epa_type)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  geom_rect(aes(xmin = week - 0.4, xmax = week + 0.4,
                ymin = -Inf, ymax = Inf, fill = outcome),
            alpha = 0.15, inherit.aes = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  scale_color_manual(values = c("pass_epa" = "#0B162A", "rush_epa" = "#C83803")) +
  scale_fill_manual(values = c("Win" = "green", "Loss" = "red")) +
  scale_x_continuous(breaks = chi_offense_split$week) +
  labs(
    title    = "Bears Pass vs Rush EPA per Play by Week",
    subtitle = "Green shading = Win, Red shading = Loss",
    x        = "Week",
    y        = "Mean EPA",
    color    = "Play Type",
    fill     = "Outcome"
  ) +
  theme_minimal()
