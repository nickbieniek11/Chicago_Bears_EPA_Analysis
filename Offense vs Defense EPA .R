# ============================================================
# Offense vs Defense EPA by Week — Win/Loss shaded
# ============================================================

chi_both <- bind_rows(
  chi_offense |>
    group_by(week) |>
    summarise(
      epa       = mean(epa, na.rm = TRUE),
      result    = first(result),
      home_team = first(home_team),
      away_team = first(away_team),
      .groups   = "drop"
    ) |>
    mutate(side = "Offense"),
  
  chi_defense |>
    group_by(week) |>
    summarise(
      epa       = mean(epa, na.rm = TRUE),
      result    = first(result),
      home_team = first(home_team),
      away_team = first(away_team),
      .groups   = "drop"
    ) |>
    mutate(side = "Defense")
) |>
  mutate(outcome = case_when(
    home_team == "CHI" & result > 0 ~ "Win",
    home_team == "CHI" & result < 0 ~ "Loss",
    away_team == "CHI" & result < 0 ~ "Win",
    away_team == "CHI" & result > 0 ~ "Loss",
    TRUE ~ "Tie"
  ))

ggplot(chi_both, aes(x = week, y = epa, color = side)) +
  geom_rect(aes(xmin = week - 0.4, xmax = week + 0.4,
                ymin = -Inf, ymax = Inf, fill = outcome),
            alpha = 0.15, inherit.aes = FALSE) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  scale_color_manual(values = c("Offense" = "#0B162A", "Defense" = "#C83803")) +
  scale_fill_manual(values = c("Win" = "green", "Loss" = "red", "Tie" = "gray")) +
  scale_x_continuous(breaks = unique(chi_both$week)) +
  labs(
    title    = "Bears Offense vs Defense EPA per Play by Week",
    subtitle = "Green = Win, Red = Loss | Defense: lower is better",
    x        = "Week",
    y        = "Mean EPA",
    color    = "Side",
    fill     = "Outcome"
  ) +
  theme_minimal()
