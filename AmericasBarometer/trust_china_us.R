# Trust in the governments of China and the United States across Latin America
# Source: LAPOP AmericasBarometer (Grand Merge 2004-2023 + 2026 merged file)
# Outcome: % answering "very" or "somewhat" trustworthy (codes 1-2 of 1-4),
# weighted with weight1500 (comparable across countries and years).

library(haven)
library(dplyr)
library(tidyr)
library(ggplot2)

# 1. Import ---------------------------------------------------

files <- c(
  grand  = "import/Grand_Merge_2004-2023_LAPOP_AmericasBarometer_v1.0_FREE.dta",
  recent = "import/2026_merge_CGD_AmericasBarometer_v1.0p.dta"
)

# Read only the needed columns (the Grand Merge is ~1 GB)
read_lapop <- function(path) {
  read_dta(path, col_select = c(pais, year, weight1500, mil10a, mil10e)) |>
    mutate(
      country = as.character(as_factor(pais)),
      across(c(year, weight1500, mil10a, mil10e), \(x) as.numeric(zap_labels(x)))
    ) |>
    select(country, year, weight = weight1500, China = mil10a, `United States` = mil10e)
}

dat <- bind_rows(lapply(files, read_lapop))

# 2. Latin American countries (excludes US, Canada, English/Dutch Caribbean) ----

latam <- c(
  "México", "Guatemala", "El Salvador", "Honduras", "Nicaragua", "Costa Rica",
  "Panamá", "Colombia", "Ecuador", "Bolivia", "Perú", "Paraguay", "Chile",
  "Uruguay", "Brasil", "Argentina", "República Dominicana", "Haití", "Cuba",
  "Venezuela"
)

# 3. Weighted % trusting each government, by year -------------------

results <- dat |>
  filter(country %in% latam, !is.na(weight), weight > 0) |>
  # Merge fieldwork years that belong to one survey round
  mutate(
    round = case_when(
      year %in% 2016:2017 ~ "2016/17",
      year %in% 2018:2019 ~ "2018/19",
      year %in% 2025:2026 ~ "2025/26",
      .default = as.character(year)
    )
  ) |>
  filter(round >= "2012") |>
  pivot_longer(c(China, `United States`), names_to = "target", values_to = "response") |>
  filter(response %in% 1:4) |>
  summarise(
    percentage = 100 * weighted.mean(response <= 2, weight),
    n = n(),
    countries = n_distinct(country),
    .by = c(round, target)
  ) |>
  arrange(round, target)

results

# 4. Plot -----------------------------------------------------

# BBC-style theme (replicates bbplot::bbc_style(), sizes scaled for a 10 x 6 in figure)
bbc_style <- function() {
  font <- "sans"
  theme(
    plot.title = element_text(family = font, size = 16, face = "bold", colour = "#222222"),
    plot.subtitle = element_text(family = font, size = 9, margin = margin(0, 0, 9, 0)),
    plot.caption = element_text(family = font, size = 7, colour = "#222222"),
    legend.position = "top",
    legend.justification = "center",
    legend.text.align = 0,
    legend.background = element_blank(),
    legend.title = element_blank(),
    legend.key = element_blank(),
    legend.text = element_text(family = font, size = 10, colour = "#222222"),
    axis.title.x = element_text(family = font, size = 9, colour = "#222222"),
    axis.title.y = element_blank(),
    axis.text = element_text(family = font, size = 10, colour = "#222222"),
    axis.text.x = element_text(margin = margin(5, b = 10)),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    panel.background = element_blank(),
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(size = 10, hjust = 0)
  )
}

# Time-proportional x axis: each round is placed at mid-year of its first
# fieldwork year (2016/17 -> 2016.5, 2018/19 -> 2018.5, 2025/26 -> 2025.5).
# Administrations run from inauguration (20 January) to the next one.
results <- mutate(results, x = as.numeric(substr(round, 1, 4)) + 0.5)
round_breaks <- distinct(results, x, round)

administrations <- tibble::tribble(
  ~label,     ~colour,    ~xmin,   ~xmax,
  "Obama",    "#1D2FC0",  2012.25, 2017.05,
  "Trump I",  "#D10011",  2017.05, 2021.05,
  "Biden",    "#1D2FC0",  2021.05, 2025.05,
  "Trump II", "#D10011",  2025.05, 2026.5
) |>
  mutate(xmid = (xmin + xmax) / 2)

# Plot labels by language
labels_fr <- list(
  China = "Chine", `United States` = "États-Unis",
  title = "La confiance envers le gouvernement chinois dépasse\ncelle accordée aux États-Unis sous Trump",
  subtitle = "Tous les pays d'Amérique latine confondus",
  x = "Cycle d'enquête",
  caption = "Élaboration propre, source : LAPOP AmericasBarometer",
  pct = \(x) paste0(x, " %"),
  suffix = ""
)

labels_en <- list(
  China = "China", `United States` = "United States",
  title = "Trust in the Chinese government exceeds trust in\nthe United States under Trump",
  subtitle = "All Latin American countries combined",
  x = "Survey round",
  caption = "Source: LAPOP AmericasBarometer",
  pct = \(x) paste0(x, "%"),
  suffix = "_en"
)

plot_trust <- function(lab) {
  series_labels <- c(China = lab$China, `United States` = lab$`United States`)

  ggplot(results, aes(x, percentage)) +
    # I() keeps administration colours outside the China/US colour scale
    geom_rect(
      data = administrations,
      aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = I(colour)),
      alpha = 0.12, inherit.aes = FALSE, show.legend = FALSE
    ) +
    geom_hline(yintercept = c(25, 50, 75), colour = "grey60", linewidth = 0.3) +
    geom_text(
      data = administrations,
      aes(x = xmid, y = 92, label = label),
      colour = "black", size = 12 / .pt, inherit.aes = FALSE, show.legend = FALSE
    ) +
    geom_line(aes(colour = target, linetype = target, group = target), linewidth = 1) +
    geom_point(aes(colour = target, shape = target), size = 2.5) +
    scale_x_continuous(
      breaks = round_breaks$x, labels = round_breaks$round,
      limits = c(2012.25, 2026.5), expand = c(0, 0)
    ) +
    scale_y_continuous(limits = c(0, 100), labels = lab$pct) +
    scale_colour_manual(
      values = c(China = "#E63115", `United States` = "#232551"),
      labels = series_labels
    ) +
    # Line type and point shape differ so the series stay distinguishable in black and white
    scale_linetype_manual(
      values = c(China = "dashed", `United States` = "solid"),
      labels = series_labels
    ) +
    scale_shape_manual(
      values = c(China = 17, `United States` = 16),
      labels = series_labels
    ) +
    labs(
      title = lab$title,
      subtitle = lab$subtitle,
      x = lab$x,
      y = "",
      colour = NULL,
      linetype = NULL,
      shape = NULL,
      caption = lab$caption
    ) +
    bbc_style() +
    theme(
      plot.title = element_text(size = 16),
      plot.subtitle = element_text(size = 14),
      plot.caption = element_text(size = 10),
      axis.text = element_text(size = 14),
      axis.title.x = element_text(size = 12),
      legend.text = element_text(size = 14)
    )
}

# Publication: 600 dpi PNG plus a vector PDF, French and English versions
for (lab in list(labels_fr, labels_en)) {
  base <- paste0("export/trust_china_us_latin_america", lab$suffix)
  p <- plot_trust(lab)
  print(p)
  ggsave(paste0(base, ".png"), p, width = 8, height = 4.5, dpi = 600)
  ggsave(paste0(base, ".pdf"), p, width = 8, height = 4.5, device = cairo_pdf)
}

# Variation: colour-coded title, direct labels, no legend, both lines solid
library(ggtext)

plot_trust_direct <- function(lab) {
  # Okabe-Ito blue: colour-blind safe next to the red, and lighter than the black title text
  blue <- "#0072B2"
  # Midpoint between the two series at 2018/19 and 2025/26
  gap <- results |>
    filter(round %in% c("2018/19", "2025/26")) |>
    summarise(mid = mean(percentage), .by = c(round, x)) |>
    arrange(x)
  annotation <- lab$annotation
  # Centred on the 2021 round, between the 0 and 25% lines
  text_x <- results$x[results$round == "2021"][1]
  text_y <- 12.5
  # Cubic Bezier arrows: each leaves the text sideways, swings round the end of the
  # series and enters the gap from outside (left arrow: 2018/19, right arrow: 2025/26)
  tt <- seq(0, 1, length.out = 60)
  bezier <- \(p) (1 - tt)^3 * p[1] + 3 * (1 - tt)^2 * tt * p[2] + 3 * (1 - tt) * tt^2 * p[3] + tt^3 * p[4]
  arrow_path <- \(start, c1, c2, tip) {
    ctrl <- rbind(start, c1, c2, tip)
    data.frame(x = bezier(ctrl[, 1]), y = bezier(ctrl[, 2]))
  }
  y0 <- text_y + 5
  tip_l <- c(gap$x[1] - 0.15, gap$mid[1])
  tip_r <- c(gap$x[2] + 0.2, gap$mid[2])
  # Left arrow mirrors the right one around the text
  path_l <- arrow_path(c(text_x - lab$dx, y0), c(2 * text_x - 2026.3, y0), c(tip_l[1] - 0.65, tip_l[2] - 1), tip_l)
  path_r <- arrow_path(c(text_x + lab$dx, y0), c(2026.3, y0), c(tip_r[1] + 0.65, tip_r[2] - 1), tip_r)
  head <- arrow(length = unit(0.05, "inches"), angle = 18, type = "closed")

  ggplot(results, aes(x, percentage, colour = target)) +
    geom_rect(
      data = administrations,
      aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = I(colour)),
      alpha = 0.12, inherit.aes = FALSE, show.legend = FALSE
    ) +
    geom_hline(yintercept = c(25, 50, 75), colour = "grey60", linewidth = 0.3) +
    geom_text(
      data = administrations,
      aes(x = xmid, y = 92, label = label),
      colour = "black", size = 12 / .pt, inherit.aes = FALSE, show.legend = FALSE
    ) +
    geom_line(aes(group = target), linewidth = 1) +
    geom_point(size = 2.5) +
    # Annotation below the lines, with arrows to the gap between the series
    # in 2018/19 (slightly left) and in 2025/26
    annotate(
      "text", x = text_x, y = text_y, label = annotation,
      size = 10 / .pt, lineheight = 1.05, colour = "#222222"
    ) +
    geom_path(data = path_l, aes(x, y), inherit.aes = FALSE, arrow = head, colour = "#222222", linewidth = 0.4) +
    geom_path(data = path_r, aes(x, y), inherit.aes = FALSE, arrow = head, colour = "#222222", linewidth = 0.4) +
    scale_x_continuous(
      breaks = round_breaks$x, labels = round_breaks$round,
      limits = c(2012.25, 2026.5), expand = c(0, 0)
    ) +
    scale_y_continuous(limits = c(0, 100), labels = lab$pct) +
    scale_colour_manual(values = c(China = "#E63115", `United States` = blue)) +
    labs(
      title = lab$title,
      subtitle = lab$subtitle,
      x = lab$x,
      y = "",
      caption = lab$caption
    ) +
    bbc_style() +
    theme(
      legend.position = "none",
      plot.title = element_markdown(size = 16, face = "bold", lineheight = 1.1),
      plot.subtitle = element_text(size = 14),
      plot.caption = element_text(size = 10),
      axis.text = element_text(size = 14),
      axis.title.x = element_text(size = 12)
    )
}

direct_labels_fr <- list(
  title = paste0(
    "La confiance envers le gouvernement <span style='color:#E63115'>chinois</span> dépasse<br>",
    "celle accordée aux <span style='color:#0072B2'>États-Unis</span> sous Trump"
  ),
  subtitle = "Tous les pays d'Amérique latine confondus",
  annotation = "La confiance envers la Chine dépasse\ncelle envers le gouvernement américain",
  x = "Cycle d'enquête",
  caption = "Élaboration propre, source : LAPOP AmericasBarometer",
  pct = \(x) paste0(x, " %"),
  dx = 2.5,
  suffix = ""
)

direct_labels_en <- list(
  title = paste0(
    "Trust in the <span style='color:#E63115'>Chinese</span> government exceeds trust in<br>",
    "the <span style='color:#0072B2'>United States</span> under Trump"
  ),
  subtitle = "All Latin American countries combined",
  annotation = "Trust in China surpasses\ntrust in the U.S. government",
  x = "Survey round",
  caption = "Source: own elaboration, LAPOP AmericasBarometer",
  pct = \(x) paste0(x, "%"),
  dx = 1.7,
  suffix = "_en"
)

for (lab in list(direct_labels_fr, direct_labels_en)) {
  base <- paste0("export/trust_china_us_latin_america", lab$suffix, "_direct")
  p <- plot_trust_direct(lab)
  print(p)
  ggsave(paste0(base, ".png"), p, width = 8, height = 4.5, dpi = 600)
  ggsave(paste0(base, ".pdf"), p, width = 8, height = 4.5, device = cairo_pdf)
}

# 5. Robustness: balanced panel since 2016/17 -----------------------

long <- dat |>
  filter(country %in% latam, !is.na(weight), weight > 0) |>
  mutate(
    round = case_when(
      year %in% 2016:2017 ~ "2016/17",
      year %in% 2018:2019 ~ "2018/19",
      year %in% 2025:2026 ~ "2025/26",
      .default = as.character(year)
    )
  ) |>
  filter(round >= "2012") |>
  pivot_longer(c(China, `United States`), names_to = "target", values_to = "response") |>
  filter(response %in% 1:4)

# Countries with valid answers on both items in every round since 2016/17
rounds_since_2016 <- c("2016/17", "2018/19", "2021", "2023", "2025/26")

panel <- long |>
  filter(round %in% rounds_since_2016) |>
  distinct(country, round, target) |>
  count(country) |>
  filter(n == length(rounds_since_2016) * 2) |>
  pull(country)

panel

compare <- bind_rows(
  mutate(results, sample = "All available countries"),
  long |>
    filter(country %in% panel, round %in% rounds_since_2016) |>
    summarise(
      percentage = 100 * weighted.mean(response <= 2, weight),
      n = n(),
      countries = n_distinct(country),
      .by = c(round, target)
    ) |>
    mutate(sample = paste0("Balanced panel (", length(panel), " countries)"))
)

ggplot(compare, aes(round, percentage, colour = target, group = target)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  facet_wrap(vars(sample)) +
  scale_y_continuous(limits = c(0, 100), labels = \(x) paste0(x, "%")) +
  scale_colour_manual(values = c(China = "#E63115", `United States` = "#232551")) +
  labs(
    title = "Trust in the Chinese and US governments: pooled vs. balanced panel",
    x = "Survey round",
    y = "% who consider the government trustworthy",
    colour = NULL,
    caption = "Source: LAPOP AmericasBarometer. Weighted (weight1500); valid responses only."
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank())

ggsave("export/trust_china_us_panel_comparison.png", width = 12, height = 6, dpi = 300)

# 6. Country-level series for the balanced panel --------------------

by_country <- long |>
  filter(country %in% panel, round %in% rounds_since_2016) |>
  summarise(
    percentage = 100 * weighted.mean(response <= 2, weight),
    .by = c(country, round, target)
  )

# Same time-proportional axis, shading and styling as the main graph
by_country <- by_country |>
  mutate(
    x = as.numeric(substr(round, 1, 4)) + 0.5,
    country = recode(
      country,
      "México" = "Mexique", "El Salvador" = "Salvador", "Perú" = "Pérou",
      "República Dominicana" = "République dominicaine"
    )
  )

panel_administrations <- administrations |>
  mutate(xmin = pmax(xmin, 2016))

ggplot(by_country, aes(x, percentage)) +
  geom_rect(
    data = panel_administrations,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = I(colour)),
    alpha = 0.12, inherit.aes = FALSE, show.legend = FALSE
  ) +
  geom_hline(yintercept = c(25, 50, 75), colour = "grey60", linewidth = 0.3) +
  geom_line(aes(colour = target, linetype = target, group = target), linewidth = 0.9) +
  geom_point(aes(colour = target, shape = target), size = 2) +
  facet_wrap(vars(country), ncol = 4) +
  scale_x_continuous(
    breaks = filter(round_breaks, round %in% rounds_since_2016)$x,
    labels = filter(round_breaks, round %in% rounds_since_2016)$round,
    limits = c(2016, 2026.5), expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(0, 100), breaks = c(0, 25, 50, 75, 100),
    labels = \(x) paste0(x, " %")
  ) +
  scale_colour_manual(
    values = c(China = "#E63115", `United States` = "#232551"),
    labels = c(China = "Chine", `United States` = "États-Unis")
  ) +
  scale_linetype_manual(
    values = c(China = "dashed", `United States` = "solid"),
    labels = c(China = "Chine", `United States` = "États-Unis")
  ) +
  scale_shape_manual(
    values = c(China = 17, `United States` = 16),
    labels = c(China = "Chine", `United States` = "États-Unis")
  ) +
  labs(
    title = "La confiance envers la Chine et les États-Unis, pays par pays",
    subtitle = "Panel équilibré de 8 pays d'Amérique latine",
    x = "Cycle d'enquête",
    colour = NULL,
    linetype = NULL,
    shape = NULL,
    caption = "Élaboration propre, source : LAPOP AmericasBarometer"
  ) +
  bbc_style() +
  theme(
    axis.text = element_text(size = 8),
    axis.text.x = element_text(size = 7, margin = margin(5, b = 5)),
    strip.text = element_text(size = 10, face = "bold", hjust = 0),
    panel.spacing = unit(1, "lines")
  )

ggsave("export/trust_china_us_by_country.png", width = 13, height = 7, dpi = 300)

# 7. Trust by age group, latest round (2025/26) ---------------------

age_groups <- read_dta(
  files[["recent"]],
  col_select = c(pais, year, weight1500, mil10a, mil10e, q2)
) |>
  mutate(
    country = as.character(as_factor(pais)),
    across(c(year, weight1500, mil10a, mil10e, q2), \(x) as.numeric(zap_labels(x)))
  ) |>
  filter(country %in% latam, year %in% 2025:2026, !is.na(weight1500), weight1500 > 0) |>
  transmute(
    weight = weight1500,
    China = mil10a,
    `United States` = mil10e,
    age_group = cut(q2, c(15, 29, 44, 59, Inf), labels = c("16-29", "30-44", "45-59", "60+"))
  ) |>
  pivot_longer(c(China, `United States`), names_to = "target", values_to = "response") |>
  filter(!is.na(age_group), response %in% 1:4) |>
  summarise(
    percentage = 100 * weighted.mean(response <= 2, weight),
    n = n(),
    # Kish effective n; approximate CI that ignores clustering and stratification
    n_eff = sum(weight)^2 / sum(weight^2),
    .by = c(age_group, target)
  ) |>
  mutate(
    se = 100 * sqrt(percentage / 100 * (1 - percentage / 100) / n_eff),
    lo = percentage - 1.96 * se,
    hi = percentage + 1.96 * se,
    age_group = factor(age_group, levels = rev(levels(age_group)))
  )

age_groups

ggplot(age_groups, aes(percentage, age_group, colour = target, shape = target)) +
  geom_pointrange(aes(xmin = lo, xmax = hi), position = position_dodge(width = 0.5), size = 0.5) +
  scale_x_continuous(limits = c(32, 62), labels = \(x) paste0(x, " %")) +
  scale_colour_manual(
    values = c(China = "#E63115", `United States` = "#232551"),
    labels = c(China = "Chine", `United States` = "États-Unis")
  ) +
  scale_shape_manual(
    values = c(China = 17, `United States` = 16),
    labels = c(China = "Chine", `United States` = "États-Unis")
  ) +
  labs(
    title = "Confiance envers les gouvernements chinois et américain\nselon l’âge",
    subtitle = "Amérique latine, cycle d'enquête 2025-26",
    x = NULL,
    colour = NULL,
    shape = NULL,
    caption = "Élaboration propre, source : LAPOP AmericasBarometer"
  ) +
  bbc_style() +
  theme(
    panel.grid.major.x = element_line(colour = "grey85", linewidth = 0.3),
    plot.title = element_text(size = 16),
    plot.subtitle = element_text(size = 14),
    plot.caption = element_text(size = 10),
    axis.text = element_text(size = 14),
    legend.text = element_text(size = 14)
  )

# Publication: 600 dpi PNG plus a vector PDF
ggsave("export/trust_china_us_by_age.png", width = 8, height = 4.5, dpi = 600)
ggsave("export/trust_china_us_by_age.pdf", width = 8, height = 4.5, device = cairo_pdf)


# Variation: colour-coded title, no legend, round markers for both series
age_plot <- ggplot(age_groups, aes(percentage, age_group, colour = target)) +
  geom_pointrange(
    aes(xmin = lo, xmax = hi), shape = 16,
    position = position_dodge(width = 0.5), size = 0.5
  ) +
  # Direct labels between the 16-29 and 30-44 groups
  annotate("text", x = 41, y = 3.5, label = "États-Unis", colour = "#0072B2",
           hjust = 0, fontface = "bold", size = 13 / .pt) +
  annotate("text", x = 54.4, y = 3.5, label = "Chine", colour = "#E63115",
           fontface = "bold", size = 13 / .pt) +
  scale_x_continuous(limits = c(32, 62), labels = \(x) paste0(x, " %")) +
  scale_colour_manual(values = c(China = "#E63115", `United States` = "#0072B2")) +
  labs(
    title = paste0(
      "Confiance envers le gouvernement <span style='color:#E63115'>chinois</span> ",
      "et celui des<br><span style='color:#0072B2'>États-Unis</span> selon l’âge"
    ),
    subtitle = "Amérique latine, cycle d'enquête 2025-26",
    x = NULL,
    caption = "Graphique: Philippe Decker \u2022 Source: LAPOP AmericasBarometer"
  ) +
  bbc_style() +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_line(colour = "grey85", linewidth = 0.3),
    plot.title = ggtext::element_markdown(size = 16, face = "bold", lineheight = 1.1),
    plot.subtitle = element_text(size = 14),
    plot.caption = element_text(size = 10),
    axis.text = element_text(size = 14)
  )

age_plot
ggsave("export/trust_china_us_by_age_direct.png", age_plot, width = 8, height = 4.5, dpi = 600)
ggsave("export/trust_china_us_by_age_direct.pdf", age_plot, width = 8, height = 4.5, device = cairo_pdf)
