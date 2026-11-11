library(comtradr)
library(tidyverse)

# Key is read from the COMTRADE_PRIMARY_KEY environment variable (set in ~/.Renviron)
set_primary_comtrade_key(Sys.getenv("COMTRADE_PRIMARY_KEY"))

latam <- c(
  "ARG", "BOL", "BRA", "CHL", "COL", "CRI", "CUB", "DOM", "ECU", "SLV",
  "GTM", "HND", "MEX", "NIC", "PAN", "PRY", "PER", "URY", "VEN"
)

# Pull one reporter at a time to stay within free-tier request limits
get_exports <- function(iso) {
  # The API allows at most 12 consecutive years per query
  windows <- list(c(2005, 2014), c(2015, 2025))
  map(windows, \(w) {
    Sys.sleep(1)
    tryCatch(
      ct_get_data(
        reporter = iso,
        partner = c("USA", "CHN"),
        commodity_code = "TOTAL",
        flow_direction = "export",
        start_date = w[1],
        end_date = w[2],
        frequency = "A"
      ),
      error = function(e) {
        message("Failed for ", iso, " ", w[1], "-", w[2], ": ", conditionMessage(e))
        NULL
      }
    )
  }) |> list_rbind()
}

raw <- map(latam, get_exports) |> list_rbind()
saveRDS(raw, "import/comtrade_latam_exports.rds")

trade <- raw |>
  select(year = period, reporter = reporter_desc, partner = partner_desc, value = primary_value) |>
  mutate(year = as.integer(year))

# One plot per country
plots <- trade |>
  split(~reporter) |>
  map(\(d) {
    ggplot(d, aes(year, value / 1e9, color = partner)) +
      geom_line(linewidth = 1) +
      labs(title = paste("Exports from", d$reporter[1]), x = NULL,
           y = "Exports (USD billions)", color = "Destination")
  })

plots

# ---- Top trading partners plot for any country ----
library(bbplot)

# Current EU27 members; the EU is summed from members for a consistent series
eu27 <- c(
  "AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", "FIN", "FRA", "DEU", "GRC", "HUN", "IRL",
  "ITA", "LVA", "LTU", "LUX", "MLT", "NLD", "POL", "PRT", "ROU", "SVK", "SVN", "ESP", "SWE"
)

# Fixed colour per partner so it is identical in every plot; China is the most salient.
# Partners not listed get a stable colour from a fallback pool (based on their name).
partner_colors <- c(
  "China" = "#E4002B",
  "USA" = "#1F5FA8",
  "EU27" = "#E0A100",
  "Brazil" = "#2E8B57",
  "Argentina" = "#7FB7E6",
  "Chile" = "#8E6BBF",
  "Mexico" = "#5B8C5A",
  "Colombia" = "#C9A66B",
  "Peru" = "#B07A9E",
  "Canada" = "#4A4A4A",
  "India" = "#E8873A",
  "Japan" = "#7A9CA8",
  "Rep. of Korea" = "#3FA7A0",
  "United Kingdom" = "#8C8C8C",
  "Switzerland" = "#A86B4F",
  "Paraguay" = "#9AC26B",
  "Uruguay" = "#6F8FD8",
  "Bolivia (Plurinational State of)" = "#B8B04A",
  "Ecuador" = "#D18AA5",
  "Guatemala" = "#5FA8A0",
  "Panama" = "#C97B63",
  "Honduras" = "#7C9A3D",
  "El Salvador" = "#9A8FC4",
  "Taiwan" = "#D4A5A5"
)
fallback_colors <- c("#6D6875", "#99A88C", "#B5838D", "#8DA9C4", "#A68A64", "#7D8F69", "#9C89B8", "#C3B59F")

colors_for <- function(partners) {
  partners <- as.character(partners)
  cols <- partner_colors[partners]
  missing <- is.na(cols)
  if (any(missing)) {
    idx <- vapply(partners[missing], \(p) sum(utf8ToInt(p)) %% length(fallback_colors) + 1, numeric(1))
    cols[missing] <- fallback_colors[idx]
  }
  set_names(cols, partners)
}

# Download exports + imports with all partners; cached in import/ unless refresh = TRUE
get_partner_trade <- function(iso, start_year, end_year, refresh = FALSE) {
  cache <- file.path("import", paste0("comtrade_", tolower(iso), "_partners.rds"))
  if (file.exists(cache) && !refresh) {
    cached <- readRDS(cache)
    # Compare with the requested range, since some countries lack data for some years
    req <- attr(cached, "requested")
    if (!is.null(req) && req[1] <= start_year && req[2] >= end_year) {
      return(cached)
    }
  }
  # The API allows at most 12 consecutive years per query
  starts <- seq(start_year, end_year, by = 12)
  raw <- map(starts, \(s) {
    ct_get_data(
      reporter = iso,
      partner = "all_countries",
      flow_direction = c("Export", "Import"),
      commodity_code = "TOTAL",
      start_date = s,
      end_date = min(s + 11, end_year),
      frequency = "A"
    )
  }) |> list_rbind()
  attr(raw, "requested") <- c(start_year, end_year)
  dir.create("import", showWarnings = FALSE)
  saveRDS(raw, cache)
  raw
}

# plot_top_partners(): line chart of a country's top trading partners over time.
#
# Arguments
#   iso         Reporter ISO3 code, e.g. "ARG", "CHL", "MEX"
#   start_year  First year (default 2000)
#   end_year    Last year (default 2025)
#   n           Number of partners shown (default 5)
#   refresh     TRUE forces a new download instead of using the cache in import/ (default FALSE)
#
# Partners are ranked by trade (exports + imports) in the latest year with data;
# EU27 members are summed into one bloc. Returns the ggplot and saves a PNG to export/.
#
# Notes on the data
# - "Other Asia, nes" is Comtrade's category for Asian trade that isn't attributed to a named
#   country. Reporters mostly use it for Taiwan, but it could also include other small Asian
#   partners. It is treated as Taiwan here, which is a standard approximation, not an exact match.
# - Cuba: data is missing for 2007-2015, which shows as a gap in the plot. The ranking uses
#   2022 as its latest year.
# - Venezuela: data stops at 2013, and its USA series swings sharply in 2007-2008, which
#   looks like a reporting problem.
plot_top_partners <- function(iso, start_year = 2000, end_year = 2025, n = 5, refresh = FALSE) {
  iso <- toupper(iso)
  raw <- get_partner_trade(iso, start_year, end_year, refresh)
  country <- na.omit(raw$reporter_desc)[1]

  trade <- raw |>
    # Drop empty placeholder rows and non-country categories (Free Zones, regional "nes" groups)
    # Comtrade reports Taiwan under "Other Asia, nes"
    mutate(partner_desc = if_else(partner_desc == "Other Asia, nes", "Taiwan", partner_desc)) |>
    filter(
      !is.na(period),
      # Exclude the reporter itself (e.g. Panama's records of imports "from" Panama)
      partner_iso != reporter_iso,
      !partner_desc %in% c("World", "Free Zones"),
      !str_detect(partner_desc, ", nes$")
    ) |>
    mutate(
      year = as.integer(period),
      partner = if_else(partner_iso %in% eu27, "EU27", partner_desc)
    ) |>
    summarise(value = sum(primary_value, na.rm = TRUE), .by = c(year, partner))

  latest <- max(trade$year)
  top_partners <- trade |>
    filter(year == latest) |>
    slice_max(value, n = n) |>
    pull(partner)

  p <- trade |>
    filter(partner %in% top_partners) |>
    # Make missing years explicit so lines break instead of bridging gaps
    complete(year = full_seq(year, 1), partner) |>
    mutate(partner = factor(partner, levels = top_partners)) |>
    ggplot(aes(year, value / 1e9, color = partner)) +
    geom_line(linewidth = 1) +
    geom_hline(yintercept = 0, linewidth = 1, colour = "#333333") +
    scale_color_manual(values = colors_for(top_partners)) +
    labs(
      title = paste0(country, "'s top ", n, " trading partners"),
      subtitle = "Exports + imports",
      y = "USD billions",
      caption = "Source: UN Comtrade"
    ) +
    bbc_style() +
    # bbc_style() hides axis titles by default
    theme(
      axis.title.y = element_text(size = 14, colour = "#222222", hjust = 1),
      plot.caption = element_text(size = 10, colour = "#555555", hjust = 1)
    )

  dir.create("export", showWarnings = FALSE)
  ggsave(file.path("export", paste0(tolower(iso), "_top", n, "_partners.png")), p,
         width = 640 / 72, height = 450 / 72, dpi = 72)
  p
}

# Examples
# plot_top_partners("ARG")                                 # defaults: 2000-2025, top 5
# plot_top_partners("CHL", start_year = 2010)              # shorter period
# plot_top_partners("BRA", n = 8)                          # top 8 partners
# plot_top_partners("MEX", start_year = 2015, end_year = 2024, n = 6)
# plot_top_partners("COL", refresh = TRUE)                 # re-download instead of using the cache

# ---- Heatmap: trade share by country and partner ----
# plot_partner_heatmap(): rows are Latin American countries (A-Z from top to bottom), columns
# are trading partners. Cells show each partner's share of the country's total trade
# (exports + imports) in `year`. Partners shown = union of each country's top `n` partners.
# Grey cells = no data for that country in `year` (e.g. Cuba, Venezuela, Honduras for 2025).
# Uses the cached files in import/ (run plot_top_partners() or get_partner_trade() first).
plot_partner_heatmap <- function(year = 2025, n = 5, countries = latam) {
  raw <- map(countries, \(iso) {
    get_partner_trade(iso, 2000, 2025)
  }) |> list_rbind() |>
    # Exclude the reporter itself from partners and from the denominator
    filter(!is.na(period), partner_iso != reporter_iso) |>
    mutate(
      country = str_remove(reporter_desc, " \\(Plurinational State of\\)"),
      yr = as.integer(period)
    )

  # Denominator: total trade with all partners, including unattributed "nes" areas
  totals <- raw |>
    summarise(total = sum(primary_value, na.rm = TRUE), .by = c(country, yr))

  # Same partner cleaning as plot_top_partners()
  trade <- raw |>
    mutate(partner_desc = if_else(partner_desc == "Other Asia, nes", "Taiwan", partner_desc)) |>
    filter(!partner_desc %in% c("World", "Free Zones"), !str_detect(partner_desc, ", nes$")) |>
    mutate(partner = if_else(partner_iso %in% eu27, "EU27", partner_desc)) |>
    summarise(value = sum(primary_value, na.rm = TRUE), .by = c(country, yr, partner)) |>
    left_join(totals, by = c("country", "yr")) |>
    mutate(share = value / total)

  in_year <- filter(trade, yr == year)
  partners <- in_year |>
    slice_max(value, n = n, by = country) |>
    distinct(partner) |>
    pull(partner)
  partner_order <- in_year |>
    filter(partner %in% partners) |>
    summarise(mean_share = mean(share), .by = partner) |>
    arrange(desc(mean_share)) |>
    pull(partner)

  all_countries <- sort(unique(raw$country))
  has_data <- unique(in_year$country)

  plot_data <- expand_grid(country = all_countries, partner = partner_order) |>
    left_join(select(in_year, country, partner, share), by = c("country", "partner")) |>
    # Countries with data but no recorded trade with a partner get 0; others stay NA (grey)
    mutate(
      share = if_else(is.na(share) & country %in% has_data, 0, share),
      country = factor(country, levels = rev(all_countries)),
      partner = factor(partner, levels = partner_order)
    )

  p <- ggplot(plot_data, aes(partner, country, fill = share)) +
    geom_tile(colour = "white", linewidth = 0.5) +
    geom_text(
      aes(
        label = if_else(!is.na(share) & share >= 0.01, scales::percent(share, accuracy = 1), ""),
        # White text on dark tiles, dark text on light tiles
        colour = !is.na(share) & share >= 0.35
      ),
      size = 3
    ) +
    scale_colour_manual(values = c("FALSE" = "#222222", "TRUE" = "white"), guide = "none") +
    scale_x_discrete(position = "top") +
    scale_fill_gradient(
      low = "#F4F6F8", high = "#1F5FA8", na.value = "#D9D9D9",
      labels = scales::percent, limits = c(0, NA)
    ) +
    labs(
      title = "Who Latin America trades with",
      subtitle = paste0("Share of each country's total trade (exports + imports), ", year),
      caption = "Source: UN Comtrade. Grey = no data for the year."
    ) +
    bbc_style() +
    theme(
      legend.position = "right",
      legend.text = element_text(size = 10),
      axis.text.x = element_text(size = 10, angle = 45, hjust = 0),
      axis.text.y = element_text(size = 10),
      panel.grid.major = element_blank(),
      plot.caption = element_text(size = 9, colour = "#555555", hjust = 1)
    )

  dir.create("export", showWarnings = FALSE)
  ggsave(file.path("export", paste0("latam_partner_heatmap_", year, ".png")), p,
         width = 900 / 72, height = 640 / 72, dpi = 72)
  p
}

# plot_partner_heatmap()                       # 2025, partners in any country's top 5
# plot_partner_heatmap(year = 2022, n = 3)     # another year, fewer partners
