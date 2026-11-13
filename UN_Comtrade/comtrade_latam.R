library(comtradr)
library(tidyverse)

# Key is read from the COMTRADE_PRIMARY_KEY environment variable (set in ~/.Renviron)
set_primary_comtrade_key(Sys.getenv("COMTRADE_PRIMARY_KEY"))

latam <- c(
  "ARG", "BOL", "BRA", "CHL", "COL", "CRI", "CUB", "DOM", "ECU", "SLV",
  "GTM", "HND", "MEX", "NIC", "PAN", "PRY", "PER", "URY", "VEN",
  "BLZ", "SUR", "GUY"
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

# Download once, then reuse the saved copy. To force a fresh download later, uncomment lines 38–39 and run them once.
# raw <- map(latam, get_exports) |> list_rbind()
# saveRDS(raw, "import/comtrade_latam_exports.rds")
raw <- readRDS("import/comtrade_latam_exports.rds")

trade <- raw |>
  select(year = period, reporter = reporter_desc, partner = partner_desc, value = primary_value) |>
  mutate(year = as.integer(year))

# One plot per country
plots <- trade |>
  split(~reporter) |>
  map(\(d) {
    ggplot(d, aes(year, value / 1e9, color = partner)) +
      geom_line(linewidth = 1) +
      ggrepel::geom_text_repel(
        data = slice_max(d, year, by = partner),
        aes(label = partner), hjust = 0, direction = "y", nudge_x = 0.3,
        xlim = c(NA, Inf), segment.color = NA, fontface = "bold"
      ) +
      scale_x_continuous(breaks = c(2005, 2015, 2025),
                         expand = expansion(mult = c(0.02, 0.15))) +
      coord_cartesian(clip = "off") +
      guides(color = "none") +
      labs(title = paste("Exports from", str_remove(d$reporter[1], " \\(Plurinational State of\\)")), x = NULL,
           y = "Exports (USD billions)")
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
# - Haiti: in Comtrade as a reporter only for 2013-2017, with no data before or after in this
#   pull. Its 2016 export spike looks like a one-off, and the series ends too early for the
#   2000-2025 window. Filling 2018 onward would need mirror data (e.g. USA/China exports to Haiti).
plot_top_partners <- function(iso, start_year = 2000, end_year = 2025, n = 5, refresh = FALSE) {
  iso <- toupper(iso)
  raw <- get_partner_trade(iso, start_year, end_year, refresh)
  country <- na.omit(raw$reporter_desc)[1] |> str_remove(" \\(Plurinational State of\\)")

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
    # Direct labels at the last year with data for each partner
    ggrepel::geom_text_repel(
      data = \(d) d |> filter(!is.na(value)) |> slice_max(year, by = partner),
      aes(label = partner), hjust = 0, direction = "y", nudge_x = 0.3,
      xlim = c(NA, Inf), segment.color = NA, fontface = "bold", size = 4,
      box.padding = 0.5, force = 2, max.iter = 20000
    ) +
    scale_color_manual(values = colors_for(top_partners), guide = "none") +
    # Ticks every 5 years, counted back from the latest year so it is always labelled
    scale_x_continuous(
      breaks = \(lim) rev(seq(latest, ceiling(lim[1]), by = -5)),
      expand = expansion(mult = c(0.02, 0.15))
    ) +
    coord_cartesian(clip = "off") +
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

# ---- Main goods/services traded with one partner ----
# Downloads (and caches) all commodity lines for one reporter-partner-year.
# Partner can be an ISO3 code or "EU27" (members are summed into one bloc).
get_commodity_trade <- function(iso, partner, year, flow = "Export",
                                type = c("goods", "services"), refresh = FALSE) {
  iso <- toupper(iso)
  type <- match.arg(type)
  flow <- match.arg(flow, c("Export", "Import"))
  partner <- toupper(partner)
  cache <- file.path(
    "import",
    paste0("comtrade_", tolower(iso), "_", tolower(partner), "_", tolower(type),
           "_", tolower(flow), "_", year, ".rds")
  )
  if (file.exists(cache) && !refresh) {
    return(readRDS(cache))
  }
  partners <- if (partner == "EU27") eu27 else partner
  raw <- ct_get_data(
    type = type,
    commodity_classification = if (type == "goods") "HS" else "EB10",
    commodity_code = "everything",
    flow_direction = flow,
    reporter = iso,
    partner = partners,
    start_date = year,
    end_date = year,
    frequency = "A"
  )
  dir.create("import", showWarnings = FALSE)
  saveRDS(raw, cache)
  raw
}

# Commodity descriptions are very long; keep the first clause and trim
shorten_label <- function(x, width = 44) {
  x |>
    str_remove("^[0-9.]+\\s+") |>
    str_remove(";.*$") |>
    str_trim() |>
    str_trunc(width)
}

# plot_top_commodities(): horizontal bar chart of the main goods (or services)
# one country trades with a single partner in a given year.
#
# Arguments
#   iso      Reporter ISO3 code, e.g. "ARG", "CHL", "MEX"
#   partner  Partner ISO3 code or "EU27", e.g. "CHN", "USA"
#   year     Year to report (default 2025)
#   n        Number of items shown (default 10)
#   flow     "Export" (default) or "Import"
#   type     "goods" (default, HS 2-digit chapters) or "services" (EBOPS top categories)
#   refresh  TRUE forces a new download instead of using the cache in import/
#
# Bars are sorted biggest to smallest. Share labels are the item's share of the
# country's total trade in that category with the partner.
plot_top_commodities <- function(iso, partner, year = 2025, n = 10,
                                 flow = "Export", type = c("goods", "services"),
                                 refresh = FALSE) {
  iso <- toupper(iso)
  partner <- toupper(partner)
  type <- match.arg(type)
  flow <- match.arg(flow, c("Export", "Import"))

  raw <- get_commodity_trade(iso, partner, year, flow, type, refresh)
  if (!"primary_value" %in% names(raw) || nrow(raw) == 0) {
    hint <- if (type == "services") {
      paste0("\nBilateral services data is very sparse in UN Comtrade (most reporters ",
             "only publish services totals against World). Try type = \"goods\".")
    } else ""
    stop("No ", type, " data for ", iso, " -> ", partner, " in ", year, ".", hint)
  }

  country <- na.omit(raw$reporter_desc)[1] |>
    str_remove(" \\(Plurinational State of\\)")
  partner_name <- if (partner == "EU27") "the EU" else na.omit(raw$partner_desc)[1]

  # Goods: HS 2-digit chapters. Services: top-level EBOPS categories.
  # Total rows are dropped so shares are computed against the sum of categories.
  level <- if (type == "goods") 2 else 1
  items <- raw |>
    filter(!is.na(primary_value), aggr_level == level) |>
    summarise(value = sum(primary_value, na.rm = TRUE), .by = c(cmd_code, cmd_desc)) |>
    filter(value > 0)

  items <- items |>
    mutate(share = value / sum(value), label = shorten_label(cmd_desc))

  # Truncation can make two chapters share a label (e.g. HS 61 and 62), which would
  # collapse them into one stacked bar. Disambiguate duplicates with the code.
  dup <- duplicated(items$label) | duplicated(items$label, fromLast = TRUE)
  items$label[dup] <- paste0(items$label[dup], " (", items$cmd_code[dup], ")")

  # Ascending order so the biggest bar sits at the top after coord_flip()
  items <- mutate(items, label = fct_reorder(label, value))

  top <- slice_max(items, value, n = n)

  # Use millions for small bilateral flows (< 10 billion) so the axis stays readable
  unit <- if (max(top$value) < 1e10) {
    list(divisor = 1e6, label = "USD millions")
  } else {
    list(divisor = 1e9, label = "USD billions")
  }

  p <- ggplot(top, aes(label, value / unit$divisor)) +
    geom_col(fill = "#1F5FA8", width = 0.7) +
    # Share labels just past the end of each bar (readable for short bars too)
    geom_text(aes(label = scales::percent(share, accuracy = 1)), hjust = -0.1,
              colour = "#222222", size = 3.5, fontface = "bold") +
    coord_flip() +
    scale_y_continuous(
      breaks = scales::breaks_pretty(n = 4),
      labels = scales::label_number(big.mark = ","),
      expand = expansion(mult = c(0, 0.12))
    ) +
    labs(
      title = paste0(country, "'s main ", type, " ", tolower(flow), "s"),
      subtitle = paste0(if (flow == "Export") "to " else "from ", partner_name, ", ", year),
      x = NULL,
      y = unit$label,
      caption = "Source: UN Comtrade"
    ) +
    bbc_style() +
    theme(
      # Align title/caption to the whole plot so long y labels don't push them off
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.title = element_text(size = 20),
      plot.subtitle = element_text(size = 14, margin = margin(4, 0, 8, 0)),
      axis.title.x = element_text(size = 14, colour = "#222222"),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 9),
      # Drop the per-row gridlines
      panel.grid.major.y = element_blank(),
      plot.caption = element_text(size = 10, colour = "#555555", hjust = 1)
    )

  p
}

# Examples
# plot_top_commodities("ARG", "CHN")                       # Argentina's exports to China, 2025
# plot_top_commodities("BRA", "USA", year = 2022)          # another year
# plot_top_commodities("CHL", "EU27", flow = "Import")     # imports from the EU
# plot_top_commodities("MEX", "USA", flow = "Import", n = 5)  # imports, top 5
# type = "services" is supported but bilateral services data is sparse in Comtrade

# Generate the chart and save it to export/ (640x450 px, matching the other charts)
p_commodities <- plot_top_commodities("NIC", "USA", year = 2025, n=10)
p_commodities
dir.create("export", showWarnings = FALSE)
ggsave(file.path("export", "nic_top10_goods_to_usa_2025.png"), p_commodities,
       width = 640 / 72, height = 450 / 72, dpi = 72)


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

# Generate and save the heatmap (export/latam_partner_heatmap_2025.png, 900x640 px)
plot_partner_heatmap()

# plot_partner_heatmap()                       # 2025, partners in any country's top 5
# plot_partner_heatmap(year = 2022, n = 3)     # another year, fewer partners
