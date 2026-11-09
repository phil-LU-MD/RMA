# GDELT media-framing pipeline in R
# Adapted from the workflow described by Mohamed Soufan (2026):
# https://hackernoon.com/building-a-pipeline-to-study-war-coverage-using-gdelt
#
# Workflow:
# 1. Collect headline metadata from the GDELT DOC 2.0 API.
# 2. Deduplicate and clean the results.
# 3. Manually classify headlines, then produce summary tables and figures.
#
# The defaults reproduce the article's Lebanon/Beirut example. Change the
# settings in the CONFIGURATION section to study another conflict or event.

# ---------------------------------------------------------------------------
# 0. Packages
# ---------------------------------------------------------------------------

packages <- c("httr", "jsonlite", "dplyr", "purrr", "stringr", "readr", "ggplot2", "tibble")
missing_packages <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

invisible(lapply(packages, library, character.only = TRUE))

# ---------------------------------------------------------------------------
# 1. Configuration: edit these values for your own study
# ---------------------------------------------------------------------------

QUERIES <- c("Lebanon", "Beirut")

# GDELT timestamps are supplied here in UTC.
START_DATETIME <- as.POSIXct("2026-04-08 00:00:00", tz = "UTC")
END_DATETIME   <- as.POSIXct("2026-04-10 23:59:59", tz = "UTC")

MAX_RECORDS <- 250L          # GDELT DOC API maximum per request
MIN_WINDOW_HOURS <- 6        # Do not recursively split below this duration
PAUSE_SECONDS <- 8           # Conservative pause between requests
MAX_RETRIES <- 5L

# Project folders are created relative to the directory from which the script
# is run. For a reproducible workflow, run it from an RStudio Project folder.
DATA_DIR <- file.path(getwd(), "data", "news")
FIGURE_DIR <- file.path(getwd(), "figures")

dir.create(DATA_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(FIGURE_DIR, recursive = TRUE, showWarnings = FALSE)

RAW_FILE <- file.path(DATA_DIR, "news_gdelt.csv")
CLEAN_FILE <- file.path(DATA_DIR, "news_gdelt_clean.csv")
CLASSIFIED_FILE <- file.path(DATA_DIR, "news_gdelt_classified.csv")

# ---------------------------------------------------------------------------
# 2. Helper functions
# ---------------------------------------------------------------------------

format_gdelt_datetime <- function(x) {
  format(x, "%Y%m%d%H%M%S", tz = "UTC")
}

normalise_title <- function(x) {
  x |>
    stringr::str_to_lower() |>
    stringr::str_replace_all("[^[:alnum:][:space:]]", " ") |>
    stringr::str_squish()
}

# Divide a date range into daily windows before querying. If any window reaches
# the API cap, fetch_window() will split it recursively into shorter windows.
build_daily_windows <- function(start_datetime, end_datetime) {
  days <- seq.Date(
    as.Date(start_datetime, tz = "UTC"),
    as.Date(end_datetime, tz = "UTC"),
    by = "day"
  )

  purrr::map_dfr(days, function(day) {
    day_start <- as.POSIXct(paste(day, "00:00:00"), tz = "UTC")
    day_end <- as.POSIXct(paste(day, "23:59:59"), tz = "UTC")

    tibble::tibble(
      start = max(start_datetime, day_start),
      end = min(end_datetime, day_end)
    )
  })
}

# Make one request to the GDELT DOC 2.0 API. HTTP 429 and server errors are
# retried with increasing waits; other HTTP errors stop the script.
query_gdelt <- function(query, start_datetime, end_datetime) {
  endpoint <- "https://api.gdeltproject.org/api/v2/doc/doc"

  parameters <- list(
    query = paste(query, "sourcelang:english"),
    mode = "artlist",
    maxrecords = MAX_RECORDS,
    startdatetime = format_gdelt_datetime(start_datetime),
    enddatetime = format_gdelt_datetime(end_datetime),
    sort = "DateDesc",
    format = "json"
  )

  for (attempt in seq_len(MAX_RETRIES)) {
    response <- tryCatch(
      httr::GET(
        url = endpoint,
        query = parameters,
        httr::user_agent("R GDELT media-framing research pipeline"),
        httr::timeout(60)
      ),
      error = function(e) e
    )

    if (inherits(response, "error")) {
      if (attempt == MAX_RETRIES) {
        stop("Request failed after ", MAX_RETRIES, " attempts: ", conditionMessage(response))
      }
      wait_seconds <- 15 * attempt
      message("Connection error: ", conditionMessage(response), ". Retrying in ", wait_seconds, " seconds.")
      Sys.sleep(wait_seconds)
      next
    }

    status <- httr::status_code(response)

    if (status == 429 || status >= 500) {
      if (attempt == MAX_RETRIES) {
        stop("GDELT returned HTTP status ", status, " after ", MAX_RETRIES, " attempts.")
      }
      wait_seconds <- if (status == 429) 60 * attempt else 15 * attempt
      message("GDELT returned HTTP ", status, ". Retrying in ", wait_seconds, " seconds.")
      Sys.sleep(wait_seconds)
      next
    }

    if (status >= 400) {
      stop("GDELT returned HTTP status ", status, ". Check the query and date range.")
    }

    response_text <- httr::content(response, as = "text", encoding = "UTF-8")
    parsed <- tryCatch(
      jsonlite::fromJSON(response_text, simplifyVector = TRUE),
      error = function(e) NULL
    )

    if (is.null(parsed) || is.null(parsed$articles) || length(parsed$articles) == 0) {
      Sys.sleep(PAUSE_SECONDS)
      return(tibble::tibble())
    }

    articles <- tibble::as_tibble(parsed$articles)

    # Some API responses may omit optional fields. Add them as NA if needed.
    required_fields <- c("title", "url", "seendate", "domain", "sourcecountry", "language")
    for (field in required_fields) {
      if (!field %in% names(articles)) {
        articles[[field]] <- NA_character_
      }
    }

    result <- articles |>
      dplyr::transmute(
        date = as.Date(substr(as.character(.data$seendate), 1, 8), format = "%Y%m%d"),
        title = as.character(.data$title),
        url = as.character(.data$url),
        domain = as.character(.data$domain),
        source_country = as.character(.data$sourcecountry),
        language = as.character(.data$language),
        query = query
      )

    Sys.sleep(PAUSE_SECONDS)
    return(result)
  }

  tibble::tibble()
}

# If a window returns the maximum number of records, split it into two and
# query each half. This reduces the risk of silently missing results because
# of the API's per-request cap.
fetch_window <- function(query, start_datetime, end_datetime) {
  results <- query_gdelt(query, start_datetime, end_datetime)
  duration_hours <- as.numeric(difftime(end_datetime, start_datetime, units = "hours"))

  message(
    "Query '", query, "' | ", format(start_datetime, "%Y-%m-%d %H:%M", tz = "UTC"),
    " to ", format(end_datetime, "%Y-%m-%d %H:%M", tz = "UTC"),
    " UTC | ", nrow(results), " results"
  )

  if (nrow(results) >= MAX_RECORDS && duration_hours > MIN_WINDOW_HOURS) {
    midpoint_seconds <- (as.numeric(start_datetime) + as.numeric(end_datetime)) / 2
    midpoint <- as.POSIXct(midpoint_seconds, origin = "1970-01-01", tz = "UTC")
    right_start <- midpoint + 1

    message("  Reaching API cap; splitting this window into two.")

    left_results <- fetch_window(query, start_datetime, midpoint)
    right_results <- fetch_window(query, right_start, end_datetime)

    return(dplyr::bind_rows(left_results, right_results))
  }

  if (nrow(results) >= MAX_RECORDS && duration_hours <= MIN_WINDOW_HOURS) {
    warning(
      "The API cap was still reached in a window of six hours or less for query '",
      query, "'. Some results may be missing. Consider narrowing the date range or query."
    )
  }

  results
}

# ---------------------------------------------------------------------------
# 3. Collect data
# ---------------------------------------------------------------------------

windows <- build_daily_windows(START_DATETIME, END_DATETIME)

raw_data <- purrr::map_dfr(QUERIES, function(search_term) {
  purrr::map_dfr(seq_len(nrow(windows)), function(i) {
    fetch_window(search_term, windows$start[i], windows$end[i])
  })
})

if (nrow(raw_data) == 0) {
  stop("GDELT returned no articles. Check the search terms, dates and API response.")
}

raw_data <- raw_data |>
  dplyr::filter(
    !is.na(.data$title), .data$title != "",
    !is.na(.data$url), .data$url != ""
  ) |>
  dplyr::mutate(normalised_title = normalise_title(.data$title)) |>
  dplyr::arrange(.data$date) |>
  dplyr::filter(
    !duplicated(.data$url),
    !duplicated(.data$normalised_title)
  ) |>
  dplyr::select(-normalised_title)

readr::write_csv(raw_data, RAW_FILE)
message("Saved ", nrow(raw_data), " deduplicated records to: ", RAW_FILE)

# ---------------------------------------------------------------------------
# 4. Clean the data
# ---------------------------------------------------------------------------

# The filters below reproduce the Lebanon/Beirut example in the article.
# Replace the keywords and case-specific exclusion patterns when studying a
# different conflict. Inspect excluded headlines rather than treating these
# filters as universally appropriate.

us_city_pattern <- stringr::regex(
  "Lebanon,?\\s*(PA|Pennsylvania|TN|Tennessee|OH|Ohio|KY|Kentucky|IN|Indiana|VA|Virginia|MO|Missouri|OR|Oregon|NH|New Hampshire|County|Valley|Township|City)",
  ignore_case = TRUE
)

finance_pattern <- stringr::regex(
  "\\b(stock|market|oil price|crude|barrel|FTSE|S&P|dow|nasdaq|shares|investor|rally|trade war|tariff|GDP|inflation|currency|forex|bond|yield|economic|economy|gas price|fuel price|refinery|hormuz|strait)\\b",
  ignore_case = TRUE
)

unrelated_pattern <- stringr::regex(
  "\\b(Hormuz|Strait|oil tanker|tanker traffic|refinery|stock market|FTSE|S&P|muni|bond|rally|uranium|Islamabad|Pakistan broker|Azerbaijan|Gulf allies|Starmer|Hegseth|clay fuller|night sky|In-N-Out|foodies|Ontario gas|nicotine|coffee consumed)\\b",
  ignore_case = TRUE
)

clean_data <- raw_data |>
  dplyr::filter(stringr::str_detect(.data$title, stringr::regex("\\b(Lebanon|Beirut)\\b", ignore_case = TRUE))) |>
  dplyr::filter(!stringr::str_detect(.data$title, us_city_pattern)) |>
  dplyr::filter(!stringr::str_detect(.data$title, finance_pattern)) |>
  dplyr::filter(!stringr::str_detect(.data$title, unrelated_pattern)) |>
  dplyr::mutate(normalised_title = normalise_title(.data$title)) |>
  dplyr::distinct(.data$normalised_title, .keep_all = TRUE) |>
  dplyr::select(-normalised_title)

readr::write_csv(clean_data, CLEAN_FILE)
message("Cleaned dataset: ", nrow(clean_data), " headlines. Saved to: ", CLEAN_FILE)

# ---------------------------------------------------------------------------
# 5. Manual classification
# ---------------------------------------------------------------------------

# The source article classified headlines manually into two categories:
#   Human Impact: casualties, displacement, civilian harm, humanitarian conditions
#   Politics & Military: diplomacy, ceasefire negotiations, military operations
#
# The first run creates a CSV with a blank 'category' column. Open that file,
# classify each headline, save it, and run this script again. Do not overwrite
# the file after adding your labels.

if (!file.exists(CLASSIFIED_FILE)) {
  classification_template <- clean_data |>
    dplyr::mutate(category = NA_character_)

  readr::write_csv(classification_template, CLASSIFIED_FILE)

  message(
    "Created a classification template: ", CLASSIFIED_FILE,
    "\nFill the 'category' column manually with 'Human Impact' or 'Politics & Military',",
    " then rerun this script to create the summaries and figures."
  )

} else {
  classified_data <- readr::read_csv(CLASSIFIED_FILE, show_col_types = FALSE)

  if (!"category" %in% names(classified_data)) {
    stop("The classified CSV needs a 'category' column.")
  }

  valid_categories <- c("Human Impact", "Politics & Military")
  invalid_categories <- setdiff(
    unique(stats::na.omit(classified_data$category)),
    valid_categories
  )

  if (length(invalid_categories) > 0) {
    stop(
      "Unrecognised category value(s): ", paste(invalid_categories, collapse = ", "),
      ". Use exactly: ", paste(valid_categories, collapse = " or "), "."
    )
  }

  analysed_data <- classified_data |>
    dplyr::filter(!is.na(.data$category), .data$category != "")

  if (nrow(analysed_data) == 0) {
    message("No classifications found yet. Fill the category column and rerun the script.")
  } else {
    # Overall distribution
    overall_summary <- analysed_data |>
      dplyr::count(.data$category, name = "articles") |>
      dplyr::mutate(percentage = 100 * .data$articles / sum(.data$articles))

    readr::write_csv(
      overall_summary,
      file.path(DATA_DIR, "framing_summary_overall.csv")
    )

    overall_plot <- ggplot2::ggplot(
      overall_summary,
      ggplot2::aes(x = .data$category, y = .data$percentage, fill = .data$category)
    ) +
      ggplot2::geom_col(width = 0.65, show.legend = FALSE) +
      ggplot2::geom_text(
        ggplot2::aes(label = paste0(round(.data$percentage, 1), "% (n = ", .data$articles, ")")),
        vjust = -0.4,
        size = 4
      ) +
      ggplot2::scale_y_continuous(limits = c(0, 110), expand = c(0, 0)) +
      ggplot2::labs(
        title = "How the media framed the event",
        x = NULL,
        y = "Share of classified headlines (%)"
      ) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 10, hjust = 1),
        plot.title = ggplot2::element_text(face = "bold")
      )

    ggplot2::ggsave(
      file.path(FIGURE_DIR, "framing_overall.png"),
      overall_plot,
      width = 8,
      height = 5,
      dpi = 300
    )

    # Distribution by GDELT source country
    country_summary <- analysed_data |>
      dplyr::mutate(source_country = dplyr::if_else(
        is.na(.data$source_country) | .data$source_country == "",
        "Unknown",
        .data$source_country
      )) |>
      dplyr::count(.data$source_country, .data$category, name = "articles") |>
      dplyr::group_by(.data$source_country) |>
      dplyr::mutate(percentage = 100 * .data$articles / sum(.data$articles)) |>
      dplyr::ungroup()

    readr::write_csv(
      country_summary,
      file.path(DATA_DIR, "framing_summary_by_country.csv")
    )

    country_plot <- ggplot2::ggplot(
      country_summary,
      ggplot2::aes(x = stats::reorder(.data$source_country, .data$articles),
                   y = .data$percentage, fill = .data$category)
    ) +
      ggplot2::geom_col(position = "fill") +
      ggplot2::coord_flip() +
      ggplot2::scale_y_continuous(labels = function(x) paste0(round(x * 100), "%")) +
      ggplot2::labs(
        title = "Media framing by source country",
        x = NULL,
        y = "Share of classified headlines",
        fill = "Framing category"
      ) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))

    ggplot2::ggsave(
      file.path(FIGURE_DIR, "framing_by_country.png"),
      country_plot,
      width = 9,
      height = 6,
      dpi = 300
    )

    print(overall_summary)
    print(country_summary)
    print(overall_plot)
    print(country_plot)

    message("Figures and summary tables saved in: ", FIGURE_DIR, " and ", DATA_DIR)
  }
}
