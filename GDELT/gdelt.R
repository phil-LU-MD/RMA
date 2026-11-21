library(httr2)
library(jsonlite)
library(dplyr)
library(ggplot2)

query <- '("China" OR "Chinese") Bolivia'

#Operators
#"..."	Searches for an exact phrase	"strategic partnership"
#OR	Searches for either term	(China OR Chinese)
#-	Excludes a term	Bolivia -Peru
#sourcelang:	Filters by original language	sourcelang:spanish
#sourcecountry:	Filters by the country where the news outlet is based	sourcecountry:bolivia
#domain:	Filters by a particular website	domain:reuters.com

response <- request(
  "https://api.gdeltproject.org/api/v2/doc/doc"
) |>
  req_url_query(
    query = query,
    mode = "ArtList",
    format = "json",
    maxrecords = 250,
    timespan = "7d",
    sort = "DateDesc"
  ) |>
  req_user_agent("R GDELT media-framing research pipeline") |>
  # GDELT can be slow to respond; allow up to 60s
  req_timeout(60) |>
  # GDELT allows roughly one request every 5 seconds per IP
  req_throttle(rate = 10 / 60, realm = "api.gdeltproject.org") |>
  # Wait and retry on rate limits (429), transient server errors,
  # and connection timeouts
  req_retry(
    max_tries = 5,
    is_transient = \(resp) resp_status(resp) %in% c(429, 500, 502, 503, 504),
    retry_on_failure = TRUE,
    backoff = \(attempt) 15
  ) |>
  req_perform()

resp_check_status(response)

gdelt_data <- fromJSON(
  resp_body_string(response),
  flatten = TRUE
)

articles <- as_tibble(gdelt_data$articles)
