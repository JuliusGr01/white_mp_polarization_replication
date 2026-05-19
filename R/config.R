ROOT <- normalizePath(".", winslash = "/", mustWork = TRUE)
DATA_DIR <- file.path(ROOT, "data")
OUTPUT_DIR <- file.path(ROOT, "output")
REFERENCE_DIR <- file.path(ROOT, "reference")

START_DATE <- as.Date("1969-01-01")
END_DATE <- as.Date("2020-12-31")
SHOCK_END_DATE <- as.Date("2008-12-31")

H_MAX <- 48L
N_LAGS_Y <- 12L
N_LAGS_SHOCK <- 12L
NW_LAGS <- 12L

LP_Y_LAG_TRANSFORM <- "diff"
LP_INCLUDE_TIME_TREND <- FALSE

TOTAL_NONAG_EMPLOYMENT_SERIES_ID <- "LNS12032187"
POPULATION_SERIES_ID <- "LNU00000000"

ROUTINE_SERIES_IDS <- c(
  "LNU02032205",
  "LNU02032208",
  "LNU02032212"
)

NONROUTINE_SERIES_IDS <- c(
  "LNU02032201",
  "LNU02032204"
)

RECESSIONS <- data.frame(
  start = as.Date(c(
    "1969-12-01", "1973-11-01", "1980-01-01", "1981-07-01",
    "1990-07-01", "2001-03-01", "2007-12-01", "2020-02-01"
  )),
  end = as.Date(c(
    "1970-11-01", "1975-03-01", "1980-07-01", "1982-11-01",
    "1991-03-01", "2001-11-01", "2009-06-01", "2020-04-01"
  ))
)
