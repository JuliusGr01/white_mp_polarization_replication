###############################################################################
### White-style 1969 employment panel                                       ###
###############################################################################

# This file builds the monthly routine/nonroutine employment panel used by the
# LP scripts when we want the same starting point as White's baseline sample.
# It uses the Employment and Earnings extraction for 1969-1982 and appends the
# existing CPS-based routine employment panel from 1983 onward.

white_sample_start <- as.Date(Sys.getenv("MP_LABORPOL_SAMPLE_START", "1969-01-01"))
white_sample_end_env <- Sys.getenv("MP_LABORPOL_SAMPLE_END", unset = NA_character_)

white_pre1983_csv <- file.path(
  "data", "data_processed", "cps_ee_1969_1982_alm_panel.csv"
)
white_post1982_cache <- Sys.getenv(
  "MP_LABORPOL_POST1982_CACHE",
  file.path("output", "cache", "routine_ts_sa.rds")
)
white_cache_dir <- file.path("output", "cache")
white_cache_file <- file.path(white_cache_dir, "routine_ts_sa_white_1969.rds")
white_scaled_cache_file <- file.path(white_cache_dir, "routine_ts_sa_scaled_white_1969.rds")

read_white_pre1983_panel <- function(path = white_pre1983_csv) {
  if (!file.exists(path)) {
    stop(
      "Missing 1969-1982 Employment and Earnings panel: ", path,
      "\nRun `python code/build_cps_ee_1969_1982_panel.py` first."
    )
  }

  readr::read_csv(path, show_col_types = FALSE) %>%
    dplyr::mutate(
      ym = as.Date(.data$ym),
      pop_16plus = NA_real_,
      routine_emp_rel_pop = NA_real_,
      nonroutine_emp_rel_pop = NA_real_,
      source_panel = "employment_earnings_1969_1982"
    ) %>%
    dplyr::filter(.data$ym < as.Date("1983-01-01")) %>%
    dplyr::select(
      ym, pop_16plus,
      routine_emp_abs, nonroutine_emp_abs,
      dplyr::any_of(c("abstract_emp_abs", "manual_emp_abs")),
      total_emp_abs,
      routine_emp_rel_pop, nonroutine_emp_rel_pop,
      routine_emp_rel_emp, nonroutine_emp_rel_emp, routine_emp_share,
      source_panel
    )
}

read_white_post1982_panel <- function(path = white_post1982_cache) {
  if (!file.exists(path)) {
    stop(
      "Missing post-1982 routine employment cache: ", path,
      "\nBuild your existing CPS routine panel first, or set MP_LABORPOL_POST1982_CACHE."
    )
  }

  readRDS(path) %>%
    dplyr::mutate(
      ym = as.Date(.data$ym),
      source_panel = "current_cps_post1982"
    ) %>%
    dplyr::filter(.data$ym >= as.Date("1983-01-01")) %>%
    dplyr::select(
      ym,
      dplyr::any_of(c(
        "pop_16plus",
        "routine_emp_abs", "nonroutine_emp_abs",
        "abstract_emp_abs", "manual_emp_abs",
        "total_emp_abs",
        "routine_emp_rel_pop", "nonroutine_emp_rel_pop",
        "routine_emp_rel_emp", "nonroutine_emp_rel_emp", "routine_emp_share"
      )),
      source_panel
    )
}

sa_if_complete <- function(x, start_date) {
  if (all(is.na(x))) {
    return(rep(NA_real_, length(x)))
  }
  if (any(!is.finite(x))) {
    return(rep(NA_real_, length(x)))
  }
  sa_x13(x, start_date)
}

load_white_shocks <- function() {
  jk_path <- file.path("data", "data_raw", "monetary_shocks", "shocks_fed_jk_m.csv")
  rr_path <- file.path("data", "data_raw", "monetary_shocks", "RR_monetary_shock_monthly.dta")

  jk <- readr::read_csv(
    jk_path,
    col_types = readr::cols(
      year       = readr::col_integer(),
      month      = readr::col_integer(),
      pc1_hf     = readr::col_double(),
      SP500_hf   = readr::col_double(),
      MP_pm      = readr::col_double(),
      CBI_pm     = readr::col_double(),
      MP_median  = readr::col_double(),
      CBI_median = readr::col_double()
    )
  ) %>%
    dplyr::mutate(ym = lubridate::make_date(.data$year, .data$month, 1)) %>%
    dplyr::select(ym, pc1_hf, MP_pm, MP_median, CBI_pm, CBI_median)

  rr <- haven::read_dta(rr_path) %>%
    dplyr::mutate(ym = as.Date(lubridate::ymd("1960-01-01") %m+% lubridate::months(.data$date))) %>%
    dplyr::select(ym, dplyr::any_of(c("resid", "resid_romer", "resid_full")))

  list(jk = jk, rr = rr)
}

build_white_1969_panel <- function(
    sample_start = white_sample_start,
    sample_end = NULL,
    pre1983_csv = white_pre1983_csv,
    post1982_cache = white_post1982_cache,
    save_cache = TRUE
) {
  pre1983 <- read_white_pre1983_panel(pre1983_csv)
  post1982 <- read_white_post1982_panel(post1982_cache)
  shocks <- load_white_shocks()

  if (is.null(sample_end)) {
    if (!is.na(white_sample_end_env)) {
      sample_end <- as.Date(white_sample_end_env)
    } else {
      sample_end <- max(shocks$rr$ym[!is.na(shocks$rr$resid_full) | !is.na(shocks$rr$resid_romer)], na.rm = TRUE)
    }
  }

  combined <- dplyr::bind_rows(pre1983, post1982) %>%
    dplyr::arrange(.data$ym) %>%
    dplyr::distinct(.data$ym, .keep_all = TRUE) %>%
    dplyr::filter(.data$ym >= sample_start, .data$ym <= sample_end)

  required_columns <- c("ym", "routine_emp_abs", "nonroutine_emp_abs", "total_emp_abs")
  missing_columns <- setdiff(required_columns, names(combined))
  if (length(missing_columns) > 0) {
    stop("White panel is missing required columns: ", paste(missing_columns, collapse = ", "))
  }

  expected_months <- seq.Date(sample_start, sample_end, by = "month")
  missing_months <- setdiff(expected_months, combined$ym)
  if (length(missing_months) > 0) {
    stop(
      "White panel is not monthly-continuous. First missing month(s): ",
      paste(utils::head(missing_months, 12), collapse = ", ")
    )
  }

  if (!"abstract_emp_abs" %in% names(combined)) combined$abstract_emp_abs <- NA_real_
  if (!"manual_emp_abs" %in% names(combined)) combined$manual_emp_abs <- NA_real_

  combined <- combined %>%
    dplyr::mutate(
      total_emp_abs = dplyr::coalesce(
        .data$total_emp_abs,
        .data$routine_emp_abs + .data$nonroutine_emp_abs
      ),
      routine_emp_rel_emp = dplyr::if_else(
        .data$total_emp_abs > 0,
        .data$routine_emp_abs / .data$total_emp_abs,
        NA_real_
      ),
      nonroutine_emp_rel_emp = dplyr::if_else(
        .data$total_emp_abs > 0,
        .data$nonroutine_emp_abs / .data$total_emp_abs,
        NA_real_
      ),
      routine_emp_share = .data$routine_emp_rel_emp,
      log_routine_emp = log(.data$routine_emp_abs),
      log_nonroutine_emp = log(.data$nonroutine_emp_abs),
      log_total_emp = log(.data$total_emp_abs),
      log_abstract_emp = dplyr::if_else(
        .data$abstract_emp_abs > 0,
        log(.data$abstract_emp_abs),
        NA_real_
      ),
      log_manual_emp = dplyr::if_else(
        .data$manual_emp_abs > 0,
        log(.data$manual_emp_abs),
        NA_real_
      )
    )

  if (min(combined$ym, na.rm = TRUE) > sample_start) {
    stop("White panel starts after requested sample_start: ", min(combined$ym, na.rm = TRUE))
  }

  x_min_ext <- min(combined$ym, na.rm = TRUE)

  routine_ts_sa <- combined %>%
    dplyr::arrange(.data$ym) %>%
    dplyr::mutate(
      routine_emp_abs_sa = sa_if_complete(.data$routine_emp_abs, x_min_ext),
      nonroutine_emp_abs_sa = sa_if_complete(.data$nonroutine_emp_abs, x_min_ext),
      total_emp_abs_sa = sa_if_complete(.data$total_emp_abs, x_min_ext),
      routine_emp_share_sa = sa_if_complete(.data$routine_emp_share, x_min_ext),
      routine_emp_rel_emp_sa = sa_if_complete(.data$routine_emp_rel_emp, x_min_ext),
      nonroutine_emp_rel_emp_sa = sa_if_complete(.data$nonroutine_emp_rel_emp, x_min_ext),
      log_routine_emp_sa = sa_if_complete(.data$log_routine_emp, x_min_ext),
      log_nonroutine_emp_sa = sa_if_complete(.data$log_nonroutine_emp, x_min_ext),
      log_total_emp_sa = sa_if_complete(.data$log_total_emp, x_min_ext),
      log_abstract_emp_sa = sa_if_complete(.data$log_abstract_emp, x_min_ext),
      log_manual_emp_sa = sa_if_complete(.data$log_manual_emp, x_min_ext)
    ) %>%
    dplyr::left_join(shocks$jk, by = "ym") %>%
    dplyr::left_join(shocks$rr, by = "ym") %>%
    dplyr::arrange(.data$ym)

  if (all(is.na(routine_ts_sa$resid_full)) && all(is.na(routine_ts_sa$resid_romer))) {
    stop("White panel has no Romer/Romer shocks after merging.")
  }

  if (save_cache) {
    dir.create(white_cache_dir, recursive = TRUE, showWarnings = FALSE)
    saveRDS(routine_ts_sa, white_cache_file)
    saveRDS(routine_ts_sa, white_scaled_cache_file)
  }

  routine_ts_sa
}

routine_ts_sa <- build_white_1969_panel()
routine_ts_sa_scaled <- routine_ts_sa

message(
  "White 1969 panel loaded: ",
  min(routine_ts_sa$ym, na.rm = TRUE), " to ",
  max(routine_ts_sa$ym, na.rm = TRUE), " (",
  nrow(routine_ts_sa), " monthly rows)."
)
