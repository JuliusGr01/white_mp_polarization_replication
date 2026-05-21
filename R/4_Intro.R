#######################################################
###  IPUMS CPS Intro Figures: Five ALM Task Groups  ###
#######################################################

# This script builds an IPUMS CPS micro-data version of the introductory
# routine-employment figures. Occupations are mapped to occ1990dd, assigned
# five original ALM/DOT 1977 task measures, and then classified by the dominant
# task score.


# 0. Control panel --------------------------------------------------------

sample_start_year <- 1968L
sample_end_year <- Inf

figure_start_date <- as.Date("1969-01-01")
stop_on_task_ties <- TRUE
task_tie_tolerance <- sqrt(.Machine$double.eps)

ipums_extract_description <- "CPS Basic Monthly - ALM five task replication"
ipums_extract_dirname <- "ipums_cps_alm5_extract"

dot77_zip_url <- paste0(
  "https://www.dropbox.com/scl/fi/ivsw3qn98etss8g0w0sh5/",
  "DOT-occ-data-sets.zip?rlkey=py7wopvg7mofr2d4x4j6tcqdx&dl=1"
)

external_raw_dir <- Sys.getenv(
  "MP_LABORPOL_RAW_DIR",
  unset = "C:/Users/wmf098/Sciebo/PhD/Masterarbeit/MP_LaborPol/data/data_raw"
)

recessions <- data.frame(
  start = as.Date(c(
    "1969-12-01", "1973-11-01", "1980-01-01", "1981-07-01",
    "1990-07-01", "2001-03-01", "2007-12-01", "2020-02-01"
  )),
  end = as.Date(c(
    "1970-11-01", "1975-03-01", "1980-07-01", "1982-11-01",
    "1991-03-01", "2001-11-01", "2009-06-01", "2020-04-01"
  ))
)


# 1. Set-up ---------------------------------------------------------------

script_dir <- function() {
  sourced_file <- tryCatch(
    normalizePath(sys.frames()[[1]]$ofile, winslash = "/", mustWork = TRUE),
    error = function(e) NA_character_
  )
  if (!is.na(sourced_file)) return(dirname(sourced_file))

  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- "--file="
  hit <- grep(paste0("^", file_arg), args)
  if (length(hit) > 0L) {
    candidate <- sub(file_arg, "", args[hit[1L]])
    if (!identical(candidate, "-")) {
      candidate <- tryCatch(
        normalizePath(candidate, winslash = "/", mustWork = TRUE),
        error = function(e) NA_character_
      )
      if (!is.na(candidate)) return(dirname(candidate))
    }
  }

  wd <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  if (file.exists(file.path(wd, "functions.R"))) return(wd)
  if (file.exists(file.path(wd, "R", "functions.R"))) return(file.path(wd, "R"))
  wd
}

r_dir <- script_dir()
input_dir <- file.path(r_dir, "data")
out_dir <- file.path(r_dir, "output")
fig_dir <- out_dir
cache_dir <- file.path(out_dir, "cache")
ipums_extract_dir <- file.path(cache_dir, ipums_extract_dirname)
external_raw_dir <- normalizePath(external_raw_dir, winslash = "/", mustWork = FALSE)

dir.create(input_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(ipums_extract_dir, recursive = TRUE, showWarnings = FALSE)

required_packages <- c(
  "dplyr", "tidyr", "readr", "stringr", "lubridate",
  "haven", "ipumsr", "data.table"
)

missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0L) {
  stop("Install required packages before running 4_Intro.R: ", paste(missing_packages, collapse = ", "))
}

invisible(lapply(required_packages, function(pkg) {
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}))

functions_path <- file.path(r_dir, "functions.R")
if (file.exists(functions_path)) source(functions_path)

if (!exists("write_white_csv", mode = "function")) {
  write_white_csv <- function(x, path) {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    utils::write.csv(x, path, row.names = FALSE, na = "")
  }
}

if (!exists("plot_descriptive_line", mode = "function")) {
  plot_descriptive_line <- function(panel,
                                    column,
                                    ylabel,
                                    title,
                                    y_ticks,
                                    y_lim,
                                    out_path,
                                    recessions) {
    png(out_path, width = 1300, height = 760, res = 200)
    par(family = "serif", mar = c(4.2, 5.0, 3.0, 1.0))
    plot(
      panel$date,
      panel[[column]],
      type = "n",
      xlab = "Year",
      ylab = ylabel,
      main = title,
      ylim = y_lim,
      yaxt = "n"
    )
    usr <- par("usr")
    for (i in seq_len(nrow(recessions))) {
      rect(recessions$start[i], usr[3], recessions$end[i], usr[4], col = "grey85", border = NA)
    }
    lines(panel$date, panel[[column]], col = "black", lwd = 1.4)
    axis(2, at = y_ticks)
    box()
    dev.off()
  }
}


# 2. Helpers --------------------------------------------------------------

require_file <- function(path, label = path) {
  if (!file.exists(path)) stop("Missing ", label, ": ", path)
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

read_crosswalk <- function(path, year_min, year_max, scheme) {
  require_file(path, paste0("crosswalk ", scheme))
  haven::read_dta(path) %>%
    dplyr::transmute(
      occ_code = as.integer(.data$occ),
      occ1990dd = as.integer(.data$occ1990dd),
      year_min = as.integer(year_min),
      year_max = as.integer(year_max),
      occ_scheme = scheme
    ) %>%
    dplyr::filter(!is.na(.data$occ_code), !is.na(.data$occ1990dd))
}

build_occupation_crosswalks <- function(raw_dir) {
  dplyr::bind_rows(
    read_crosswalk(file.path(raw_dir, "occ1960_occ1990dd", "occ1960_occ1990dd.dta"), 1968L, 1970L, "occ1960"),
    read_crosswalk(file.path(raw_dir, "occ1970_occ1990dd", "occ1970_occ1990dd.dta"), 1971L, 1982L, "occ1970"),
    read_crosswalk(file.path(raw_dir, "occ1980_occ1990dd", "occ1980_occ1990dd.dta"), 1983L, 1991L, "occ1980"),
    read_crosswalk(file.path(raw_dir, "occ1990_occ1990dd", "occ1990_occ1990dd.dta"), 1992L, 2002L, "occ1990"),
    read_crosswalk(file.path(raw_dir, "occ2000_occ1990dd", "occ2000_occ1990dd.dta"), 2003L, 2010L, "occ2000"),
    read_crosswalk(file.path(raw_dir, "occ2010_occ1990dd", "occ2010_occ1990dd.dta"), 2011L, 9999L, "occ2010")
  )
}

ensure_dot77_archive <- function(cache_dir, zip_url) {
  dot_dir <- file.path(cache_dir, "DOT-occ-data-sets")
  dot_file <- file.path(dot_dir, "dot77-8090.dta")
  if (file.exists(dot_file)) return(dot_dir)

  dir.create(dot_dir, recursive = TRUE, showWarnings = FALSE)
  zip_path <- file.path(cache_dir, "DOT-occ-data-sets.zip")

  if (!file.exists(zip_path)) {
    message("Downloading DOT occupation task archive to cache.")
    utils::download.file(zip_url, destfile = zip_path, mode = "wb", quiet = TRUE)
  }

  utils::unzip(zip_path, exdir = dot_dir)
  require_file(dot_file, "DOT 1977 occupation task file")
  dot_dir
}

build_dot77_occ1990dd_tasks <- function(raw_dir,
                                        cache_dir,
                                        zip_url,
                                        tie_tolerance = sqrt(.Machine$double.eps),
                                        stop_on_ties = TRUE,
                                        tie_audit_path = NULL,
                                        diagnostic_path = NULL) {
  dot_dir <- ensure_dot77_archive(cache_dir, zip_url)

  dot77 <- haven::read_dta(file.path(dot_dir, "dot77-8090.dta")) %>%
    dplyr::transmute(
      occ = as.integer(.data$occ8090),
      task_nonroutine_analytic = as.numeric(.data$math),
      task_nonroutine_interactive = as.numeric(.data$dcp),
      task_nonroutine_manual = as.numeric(.data$ehf),
      task_routine_cognitive = as.numeric(.data$sts),
      task_routine_manual = as.numeric(.data$finger)
    ) %>%
    dplyr::filter(!is.na(.data$occ))

  cw90 <- haven::read_dta(file.path(raw_dir, "occ1990_occ1990dd", "occ1990_occ1990dd.dta")) %>%
    dplyr::transmute(
      occ = as.integer(.data$occ),
      occ1990dd = as.integer(.data$occ1990dd)
    ) %>%
    dplyr::filter(!is.na(.data$occ), !is.na(.data$occ1990dd))

  score_cols <- c(
    "task_nonroutine_analytic",
    "task_nonroutine_interactive",
    "task_nonroutine_manual",
    "task_routine_cognitive",
    "task_routine_manual"
  )
  task_groups <- c(
    "nonroutine_analytic",
    "nonroutine_interactive",
    "nonroutine_manual",
    "routine_cognitive",
    "routine_manual"
  )

  tasks <- cw90 %>%
    dplyr::inner_join(dot77, by = "occ") %>%
    dplyr::filter(.data$occ1990dd >= 4L, .data$occ1990dd <= 889L) %>%
    dplyr::group_by(.data$occ1990dd) %>%
    dplyr::summarise(
      dplyr::across(dplyr::all_of(score_cols), ~ mean(.x, na.rm = TRUE)),
      source_occ_codes = paste(sort(unique(.data$occ)), collapse = ";"),
      source_occ_n = dplyr::n_distinct(.data$occ),
      .groups = "drop"
    )

  score_mat <- as.matrix(tasks[, score_cols])
  max_score <- apply(score_mat, 1L, max, na.rm = TRUE)
  tie_n <- rowSums(abs(score_mat - max_score) <= tie_tolerance)
  dominant_idx <- max.col(score_mat, ties.method = "first")
  tasks$dominant_task_tie_n <- tie_n
  tasks$dominant_task_tied_groups <- apply(score_mat, 1L, function(row) {
    paste(task_groups[abs(row - max(row, na.rm = TRUE)) <= tie_tolerance], collapse = ";")
  })
  tasks$alm_task_group <- task_groups[dominant_idx]

  tied <- tasks[tasks$dominant_task_tie_n > 1L, ]
  if (nrow(tied) > 0L) {
    if (!is.null(tie_audit_path)) write_white_csv(tied, tie_audit_path)
    if (isTRUE(stop_on_ties)) {
      stop(
        "Found exact highest-score ties in DOT 1977 ALM task classification. ",
        "Audit written to: ", tie_audit_path
      )
    }
  }

  tasks$alm_nonroutine_analytic <- as.integer(tasks$alm_task_group == "nonroutine_analytic")
  tasks$alm_nonroutine_interactive <- as.integer(tasks$alm_task_group == "nonroutine_interactive")
  tasks$alm_nonroutine_manual <- as.integer(tasks$alm_task_group == "nonroutine_manual")
  tasks$alm_routine_cognitive <- as.integer(tasks$alm_task_group == "routine_cognitive")
  tasks$alm_routine_manual <- as.integer(tasks$alm_task_group == "routine_manual")
  tasks$alm_routine <- tasks$alm_routine_cognitive + tasks$alm_routine_manual
  tasks$alm_nonroutine <- tasks$alm_nonroutine_analytic +
    tasks$alm_nonroutine_interactive +
    tasks$alm_nonroutine_manual

  dorn_task_path <- file.path(raw_dir, "occ1990dd_task_alm", "occ1990dd_task_alm.dta")
  if (file.exists(dorn_task_path)) {
    dorn_tasks <- haven::read_dta(dorn_task_path) %>%
      dplyr::transmute(
        occ1990dd = as.integer(.data$occ1990dd),
        dorn_task_abstract = as.numeric(.data$task_abstract),
        dorn_task_routine = as.numeric(.data$task_routine),
        dorn_task_manual = as.numeric(.data$task_manual)
      )

    diagnostic <- tasks %>%
      dplyr::transmute(
        occ1990dd,
        task_abstract_rebuilt = (.data$task_nonroutine_analytic + .data$task_nonroutine_interactive) / 2,
        task_routine_rebuilt = (.data$task_routine_cognitive + .data$task_routine_manual) / 2,
        task_manual_rebuilt = .data$task_nonroutine_manual
      ) %>%
      dplyr::left_join(dorn_tasks, by = "occ1990dd")

    cor_diagnostic <- data.frame(
      comparison = c("abstract", "routine", "manual"),
      correlation = c(
        stats::cor(diagnostic$task_abstract_rebuilt, diagnostic$dorn_task_abstract, use = "pairwise.complete.obs"),
        stats::cor(diagnostic$task_routine_rebuilt, diagnostic$dorn_task_routine, use = "pairwise.complete.obs"),
        stats::cor(diagnostic$task_manual_rebuilt, diagnostic$dorn_task_manual, use = "pairwise.complete.obs")
      )
    )

    if (!is.null(diagnostic_path)) write_white_csv(cor_diagnostic, diagnostic_path)
  }

  tasks
}

configure_ipums_key <- function() {
  key <- Sys.getenv("IPUMS_API_KEY", unset = "")
  if (!nzchar(key)) {
    stop(
      "IPUMS_API_KEY is not set. Configure it outside tracked code, e.g. ",
      "ipumsr::set_ipums_api_key(\"<your key>\", save = TRUE), then rerun."
    )
  }
  invisible(TRUE)
}

extract_sample_year <- function(sample_info) {
  from_desc <- stringr::str_extract(sample_info$description, "(19|20)[0-9]{2}")
  from_name <- stringr::str_extract(sample_info$name, "(19|20)[0-9]{2}")
  as.integer(dplyr::coalesce(from_desc, from_name))
}

get_basic_monthly_samples <- function(start_year = 1968L, end_year = Inf) {
  configure_ipums_key()

  sample_info <- ipumsr::get_sample_info(collection = "cps")
  sample_info$sample_year <- extract_sample_year(sample_info)

  sample_info %>%
    dplyr::filter(
      !is.na(.data$sample_year),
      .data$sample_year >= start_year,
      .data$sample_year <= end_year,
      !stringr::str_detect(.data$description, stringr::regex("ASEC|Annual Social|March Supplement", ignore_case = TRUE))
    ) %>%
    dplyr::arrange(.data$sample_year, .data$name) %>%
    dplyr::pull(.data$name)
}

fetch_or_read_ipums_cps <- function(extract_dir,
                                    description,
                                    start_year = 1968L,
                                    end_year = Inf) {
  xml_files <- list.files(extract_dir, pattern = "\\.xml$", full.names = TRUE)
  dat_files <- list.files(extract_dir, pattern = "\\.(dat|dat\\.gz)$", full.names = TRUE)

  if (length(xml_files) > 0L && length(dat_files) > 0L) {
    message("Using cached IPUMS extract in: ", extract_dir)
    ddi <- ipumsr::read_ipums_ddi(xml_files[[1L]])
    return(ipumsr::read_ipums_micro(ddi, verbose = FALSE) %>% tibble::as_tibble())
  }

  samples_basic <- get_basic_monthly_samples(start_year, end_year)
  if (length(samples_basic) == 0L) stop("No CPS Basic Monthly IPUMS samples matched the requested year range.")

  extract_def <- ipumsr::define_extract_micro(
    collection = "cps",
    description = description,
    samples = samples_basic,
    variables = c(
      "YEAR", "MONTH", "AGE", "EMPSTAT",
      "OCC", "OCC2010", "WTFINL", "CPSIDP"
    )
  )

  message("Submitting IPUMS CPS extract with ", length(samples_basic), " samples.")
  submitted <- ipumsr::submit_extract(extract_def)
  completed <- ipumsr::wait_for_extract(submitted, timeout_seconds = 10800, verbose = TRUE)
  downloaded <- ipumsr::download_extract(completed, download_dir = extract_dir, overwrite = TRUE)
  ddi <- ipumsr::read_ipums_ddi(downloaded)
  ipumsr::read_ipums_micro(ddi, verbose = FALSE) %>% tibble::as_tibble()
}

map_cps_to_occ1990dd <- function(cps_raw, crosswalks) {
  cps_base <- cps_raw %>%
    dplyr::mutate(
      year = as.integer(.data$YEAR),
      month = as.integer(.data$MONTH),
      ym = lubridate::make_date(.data$year, .data$month, 1L),
      age = as.integer(.data$AGE),
      empstat = as.integer(.data$EMPSTAT),
      weight = as.numeric(.data$WTFINL)
    )

  map_regime <- function(data, year_min, year_max, scheme, code_var) {
    cw <- crosswalks %>%
      dplyr::filter(.data$occ_scheme == scheme) %>%
      dplyr::select(occ_code, occ1990dd, year_min, year_max, occ_scheme)

    data %>%
      dplyr::filter(.data$year >= year_min, .data$year <= year_max) %>%
      dplyr::mutate(occ_code = as.integer(.data[[code_var]])) %>%
      dplyr::left_join(cw, by = "occ_code")
  }

  dplyr::bind_rows(
    map_regime(cps_base, 1968L, 1970L, "occ1960", "OCC"),
    map_regime(cps_base, 1971L, 1982L, "occ1970", "OCC"),
    map_regime(cps_base, 1983L, 1991L, "occ1980", "OCC"),
    map_regime(cps_base, 1992L, 2002L, "occ1990", "OCC"),
    map_regime(cps_base, 2003L, 2010L, "occ2000", "OCC"),
    map_regime(cps_base, 2011L, 9999L, "occ2010", "OCC2010")
  ) %>%
    dplyr::arrange(.data$year, .data$month)
}

add_alm5_tasks_to_cps <- function(cps_xwalked, alm5_tasks) {
  cps_xwalked %>%
    dplyr::left_join(alm5_tasks, by = "occ1990dd") %>%
    dplyr::mutate(
      mapped_alm5 = as.integer(!is.na(.data$alm_task_group))
    )
}

write_individual_alm5_csv <- function(individual, path) {
  keep_cols <- c(
    "YEAR", "MONTH", "ym", "AGE", "EMPSTAT", "OCC", "OCC2010",
    "WTFINL", "CPSIDP", "year", "month", "age", "empstat", "weight",
    "occ_code", "occ_scheme", "occ1990dd", "alm_task_group", "mapped_alm5",
    "task_nonroutine_analytic", "task_nonroutine_interactive",
    "task_nonroutine_manual", "task_routine_cognitive", "task_routine_manual",
    "alm_nonroutine_analytic", "alm_nonroutine_interactive",
    "alm_nonroutine_manual", "alm_routine_cognitive", "alm_routine_manual",
    "alm_routine", "alm_nonroutine"
  )
  keep_cols <- intersect(keep_cols, names(individual))
  readr::write_csv(individual[, keep_cols], path, na = "")
}

validate_individual_alm5 <- function(individual, out_dir) {
  five_dummy_cols <- c(
    "alm_nonroutine_analytic",
    "alm_nonroutine_interactive",
    "alm_nonroutine_manual",
    "alm_routine_cognitive",
    "alm_routine_manual"
  )

  mapped <- individual[!is.na(individual$alm_task_group), ]
  five_sum <- rowSums(mapped[, five_dummy_cols], na.rm = FALSE)
  bad_onehot <- mapped[is.na(five_sum) | five_sum != 1L, ]

  if (nrow(bad_onehot) > 0L) {
    audit_path <- file.path(out_dir, "ipums_cps_alm5_bad_onehot_rows.csv")
    write_white_csv(utils::head(bad_onehot, 500L), audit_path)
    stop("Some mapped observations are not one-hot in the five ALM task dummies. Audit: ", audit_path)
  }

  routine_sum <- mapped$alm_routine + mapped$alm_nonroutine
  bad_agg <- mapped[is.na(routine_sum) | routine_sum != 1L, ]
  if (nrow(bad_agg) > 0L) {
    audit_path <- file.path(out_dir, "ipums_cps_alm5_bad_routine_nonroutine_rows.csv")
    write_white_csv(utils::head(bad_agg, 500L), audit_path)
    stop("Some mapped observations are not one-hot in routine/nonroutine dummies. Audit: ", audit_path)
  }

  invisible(TRUE)
}

build_monthly_alm5_panel <- function(individual) {
  individual %>%
    dplyr::filter(.data$age >= 16L) %>%
    dplyr::mutate(
      employed = as.integer(.data$empstat %in% c(10L, 12L)),
      mapped_employed = .data$employed * .data$mapped_alm5,
      nr_analytic_jobber = .data$employed * dplyr::coalesce(.data$alm_nonroutine_analytic, 0L),
      nr_interactive_jobber = .data$employed * dplyr::coalesce(.data$alm_nonroutine_interactive, 0L),
      nr_manual_jobber = .data$employed * dplyr::coalesce(.data$alm_nonroutine_manual, 0L),
      routine_cognitive_jobber = .data$employed * dplyr::coalesce(.data$alm_routine_cognitive, 0L),
      routine_manual_jobber = .data$employed * dplyr::coalesce(.data$alm_routine_manual, 0L),
      routine_jobber = .data$employed * dplyr::coalesce(.data$alm_routine, 0L),
      nonroutine_jobber = .data$employed * dplyr::coalesce(.data$alm_nonroutine, 0L)
    ) %>%
    dplyr::group_by(.data$ym) %>%
    dplyr::summarise(
      date = min(.data$ym),
      pop_16plus = sum(.data$weight, na.rm = TRUE),
      total_emp_abs = sum(.data$weight * .data$employed, na.rm = TRUE),
      mapped_emp_abs = sum(.data$weight * .data$mapped_employed, na.rm = TRUE),
      nonroutine_analytic_emp_abs = sum(.data$weight * .data$nr_analytic_jobber, na.rm = TRUE),
      nonroutine_interactive_emp_abs = sum(.data$weight * .data$nr_interactive_jobber, na.rm = TRUE),
      nonroutine_manual_emp_abs = sum(.data$weight * .data$nr_manual_jobber, na.rm = TRUE),
      routine_cognitive_emp_abs = sum(.data$weight * .data$routine_cognitive_jobber, na.rm = TRUE),
      routine_manual_emp_abs = sum(.data$weight * .data$routine_manual_jobber, na.rm = TRUE),
      routine_emp_abs = sum(.data$weight * .data$routine_jobber, na.rm = TRUE),
      nonroutine_emp_abs = sum(.data$weight * .data$nonroutine_jobber, na.rm = TRUE),
      n_obs = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::arrange(.data$date) %>%
    dplyr::mutate(
      unmapped_emp_abs = .data$total_emp_abs - .data$mapped_emp_abs,
      mapped_emp_share = .data$mapped_emp_abs / .data$total_emp_abs,
      routine_share = .data$routine_emp_abs / .data$total_emp_abs,
      nonroutine_share = .data$nonroutine_emp_abs / .data$total_emp_abs,
      routine_share_percent = 100 * .data$routine_share,
      routine_emp_per_capita = .data$routine_emp_abs / .data$pop_16plus,
      log_total = log(.data$total_emp_abs),
      log_routine = log(.data$routine_emp_abs),
      log_nonroutine = log(.data$nonroutine_emp_abs)
    )
}

add_alm5_seasonal_adjustment <- function(monthly) {
  if (!exists("add_statsmodels_stl_sa", mode = "function")) {
    stop("Missing add_statsmodels_stl_sa(). Source functions.R before seasonal adjustment.")
  }

  panel_for_sa <- monthly %>%
    dplyr::transmute(
      date,
      routine_emp = .data$routine_emp_abs,
      nonroutine_emp = .data$nonroutine_emp_abs,
      total_emp = .data$total_emp_abs
    )

  sa <- add_statsmodels_stl_sa(panel_for_sa) %>%
    dplyr::select(
      date,
      routine_emp_sa,
      nonroutine_emp_sa,
      total_emp_sa,
      routine_share_sa,
      log_total_sa,
      log_routine_sa,
      log_nonroutine_sa
    )

  monthly %>%
    dplyr::left_join(sa, by = "date") %>%
    dplyr::mutate(
      routine_emp_per_capita_sa = .data$routine_emp_sa / .data$pop_16plus,
      routine_share_percent_sa = 100 * .data$routine_share_sa
    )
}

write_mapping_audits <- function(individual, monthly, out_dir) {
  yearly_mapping <- individual %>%
    dplyr::filter(.data$age >= 16L, .data$empstat %in% c(10L, 12L)) %>%
    dplyr::group_by(.data$year) %>%
    dplyr::summarise(
      total_emp_weight = sum(.data$weight, na.rm = TRUE),
      mapped_emp_weight = sum(.data$weight * .data$mapped_alm5, na.rm = TRUE),
      mapped_emp_share = .data$mapped_emp_weight / .data$total_emp_weight,
      .groups = "drop"
    )

  write_white_csv(yearly_mapping, file.path(out_dir, "ipums_cps_alm5_yearly_mapping_audit.csv"))

  monthly_audit <- monthly %>%
    dplyr::select(date, n_obs, total_emp_abs, mapped_emp_abs, unmapped_emp_abs, mapped_emp_share)
  write_white_csv(monthly_audit, file.path(out_dir, "ipums_cps_alm5_monthly_mapping_audit.csv"))

  if (any(monthly$mapped_emp_share < 0.95, na.rm = TRUE)) {
    warning("At least one month maps less than 95 percent of weighted employment to ALM tasks. See mapping audit files.")
  }
}


# 3. Build ALM task map ---------------------------------------------------

occupation_crosswalks <- build_occupation_crosswalks(external_raw_dir)

alm5_task_map <- build_dot77_occ1990dd_tasks(
  raw_dir = external_raw_dir,
  cache_dir = cache_dir,
  zip_url = dot77_zip_url,
  tie_tolerance = task_tie_tolerance,
  stop_on_ties = stop_on_task_ties,
  tie_audit_path = file.path(out_dir, "ipums_cps_alm5_task_ties.csv"),
  diagnostic_path = file.path(out_dir, "ipums_cps_alm5_dorn_task_correlation.csv")
)

write_white_csv(alm5_task_map, file.path(input_dir, "alm5_dot77_occ1990dd_task_map.csv"))


# 4. Fetch and classify CPS microdata -------------------------------------

cps_raw <- fetch_or_read_ipums_cps(
  extract_dir = ipums_extract_dir,
  description = ipums_extract_description,
  start_year = sample_start_year,
  end_year = sample_end_year
)

cps_xwalked <- map_cps_to_occ1990dd(cps_raw, occupation_crosswalks)
cps_alm5 <- add_alm5_tasks_to_cps(cps_xwalked, alm5_task_map)

validate_individual_alm5(cps_alm5, out_dir)

individual_out <- file.path(input_dir, "ipums_cps_alm5_individual.csv.gz")
write_individual_alm5_csv(cps_alm5, individual_out)


# 5. Aggregate and plot Figures 1 and 2 -----------------------------------

monthly_alm5 <- build_monthly_alm5_panel(cps_alm5)
monthly_alm5 <- add_alm5_seasonal_adjustment(monthly_alm5)

write_mapping_audits(cps_alm5, monthly_alm5, out_dir)

monthly_out <- file.path(input_dir, "ipums_cps_alm5_monthly.csv")
fig12_out <- file.path(out_dir, "ipums_cps_alm5_figures_1_2_series.csv")

write_white_csv(monthly_alm5, monthly_out)
write_white_csv(
  monthly_alm5 %>%
    dplyr::select(
      date,
      routine_emp = routine_emp_abs,
      nonroutine_emp = nonroutine_emp_abs,
      total_emp = total_emp_abs,
      population = pop_16plus,
      routine_emp_per_capita,
      routine_emp_per_capita_sa,
      routine_share,
      routine_share_sa,
      routine_share_percent,
      routine_share_percent_sa
    ),
  fig12_out
)

figure_panel <- monthly_alm5 %>%
  dplyr::filter(.data$date >= figure_start_date)

plot_descriptive_line(
  panel = figure_panel,
  column = "routine_emp_per_capita_sa",
  ylabel = "Routine Emp. / Civilian Pop. (16+)",
  title = "Per Capita Employment in Routine Jobs",
  y_ticks = c(0.20, 0.25, 0.30, 0.35, 0.40),
  y_lim = c(0.20, 0.40),
  out_path = file.path(fig_dir, "figure1_ipums_alm5_routine_employment_per_capita.png"),
  recessions = recessions
)

plot_descriptive_line(
  panel = figure_panel,
  column = "routine_share_percent_sa",
  ylabel = "Percent of Total Employment",
  title = "Routine Jobs as a Share of Total Employment",
  y_ticks = c(40, 45, 50, 55, 60, 65, 70),
  y_lim = c(40, 70),
  out_path = file.path(fig_dir, "figure2_ipums_alm5_routine_employment_share.png"),
  recessions = recessions
)

message("Wrote individual ALM file: ", individual_out)
message("Wrote monthly ALM file: ", monthly_out)
message("Wrote Figure 1/2 series: ", fig12_out)
