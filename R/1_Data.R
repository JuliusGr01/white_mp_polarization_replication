#######################################################
###   1. Data Build: Excel + BLS Raw Files          ###
#######################################################

# This file documents the construction of the monthly panel used by 2_LP.R.
# It starts from the Employment and Earnings Excel extraction for 1969-1982 and
# the BLS flat files in data/bls_raw.  The final output is the same merged LP
# panel that 2_LP.R estimates on.


# 0. Control panel --------------------------------------------------------

get_data_script_dir <- function() {
  sourced_file <- tryCatch(
    normalizePath(sys.frames()[[1]]$ofile, winslash = "/", mustWork = TRUE),
    error = function(e) NA_character_
  )
  if (!is.na(sourced_file)) return(dirname(sourced_file))

  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- "--file="
  hit <- grep(paste0("^", file_arg), args)
  if (length(hit) > 0L) {
    return(dirname(normalizePath(sub(file_arg, "", args[hit[1L]]), winslash = "/", mustWork = TRUE)))
  }

  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

setwd(get_data_script_dir())

if (!exists("input_dir")) input_dir <- file.path("data")
if (!exists("ref_dir")) ref_dir <- file.path("reference")
if (!exists("out_dir")) out_dir <- file.path("output")
if (!exists("cache_dir")) cache_dir <- file.path(out_dir, "cache")
if (!exists("fig_dir")) fig_dir <- out_dir
if (!exists("tab_dir")) tab_dir <- out_dir
if (!exists("read_white_csv")) source("functions.R")

build_dir <- file.path(out_dir, "data_build")
dir.create(build_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(input_dir, recursive = TRUE, showWarnings = FALSE)

cps_xlsx <- file.path(input_dir, "1969_1982_CPS.xlsx")
crosswalk_csv <- file.path(input_dir, "cps_ee_1969_1982_alm_crosswalk.csv")
bls_raw_all_data <- file.path(input_dir, "bls_raw", "ln.data.1.AllData")
rr_shock_csv <- file.path(input_dir, "RR_MPshocks_Updated(GBforecasts).csv")

if (!file.exists(cps_xlsx)) stop("Missing CPS Excel workbook: ", cps_xlsx)
if (!file.exists(crosswalk_csv)) stop("Missing ALM crosswalk: ", crosswalk_csv)
if (!file.exists(bls_raw_all_data)) stop("Missing BLS raw file: ", bls_raw_all_data)
if (!file.exists(rr_shock_csv)) stop("Missing Romer-Romer shock file: ", rr_shock_csv)


# 1. Historical CPS Employment and Earnings data -------------------------

cps_ee_1969_1982 <- build_cps_ee_from_excel(
  xlsx_path = cps_xlsx,
  crosswalk_path = crosswalk_csv,
  out_dir = input_dir
)

copy_if_present(
  file.path(input_dir, "cps_ee_1969_1982_leaf_alm_long.csv"),
  file.path(build_dir, "cps_ee_1969_1982_leaf_alm_long.csv")
)

copy_if_present(
  file.path(input_dir, "cps_ee_1969_1982_alm_panel.csv"),
  file.path(build_dir, "cps_ee_1969_1982_alm_panel.csv")
)

copy_if_present(
  file.path(input_dir, "cps_ee_1969_1982_employment_monthly.csv"),
  file.path(build_dir, "cps_ee_1969_1982_employment_monthly.csv")
)


# 2. BLS broad occupation series, 1983 onward ----------------------------

bls_occ_panel <- build_bls_occ_panel_from_raw(
  all_data_path = bls_raw_all_data,
  out_path = file.path(build_dir, "bls_occ_employment_monthly_from_raw.csv")
)


# 3. Merge historical CPS and BLS occupation panels ----------------------

employment_monthly_extended <- build_extended_panel_from_cps_and_bls_raw(
  cps_ee = cps_ee_1969_1982,
  bls_raw_path = bls_raw_all_data,
  out_path = file.path(input_dir, "employment_monthly_extended.csv")
)

copy_if_present(
  file.path(input_dir, "employment_monthly_extended.csv"),
  file.path(build_dir, "employment_monthly_extended.csv")
)


# 4. Build exact White-style LP panel ------------------------------------

# The Python pipeline used statsmodels STL for seasonal adjustment.  To keep
# this R build numerically identical to the Python replication, the STL step is
# delegated to Python from this R script.  Everything around that step is built
# from the Excel workbook and BLS raw files above.

white_lp_panel <- build_white_lp_panel_from_raw(
  extended_panel = employment_monthly_extended,
  bls_raw_path = bls_raw_all_data,
  rr_shock_path = rr_shock_csv,
  out_path = file.path(input_dir, "employment_monthly_white_lp.csv")
)

copy_if_present(
  file.path(input_dir, "employment_monthly_white_lp.csv"),
  file.path(build_dir, "employment_monthly_white_lp.csv")
)


# 5. Validation -----------------------------------------------------------

reference_lp <- file.path(ref_dir, "python_employment_monthly_white_lp.csv")
if (file.exists(reference_lp)) {
  panel_validation <- validate_panel_against_reference(
    built_path = file.path(input_dir, "employment_monthly_white_lp.csv"),
    reference_path = reference_lp,
    out_path = file.path(build_dir, "employment_monthly_white_lp_validation.csv")
  )

  max_panel_diff <- max(panel_validation$max_abs_diff, na.rm = TRUE)
  message("Max absolute difference vs. current 2_LP.R reference panel: ", signif(max_panel_diff, 6))
}

message("Built data files under: ", normalizePath(build_dir, winslash = "/", mustWork = FALSE))
