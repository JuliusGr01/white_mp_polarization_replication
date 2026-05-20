#######################################################
###   1. Data Build: Excel + BLS Raw Files          ###
#######################################################

# This file documents the construction of the monthly panel used by 2_LP.R.
# It starts from the Employment and Earnings Excel extraction for 1969-1982 and
# the BLS flat files in data/bls_raw.  The final output is the merged monthly employment by occupation


# 0. Control panel --------------------------------------------------------

cps_xlsx <- file.path(input_dir, "1969_1982_CPS.xlsx")
crosswalk_csv <- file.path(input_dir, "cps_ee_1969_1982_alm_crosswalk.csv")
bls_raw_all_data <- file.path(input_dir, "bls_raw", "ln.data.1.AllData")
rr_shock_csv <- file.path(input_dir, "RR_MPshocks_Updated(GBforecasts).csv")

# 1. Historical CPS Employment and Earnings data -------------------------

cps_ee_1969_1982 <- build_cps_ee_from_excel(
  xlsx_path = cps_xlsx,
  crosswalk_path = crosswalk_csv,
  out_dir = input_dir
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


# 4. White-style LP panel for replication------------------------------------

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
