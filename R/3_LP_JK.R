#######################################################
###      Figure 3 with Jarocinski-Karadi Shocks     ###
#######################################################

# This script mirrors the Figure 3 exercise in 2_LP.R, but replaces the
# prolonged Romer-Romer shock with the Jarocinski-Karadi monthly decomposition.
# MP and CBI are estimated jointly in each LP, with lags of both components.


# 0. Control panel --------------------------------------------------------

H <- 48L
n_lags_y <- 12L
n_lags_shock <- 12L
nw_lags <- 12L
confint <- 1.645

y_lag_transform <- "diff"
include_time_trend <- FALSE

jk_shock_vars <- c("MP", "CBI")
jk_shock_labels <- c(MP = "MP", CBI = "CBI")

comparison_start <- as.Date("1990-01-01")
comparison_end <- as.Date("2008-12-01")
comparison_shock_vars <- c("eps", "MP", "CBI", "pc1_hf")
comparison_shock_labels <- c(
  eps = "Romer-Romer",
  MP = "JK MP",
  CBI = "JK CBI",
  pc1_hf = "JK pc1_hf"
)
comparison_shock_colors <- c(
  eps = "black",
  MP = "steelblue4",
  CBI = "firebrick3",
  pc1_hf = "darkgreen"
)
comparison_shock_ltys <- c(eps = 1, MP = 2, CBI = 3, pc1_hf = 4)

if (!exists("input_dir")) input_dir <- file.path("data")
if (!exists("ref_dir")) ref_dir <- file.path("reference")
if (!exists("out_dir")) out_dir <- file.path("output")
if (!exists("fig_dir")) fig_dir <- out_dir
if (!exists("tab_dir")) tab_dir <- out_dir

dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(tab_dir, recursive = TRUE, showWarnings = FALSE)

if (!exists("read_white_csv")) source("functions.R")


# 1. Data import ----------------------------------------------------------

lp_panel_file <- file.path(input_dir, "employment_monthly_white_lp.csv")
lp_reference <- file.path(ref_dir, "python_employment_monthly_white_lp.csv")
jk_shock_file <- file.path(input_dir, "shocks_fed_jk_m.csv")

if (!file.exists(lp_panel_file) && !file.exists(lp_reference)) {
  stop("Missing LP panel. Run 1_Data.R or provide reference file: ", lp_reference)
}
if (!file.exists(jk_shock_file)) stop("Missing JK shock file: ", jk_shock_file)

if (file.exists(lp_panel_file)) {
  routine_ts_sa <- read_white_csv(lp_panel_file)
} else {
  routine_ts_sa <- read_white_csv(lp_reference)
}
routine_ts_sa$date <- as.Date(routine_ts_sa$date)
routine_ts_sa_base <- routine_ts_sa

routine_ts_sa <- merge_jk_shocks_white(
  panel = routine_ts_sa,
  shock_path = jk_shock_file,
  mp_col = "MP_pm",
  cbi_col = "CBI_pm",
  shock_names = jk_shock_vars
)

message(
  "JK LP sample: ",
  min(routine_ts_sa$date),
  " to ",
  max(routine_ts_sa$date),
  " (",
  nrow(routine_ts_sa),
  " monthly rows before LP lag trimming)."
)


# 2. Linear LP estimation -------------------------------------------------

irf_level_total <- LP_white_multi_shock(
  data = routine_ts_sa,
  H = H,
  y_var = "log_total",
  shock_vars = jk_shock_vars,
  shock_labels = jk_shock_labels,
  n_lags_y = n_lags_y,
  n_lags_shock = n_lags_shock,
  nw_lags = nw_lags,
  scale = 100,
  confint = confint,
  y_lag_transform = y_lag_transform,
  include_time_trend = include_time_trend
)

irf_level_routine <- LP_white_multi_shock(
  data = routine_ts_sa,
  H = H,
  y_var = "log_routine",
  shock_vars = jk_shock_vars,
  shock_labels = jk_shock_labels,
  n_lags_y = n_lags_y,
  n_lags_shock = n_lags_shock,
  nw_lags = nw_lags,
  scale = 100,
  confint = confint,
  y_lag_transform = y_lag_transform,
  include_time_trend = include_time_trend
)

irf_level_nonroutine <- LP_white_multi_shock(
  data = routine_ts_sa,
  H = H,
  y_var = "log_nonroutine",
  shock_vars = jk_shock_vars,
  shock_labels = jk_shock_labels,
  n_lags_y = n_lags_y,
  n_lags_shock = n_lags_shock,
  nw_lags = nw_lags,
  scale = 100,
  confint = confint,
  y_lag_transform = y_lag_transform,
  include_time_trend = include_time_trend
)

irf_routine_share <- LP_white_multi_shock(
  data = routine_ts_sa,
  H = H,
  y_var = "routine_share",
  shock_vars = jk_shock_vars,
  shock_labels = jk_shock_labels,
  n_lags_y = n_lags_y,
  n_lags_shock = n_lags_shock,
  nw_lags = nw_lags,
  scale = 100,
  confint = confint,
  y_lag_transform = y_lag_transform,
  include_time_trend = include_time_trend
)


# 3. Figure 3 -------------------------------------------------------------

figure3_jk_irfs_raw <- list(
  log_total = irf_level_total,
  log_routine = irf_level_routine,
  log_nonroutine = irf_level_nonroutine,
  routine_share = irf_routine_share
)

plot_figure3_multi_shock_white(
  irf_list = figure3_jk_irfs_raw,
  out_path = file.path(fig_dir, "figure3_jk_mp_cbi_occupations.png"),
  shock_labels = jk_shock_labels
)

figure3_jk_irfs_df <- write_figure3_multi_shock_irf_csv(
  irfs = figure3_jk_irfs_raw,
  out_path = file.path(tab_dir, "figure3_jk_mp_cbi_irfs.csv")
)


# 4. Common-sample shock comparison --------------------------------------

# Estimate each comparison line in a separate LP on the same 1990-2008 sample,
# making each line analogous to the one-shock specification in 2_LP.R. The
# MP/CBI-only figure above remains the jointly estimated component exercise.

jk_comparison_shocks <- load_jk_shocks_white(
  path = jk_shock_file,
  mp_col = "MP_pm",
  cbi_col = "CBI_pm",
  pc1_col = "pc1_hf"
)

comparison_panel <- merge(routine_ts_sa_base, jk_comparison_shocks, by = "date", all = FALSE)
comparison_panel <- comparison_panel[
  comparison_panel$date >= comparison_start & comparison_panel$date <= comparison_end,
]
comparison_panel <- comparison_panel[
  stats::complete.cases(comparison_panel[, comparison_shock_vars, drop = FALSE]),
]
comparison_panel <- comparison_panel[order(comparison_panel$date), ]

message(
  "Shock comparison LP sample: ",
  min(comparison_panel$date),
  " to ",
  max(comparison_panel$date),
  " (",
  nrow(comparison_panel),
  " monthly rows before LP lag trimming)."
)

irf_compare_total <- LP_white_shock_comparison(
  data = comparison_panel,
  H = H,
  y_var = "log_total",
  shock_vars = comparison_shock_vars,
  shock_labels = comparison_shock_labels,
  n_lags_y = n_lags_y,
  n_lags_shock = n_lags_shock,
  nw_lags = nw_lags,
  scale = 100,
  confint = confint,
  y_lag_transform = y_lag_transform,
  include_time_trend = include_time_trend
)

irf_compare_routine <- LP_white_shock_comparison(
  data = comparison_panel,
  H = H,
  y_var = "log_routine",
  shock_vars = comparison_shock_vars,
  shock_labels = comparison_shock_labels,
  n_lags_y = n_lags_y,
  n_lags_shock = n_lags_shock,
  nw_lags = nw_lags,
  scale = 100,
  confint = confint,
  y_lag_transform = y_lag_transform,
  include_time_trend = include_time_trend
)

irf_compare_nonroutine <- LP_white_shock_comparison(
  data = comparison_panel,
  H = H,
  y_var = "log_nonroutine",
  shock_vars = comparison_shock_vars,
  shock_labels = comparison_shock_labels,
  n_lags_y = n_lags_y,
  n_lags_shock = n_lags_shock,
  nw_lags = nw_lags,
  scale = 100,
  confint = confint,
  y_lag_transform = y_lag_transform,
  include_time_trend = include_time_trend
)

irf_compare_share <- LP_white_shock_comparison(
  data = comparison_panel,
  H = H,
  y_var = "routine_share",
  shock_vars = comparison_shock_vars,
  shock_labels = comparison_shock_labels,
  n_lags_y = n_lags_y,
  n_lags_shock = n_lags_shock,
  nw_lags = nw_lags,
  scale = 100,
  confint = confint,
  y_lag_transform = y_lag_transform,
  include_time_trend = include_time_trend
)

figure3_comparison_irfs_raw <- list(
  log_total = irf_compare_total,
  log_routine = irf_compare_routine,
  log_nonroutine = irf_compare_nonroutine,
  routine_share = irf_compare_share
)

plot_figure3_multi_shock_white(
  irf_list = figure3_comparison_irfs_raw,
  out_path = file.path(fig_dir, "figure3_shock_comparison_1990_2008.png"),
  shock_labels = comparison_shock_labels,
  shock_colors = comparison_shock_colors,
  shock_ltys = comparison_shock_ltys,
  legend_position = "topright"
)

figure3_comparison_irfs_df <- write_figure3_multi_shock_irf_csv(
  irfs = figure3_comparison_irfs_raw,
  out_path = file.path(tab_dir, "figure3_shock_comparison_1990_2008_irfs.csv")
)


# 5. Final message --------------------------------------------------------

message("Done. JK Figure 3 outputs are under: ", normalizePath(out_dir, winslash = "/", mustWork = FALSE))
