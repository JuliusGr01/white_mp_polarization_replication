#######################################################
###   White (2022): Employment Responses via LP     ###
#######################################################

# This script follows the style of the final project folder: a small control
# panel, explicit data import, then the local-projection exercises and saves.
# The numerical target is the Python replication in the repository root.


# 0. Control panel --------------------------------------------------------

H <- 48L
n_lags_y <- 12L
n_lags_shock <- 12L
nw_lags <- 12L
confint <- 1.645

y_lag_transform <- "diff"
include_time_trend <- FALSE

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


# 1. Data import ----------------------------------------------------------

# The Python pipeline already materializes the exact seasonal-adjusted panels.
# We use those as the canonical inputs here, because base R STL and statsmodels
# STL are not identical at machine precision.

fig12_reference <- file.path(ref_dir, "python_figures_1_2_series.csv")
lp_panel_file <- file.path(input_dir, "employment_monthly_white_lp.csv")
lp_reference <- file.path(ref_dir, "python_employment_monthly_white_lp.csv")
extended_reference <- file.path(ref_dir, "python_employment_monthly_extended.csv")

if (!file.exists(fig12_reference)) stop("Missing reference file: ", fig12_reference)
if (!file.exists(lp_panel_file) && !file.exists(lp_reference)) {
  stop("Missing LP panel. Run 1_Data.R or provide reference file: ", lp_reference)
}

routine_ts_fig12 <- read_white_csv(fig12_reference)
routine_ts_fig12$date <- as.Date(routine_ts_fig12$date)

if (file.exists(lp_panel_file)) {
  routine_ts_sa <- read_white_csv(lp_panel_file)
} else {
  routine_ts_sa <- read_white_csv(lp_reference)
}
routine_ts_sa$date <- as.Date(routine_ts_sa$date)


# 2. Figures 1 and 2 ------------------------------------------------------

plot_descriptive_line(
  panel = routine_ts_fig12,
  column = "routine_emp_per_capita_sa",
  ylabel = "Routine Emp. / Civilian Pop. (16+)",
  title = "Per Capita Employment in Routine Jobs",
  y_ticks = c(0.20, 0.25, 0.30, 0.35, 0.40),
  y_lim = c(0.20, 0.40),
  out_path = file.path(fig_dir, "figure1_routine_employment_per_capita.png"),
  recessions = recessions
)

plot_descriptive_line(
  panel = routine_ts_fig12,
  column = "routine_share_percent_sa",
  ylabel = "Percent of Total Employment",
  title = "Routine Jobs as a Share of Total Employment",
  y_ticks = c(40, 45, 50, 55, 60, 65, 70),
  y_lim = c(40, 70),
  out_path = file.path(fig_dir, "figure2_routine_employment_share.png"),
  recessions = recessions
)


# 3. Linear LP estimation -------------------------------------------------

irf_level_total <- LP_white(
  data = routine_ts_sa,
  H = H,
  y_var = "log_total",
  shock_var = "eps",
  n_lags_y = n_lags_y,
  n_lags_shock = n_lags_shock,
  nw_lags = nw_lags,
  scale = 100,
  confint = confint,
  y_lag_transform = y_lag_transform,
  include_time_trend = include_time_trend
)

irf_level_routine <- LP_white(
  data = routine_ts_sa,
  H = H,
  y_var = "log_routine",
  shock_var = "eps",
  n_lags_y = n_lags_y,
  n_lags_shock = n_lags_shock,
  nw_lags = nw_lags,
  scale = 100,
  confint = confint,
  y_lag_transform = y_lag_transform,
  include_time_trend = include_time_trend
)

irf_level_nonroutine <- LP_white(
  data = routine_ts_sa,
  H = H,
  y_var = "log_nonroutine",
  shock_var = "eps",
  n_lags_y = n_lags_y,
  n_lags_shock = n_lags_shock,
  nw_lags = nw_lags,
  scale = 100,
  confint = confint,
  y_lag_transform = y_lag_transform,
  include_time_trend = include_time_trend
)

irf_routine_share <- LP_white(
  data = routine_ts_sa,
  H = H,
  y_var = "routine_share",
  shock_var = "eps",
  n_lags_y = n_lags_y,
  n_lags_shock = n_lags_shock,
  nw_lags = nw_lags,
  scale = 100,
  confint = confint,
  y_lag_transform = y_lag_transform,
  include_time_trend = include_time_trend
)

plot_irf_white(
  irf_df = irf_level_total,
  out_path = file.path(fig_dir, "irf_linear_log_total_employment.png"),
  title = "Linear LP - log total employment",
  ylabel = "Percent"
)

plot_irf_white(
  irf_df = irf_level_routine,
  out_path = file.path(fig_dir, "irf_linear_log_routine_employment.png"),
  title = "Linear LP - log routine employment",
  ylabel = "Percent"
)

plot_irf_white(
  irf_df = irf_level_nonroutine,
  out_path = file.path(fig_dir, "irf_linear_log_nonroutine_employment.png"),
  title = "Linear LP - log nonroutine employment",
  ylabel = "Percent"
)

plot_irf_white(
  irf_df = irf_routine_share,
  out_path = file.path(fig_dir, "irf_linear_routine_share_of_employment.png"),
  title = "Linear LP - routine share of employment",
  ylabel = "% Points"
)


# 4. Sign-split LPs -------------------------------------------------------

irf_sign_total <- LP_white_sign(
  routine_ts_sa, H, "log_total", "eps",
  n_lags_y, n_lags_shock, nw_lags, 100, confint,
  y_lag_transform, include_time_trend
)

irf_sign_routine <- LP_white_sign(
  routine_ts_sa, H, "log_routine", "eps",
  n_lags_y, n_lags_shock, nw_lags, 100, confint,
  y_lag_transform, include_time_trend
)

irf_sign_nonroutine <- LP_white_sign(
  routine_ts_sa, H, "log_nonroutine", "eps",
  n_lags_y, n_lags_shock, nw_lags, 100, confint,
  y_lag_transform, include_time_trend
)

irf_sign_share <- LP_white_sign(
  routine_ts_sa, H, "routine_share", "eps",
  n_lags_y, n_lags_shock, nw_lags, 100, confint,
  y_lag_transform, include_time_trend
)

plot_irf_white(irf_sign_total[irf_sign_total$shock == "eps_plus", ], file.path(fig_dir, "irf_sign_contraction_log_total_employment.png"), "Sign-split LP - contractionary eps+ - log total employment", "Percent")
plot_irf_white(irf_sign_total[irf_sign_total$shock == "eps_minus", ], file.path(fig_dir, "irf_sign_expansion_log_total_employment.png"), "Sign-split LP - expansionary eps- - log total employment", "Percent")

plot_irf_white(irf_sign_routine[irf_sign_routine$shock == "eps_plus", ], file.path(fig_dir, "irf_sign_contraction_log_routine_employment.png"), "Sign-split LP - contractionary eps+ - log routine employment", "Percent")
plot_irf_white(irf_sign_routine[irf_sign_routine$shock == "eps_minus", ], file.path(fig_dir, "irf_sign_expansion_log_routine_employment.png"), "Sign-split LP - expansionary eps- - log routine employment", "Percent")

plot_irf_white(irf_sign_nonroutine[irf_sign_nonroutine$shock == "eps_plus", ], file.path(fig_dir, "irf_sign_contraction_log_nonroutine_employment.png"), "Sign-split LP - contractionary eps+ - log nonroutine employment", "Percent")
plot_irf_white(irf_sign_nonroutine[irf_sign_nonroutine$shock == "eps_minus", ], file.path(fig_dir, "irf_sign_expansion_log_nonroutine_employment.png"), "Sign-split LP - expansionary eps- - log nonroutine employment", "Percent")

plot_irf_white(irf_sign_share[irf_sign_share$shock == "eps_plus", ], file.path(fig_dir, "irf_sign_contraction_routine_share_of_employment.png"), "Sign-split LP - contractionary eps+ - routine share of employment", "% Points")
plot_irf_white(irf_sign_share[irf_sign_share$shock == "eps_minus", ], file.path(fig_dir, "irf_sign_expansion_routine_share_of_employment.png"), "Sign-split LP - expansionary eps- - routine share of employment", "% Points")


# 5. Quadratic LPs --------------------------------------------------------

irf_quad_total <- LP_white_quad(
  routine_ts_sa, H, "log_total", "eps",
  n_lags_y, n_lags_shock, nw_lags, 100, confint,
  y_lag_transform, include_time_trend
)

irf_quad_routine <- LP_white_quad(
  routine_ts_sa, H, "log_routine", "eps",
  n_lags_y, n_lags_shock, nw_lags, 100, confint,
  y_lag_transform, include_time_trend
)

irf_quad_nonroutine <- LP_white_quad(
  routine_ts_sa, H, "log_nonroutine", "eps",
  n_lags_y, n_lags_shock, nw_lags, 100, confint,
  y_lag_transform, include_time_trend
)

irf_quad_share <- LP_white_quad(
  routine_ts_sa, H, "routine_share", "eps",
  n_lags_y, n_lags_shock, nw_lags, 100, confint,
  y_lag_transform, include_time_trend
)

plot_irf_white(irf_quad_total[irf_quad_total$shock == "+1pp", ], file.path(fig_dir, "irf_quad_contraction_log_total_employment.png"), "Quadratic LP - +1 pp shock - log total employment", "Percent")
plot_irf_white(irf_quad_total[irf_quad_total$shock == "-1pp", ], file.path(fig_dir, "irf_quad_expansion_log_total_employment.png"), "Quadratic LP - -1 pp shock - log total employment", "Percent")

plot_irf_white(irf_quad_routine[irf_quad_routine$shock == "+1pp", ], file.path(fig_dir, "irf_quad_contraction_log_routine_employment.png"), "Quadratic LP - +1 pp shock - log routine employment", "Percent")
plot_irf_white(irf_quad_routine[irf_quad_routine$shock == "-1pp", ], file.path(fig_dir, "irf_quad_expansion_log_routine_employment.png"), "Quadratic LP - -1 pp shock - log routine employment", "Percent")

plot_irf_white(irf_quad_nonroutine[irf_quad_nonroutine$shock == "+1pp", ], file.path(fig_dir, "irf_quad_contraction_log_nonroutine_employment.png"), "Quadratic LP - +1 pp shock - log nonroutine employment", "Percent")
plot_irf_white(irf_quad_nonroutine[irf_quad_nonroutine$shock == "-1pp", ], file.path(fig_dir, "irf_quad_expansion_log_nonroutine_employment.png"), "Quadratic LP - -1 pp shock - log nonroutine employment", "Percent")

plot_irf_white(irf_quad_share[irf_quad_share$shock == "+1pp", ], file.path(fig_dir, "irf_quad_contraction_routine_share_of_employment.png"), "Quadratic LP - +1 pp shock - routine share of employment", "% Points")
plot_irf_white(irf_quad_share[irf_quad_share$shock == "-1pp", ], file.path(fig_dir, "irf_quad_expansion_routine_share_of_employment.png"), "Quadratic LP - -1 pp shock - routine share of employment", "% Points")


# 6. Figure 3 -------------------------------------------------------------

figure3_irfs_raw <- list(
  log_total = irf_level_total,
  log_routine = irf_level_routine,
  log_nonroutine = irf_level_nonroutine,
  routine_share = irf_routine_share
)

plot_figure3_white(
  irf_list = figure3_irfs_raw,
  out_path = file.path(fig_dir, "figure3_linear_occupations.png")
)

figure3_irfs_df <- write_figure3_irf_csv(
  irfs = figure3_irfs_raw,
  out_path = file.path(tab_dir, "figure3_linear_irfs.csv")
)


# 7. FEV table ------------------------------------------------------------

fev_total <- FEV_white(
  routine_ts_sa, H, "log_total", "eps",
  n_lags_y, n_lags_shock, y_lag_transform, include_time_trend
)

fev_routine <- FEV_white(
  routine_ts_sa, H, "log_routine", "eps",
  n_lags_y, n_lags_shock, y_lag_transform, include_time_trend
)

fev_nonroutine <- FEV_white(
  routine_ts_sa, H, "log_nonroutine", "eps",
  n_lags_y, n_lags_shock, y_lag_transform, include_time_trend
)

fev_share <- FEV_white(
  routine_ts_sa, H, "routine_share", "eps",
  n_lags_y, n_lags_shock, y_lag_transform, include_time_trend
)

write_fev_csv(
  fev_rows = list(fev_total, fev_routine, fev_nonroutine, fev_share),
  out_path = file.path(tab_dir, "fev_linear.csv")
)

# Final message

message("Done. R outputs are in the folder: ", print(out_dir))
