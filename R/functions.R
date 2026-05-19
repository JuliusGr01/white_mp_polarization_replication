month_start <- function(x) {
  as.Date(format(as.Date(x), "%Y-%m-01"))
}

year_num <- function(x) as.integer(format(as.Date(x), "%Y"))
month_num <- function(x) as.integer(format(as.Date(x), "%m"))

lag_vec <- function(x, n) {
  if (n == 0L) return(x)
  c(rep(NA, n), x[seq_len(length(x) - n)])
}

lead_vec <- function(x, n) {
  if (n == 0L) return(x)
  c(x[(n + 1L):length(x)], rep(NA, n))
}

read_csv_base <- function(path, ...) {
  read.csv(path, stringsAsFactors = FALSE, check.names = FALSE, ...)
}

as_numeric_loose <- function(x) {
  if (is.numeric(x)) return(x)
  as.numeric(gsub(",", ".", trimws(as.character(x)), fixed = TRUE))
}

load_bls_monthly_series <- function(series_id, source_path = file.path(DATA_DIR, "bls_raw", "ln.data.1.AllData")) {
  raw <- read.table(
    source_path,
    header = TRUE,
    sep = "",
    stringsAsFactors = FALSE,
    strip.white = TRUE,
    comment.char = "",
    quote = ""
  )
  raw <- raw[raw$series_id == series_id & grepl("^M[0-9]{2}$", raw$period) & raw$period != "M13", ]
  raw$month <- as.integer(sub("^M", "", raw$period))
  raw$date <- as.Date(sprintf("%04d-%02d-01", as.integer(raw$year), raw$month))
  raw$value <- as.numeric(raw$value)
  raw <- raw[!is.na(raw$date) & !is.na(raw$value), c("date", "value")]
  raw[order(raw$date), ]
}

build_employment_panel <- function(source_path = file.path(DATA_DIR, "bls_occ_employed_monthly.csv")) {
  raw <- read_csv_base(source_path)
  expected <- c(ROUTINE_SERIES_IDS, NONROUTINE_SERIES_IDS)
  missing <- setdiff(expected, unique(raw$series_id))
  if (length(missing) > 0L) {
    stop("Missing expected BLS occupation series: ", paste(missing, collapse = ", "))
  }

  raw <- raw[raw$series_id %in% expected, ]
  raw$date <- as.Date(raw$date)
  raw$employed <- as.numeric(raw$employed_thousands) * 1000
  raw$is_routine <- raw$series_id %in% ROUTINE_SERIES_IDS
  raw <- raw[!is.na(raw$date) & !is.na(raw$employed), ]

  routine <- aggregate(employed ~ date, raw[raw$is_routine, ], sum)
  nonroutine <- aggregate(employed ~ date, raw[!raw$is_routine, ], sum)
  names(routine)[2] <- "routine_emp"
  names(nonroutine)[2] <- "nonroutine_emp"
  out <- merge(routine, nonroutine, by = "date", all = TRUE)
  out <- out[order(out$date), ]
  out$total_emp <- out$routine_emp + out$nonroutine_emp
  out$routine_share <- out$routine_emp / out$total_emp
  out$log_total <- log(out$total_emp)
  out$log_routine <- log(out$routine_emp)
  out$log_nonroutine <- log(out$nonroutine_emp)
  out[, c(
    "date", "routine_emp", "nonroutine_emp", "total_emp", "routine_share",
    "log_total", "log_routine", "log_nonroutine"
  )]
}

build_extended_employment_panel <- function(
    ee_path = file.path(DATA_DIR, "cps_ee_1969_1982_employment_monthly.csv"),
    bls_source_path = file.path(DATA_DIR, "bls_occ_employed_monthly.csv")) {
  if (!file.exists(ee_path)) stop("Missing historical E&E panel: ", ee_path)
  if (!file.exists(bls_source_path)) stop("Missing BLS occupation panel: ", bls_source_path)

  required <- c(
    "date", "routine_emp", "nonroutine_emp", "total_emp", "routine_share",
    "log_total", "log_routine", "log_nonroutine"
  )
  ee <- read_csv_base(ee_path)
  missing <- setdiff(required, names(ee))
  if (length(missing) > 0L) stop("Historical E&E panel is missing: ", paste(missing, collapse = ", "))
  ee$date <- as.Date(ee$date)
  ee <- ee[ee$date >= as.Date("1969-01-01") & ee$date < as.Date("1983-01-01"), required]

  bls <- build_employment_panel(bls_source_path)
  bls <- bls[bls$date >= as.Date("1983-01-01"), required]

  out <- rbind(ee, bls)
  out <- out[order(out$date), ]
  out <- out[!duplicated(out$date), ]
  expected_months <- seq(min(out$date), max(out$date), by = "month")
  missing_months <- setdiff(as.character(expected_months), as.character(out$date))
  if (length(missing_months) > 0L) {
    stop("Extended employment panel is not monthly-continuous. First missing: ", paste(head(missing_months, 12), collapse = ", "))
  }
  out
}

seasonally_adjust_positive_series <- function(series, dates) {
  y <- as.numeric(series)
  if (any(!is.finite(y)) || any(y <= 0)) {
    stop("Seasonal adjustment requires a complete positive series.")
  }
  start <- c(year_num(min(dates)), month_num(min(dates)))
  y_ts <- ts(log(y), frequency = 12, start = start)
  fit <- stats::stl(y_ts, s.window = 13, robust = TRUE)
  as.numeric(exp(as.numeric(y_ts) - as.numeric(fit$time.series[, "seasonal"])))
}

add_employment_sa_columns <- function(panel) {
  out <- panel
  out$routine_emp_sa <- seasonally_adjust_positive_series(out$routine_emp, out$date)
  out$nonroutine_emp_sa <- seasonally_adjust_positive_series(out$nonroutine_emp, out$date)
  out$total_emp_sa <- seasonally_adjust_positive_series(out$total_emp, out$date)
  out$routine_share_sa <- out$routine_emp_sa / out$total_emp_sa
  out$log_total_sa <- log(out$total_emp_sa)
  out$log_routine_sa <- log(out$routine_emp_sa)
  out$log_nonroutine_sa <- log(out$nonroutine_emp_sa)
  out
}

zfill <- function(x, width) {
  x <- as.character(x)
  paste0(vapply(pmax(width - nchar(x), 0L), function(n) paste(rep("0", n), collapse = ""), character(1)), x)
}

parse_mtgdate <- function(values) {
  text <- trimws(as.character(values))
  text <- sub("\\.0$", "", text)
  out <- as.Date(rep(NA_character_, length(text)))

  yyyymmdd <- grepl("^(19|20)[0-9]{6}$", text)
  out[yyyymmdd] <- as.Date(text[yyyymmdd], format = "%Y%m%d")

  yyyymm <- grepl("^(19|20)[0-9]{2}(0[1-9]|1[0-2])$", text) & is.na(out)
  out[yyyymm] <- as.Date(paste0(text[yyyymm], "01"), format = "%Y%m%d")

  mmddyy <- grepl("^[0-9]{5,6}$", text) & is.na(out)
  if (any(mmddyy)) {
    padded <- zfill(text[mmddyy], 6L)
    mm <- as.integer(substr(padded, 1L, 2L))
    dd <- as.integer(substr(padded, 3L, 4L))
    yy <- as.integer(substr(padded, 5L, 6L))
    yyyy <- ifelse(yy <= 30L, 2000L + yy, 1900L + yy)
    out[mmddyy] <- as.Date(sprintf("%04d-%02d-%02d", yyyy, mm, dd))
  }

  numeric <- suppressWarnings(as.numeric(text))
  remaining <- is.na(out) & !is.na(numeric)
  stata_like <- remaining & numeric >= 2500 & numeric <= 20000
  out[stata_like] <- as.Date(numeric[stata_like], origin = "1960-01-01")

  excel_like <- is.na(out) & !is.na(numeric) & numeric >= 20000 & numeric <= 50000
  out[excel_like] <- as.Date(numeric[excel_like], origin = "1899-12-30")

  string_like <- is.na(out) & is.na(numeric)
  if (any(string_like)) {
    formats <- c("%Y-%m-%d", "%m/%d/%Y", "%m/%d/%y", "%d/%m/%Y", "%d/%m/%y", "%b %Y", "%b %y", "%B %Y", "%B %y")
    for (fmt in formats) {
      idx <- string_like & is.na(out)
      if (!any(idx)) break
      out[idx] <- as.Date(text[idx], format = fmt)
    }
  }
  out
}

load_rr_shock_monthly <- function(path = file.path(DATA_DIR, "RR_MPshocks_Updated(GBforecasts).csv")) {
  if (!file.exists(path)) stop("Missing Romer-Romer shock file: ", path)
  raw <- read.csv2(path, stringsAsFactors = FALSE, check.names = FALSE)
  date_col <- names(raw)[toupper(names(raw)) == "MTGDATE"][1]
  if (is.na(date_col)) stop("Shock file must contain MTGDATE.")
  shock_col <- names(raw)[length(names(raw))]
  if (identical(shock_col, date_col)) stop("Shock file must contain a shock column after MTGDATE.")

  d <- data.frame(
    date = month_start(parse_mtgdate(raw[[date_col]])),
    shock = as_numeric_loose(raw[[shock_col]])
  )
  d <- d[!is.na(d$date) & !is.na(d$shock), ]
  if (nrow(d) == 0L) stop("Could not parse valid meeting dates/shocks.")
  monthly <- aggregate(shock ~ date, d, sum)
  full <- data.frame(date = seq(min(monthly$date), max(monthly$date), by = "month"))
  monthly <- merge(full, monthly, by = "date", all.x = TRUE)
  monthly$shock[is.na(monthly$shock)] <- 0
  monthly[order(monthly$date), ]
}

merge_monthly_panel <- function() {
  emp <- build_extended_employment_panel()
  write.csv(emp, file.path(DATA_DIR, "employment_monthly_extended.csv"), row.names = FALSE)
  shock <- load_rr_shock_monthly()
  names(shock)[names(shock) == "shock"] <- "eps"
  merged <- merge(emp, shock, by = "date", all.x = TRUE)
  merged <- merged[order(merged$date), ]
  merged <- merged[merged$date >= START_DATE & merged$date <= END_DATE, ]
  row.names(merged) <- NULL
  merged
}

build_white_lp_panel <- function() {
  panel <- add_employment_sa_columns(merge_monthly_panel())
  total_nonag <- load_bls_monthly_series(TOTAL_NONAG_EMPLOYMENT_SERIES_ID)
  names(total_nonag)[names(total_nonag) == "value"] <- "total_nonag_employment_thousands"
  total_nonag$total_nonag_emp <- total_nonag$total_nonag_employment_thousands * 1000

  panel <- merge(panel, total_nonag[, c("date", "total_nonag_emp")], by = "date", all.x = TRUE)
  panel <- panel[order(panel$date), ]
  if (any(is.na(panel$total_nonag_emp))) {
    missing <- format(head(panel$date[is.na(panel$total_nonag_emp)], 12), "%Y-%m")
    stop("Missing aggregate nonagricultural employment for: ", paste(missing, collapse = ", "))
  }

  panel$routine_share <- panel$routine_share_sa
  panel$total_emp <- panel$total_nonag_emp
  panel$routine_emp <- panel$routine_share * panel$total_emp
  panel$nonroutine_emp <- (1 - panel$routine_share) * panel$total_emp
  panel$log_total <- log(panel$total_emp)
  panel$log_routine <- log(panel$routine_emp)
  panel$log_nonroutine <- log(panel$nonroutine_emp)
  panel <- panel[panel$date >= START_DATE & panel$date <= SHOCK_END_DATE, ]
  row.names(panel) <- NULL
  write.csv(panel, file.path(DATA_DIR, "employment_monthly_white_lp.csv"), row.names = FALSE)
  panel
}

build_descriptive_panel <- function() {
  emp <- build_extended_employment_panel()
  pop <- load_bls_monthly_series(POPULATION_SERIES_ID)
  names(pop)[names(pop) == "value"] <- "civilian_noninstitutional_population_thousands"
  panel <- merge(emp, pop, by = "date", all = FALSE)
  panel <- panel[order(panel$date), ]
  panel <- add_employment_sa_columns(panel)
  panel$population <- panel$civilian_noninstitutional_population_thousands * 1000
  panel$routine_emp_per_capita <- panel$routine_emp / panel$population
  panel$routine_emp_per_capita_sa <- panel$routine_emp_sa / panel$population
  panel$routine_share_percent <- panel$routine_share * 100
  panel$routine_share_percent_sa <- panel$routine_share_sa * 100
  end_date <- min(END_DATE, max(panel$date))
  panel <- panel[panel$date >= START_DATE & panel$date <= end_date, ]
  row.names(panel) <- NULL
  panel
}

control_cols <- function(n_lag_y, n_lag_eps, include_time_trend = LP_INCLUDE_TIME_TREND) {
  cols <- "const"
  if (include_time_trend) cols <- c(cols, "time_trend")
  cols <- c(cols, paste0("y_L", seq_len(n_lag_y)))
  if (n_lag_eps > 0L) cols <- c(cols, paste0("eps_L", seq_len(n_lag_eps)))
  cols
}

build_design <- function(df, dep_level, shock, n_lag_y, n_lag_eps) {
  out <- df
  out$const <- 1
  if (LP_INCLUDE_TIME_TREND) out$time_trend <- seq_len(nrow(out)) - 1
  if (identical(LP_Y_LAG_TRANSFORM, "level")) {
    y_lag_source <- out[[dep_level]]
  } else if (identical(LP_Y_LAG_TRANSFORM, "diff")) {
    y_lag_source <- c(NA, diff(out[[dep_level]]))
  } else {
    stop("Unsupported y lag transform: ", LP_Y_LAG_TRANSFORM)
  }
  for (i in seq_len(n_lag_y)) out[[paste0("y_L", i)]] <- lag_vec(y_lag_source, i)
  for (i in seq_len(n_lag_eps)) out[[paste0("eps_L", i)]] <- lag_vec(out[[shock]], i)
  out$eps_plus <- pmax(out[[shock]], 0)
  out$eps_minus <- pmin(out[[shock]], 0)
  out$eps_sq <- out[[shock]]^2
  out
}

fit_ols_hac <- function(y, X, maxlags = NW_LAGS) {
  X <- as.data.frame(X)
  reg <- data.frame(lhs = y, X, check.names = FALSE)
  reg <- reg[stats::complete.cases(reg), ]
  y <- reg$lhs
  X <- as.matrix(reg[, setdiff(names(reg), "lhs"), drop = FALSE])
  fit <- lm.fit(X, y)
  beta <- fit$coefficients
  resid <- as.numeric(y - X %*% beta)
  n <- nrow(X)
  xtx_inv <- solve(crossprod(X))
  xu <- X * resid
  s_mat <- crossprod(xu)
  if (maxlags > 0L) {
    for (lag in seq_len(maxlags)) {
      weight <- 1 - lag / (maxlags + 1)
      gamma <- crossprod(xu[(lag + 1L):n, , drop = FALSE], xu[1L:(n - lag), , drop = FALSE])
      s_mat <- s_mat + weight * (gamma + t(gamma))
    }
  }
  cov <- xtx_inv %*% s_mat %*% xtx_inv
  colnames(cov) <- rownames(cov) <- colnames(X)
  se <- sqrt(diag(cov))
  names(se) <- colnames(X)
  list(coef = beta, se = se, cov = cov, resid = resid, n = n)
}

fit_ols_plain <- function(y, X) {
  X <- as.data.frame(X)
  reg <- data.frame(lhs = y, X, check.names = FALSE)
  reg <- reg[stats::complete.cases(reg), ]
  y <- reg$lhs
  X <- as.matrix(reg[, setdiff(names(reg), "lhs"), drop = FALSE])
  fit <- lm.fit(X, y)
  resid <- as.numeric(y - X %*% fit$coefficients)
  list(coef = fit$coefficients, resid = resid)
}

irf_result <- function(horizons, coef, se, spec, outcome) {
  list(horizons = as.integer(horizons), coef = as.numeric(coef), se = as.numeric(se), spec = spec, outcome = outcome)
}

estimate_irf_linear <- function(df, dep_level, shock, horizons, n_lag_y, n_lag_eps) {
  base <- build_design(df, dep_level, shock, n_lag_y, n_lag_eps)
  xcols <- c(control_cols(n_lag_y, n_lag_eps), shock)
  coefs <- ses <- numeric(length(horizons))
  for (j in seq_along(horizons)) {
    h <- horizons[j]
    lhs <- lead_vec(base[[dep_level]], h) - base[[dep_level]]
    fit <- fit_ols_hac(lhs, base[, xcols, drop = FALSE])
    coefs[j] <- fit$coef[[shock]]
    ses[j] <- fit$se[[shock]]
  }
  irf_result(horizons, coefs, ses, "linear", dep_level)
}

estimate_irf_sign_both <- function(df, dep_level, shock, horizons, n_lag_y, n_lag_eps) {
  base <- build_design(df, dep_level, shock, n_lag_y, n_lag_eps)
  xcols <- c(control_cols(n_lag_y, n_lag_eps), "eps_plus", "eps_minus")
  cp <- sp <- cm <- sm <- numeric(length(horizons))
  for (j in seq_along(horizons)) {
    h <- horizons[j]
    lhs <- lead_vec(base[[dep_level]], h) - base[[dep_level]]
    fit <- fit_ols_hac(lhs, base[, xcols, drop = FALSE])
    cp[j] <- fit$coef[["eps_plus"]]
    sp[j] <- fit$se[["eps_plus"]]
    cm[j] <- fit$coef[["eps_minus"]]
    sm[j] <- fit$se[["eps_minus"]]
  }
  list(
    plus = irf_result(horizons, cp, sp, "sign", paste0(dep_level, "|eps_plus")),
    minus = irf_result(horizons, cm, sm, "sign", paste0(dep_level, "|eps_minus"))
  )
}

estimate_irf_quad <- function(df, dep_level, shock, horizons, n_lag_y, n_lag_eps) {
  base <- build_design(df, dep_level, shock, n_lag_y, n_lag_eps)
  xcols <- c(control_cols(n_lag_y, n_lag_eps), shock, "eps_sq")
  c_pos <- s_pos <- c_neg <- s_neg <- numeric(length(horizons))
  for (j in seq_along(horizons)) {
    h <- horizons[j]
    lhs <- lead_vec(base[[dep_level]], h) - base[[dep_level]]
    fit <- fit_ols_hac(lhs, base[, xcols, drop = FALSE])
    g1 <- fit$coef[[shock]]
    g2 <- fit$coef[["eps_sq"]]
    vc <- fit$cov[c(shock, "eps_sq"), c(shock, "eps_sq")]
    j_pos <- c(1, 1)
    j_neg <- c(-1, 1)
    c_pos[j] <- g1 + g2
    c_neg[j] <- -g1 + g2
    s_pos[j] <- sqrt(drop(t(j_pos) %*% vc %*% j_pos))
    s_neg[j] <- sqrt(drop(t(j_neg) %*% vc %*% j_neg))
  }
  list(
    pos = irf_result(horizons, c_pos, s_pos, "quad", paste0(dep_level, "|+1pp")),
    neg = irf_result(horizons, c_neg, s_neg, "quad", paste0(dep_level, "|-1pp"))
  )
}

fev_share_linear <- function(df, dep_level, shock, horizons, n_lag_y, n_lag_eps) {
  base <- build_design(df, dep_level, shock, n_lag_y, n_lag_eps)
  xeps <- c(shock, paste0("eps_L", seq_len(n_lag_eps)))
  control_base <- control_cols(n_lag_y, 0L)
  rows <- vector("list", length(horizons))
  for (j in seq_along(horizons)) {
    h <- horizons[j]
    lhs <- lead_vec(base[[dep_level]], h) - base[[dep_level]]
    full <- fit_ols_plain(lhs, base[, c(control_base, xeps), drop = FALSE])
    restricted <- fit_ols_plain(lhs, base[, control_base, drop = FALSE])
    mse_full <- mean(full$resid^2)
    mse_restricted <- mean(restricted$resid^2)
    rows[[j]] <- data.frame(
      horizon = h,
      fev_share = if (mse_restricted > 0) 1 - mse_full / mse_restricted else NA_real_,
      outcome = dep_level
    )
  }
  do.call(rbind, rows)
}

scale_irf <- function(res, scale) {
  res$coef <- res$coef * scale
  res$se <- res$se * scale
  res
}

plot_irf <- function(res, out_path, title, ylabel) {
  z <- 1.645
  h <- c(0, res$horizons)
  coef <- c(0, res$coef)
  band <- c(0, z * res$se)
  lo <- coef - band
  hi <- coef + band
  png(out_path, width = 1400, height = 800, res = 200)
  par(family = "serif", mar = c(4.2, 4.5, 3.2, 1.2))
  plot(h, coef, type = "n", xlab = "Months", ylab = ylabel, main = title, xlim = c(0, max(h)), ylim = range(lo, hi, finite = TRUE))
  polygon(c(h, rev(h)), c(lo, rev(hi)), col = "grey85", border = NA)
  lines(h, coef, col = "black", lwd = 2)
  abline(h = 0, col = "grey30", lty = "dotted")
  dev.off()
}

plot_figure3 <- function(irfs, out_path) {
  panels <- list(
    list(key = "log_routine", title = "Routine Employment", ylabel = "Percent", ylim = c(-3, 1)),
    list(key = "log_nonroutine", title = "Nonroutine Employment", ylabel = "Percent", ylim = c(-3, 1)),
    list(key = "routine_share", title = "Routine Share", ylabel = "% Points", ylim = c(-1, 0.5)),
    list(key = "log_total", title = "Total Employment", ylabel = "Percent", ylim = c(-3, 1))
  )
  z <- 1.645
  png(out_path, width = 1600, height = 1160, res = 200)
  par(family = "serif", mfrow = c(2, 2), mar = c(4.2, 4.4, 3.2, 1.0))
  for (panel in panels) {
    res <- irfs[[panel$key]]
    h <- c(0, res$horizons)
    coef <- c(0, res$coef)
    band <- c(0, z * res$se)
    plot(h, coef, type = "n", xlab = "Months", ylab = panel$ylabel, main = panel$title, xlim = c(0, 48), ylim = panel$ylim, xaxt = "n")
    polygon(c(h, rev(h)), c(coef - band, rev(coef + band)), col = "grey88", border = NA)
    lines(h, coef, col = "black", lwd = 2)
    abline(h = 0, col = "grey30", lty = "dotted")
    axis(1, at = c(0, 12, 24, 36, 48))
  }
  dev.off()
}

plot_descriptive_line <- function(panel, column, ylabel, title, y_ticks, y_lim, out_path) {
  png(out_path, width = 1300, height = 760, res = 200)
  par(family = "serif", mar = c(4.2, 5.0, 3.0, 1.0))
  plot(panel$date, panel[[column]], type = "n", xlab = "Year", ylab = ylabel, main = title, ylim = y_lim, yaxt = "n")
  usr <- par("usr")
  for (i in seq_len(nrow(RECESSIONS))) {
    rect(RECESSIONS$start[i], usr[3], RECESSIONS$end[i], usr[4], col = "grey85", border = NA)
  }
  lines(panel$date, panel[[column]], col = "black", lwd = 1.4)
  axis(2, at = y_ticks)
  box()
  dev.off()
}

write_figure3_irf_csv <- function(irfs, out_path) {
  rows <- list()
  for (outcome in names(irfs)) {
    res <- irfs[[outcome]]
    rows[[outcome]] <- data.frame(
      outcome = outcome,
      horizon = res$horizons,
      coef_raw = res$coef,
      se_raw = res$se,
      coef_plotted = res$coef,
      se_plotted = res$se
    )
  }
  out <- do.call(rbind, rows)
  row.names(out) <- NULL
  write.csv(out, out_path, row.names = FALSE)
  out
}

write_fev_csv <- function(fev_rows, out_path) {
  fev <- do.call(rbind, fev_rows)
  fev <- fev[, c("horizon", "fev_share", "outcome")]
  wide <- reshape(fev, idvar = "horizon", timevar = "outcome", direction = "wide")
  names(wide) <- sub("^fev_share\\.", "", names(wide))
  wide <- wide[order(wide$horizon), ]
  write.csv(wide, out_path, row.names = FALSE)
  invisible(wide)
}

validate_against_python_reference <- function(r_irfs) {
  ref_path <- file.path(REFERENCE_DIR, "python_figure3_linear_irfs_unsmoothed.csv")
  if (!file.exists(ref_path)) return(invisible(NULL))
  ref <- read_csv_base(ref_path)
  ref$horizon <- as.integer(ref$horizon)
  merged <- merge(
    r_irfs[, c("outcome", "horizon", "coef_raw", "se_raw")],
    ref,
    by = c("outcome", "horizon"),
    suffixes = c("_r", "_python")
  )
  merged$coef_abs_diff <- abs(merged$coef_raw_r - as.numeric(merged$coef_raw_python))
  merged$se_abs_diff <- abs(merged$se_raw_r - as.numeric(merged$se_raw_python))
  write.csv(merged, file.path(OUTPUT_DIR, "validation_against_python_unsmoothed.csv"), row.names = FALSE)
  message(
    "Validation against Python unsmoothed reference: max coef diff = ",
    signif(max(merged$coef_abs_diff, na.rm = TRUE), 6),
    ", max SE diff = ",
    signif(max(merged$se_abs_diff, na.rm = TRUE), 6)
  )
  invisible(merged)
}
