########################
###### FUNCTIONS #######
########################


# 1. Small helpers --------------------------------------------------------

read_white_csv <- function(path, ...) {
  read.csv(path, stringsAsFactors = FALSE, check.names = FALSE, ...)
}

lag_vec <- function(x, n) {
  if (n == 0L) return(x)
  c(rep(NA_real_, n), x[seq_len(length(x) - n)])
}

lead_vec <- function(x, n) {
  if (n == 0L) return(x)
  c(x[(n + 1L):length(x)], rep(NA_real_, n))
}

copy_if_present <- function(from, to) {
  if (file.exists(from)) {
    dir.create(dirname(to), recursive = TRUE, showWarnings = FALSE)
    file.copy(from, to, overwrite = TRUE)
  }
}


# 2. Local projections ----------------------------------------------------

control_cols_white <- function(n_lag_y,
                               n_lag_eps,
                               include_time_trend = FALSE) {
  cols <- "const"
  if (include_time_trend) cols <- c(cols, "time_trend")
  cols <- c(cols, paste0("y_L", seq_len(n_lag_y)))
  if (n_lag_eps > 0L) cols <- c(cols, paste0("eps_L", seq_len(n_lag_eps)))
  cols
}

build_design_white <- function(data,
                               y_var,
                               shock_var,
                               n_lag_y,
                               n_lag_eps,
                               y_lag_transform = "diff",
                               include_time_trend = FALSE) {
  df0 <- data[order(data$date), ]
  df0$const <- 1
  if (include_time_trend) df0$time_trend <- seq_len(nrow(df0)) - 1

  if (identical(y_lag_transform, "level")) {
    y_lag_source <- df0[[y_var]]
  } else if (identical(y_lag_transform, "diff")) {
    y_lag_source <- c(NA_real_, diff(df0[[y_var]]))
  } else {
    stop("Unsupported y_lag_transform: ", y_lag_transform)
  }

  for (L in seq_len(n_lag_y)) {
    df0[[paste0("y_L", L)]] <- lag_vec(y_lag_source, L)
  }

  for (L in seq_len(n_lag_eps)) {
    df0[[paste0("eps_L", L)]] <- lag_vec(df0[[shock_var]], L)
  }

  df0$eps_plus <- pmax(df0[[shock_var]], 0)
  df0$eps_minus <- pmin(df0[[shock_var]], 0)
  df0$eps_sq <- df0[[shock_var]]^2

  df0
}

fit_ols_hac_white <- function(y, X, maxlags = 12L) {
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
    for (L in seq_len(maxlags)) {
      weight <- 1 - L / (maxlags + 1)
      gamma <- crossprod(
        xu[(L + 1L):n, , drop = FALSE],
        xu[1L:(n - L), , drop = FALSE]
      )
      s_mat <- s_mat + weight * (gamma + t(gamma))
    }
  }

  vcov <- xtx_inv %*% s_mat %*% xtx_inv
  colnames(vcov) <- rownames(vcov) <- colnames(X)
  se <- sqrt(diag(vcov))
  names(se) <- colnames(X)

  list(coef = beta, se = se, vcov = vcov, resid = resid)
}

fit_ols_plain_white <- function(y, X) {
  X <- as.data.frame(X)
  reg <- data.frame(lhs = y, X, check.names = FALSE)
  reg <- reg[stats::complete.cases(reg), ]

  y <- reg$lhs
  X <- as.matrix(reg[, setdiff(names(reg), "lhs"), drop = FALSE])
  fit <- lm.fit(X, y)

  list(
    coef = fit$coefficients,
    resid = as.numeric(y - X %*% fit$coefficients)
  )
}

LP_white <- function(data,
                     H = 48L,
                     y_var,
                     shock_var = "eps",
                     n_lags_y = 12L,
                     n_lags_shock = 12L,
                     nw_lags = 12L,
                     scale = 1,
                     confint = 1.645,
                     y_lag_transform = "diff",
                     include_time_trend = FALSE) {
  df0 <- build_design_white(
    data = data,
    y_var = y_var,
    shock_var = shock_var,
    n_lag_y = n_lags_y,
    n_lag_eps = n_lags_shock,
    y_lag_transform = y_lag_transform,
    include_time_trend = include_time_trend
  )

  horizons <- seq_len(H)
  x_cols <- c(
    control_cols_white(n_lags_y, n_lags_shock, include_time_trend),
    shock_var
  )

  out <- vector("list", H)

  for (h_val in horizons) {
    dy_h <- lead_vec(df0[[y_var]], h_val) - df0[[y_var]]
    fit <- fit_ols_hac_white(dy_h, df0[, x_cols, drop = FALSE], maxlags = nw_lags)

    estimate <- unname(fit$coef[[shock_var]]) * scale
    se <- unname(fit$se[[shock_var]]) * scale

    out[[h_val]] <- data.frame(
      outcome = y_var,
      h = h_val,
      estimate_raw = estimate,
      se = se,
      conf_low_raw = estimate - confint * se,
      conf_high_raw = estimate + confint * se
    )
  }

  do.call(rbind, out)
}

LP_white_sign <- function(data,
                          H = 48L,
                          y_var,
                          shock_var = "eps",
                          n_lags_y = 12L,
                          n_lags_shock = 12L,
                          nw_lags = 12L,
                          scale = 1,
                          confint = 1.645,
                          y_lag_transform = "diff",
                          include_time_trend = FALSE) {
  df0 <- build_design_white(
    data = data,
    y_var = y_var,
    shock_var = shock_var,
    n_lag_y = n_lags_y,
    n_lag_eps = n_lags_shock,
    y_lag_transform = y_lag_transform,
    include_time_trend = include_time_trend
  )

  horizons <- seq_len(H)
  shock_cols <- c("eps_plus", "eps_minus")
  x_cols <- c(control_cols_white(n_lags_y, n_lags_shock, include_time_trend), shock_cols)
  out <- list()

  for (h_val in horizons) {
    dy_h <- lead_vec(df0[[y_var]], h_val) - df0[[y_var]]
    fit <- fit_ols_hac_white(dy_h, df0[, x_cols, drop = FALSE], maxlags = nw_lags)

    for (s in shock_cols) {
      estimate <- unname(fit$coef[[s]]) * scale
      se <- unname(fit$se[[s]]) * scale
      out[[length(out) + 1L]] <- data.frame(
        outcome = y_var,
        shock = s,
        h = h_val,
        estimate_raw = estimate,
        se = se,
        conf_low_raw = estimate - confint * se,
        conf_high_raw = estimate + confint * se
      )
    }
  }

  do.call(rbind, out)
}

LP_white_quad <- function(data,
                          H = 48L,
                          y_var,
                          shock_var = "eps",
                          n_lags_y = 12L,
                          n_lags_shock = 12L,
                          nw_lags = 12L,
                          scale = 1,
                          confint = 1.645,
                          y_lag_transform = "diff",
                          include_time_trend = FALSE) {
  df0 <- build_design_white(
    data = data,
    y_var = y_var,
    shock_var = shock_var,
    n_lag_y = n_lags_y,
    n_lag_eps = n_lags_shock,
    y_lag_transform = y_lag_transform,
    include_time_trend = include_time_trend
  )

  horizons <- seq_len(H)
  x_cols <- c(
    control_cols_white(n_lags_y, n_lags_shock, include_time_trend),
    shock_var,
    "eps_sq"
  )
  out <- list()

  for (h_val in horizons) {
    dy_h <- lead_vec(df0[[y_var]], h_val) - df0[[y_var]]
    fit <- fit_ols_hac_white(dy_h, df0[, x_cols, drop = FALSE], maxlags = nw_lags)

    g1 <- unname(fit$coef[[shock_var]])
    g2 <- unname(fit$coef[["eps_sq"]])
    vc <- fit$vcov[c(shock_var, "eps_sq"), c(shock_var, "eps_sq")]

    j_pos <- c(1, 1)
    j_neg <- c(-1, 1)

    estimates <- c("+1pp" = g1 + g2, "-1pp" = -g1 + g2) * scale
    ses <- c(
      "+1pp" = sqrt(drop(t(j_pos) %*% vc %*% j_pos)) * scale,
      "-1pp" = sqrt(drop(t(j_neg) %*% vc %*% j_neg)) * scale
    )

    for (s in names(estimates)) {
      out[[length(out) + 1L]] <- data.frame(
        outcome = y_var,
        shock = s,
        h = h_val,
        estimate_raw = unname(estimates[[s]]),
        se = unname(ses[[s]]),
        conf_low_raw = unname(estimates[[s]]) - confint * unname(ses[[s]]),
        conf_high_raw = unname(estimates[[s]]) + confint * unname(ses[[s]])
      )
    }
  }

  do.call(rbind, out)
}

FEV_white <- function(data,
                      H = 48L,
                      y_var,
                      shock_var = "eps",
                      n_lags_y = 12L,
                      n_lags_shock = 12L,
                      y_lag_transform = "diff",
                      include_time_trend = FALSE) {
  df0 <- build_design_white(
    data = data,
    y_var = y_var,
    shock_var = shock_var,
    n_lag_y = n_lags_y,
    n_lag_eps = n_lags_shock,
    y_lag_transform = y_lag_transform,
    include_time_trend = include_time_trend
  )

  horizons <- seq_len(H)
  shock_lags <- paste0("eps_L", seq_len(n_lags_shock))
  x_eps <- c(shock_var, shock_lags)
  control_base <- control_cols_white(n_lags_y, 0L, include_time_trend)
  out <- vector("list", H)

  for (h_val in horizons) {
    dy_h <- lead_vec(df0[[y_var]], h_val) - df0[[y_var]]
    fit_full <- fit_ols_plain_white(dy_h, df0[, c(control_base, x_eps), drop = FALSE])
    fit_restricted <- fit_ols_plain_white(dy_h, df0[, control_base, drop = FALSE])

    mse_full <- mean(fit_full$resid^2)
    mse_restricted <- mean(fit_restricted$resid^2)

    out[[h_val]] <- data.frame(
      horizon = h_val,
      fev_share = if (mse_restricted > 0) 1 - mse_full / mse_restricted else NA_real_,
      outcome = y_var
    )
  }

  do.call(rbind, out)
}


# 3. IRF smoothing and output helpers ------------------------------------

smooth_white_irf <- function(irf_df,
                             window = 7L,
                             se_floor_ratio = 0.85,
                             confint = 1.645) {
  if (window <= 1L) return(irf_df)
  if (window %% 2L == 0L) stop("IRF smoothing window must be odd.")
  if (se_floor_ratio < 0 || se_floor_ratio > 1) stop("SE floor ratio must lie between 0 and 1.")

  out <- irf_df[order(irf_df$h), ]
  half <- window %/% 2L
  n <- nrow(out)
  estimate_s <- numeric(n)
  se_s <- numeric(n)

  for (i in seq_len(n)) {
    lo <- max(1L, i - half)
    hi <- min(n, i + half)
    offsets <- abs(seq(lo, hi) - i)
    weights <- as.numeric(half + 1L - offsets)
    weights <- weights / sum(weights)

    estimate_s[i] <- sum(weights * out$estimate_raw[lo:hi])
    independent_se <- sqrt(sum((weights * out$se[lo:hi])^2))
    local_mean_se <- sum(weights * out$se[lo:hi])
    se_s[i] <- max(independent_se, se_floor_ratio * local_mean_se)
  }

  out$estimate_raw <- estimate_s
  out$se <- se_s
  out$conf_low_raw <- estimate_s - confint * se_s
  out$conf_high_raw <- estimate_s + confint * se_s
  out
}

write_figure3_irf_csv <- function(raw_irfs, plotted_irfs, out_path) {
  rows <- list()

  for (nm in names(raw_irfs)) {
    raw <- raw_irfs[[nm]][order(raw_irfs[[nm]]$h), ]
    plotted <- plotted_irfs[[nm]][order(plotted_irfs[[nm]]$h), ]

    rows[[nm]] <- data.frame(
      outcome = nm,
      horizon = raw$h,
      coef_raw = raw$estimate_raw,
      se_raw = raw$se,
      coef_plotted = plotted$estimate_raw,
      se_plotted = plotted$se
    )
  }

  out <- do.call(rbind, rows)
  row.names(out) <- NULL
  write.csv(out, out_path, row.names = FALSE)
  out
}

write_fev_csv <- function(fev_rows, out_path) {
  fev <- do.call(rbind, fev_rows)
  wide <- reshape(fev, idvar = "horizon", timevar = "outcome", direction = "wide")
  names(wide) <- sub("^fev_share\\.", "", names(wide))

  python_order <- c("horizon", "log_nonroutine", "log_routine", "log_total", "routine_share")
  if (all(python_order %in% names(wide))) {
    wide <- wide[, python_order]
  }

  wide <- wide[order(wide$horizon), ]
  write.csv(wide, out_path, row.names = FALSE)
  invisible(wide)
}


# 4. Plotting -------------------------------------------------------------

plot_irf_white <- function(irf_df, out_path, title, ylabel) {
  z <- 1.645
  irf_df <- irf_df[order(irf_df$h), ]
  h <- c(0, irf_df$h)
  estimate <- c(0, irf_df$estimate_raw)
  band <- c(0, z * irf_df$se)
  lo <- estimate - band
  hi <- estimate + band

  png(out_path, width = 1400, height = 800, res = 200)
  par(family = "serif", mar = c(4.2, 4.5, 3.2, 1.2))
  plot(
    h, estimate,
    type = "n",
    xlab = "Months",
    ylab = ylabel,
    main = title,
    xlim = c(0, max(h)),
    ylim = range(lo, hi, finite = TRUE)
  )
  polygon(c(h, rev(h)), c(lo, rev(hi)), col = "grey85", border = NA)
  lines(h, estimate, col = "black", lwd = 2)
  abline(h = 0, col = "grey30", lty = "dotted")
  dev.off()
}

plot_figure3_white <- function(irf_list, out_path) {
  panels <- list(
    list(key = "log_routine", title = "Routine Employment", ylabel = "Percent", ylim = c(-3, 1)),
    list(key = "log_nonroutine", title = "Nonroutine Employment", ylabel = "Percent", ylim = c(-3, 1)),
    list(key = "routine_share", title = "Routine Share", ylabel = "% Points", ylim = c(-1, 0.5)),
    list(key = "log_total", title = "Total Employment", ylabel = "Percent", ylim = c(-3, 1))
  )

  z <- 1.645
  png(out_path, width = 1600, height = 1160, res = 200)
  par(family = "serif", mfrow = c(2, 2), mar = c(4.2, 4.4, 3.2, 1.0))

  for (p in panels) {
    irf_df <- irf_list[[p$key]][order(irf_list[[p$key]]$h), ]
    h <- c(0, irf_df$h)
    estimate <- c(0, irf_df$estimate_raw)
    band <- c(0, z * irf_df$se)

    plot(
      h, estimate,
      type = "n",
      xlab = "Months",
      ylab = p$ylabel,
      main = p$title,
      xlim = c(0, 48),
      ylim = p$ylim,
      xaxt = "n"
    )
    polygon(c(h, rev(h)), c(estimate - band, rev(estimate + band)), col = "grey88", border = NA)
    lines(h, estimate, col = "black", lwd = 2)
    abline(h = 0, col = "grey30", lty = "dotted")
    axis(1, at = c(0, 12, 24, 36, 48))
  }

  dev.off()
}

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


# 5. Validation -----------------------------------------------------------

validate_against_python <- function(r_irfs,
                                    reference_dir,
                                    output_dir) {
  ref_path <- file.path(reference_dir, "python_figure3_linear_irfs_raw_and_smoothed.csv")
  if (!file.exists(ref_path)) return(invisible(NULL))

  ref <- read_white_csv(ref_path)
  ref$horizon <- as.integer(ref$horizon)

  merged <- merge(
    r_irfs[, c("outcome", "horizon", "coef_raw", "se_raw", "coef_plotted", "se_plotted")],
    ref,
    by = c("outcome", "horizon"),
    suffixes = c("_r", "_python")
  )

  merged$coef_abs_diff <- abs(merged$coef_raw_r - as.numeric(merged$coef_raw_python))
  merged$se_abs_diff <- abs(merged$se_raw_r - as.numeric(merged$se_raw_python))
  merged$coef_plotted_abs_diff <- abs(merged$coef_plotted_r - as.numeric(merged$coef_plotted_python))
  merged$se_plotted_abs_diff <- abs(merged$se_plotted_r - as.numeric(merged$se_plotted_python))

  write.csv(merged, file.path(output_dir, "validation_against_python.csv"), row.names = FALSE)

  message(
    "Validation against Python reference: max raw coef diff = ",
    signif(max(merged$coef_abs_diff, na.rm = TRUE), 6),
    ", max raw SE diff = ",
    signif(max(merged$se_abs_diff, na.rm = TRUE), 6),
    ", max plotted coef diff = ",
    signif(max(merged$coef_plotted_abs_diff, na.rm = TRUE), 6),
    ", max plotted SE diff = ",
    signif(max(merged$se_plotted_abs_diff, na.rm = TRUE), 6)
  )

  invisible(merged)
}
