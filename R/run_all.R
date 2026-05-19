script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- "--file="
  hit <- grep(paste0("^", file_arg), args)
  if (length(hit) > 0L) {
    return(dirname(normalizePath(sub(file_arg, "", args[hit[1L]]), winslash = "/", mustWork = TRUE)))
  }
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

setwd(script_dir())
source("config.R")
source("functions.R")

dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

run_pipeline <- function() {
  message("Building descriptive panel for Figures 1 and 2...")
  descriptive <- build_descriptive_panel()
  write.csv(
    descriptive[, c(
      "date", "routine_emp", "nonroutine_emp", "total_emp", "population",
      "routine_emp_per_capita", "routine_emp_per_capita_sa",
      "routine_share", "routine_share_sa",
      "routine_share_percent", "routine_share_percent_sa"
    )],
    file.path(OUTPUT_DIR, "figures_1_2_series.csv"),
    row.names = FALSE
  )
  plot_descriptive_line(
    descriptive,
    "routine_emp_per_capita_sa",
    "Routine Emp. / Civilian Pop. (16+)",
    "Per Capita Employment in Routine Jobs",
    c(0.20, 0.25, 0.30, 0.35, 0.40),
    c(0.20, 0.40),
    file.path(OUTPUT_DIR, "figure1_routine_employment_per_capita.png")
  )
  plot_descriptive_line(
    descriptive,
    "routine_share_percent_sa",
    "Percent of Total Employment",
    "Routine Jobs as a Share of Total Employment",
    c(40, 45, 50, 55, 60, 65, 70),
    c(40, 70),
    file.path(OUTPUT_DIR, "figure2_routine_employment_share.png")
  )

  message("Building White-style LP panel...")
  panel <- build_white_lp_panel()
  horizons <- seq_len(H_MAX)
  outcomes <- data.frame(
    dep = c("log_total", "log_routine", "log_nonroutine", "routine_share"),
    slug = c("log total employment", "log routine employment", "log nonroutine employment", "routine share of employment"),
    ylab = c("Percent", "Percent", "Percent", "% Points"),
    scale = c(100, 100, 100, 100),
    stringsAsFactors = FALSE
  )

  fev_rows <- list()
  figure3_irfs <- list()

  for (i in seq_len(nrow(outcomes))) {
    dep <- outcomes$dep[i]
    slug <- gsub(" ", "_", outcomes$slug[i], fixed = TRUE)
    ylab <- outcomes$ylab[i]
    scale <- outcomes$scale[i]
    message("Estimating unsmoothed LPs for ", dep, "...")

    ir <- estimate_irf_linear(panel, dep, "eps", horizons, N_LAGS_Y, N_LAGS_SHOCK)
    ir_scaled <- scale_irf(ir, scale)
    figure3_irfs[[dep]] <- ir_scaled
    plot_irf(
      ir_scaled,
      file.path(OUTPUT_DIR, paste0("irf_linear_", slug, ".png")),
      paste0("Linear LP - ", outcomes$slug[i]),
      ylab
    )
    fev_rows[[dep]] <- fev_share_linear(panel, dep, "eps", horizons, N_LAGS_Y, N_LAGS_SHOCK)

    sign_irfs <- estimate_irf_sign_both(panel, dep, "eps", horizons, N_LAGS_Y, N_LAGS_SHOCK)
    plot_irf(
      scale_irf(sign_irfs$plus, scale),
      file.path(OUTPUT_DIR, paste0("irf_sign_contraction_", slug, ".png")),
      paste0("Sign-split LP - contractionary eps+ - ", outcomes$slug[i]),
      ylab
    )
    plot_irf(
      scale_irf(sign_irfs$minus, scale),
      file.path(OUTPUT_DIR, paste0("irf_sign_expansion_", slug, ".png")),
      paste0("Sign-split LP - expansionary eps- - ", outcomes$slug[i]),
      ylab
    )

    quad_irfs <- estimate_irf_quad(panel, dep, "eps", horizons, N_LAGS_Y, N_LAGS_SHOCK)
    plot_irf(
      scale_irf(quad_irfs$pos, scale),
      file.path(OUTPUT_DIR, paste0("irf_quad_contraction_", slug, ".png")),
      paste0("Quadratic LP - +1 pp shock - ", outcomes$slug[i]),
      ylab
    )
    plot_irf(
      scale_irf(quad_irfs$neg, scale),
      file.path(OUTPUT_DIR, paste0("irf_quad_expansion_", slug, ".png")),
      paste0("Quadratic LP - -1 pp shock - ", outcomes$slug[i]),
      ylab
    )
  }

  message("Writing unsmoothed Figure 3 outputs...")
  plot_figure3(figure3_irfs, file.path(OUTPUT_DIR, "figure3_linear_occupations_unsmoothed.png"))
  plot_figure3(figure3_irfs, file.path(OUTPUT_DIR, "figure3_linear_occupations.png"))
  figure3_csv <- write_figure3_irf_csv(figure3_irfs, file.path(OUTPUT_DIR, "figure3_linear_irfs_unsmoothed.csv"))
  write.csv(figure3_csv, file.path(OUTPUT_DIR, "figure3_linear_irfs.csv"), row.names = FALSE)
  write_fev_csv(fev_rows, file.path(OUTPUT_DIR, "fev_linear.csv"))
  validate_against_python_reference(figure3_csv)

  message("Done. R outputs are under: ", OUTPUT_DIR)
}

run_pipeline()
