########################
######### MAIN #########
########################

# This file manages the R replication folder and sources the empirical scripts.
# The layout mirrors the final project folder: setup here, reusable helper
# functions in functions.R, and the White (2022) local-projection workflow in
# 2_LP.R.


# 0. Set-up ---------------------------------------------------------------

setwd("C:/Users/wmf098/Desktop/white_mp_polarization_replication/white_mp_polarization_replication")

repo_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
r_dir <- file.path(repo_dir, "R")

# 0.1 Load libraries -------------------------------------------------------

libs <- c(
  
  # General #
  
  "dplyr",        
  "tidyr", 
  "ggplot2",
  "magrittr",
  "writexl",
  "fs",
  "zip",          # CEX data is in ZIP files
  "purrr",
  "broom",
  "ggforce",
  "readxl",
  "patchwork",
  "readr",

  # Data manipulation #
  
  "data.table",   
  "ipumsr",       # download CPS data
  "quantmod",
  "haven",        # read Autor & Dorn and CEX files (.dta/.sas7bdat)
  "janitor",      
  "stringr",
  "jsonlite",
  "lubridate",    # date handling
  "arrow",        # for transforming large data into parquet
  "zoo",
  "xts",
  "gt",
  "scales",
  "tikzDevice",
  "here",
  
  # EDA #
  
    # Survey #
  "srvyr",
    # LP #
  "fixest",        # regression / local projections later
  "lpirfs",
  "brms",          # for bayesian smoothing
  "splines",       # for smoothing local projections
  "sandwich",
  
    # FAVAR #
  "FAVAR",
  "fredr",         # for data retrieval from FRED
  "seasonal",      # for seasonal adjustment
  "imputeTS",      # for imputing before seasonal adjustment
  "vars",          # for FAVAR

    # EXTRAS #
  "showtext",
  "sysfonts",
  "kableExtra",
  "kableExtra",
  "Hmisc"
)

# Load each library quietly. Set WHITE_R_SKIP_PACKAGE_LOAD=1 when checking the
# reproducible pipeline on a clean machine without installing the full toolbox.
if (Sys.getenv("WHITE_R_SKIP_PACKAGE_LOAD", "0") != "1") {
  invisible(lapply(libs, function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
    suppressPackageStartupMessages(library(pkg, character.only = TRUE))
  }))
}


# 0.2 Paths ---------------------------------------------------------------

input_dir <- file.path(r_dir, "data")
ref_dir <- file.path(r_dir, "reference")
out_dir <- file.path(r_dir, "output")
fig_dir <- out_dir
tab_dir <- out_dir
cache_dir <- file.path(out_dir, "cache")

dir.create(input_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(ref_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

# 0.3 Helper functions ----------------------------------------------------

source(file.path(r_dir, "functions.R"))


# 1. Data build -----------------------------------------------------------

source(file.path(r_dir, "1_Data.R"))


# 2. White (2022) replication --------------------------------------------

source(file.path(r_dir, "2_LP.R"))
