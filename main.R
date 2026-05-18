########################
######### MAIN #########
########################

# This file manages all the separate files and serves as a main file setting up the infrastructure

# TO-DO:
# 1. Exclude government jobs as Morrison does/proposes
# 2. Exclude sharp drop in 2010 (I think was also mentioned in Morrison paper)
# 3. Include controls in Local Projections estimation
# 4. Look at BEA Fixed Assset Tables (U.S.) and add to panel
# 5. Check if with new cps_rti_ds the results differ!

# 0. Set-up ---------------------------------------------------------------

# when working on server:
setwd("C:/Users/wmf098/Sciebo/PhD/Masterarbeit/MP_LaborPol")

# packages used/needed
# Merge this to all cps_parquet files

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

# Load each library quietly
invisible(lapply(libs, function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}))

# Output path for Figures
fig_dir <- "C:/Users/wmf098/Sciebo/PhD/Masterarbeit/MP_LaborPol/figures"
tab_dir <- "C:/Users/wmf098/Sciebo/PhD/Masterarbeit/MP_LaborPol/tables"

# importing function-skript
source("code/functions.R")

# White baseline sample ---------------------------------------------------

# Default to White's baseline timing: the Romer/Romer shock sample starts in
# 1969, and the pre-1983 occupation panel comes from Employment and Earnings.
# Set MP_LABORPOL_USE_WHITE_1969=0 before sourcing this file to keep the older
# CPS-only setup.
use_white_1969_sample <- Sys.getenv("MP_LABORPOL_USE_WHITE_1969", "1") != "0"
options(mp_laborpol.use_white_1969 = use_white_1969_sample)

if (use_white_1969_sample) {
  source("code/build_white_1969_panel.R")
  cache_file <- white_scaled_cache_file
}

# 1. Data import and manipulation -----------------------------------------

# source("1_data_manip.R")


# 2. Installing latex font ------------------------------------------------

library(sysfonts)
library(showtext)

font_add("Latin Modern Math", "C:/USERS/WMF098/APPDATA/LOCAL/MICROSOFT/WINDOWS/FONTS/LATINMODERN-MATH.OTF")  # adjust path
showtext_auto()
