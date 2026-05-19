########################
######### MAIN #########
########################

# This file manages the R replication folder and sources the empirical scripts.
# The layout mirrors the final project folder: setup here, reusable helper
# functions in functions.R, and the White (2022) local-projection workflow in
# 2_LP.R.


# 0. Set-up ---------------------------------------------------------------

get_script_dir <- function() {
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

setwd(get_script_dir())

# The exact-match replication below uses base R only.  The object is kept in the
# same style as the final project main.R, where dependencies are declared in one
# place.
libs <- character(0)

invisible(lapply(libs, function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}))


# 0.1 Paths ---------------------------------------------------------------

input_dir <- file.path("data")
ref_dir <- file.path("reference")
out_dir <- file.path("output")
fig_dir <- out_dir
tab_dir <- out_dir
cache_dir <- file.path(out_dir, "cache")

dir.create(input_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(ref_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

# Remove a validation artifact from the first draft of the R port if it exists.
unlink(file.path(out_dir, "validation_against_python_unsmoothed.csv"))


# 0.2 Helper functions ----------------------------------------------------

source("functions.R")


# 1. White (2022) replication --------------------------------------------

source("2_LP.R")
