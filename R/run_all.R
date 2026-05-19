########################
###### RUN ALL #########
########################

# Compatibility wrapper.  The active R pipeline follows the final-project
# structure and is orchestrated by main.R.

script_dir <- function() {
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

source(file.path(script_dir(), "main.R"))
