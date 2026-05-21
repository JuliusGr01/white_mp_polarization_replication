########################
###### FUNCTIONS #######
########################


# 1. Small helpers --------------------------------------------------------

read_white_csv <- function(path, ...) {
  read.csv(path, stringsAsFactors = FALSE, check.names = FALSE, ...)
}

write_white_csv <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  old_digits <- getOption("digits")
  on.exit(options(digits = old_digits), add = TRUE)
  options(digits = 17)
  write.csv(x, path, row.names = FALSE, na = "")
}

lag_vec <- function(x, n) {
  if (n == 0L) return(x)
  c(rep(NA_real_, n), x[seq_len(length(x) - n)])
}

lead_vec <- function(x, n) {
  if (n == 0L) return(x)
  c(x[(n + 1L):length(x)], rep(NA_real_, n))
}

xml_unescape_white <- function(x) {
  x <- gsub("&lt;", "<", x, fixed = TRUE)
  x <- gsub("&gt;", ">", x, fixed = TRUE)
  x <- gsub("&quot;", "\"", x, fixed = TRUE)
  x <- gsub("&apos;", "'", x, fixed = TRUE)
  x <- gsub("&amp;", "&", x, fixed = TRUE)
  x
}

xlsx_col_to_index <- function(cell_ref) {
  letters <- gsub("[^A-Za-z]", "", cell_ref)
  chars <- strsplit(toupper(letters), "", fixed = TRUE)[[1]]
  out <- 0L
  for (ch in chars) out <- out * 26L + match(ch, LETTERS)
  out
}

extract_xml_matches <- function(text, pattern) {
  hit <- gregexpr(pattern, text, perl = TRUE)[[1]]
  if (identical(hit[1], -1L)) return(character(0))
  regmatches(text, list(hit))[[1]]
}

extract_xml_attr <- function(text, attr) {
  pattern <- paste0(attr, "=\"([^\"]*)\"")
  if (!grepl(pattern, text, perl = TRUE)) return(NA_character_)
  sub(paste0(".*", pattern, ".*"), "\\1", text, perl = TRUE)
}

extract_xml_text_nodes <- function(text) {
  nodes <- extract_xml_matches(text, "<t[^>]*>[\\s\\S]*?</t>")
  if (length(nodes) == 0L) return("")
  nodes <- gsub("<t[^>]*>", "", nodes, perl = TRUE)
  nodes <- gsub("</t>", "", nodes, fixed = TRUE)
  paste0(xml_unescape_white(nodes), collapse = "")
}

read_xlsx_first_sheet_base <- function(path) {
  if (!file.exists(path)) stop("Missing workbook: ", path)

  tmp <- tempfile("xlsx_")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  utils::unzip(path, exdir = tmp)
  sheet_path <- file.path(tmp, "xl", "worksheets", "sheet1.xml")
  if (!file.exists(sheet_path)) stop("Workbook has no xl/worksheets/sheet1.xml: ", path)

  shared_strings <- character(0)
  shared_path <- file.path(tmp, "xl", "sharedStrings.xml")
  if (file.exists(shared_path)) {
    shared_xml <- paste(readLines(shared_path, warn = FALSE, encoding = "UTF-8"), collapse = "")
    si_nodes <- extract_xml_matches(shared_xml, "<si[\\s\\S]*?</si>")
    shared_strings <- vapply(si_nodes, extract_xml_text_nodes, character(1))
  }

  sheet_xml <- paste(readLines(sheet_path, warn = FALSE, encoding = "UTF-8"), collapse = "")
  row_nodes <- extract_xml_matches(sheet_xml, "<row[^>]*>[\\s\\S]*?</row>")
  if (length(row_nodes) == 0L) stop("No rows found in workbook: ", path)

  parsed_rows <- vector("list", length(row_nodes))
  max_col <- 0L

  for (i in seq_along(row_nodes)) {
    cell_nodes <- extract_xml_matches(row_nodes[[i]], "<c[^>]*(?:>[\\s\\S]*?</c>|/>)")
    values <- list()

    for (cell in cell_nodes) {
      ref <- extract_xml_attr(cell, "r")
      col <- xlsx_col_to_index(ref)
      max_col <- max(max_col, col)
      type <- extract_xml_attr(cell, "t")

      if (!is.na(type) && identical(type, "inlineStr")) {
        value <- extract_xml_text_nodes(cell)
      } else {
        v_node <- extract_xml_matches(cell, "<v[^>]*>[\\s\\S]*?</v>")
        if (length(v_node) == 0L) {
          value <- ""
        } else {
          value <- gsub("<v[^>]*>", "", v_node[[1]], perl = TRUE)
          value <- gsub("</v>", "", value, fixed = TRUE)
          value <- xml_unescape_white(value)
          if (!is.na(type) && identical(type, "s")) {
            idx <- as.integer(value) + 1L
            value <- shared_strings[[idx]]
          }
        }
      }
      values[[as.character(col)]] <- value
    }
    parsed_rows[[i]] <- values
  }

  matrix_out <- matrix("", nrow = length(parsed_rows), ncol = max_col)
  for (i in seq_along(parsed_rows)) {
    row_values <- parsed_rows[[i]]
    for (nm in names(row_values)) {
      matrix_out[i, as.integer(nm)] <- row_values[[nm]]
    }
  }

  header <- trimws(matrix_out[1, ])
  out <- as.data.frame(matrix_out[-1, , drop = FALSE], stringsAsFactors = FALSE, check.names = FALSE)
  names(out) <- header
  out
}

parse_month_year_white <- function(x) {
  m <- regexec("^\\s*([0-9]{4})[_-]([0-9]{1,2})\\s*$", as.character(x))
  parts <- regmatches(as.character(x), m)
  vapply(parts, function(p) {
    if (length(p) != 3L) stop("Could not parse Month_Year value: ", x)
    sprintf("%04d-%02d-01", as.integer(p[[2]]), as.integer(p[[3]]))
  }, character(1))
}

parse_employment_value_white <- function(x) {
  text <- gsub("\u00a0", "", trimws(as.character(x)), fixed = TRUE)
  out <- rep(NA_real_, length(text))
  has_comma <- grepl(",", text, fixed = TRUE)
  out[has_comma] <- as.numeric(gsub(",", "", text[has_comma], fixed = TRUE))
  no_comma <- !has_comma & nzchar(text)
  numeric <- as.numeric(text[no_comma])
  decimal_noninteger <- grepl(".", text[no_comma], fixed = TRUE) & numeric != floor(numeric)
  numeric[decimal_noninteger] <- round(numeric[decimal_noninteger] * 1000)
  out[no_comma] <- numeric
  out
}

build_cps_ee_from_excel <- function(xlsx_path,
                                    crosswalk_path,
                                    out_dir) {
  raw <- read_xlsx_first_sheet_base(xlsx_path)
  raw$occupation <- trimws(raw$occupation)
  raw$period <- trimws(raw$period)
  raw$occ_pos <- as.integer(raw$occ_pos)
  raw$value_thousands <- parse_employment_value_white(raw$value)
  raw$ym <- as.Date(parse_month_year_white(raw$Month_Year))
  raw$year <- as.integer(format(raw$ym, "%Y"))
  raw$month <- as.integer(format(raw$ym, "%m"))

  raw$value_thousands_original <- raw$value_thousands
  raw$value_correction_note <- ""

  correction_1973 <- raw$Month_Year == "1973_08" &
    raw$period == "021972 - 011974" &
    raw$occ_pos == 30L
  correction_1981 <- raw$Month_Year == "1981_04" &
    raw$period == "021974 - 121982" &
    raw$occ_pos == 45L

  if (sum(correction_1973) != 1L) stop("1973_08 value correction did not match exactly one row.")
  if (sum(correction_1981) != 1L) stop("1981_04 value correction did not match exactly one row.")

  raw$value_thousands[correction_1973] <- 2726
  raw$value_correction_note[correction_1973] <- "Corrected from 3726 using the transport-equipment parent residual: 3174 - 448."
  raw$value_thousands[correction_1981] <- 810
  raw$value_correction_note[correction_1981] <- "Corrected from 210 using the farm-laborers parent residual: 1030 - 220."

  crosswalk <- read_white_csv(crosswalk_path)
  crosswalk$occ_pos <- as.integer(crosswalk$occ_pos)
  classified <- merge(
    raw,
    crosswalk,
    by = c("period", "occ_pos", "occupation"),
    all.x = TRUE,
    sort = FALSE
  )
  if (any(is.na(classified$is_leaf))) {
    bad <- unique(classified[is.na(classified$is_leaf), c("period", "occ_pos", "occupation")])
    stop("Missing crosswalk rows. First unmatched: ", paste(utils::capture.output(print(utils::head(bad, 10))), collapse = " "))
  }

  classified$is_leaf <- classified$is_leaf %in% c(TRUE, "True", "TRUE", "true", "1", 1)
  leaf <- classified[classified$is_leaf, ]
  leaf$employment <- leaf$value_thousands * 1000

  classified_leaf_groups <- leaf[!is.na(leaf$alm_group) & leaf$alm_group != "", ]
  routine <- aggregate(employment ~ ym, classified_leaf_groups[classified_leaf_groups$alm_group == "Routine", ], sum)
  abstract <- aggregate(employment ~ ym, classified_leaf_groups[classified_leaf_groups$alm_group == "Abstract", ], sum)
  manual <- aggregate(employment ~ ym, classified_leaf_groups[classified_leaf_groups$alm_group == "Manual", ], sum)
  names(routine)[2] <- "routine_emp_abs"
  names(abstract)[2] <- "abstract_emp_abs"
  names(manual)[2] <- "manual_emp_abs"

  panel <- Reduce(function(x, y) merge(x, y, by = "ym", all = TRUE), list(routine, abstract, manual))
  panel[is.na(panel)] <- 0
  panel <- panel[order(panel$ym), ]
  panel$nonroutine_emp_abs <- panel$abstract_emp_abs + panel$manual_emp_abs
  panel$total_emp_abs <- panel$routine_emp_abs + panel$nonroutine_emp_abs
  panel$routine_emp_rel_emp <- panel$routine_emp_abs / panel$total_emp_abs
  panel$nonroutine_emp_rel_emp <- panel$nonroutine_emp_abs / panel$total_emp_abs
  panel$routine_emp_share <- panel$routine_emp_rel_emp
  panel$log_routine_emp <- log(panel$routine_emp_abs)
  panel$log_nonroutine_emp <- log(panel$nonroutine_emp_abs)
  panel$log_abstract_emp <- log(panel$abstract_emp_abs)
  panel$log_manual_emp <- log(panel$manual_emp_abs)
  panel$log_total_emp <- log(panel$total_emp_abs)

  panel <- panel[, c(
    "ym", "routine_emp_abs", "nonroutine_emp_abs", "abstract_emp_abs", "manual_emp_abs",
    "total_emp_abs", "routine_emp_rel_emp", "nonroutine_emp_rel_emp", "routine_emp_share",
    "log_routine_emp", "log_nonroutine_emp", "log_abstract_emp", "log_manual_emp", "log_total_emp"
  )]

  employment_monthly <- data.frame(
    date = panel$ym,
    routine_emp = panel$routine_emp_abs,
    nonroutine_emp = panel$nonroutine_emp_abs,
    total_emp = panel$total_emp_abs,
    routine_share = panel$routine_emp_share,
    log_total = panel$log_total_emp,
    log_routine = panel$log_routine_emp,
    log_nonroutine = panel$log_nonroutine_emp
  )

  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  write_white_csv(leaf[order(leaf$ym, leaf$occ_pos), ], file.path(out_dir, "cps_ee_1969_1982_leaf_alm_long.csv"))
  write_white_csv(panel, file.path(out_dir, "cps_ee_1969_1982_alm_panel.csv"))
  write_white_csv(employment_monthly, file.path(out_dir, "cps_ee_1969_1982_employment_monthly.csv"))
  write_white_csv(crosswalk, file.path(out_dir, "cps_ee_1969_1982_alm_crosswalk.csv"))

  employment_monthly
}

load_bls_series_from_raw <- function(series_id,
                                     all_data_path) {
  raw <- read.delim(
    all_data_path,
    header = TRUE,
    sep = "\t",
    stringsAsFactors = FALSE,
    strip.white = TRUE,
    comment.char = "",
    quote = "",
    fill = TRUE
  )
  raw <- raw[raw$series_id == series_id & grepl("^M[0-9]{2}$", raw$period) & raw$period != "M13", ]
  raw$month <- as.integer(sub("^M", "", raw$period))
  raw$date <- as.Date(sprintf("%04d-%02d-01", as.integer(raw$year), raw$month))
  raw$value <- suppressWarnings(as.numeric(raw$value))
  raw <- raw[!is.na(raw$date) & !is.na(raw$value), c("date", "value")]
  raw[order(raw$date), ]
}

build_bls_occ_panel_from_raw <- function(all_data_path,
                                         out_path = NULL) {
  routine_ids <- c("LNU02032205", "LNU02032208", "LNU02032212")
  nonroutine_ids <- c("LNU02032201", "LNU02032204")
  series_ids <- c(routine_ids, nonroutine_ids)

  raw <- do.call(rbind, lapply(series_ids, function(series_id) {
    x <- load_bls_series_from_raw(series_id, all_data_path)
    data.frame(series_id = series_id, date = x$date, employed = x$value * 1000)
  }))

  raw$is_routine <- raw$series_id %in% routine_ids
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
  out <- out[, c("date", "routine_emp", "nonroutine_emp", "total_emp", "routine_share", "log_total", "log_routine", "log_nonroutine")]

  if (!is.null(out_path)) write_white_csv(out, out_path)
  out
}

build_extended_panel_from_cps_and_bls_raw <- function(cps_ee,
                                                      bls_raw_path,
                                                      out_path = NULL) {
  bls <- build_bls_occ_panel_from_raw(bls_raw_path)
  cps_ee <- cps_ee[cps_ee$date >= as.Date("1969-01-01") & cps_ee$date < as.Date("1983-01-01"), ]
  bls <- bls[bls$date >= as.Date("1983-01-01"), ]
  out <- rbind(cps_ee, bls)
  out <- out[order(out$date), ]
  out <- out[!duplicated(out$date), ]
  if (!is.null(out_path)) write_white_csv(out, out_path)
  out
}

parse_mtgdate_white <- function(x) {
  text <- sub("\\.0$", "", trimws(as.character(x)))
  out <- as.Date(rep(NA_character_, length(text)))

  yyyymmdd <- grepl("^(19|20)[0-9]{6}$", text)
  out[yyyymmdd] <- as.Date(text[yyyymmdd], format = "%Y%m%d")

  yyyymm <- grepl("^(19|20)[0-9]{2}(0[1-9]|1[0-2])$", text) & is.na(out)
  out[yyyymm] <- as.Date(paste0(text[yyyymm], "01"), format = "%Y%m%d")

  mmddyy <- grepl("^[0-9]{5,6}$", text) & is.na(out)
  if (any(mmddyy)) {
    padded <- sprintf("%06d", as.integer(text[mmddyy]))
    mm <- as.integer(substr(padded, 1, 2))
    dd <- as.integer(substr(padded, 3, 4))
    yy <- as.integer(substr(padded, 5, 6))
    yyyy <- ifelse(yy <= 30L, 2000L + yy, 1900L + yy)
    out[mmddyy] <- as.Date(sprintf("%04d-%02d-%02d", yyyy, mm, dd))
  }

  numeric <- suppressWarnings(as.numeric(text))
  stata_like <- is.na(out) & !is.na(numeric) & numeric >= 2500 & numeric <= 20000
  out[stata_like] <- as.Date(numeric[stata_like], origin = "1960-01-01")
  excel_like <- is.na(out) & !is.na(numeric) & numeric >= 20000 & numeric <= 50000
  out[excel_like] <- as.Date(numeric[excel_like], origin = "1899-12-30")

  month_label <- is.na(out)
  if (any(month_label)) {
    normalized <- tolower(gsub("-", " ", text[month_label], fixed = TRUE))
    normalized <- gsub("\\s+", " ", trimws(normalized))
    month_lookup <- c(
      jan = 1L, january = 1L,
      feb = 2L, february = 2L,
      mar = 3L, march = 3L,
      apr = 4L, april = 4L,
      may = 5L,
      jun = 6L, june = 6L,
      jul = 7L, july = 7L,
      aug = 8L, august = 8L,
      sep = 9L, sept = 9L, september = 9L,
      oct = 10L, october = 10L,
      nov = 11L, november = 11L,
      dec = 12L, december = 12L
    )
    parsed <- vapply(normalized, function(value) {
      pieces <- strsplit(value, " ", fixed = TRUE)[[1]]
      if (length(pieces) != 2L || !pieces[[1]] %in% names(month_lookup)) return(NA_character_)
      yy <- suppressWarnings(as.integer(pieces[[2]]))
      if (is.na(yy)) return(NA_character_)
      yyyy <- if (yy < 100L) {
        if (yy <= 30L) 2000L + yy else 1900L + yy
      } else {
        yy
      }
      sprintf("%04d-%02d-01", yyyy, unname(month_lookup[[pieces[[1]]]]))
    }, character(1))
    parsed <- as.Date(parsed)
    out[month_label] <- parsed
    }
  out
}

load_rr_shocks_white <- function(path) {
  raw <- read.csv2(path, stringsAsFactors = FALSE, check.names = FALSE)
  date_col <- names(raw)[toupper(names(raw)) == "MTGDATE"][1]
  shock_col <- names(raw)[length(names(raw))]
  if (is.na(date_col)) stop("RR shock file must contain MTGDATE.")
  date <- as.Date(format(parse_mtgdate_white(raw[[date_col]]), "%Y-%m-01"))
  shock <- as.numeric(gsub(",", ".", trimws(as.character(raw[[shock_col]])), fixed = TRUE))
  d <- data.frame(date = date, shock = shock)
  d <- d[!is.na(d$date) & !is.na(d$shock), ]
  monthly <- aggregate(shock ~ date, d, sum)
  full <- data.frame(date = seq(min(monthly$date), max(monthly$date), by = "month"))
  monthly <- merge(full, monthly, by = "date", all.x = TRUE)
  monthly$shock[is.na(monthly$shock)] <- 0
  monthly[order(monthly$date), ]
}

load_jk_shocks_white <- function(path,
                                 mp_col = "MP_pm",
                                 cbi_col = "CBI_pm",
                                 pc1_col = NULL) {
  raw <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  required <- c("year", "month", mp_col, cbi_col)
  if (!is.null(pc1_col)) required <- c(required, pc1_col)
  missing <- setdiff(required, names(raw))
  if (length(missing) > 0L) stop("JK shock file missing columns: ", paste(missing, collapse = ", "))

  parse_num <- function(x) {
    as.numeric(gsub(",", ".", trimws(as.character(x)), fixed = TRUE))
  }

  out <- data.frame(
    date = as.Date(sprintf("%04d-%02d-01", as.integer(raw$year), as.integer(raw$month))),
    MP = parse_num(raw[[mp_col]]),
    CBI = parse_num(raw[[cbi_col]])
  )
  if (!is.null(pc1_col)) out[[pc1_col]] <- parse_num(raw[[pc1_col]])

  out <- out[!is.na(out$date), ]
  value_cols <- setdiff(names(out), "date")
  out <- aggregate(out[value_cols], by = list(date = out$date), sum, na.rm = TRUE)

  full <- data.frame(date = seq(min(out$date), max(out$date), by = "month"))
  out <- merge(full, out, by = "date", all.x = TRUE)
  for (col in value_cols) out[[col]][is.na(out[[col]])] <- 0
  out[order(out$date), ]
}

merge_jk_shocks_white <- function(panel,
                                  shock_path,
                                  mp_col = "MP_pm",
                                  cbi_col = "CBI_pm",
                                  shock_names = c("MP", "CBI")) {
  shocks <- load_jk_shocks_white(shock_path, mp_col = mp_col, cbi_col = cbi_col)
  names(shocks)[names(shocks) == "MP"] <- shock_names[[1]]
  names(shocks)[names(shocks) == "CBI"] <- shock_names[[2]]

  panel <- merge(panel, shocks, by = "date", all.x = TRUE)
  panel <- panel[panel$date >= min(shocks$date) & panel$date <= max(shocks$date), ]
  panel <- panel[order(panel$date), ]

  missing_shocks <- !stats::complete.cases(panel[, shock_names, drop = FALSE])
  if (any(missing_shocks)) {
    first_missing <- panel$date[missing_shocks][[1]]
    stop("Missing JK shock values in merged LP panel. First missing month: ", first_missing)
  }

  panel
}

add_statsmodels_stl_sa <- function(panel) {
  tmp_in <- tempfile(fileext = ".csv")
  tmp_out <- tempfile(fileext = ".csv")
  write.csv(panel, tmp_in, row.names = FALSE)

  py_code <- paste(
    "import numpy as np, pandas as pd",
    "from statsmodels.tsa.seasonal import STL",
    "inp, outp = r'''__IN__''', r'''__OUT__'''",
    "df = pd.read_csv(inp, parse_dates=['date'])",
    "def sa(s):",
    "    y = pd.to_numeric(s, errors='raise').astype(float)",
    "    fit = STL(np.log(y.to_numpy()), period=12, seasonal=13, robust=True).fit()",
    "    return np.exp(np.log(y.to_numpy()) - fit.seasonal)",
    "df['routine_emp_sa'] = sa(df['routine_emp'])",
    "df['nonroutine_emp_sa'] = sa(df['nonroutine_emp'])",
    "df['total_emp_sa'] = sa(df['total_emp'])",
    "df['routine_share_sa'] = df['routine_emp_sa'] / df['total_emp_sa']",
    "df['log_total_sa'] = np.log(df['total_emp_sa'])",
    "df['log_routine_sa'] = np.log(df['routine_emp_sa'])",
    "df['log_nonroutine_sa'] = np.log(df['nonroutine_emp_sa'])",
    "df.to_csv(outp, index=False)",
    sep = "\n"
  )
  py_code <- gsub("__IN__", gsub("\\\\", "/", tmp_in), py_code, fixed = TRUE)
  py_code <- gsub("__OUT__", gsub("\\\\", "/", tmp_out), py_code, fixed = TRUE)

  status <- system2("python", c("-c", shQuote(py_code)))
  if (!identical(status, 0L)) stop("Python statsmodels STL seasonal adjustment failed.")
  out <- read_white_csv(tmp_out)
  out$date <- as.Date(out$date)
  out
}

build_white_lp_panel_from_raw <- function(extended_panel,
                                          bls_raw_path,
                                          rr_shock_path,
                                          out_path = NULL) {
  shocks <- load_rr_shocks_white(rr_shock_path)
  names(shocks)[names(shocks) == "shock"] <- "eps"
  panel <- merge(extended_panel, shocks, by = "date", all.x = TRUE)
  panel <- panel[panel$date >= as.Date("1969-01-01") & panel$date <= as.Date("2020-12-31"), ]
  panel <- panel[order(panel$date), ]
  panel <- add_statsmodels_stl_sa(panel)

  total_nonag <- load_bls_series_from_raw("LNS12032187", bls_raw_path)
  names(total_nonag)[names(total_nonag) == "value"] <- "total_nonag_employment_thousands"
  total_nonag$total_nonag_emp <- total_nonag$total_nonag_employment_thousands * 1000
  panel <- merge(panel, total_nonag[, c("date", "total_nonag_emp")], by = "date", all.x = TRUE)
  panel <- panel[order(panel$date), ]
  if (any(is.na(panel$total_nonag_emp))) stop("Missing BLS nonagricultural employment in final LP panel.")

  panel$routine_share <- panel$routine_share_sa
  panel$total_emp <- panel$total_nonag_emp
  panel$routine_emp <- panel$routine_share * panel$total_emp
  panel$nonroutine_emp <- (1 - panel$routine_share) * panel$total_emp
  panel$log_total <- log(panel$total_emp)
  panel$log_routine <- log(panel$routine_emp)
  panel$log_nonroutine <- log(panel$nonroutine_emp)
  panel <- panel[panel$date >= as.Date("1969-01-01") & panel$date <= as.Date("2008-12-31"), ]

  panel <- panel[, c(
    "date", "routine_emp", "nonroutine_emp", "total_emp", "routine_share",
    "log_total", "log_routine", "log_nonroutine", "eps",
    "routine_emp_sa", "nonroutine_emp_sa", "total_emp_sa", "routine_share_sa",
    "log_total_sa", "log_routine_sa", "log_nonroutine_sa", "total_nonag_emp"
  )]

  if (!is.null(out_path)) write_white_csv(panel, out_path)
  panel
}

build_white_lp_outcome_panel_from_extended <- function(extended_panel,
                                                       bls_raw_path,
                                                       start_date = as.Date("1969-01-01"),
                                                       end_date = NULL) {
  panel <- extended_panel
  panel$date <- as.Date(panel$date)
  if (is.null(end_date)) end_date <- max(panel$date, na.rm = TRUE)
  panel <- panel[panel$date >= start_date & panel$date <= end_date, ]
  panel <- panel[order(panel$date), ]
  panel <- add_statsmodels_stl_sa(panel)

  total_nonag <- load_bls_series_from_raw("LNS12032187", bls_raw_path)
  names(total_nonag)[names(total_nonag) == "value"] <- "total_nonag_employment_thousands"
  total_nonag$total_nonag_emp <- total_nonag$total_nonag_employment_thousands * 1000
  panel <- merge(panel, total_nonag[, c("date", "total_nonag_emp")], by = "date", all.x = TRUE)
  panel <- panel[order(panel$date), ]
  if (any(is.na(panel$total_nonag_emp))) stop("Missing BLS nonagricultural employment in long LP outcome panel.")

  panel$routine_share <- panel$routine_share_sa
  panel$total_emp <- panel$total_nonag_emp
  panel$routine_emp <- panel$routine_share * panel$total_emp
  panel$nonroutine_emp <- (1 - panel$routine_share) * panel$total_emp
  panel$log_total <- log(panel$total_emp)
  panel$log_routine <- log(panel$routine_emp)
  panel$log_nonroutine <- log(panel$nonroutine_emp)

  panel[, c(
    "date", "routine_emp", "nonroutine_emp", "total_emp", "routine_share",
    "log_total", "log_routine", "log_nonroutine",
    "routine_emp_sa", "nonroutine_emp_sa", "total_emp_sa", "routine_share_sa",
    "log_total_sa", "log_routine_sa", "log_nonroutine_sa", "total_nonag_emp"
  )]
}

validate_panel_against_reference <- function(built_path,
                                             reference_path,
                                             out_path) {
  built <- read_white_csv(built_path)
  ref <- read_white_csv(reference_path)
  common <- intersect(names(built), names(ref))
  if (!identical(names(built), names(ref))) {
    warning("Column names differ between built and reference panel.")
  }
  rows <- list()
  for (col in common) {
    bx <- suppressWarnings(as.numeric(built[[col]]))
    rx <- suppressWarnings(as.numeric(ref[[col]]))
    numeric_col <- any(!is.na(bx)) || any(!is.na(rx))
    if (numeric_col) {
      rows[[col]] <- data.frame(column = col, max_abs_diff = max(abs(bx - rx), na.rm = TRUE))
    } else {
      rows[[col]] <- data.frame(column = col, max_abs_diff = if (identical(as.character(built[[col]]), as.character(ref[[col]]))) 0 else NA_real_)
    }
  }
  out <- do.call(rbind, rows)
  row.names(out) <- NULL
  write_white_csv(out, out_path)
  out
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

shock_lag_cols_white <- function(shock_vars, n_lags_shock) {
  if (n_lags_shock <= 0L) return(character(0))
  unlist(lapply(shock_vars, function(shock_var) {
    paste0(shock_var, "_L", seq_len(n_lags_shock))
  }), use.names = FALSE)
}

control_cols_multi_shock_white <- function(n_lag_y,
                                           shock_vars,
                                           n_lags_shock,
                                           include_time_trend = FALSE) {
  cols <- "const"
  if (include_time_trend) cols <- c(cols, "time_trend")
  cols <- c(cols, paste0("y_L", seq_len(n_lag_y)))
  cols <- c(cols, shock_lag_cols_white(shock_vars, n_lags_shock))
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

build_design_multi_shock_white <- function(data,
                                           y_var,
                                           shock_vars,
                                           n_lag_y,
                                           n_lags_shock,
                                           y_lag_transform = "diff",
                                           include_time_trend = FALSE) {
  required <- c("date", y_var, shock_vars)
  missing <- setdiff(required, names(data))
  if (length(missing) > 0L) stop("LP data missing columns: ", paste(missing, collapse = ", "))

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

  for (shock_var in shock_vars) {
    for (L in seq_len(n_lags_shock)) {
      df0[[paste0(shock_var, "_L", L)]] <- lag_vec(df0[[shock_var]], L)
    }
  }

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

LP_white_event_window <- function(data,
                                  H = 48L,
                                  y_var,
                                  shock_var = "eps",
                                  shock_start = NULL,
                                  shock_end = NULL,
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

  event_window <- rep(TRUE, nrow(df0))
  if (!is.null(shock_start)) event_window <- event_window & df0$date >= shock_start
  if (!is.null(shock_end)) event_window <- event_window & df0$date <= shock_end

  horizons <- seq_len(H)
  x_cols <- c(
    control_cols_white(n_lags_y, n_lags_shock, include_time_trend),
    shock_var
  )

  out <- vector("list", H)

  for (h_val in horizons) {
    dy_h <- lead_vec(df0[[y_var]], h_val) - df0[[y_var]]
    fit <- fit_ols_hac_white(
      dy_h[event_window],
      df0[event_window, x_cols, drop = FALSE],
      maxlags = nw_lags
    )

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

LP_white_multi_shock <- function(data,
                                 H = 48L,
                                 y_var,
                                 shock_vars,
                                 shock_labels = NULL,
                                 n_lags_y = 12L,
                                 n_lags_shock = 12L,
                                 nw_lags = 12L,
                                 scale = 1,
                                 confint = 1.645,
                                 y_lag_transform = "diff",
                                 include_time_trend = FALSE) {
  if (is.null(shock_labels)) shock_labels <- shock_vars
  if (is.null(names(shock_labels))) names(shock_labels) <- shock_vars

  df0 <- build_design_multi_shock_white(
    data = data,
    y_var = y_var,
    shock_vars = shock_vars,
    n_lag_y = n_lags_y,
    n_lags_shock = n_lags_shock,
    y_lag_transform = y_lag_transform,
    include_time_trend = include_time_trend
  )

  horizons <- seq_len(H)
  x_cols <- c(
    control_cols_multi_shock_white(n_lags_y, shock_vars, n_lags_shock, include_time_trend),
    shock_vars
  )

  out <- list()

  for (h_val in horizons) {
    dy_h <- lead_vec(df0[[y_var]], h_val) - df0[[y_var]]
    fit <- fit_ols_hac_white(dy_h, df0[, x_cols, drop = FALSE], maxlags = nw_lags)

    for (shock_var in shock_vars) {
      estimate <- unname(fit$coef[[shock_var]]) * scale
      se <- unname(fit$se[[shock_var]]) * scale

      out[[length(out) + 1L]] <- data.frame(
        outcome = y_var,
        shock = shock_var,
        shock_label = unname(shock_labels[[shock_var]]),
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

LP_white_shock_comparison <- function(data,
                                      H = 48L,
                                      y_var,
                                      shock_vars,
                                      shock_labels = NULL,
                                      n_lags_y = 12L,
                                      n_lags_shock = 12L,
                                      nw_lags = 12L,
                                      scale = 1,
                                      confint = 1.645,
                                      y_lag_transform = "diff",
                                      include_time_trend = FALSE) {
  if (is.null(shock_labels)) shock_labels <- shock_vars
  if (is.null(names(shock_labels))) names(shock_labels) <- shock_vars

  pieces <- lapply(shock_vars, function(shock_var) {
    irf <- LP_white(
      data = data,
      H = H,
      y_var = y_var,
      shock_var = shock_var,
      n_lags_y = n_lags_y,
      n_lags_shock = n_lags_shock,
      nw_lags = nw_lags,
      scale = scale,
      confint = confint,
      y_lag_transform = y_lag_transform,
      include_time_trend = include_time_trend
    )

    irf$shock <- shock_var
    irf$shock_label <- unname(shock_labels[[shock_var]])
    irf[, c(
      "outcome", "shock", "shock_label", "h",
      "estimate_raw", "se", "conf_low_raw", "conf_high_raw"
    )]
  })

  out <- do.call(rbind, pieces)
  row.names(out) <- NULL
  out
}

LP_white_shock_comparison_event_window <- function(data,
                                                   H = 48L,
                                                   y_var,
                                                   shock_vars,
                                                   shock_windows,
                                                   shock_labels = NULL,
                                                   n_lags_y = 12L,
                                                   n_lags_shock = 12L,
                                                   nw_lags = 12L,
                                                   scale = 1,
                                                   confint = 1.645,
                                                   y_lag_transform = "diff",
                                                   include_time_trend = FALSE) {
  if (is.null(shock_labels)) shock_labels <- shock_vars
  if (is.null(names(shock_labels))) names(shock_labels) <- shock_vars
  required_window_cols <- c("shock", "start", "end")
  missing_window_cols <- setdiff(required_window_cols, names(shock_windows))
  if (length(missing_window_cols) > 0L) {
    stop("shock_windows missing columns: ", paste(missing_window_cols, collapse = ", "))
  }

  pieces <- lapply(shock_vars, function(shock_var) {
    window_row <- shock_windows[shock_windows$shock == shock_var, ]
    if (nrow(window_row) != 1L) stop("Expected exactly one shock window for: ", shock_var)

    irf <- LP_white_event_window(
      data = data,
      H = H,
      y_var = y_var,
      shock_var = shock_var,
      shock_start = window_row$start[[1]],
      shock_end = window_row$end[[1]],
      n_lags_y = n_lags_y,
      n_lags_shock = n_lags_shock,
      nw_lags = nw_lags,
      scale = scale,
      confint = confint,
      y_lag_transform = y_lag_transform,
      include_time_trend = include_time_trend
    )

    irf$shock <- shock_var
    irf$shock_label <- unname(shock_labels[[shock_var]])
    irf$shock_start <- window_row$start[[1]]
    irf$shock_end <- window_row$end[[1]]
    irf[, c(
      "outcome", "shock", "shock_label", "shock_start", "shock_end", "h",
      "estimate_raw", "se", "conf_low_raw", "conf_high_raw"
    )]
  })

  out <- do.call(rbind, pieces)
  row.names(out) <- NULL
  out
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


# 3. IRF output helpers ---------------------------------------------------

write_figure3_irf_csv <- function(irfs, out_path) {
  rows <- list()

  for (nm in names(irfs)) {
    irf <- irfs[[nm]][order(irfs[[nm]]$h), ]

    rows[[nm]] <- data.frame(
      outcome = nm,
      horizon = irf$h,
      estimate = irf$estimate_raw,
      se = irf$se,
      conf_low = irf$conf_low_raw,
      conf_high = irf$conf_high_raw
    )
  }

  out <- do.call(rbind, rows)
  row.names(out) <- NULL
  write_white_csv(out, out_path)
  out
}

write_figure3_multi_shock_irf_csv <- function(irfs, out_path) {
  rows <- list()

  for (nm in names(irfs)) {
    irf <- irfs[[nm]]
    shock_levels <- unique(irf$shock)

    for (shock in shock_levels) {
      irf_s <- irf[irf$shock == shock, ]
      irf_s <- irf_s[order(irf_s$h), ]

      label <- shock
      if ("shock_label" %in% names(irf_s) && nrow(irf_s) > 0L) {
        label <- irf_s$shock_label[[1]]
      }

      row <- data.frame(
        outcome = nm,
        shock = shock,
        shock_label = label,
        horizon = irf_s$h,
        estimate = irf_s$estimate_raw,
        se = irf_s$se,
        conf_low = irf_s$conf_low_raw,
        conf_high = irf_s$conf_high_raw
      )
      if (all(c("shock_start", "shock_end") %in% names(irf_s))) {
        row$shock_start <- irf_s$shock_start
        row$shock_end <- irf_s$shock_end
      }

      rows[[paste(nm, shock, sep = "_")]] <- row
    }
  }

  out <- do.call(rbind, rows)
  row.names(out) <- NULL
  write_white_csv(out, out_path)
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
  write_white_csv(wide, out_path)
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

plot_figure3_multi_shock_white <- function(irf_list,
                                           out_path,
                                           shock_labels = c(MP = "MP", CBI = "CBI"),
                                           shock_colors = c(MP = "black", CBI = "firebrick3"),
                                           shock_ltys = c(MP = 1, CBI = 2),
                                           legend_position = "topleft",
                                           show_conf_bands = FALSE,
                                           band_alpha = 0.14) {
  panels <- list(
    list(key = "log_routine", title = "Routine Employment", ylabel = "Percent", ylim = c(-3, 1)),
    list(key = "log_nonroutine", title = "Nonroutine Employment", ylabel = "Percent", ylim = c(-3, 1)),
    list(key = "routine_share", title = "Routine Share", ylabel = "% Points", ylim = c(-1, 0.5)),
    list(key = "log_total", title = "Total Employment", ylabel = "Percent", ylim = c(-3, 1))
  )

  shock_order <- names(shock_labels)
  if (is.null(shock_order)) shock_order <- unique(irf_list[[1]]$shock)

  png(out_path, width = 1600, height = 1160, res = 200)
  par(family = "serif", mfrow = c(2, 2), mar = c(4.2, 4.4, 3.2, 1.0))

  for (i in seq_along(panels)) {
    p <- panels[[i]]
    irf_df <- irf_list[[p$key]]
    panel_values <- c(0, irf_df$estimate_raw)
    if (show_conf_bands && all(c("conf_low_raw", "conf_high_raw") %in% names(irf_df))) {
      panel_values <- c(panel_values, irf_df$conf_low_raw, irf_df$conf_high_raw)
    }
    ylim <- range(c(p$ylim, panel_values), finite = TRUE)
    pad <- diff(ylim) * 0.06
    if (!is.finite(pad) || pad == 0) pad <- 0.1
    ylim <- ylim + c(-pad, pad)

    plot(
      c(0, 48),
      c(0, 0),
      type = "n",
      xlab = "Months",
      ylab = p$ylabel,
      main = p$title,
      xlim = c(0, 48),
      ylim = ylim,
      xaxt = "n"
    )
    abline(h = 0, col = "grey30", lty = "dotted")
    axis(1, at = c(0, 12, 24, 36, 48))

    for (shock in shock_order) {
      shock_df <- irf_df[irf_df$shock == shock, ]
      shock_df <- shock_df[order(shock_df$h), ]
      h <- c(0, shock_df$h)
      shock_color <- unname(shock_colors[[shock]])

      if (show_conf_bands && all(c("conf_low_raw", "conf_high_raw") %in% names(shock_df))) {
        conf_low <- c(0, shock_df$conf_low_raw)
        conf_high <- c(0, shock_df$conf_high_raw)
        polygon(
          c(h, rev(h)),
          c(conf_low, rev(conf_high)),
          col = grDevices::adjustcolor(shock_color, alpha.f = band_alpha),
          border = NA
        )
      }
    }

    for (shock in shock_order) {
      shock_df <- irf_df[irf_df$shock == shock, ]
      shock_df <- shock_df[order(shock_df$h), ]
      h <- c(0, shock_df$h)
      estimate <- c(0, shock_df$estimate_raw)
      lines(
        h,
        estimate,
        col = unname(shock_colors[[shock]]),
        lty = unname(shock_ltys[[shock]]),
        lwd = 2
      )
    }

    if (i == 1L) {
      legend(
        legend_position,
        legend = unname(shock_labels[shock_order]),
        col = unname(shock_colors[shock_order]),
        lty = unname(shock_ltys[shock_order]),
        lwd = 2,
        bty = "n"
      )
    }
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
