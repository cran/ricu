#' Read and write utilities
#'
#' Support for reading from and writing to pipe separated values (`.psv`)
#' files as used for the PhysioNet Sepsis Challenge.
#'
#' @details
#' Data for the PhysioNet Sepsis Challenge is distributed as pipe separated
#' values (`.psv`) files, split into separate files per patient ID, containing
#' time stamped rows with measured variables as columns. Files are named with
#' patient IDs and do not contain any patient identifiers as data. Functions
#' `read_psv()` and `write_psv()` can be used to read from and write to such
#' a data format.
#'
#' @param x Object to write to files
#' @param dir Directory to write the (many) files to or read from
#' @param na_rows If `TRUE` missing time steps are filled with `NaN` values,
#' if `FALSE`, rows where all data columns entries are missing are removed and
#' if `NULL`, data is written as-is
#'
#' @return While `write_psv()` is called for side effects and returns `NULL`
#' invisibly, `read_psv()` returns an object inheriting from `id_tbl`.
#'
#' @references
#' Reyna, M., Josef, C., Jeter, R., Shashikumar, S., Moody, B., Westover, M.
#' B., Sharma, A., Nemati, S., & Clifford, G. (2019). Early Prediction of
#' Sepsis from Clinical Data -- the PhysioNet Computing in Cardiology
#' Challenge 2019 (version 1.0.0). PhysioNet.
#' https://doi.org/10.13026/v64v-d857.
#'
#' @rdname data_export
#' @export
#'
write_psv <- function(x, dir, na_rows = NULL) {

  if (isTRUE(na_rows)) {
    x <- fill_gaps(x)
  } else if (isFALSE(na_rows)) {
    x <- rm_na(x, cols = data_vars(x), mode = "all")
  } else {
    assert_that(is.null(na_rows))
  }

  if (!identical(time_unit(x), "hours")) {
    new_int <- interval(x)
    units(new_int) <- "hours"
    x <- change_interval(x, new_int)
  }

  dat <- split(x, by = id_vars(x), keep.by = FALSE)
  files <- file.path(dir, paste0(names(dat), sep = ".psv"))

  Map(readr::write_delim, dat, files, delim = "|", na = "NaN")

  invisible(NULL)
}

#' @param col_spec A column specification as created by [readr::cols()]
#' @param id_var Name of the id column (IDs are generated from file names)
#' @param index_var Optional name of index column (will be coerced to
#' `difftime`)
#'
#' @rdname data_export
#' @export
#'
read_psv <- function(dir, col_spec = NULL, id_var = "stay_id",
                     index_var = NULL) {

  add_id <- function(x, val) {
    x[[id_var]] <- val
    x
  }

  assert_that(is.string(id_var), null_or(index_var, is.string))

  files <- list.files(dir, full.names = TRUE)

  dat <- lapply(files, readr::read_delim, col_types = col_spec, delim = "|",
                na = "NaN")
  ids <- as.integer(sub("\\.psv", "", sub("^p", "", basename(files))))

  dat <- Map(add_id, dat, ids)
  res <- rbindlist(lapply(dat, setDT))

  if (not_null(index_var)) {

    res <- res[, c(index_var) := hours(get(index_var))]

    as_ts_tbl(res, id_vars = id_var, index_var = index_var, by_ref = TRUE)

  } else {

    as_id_tbl(res, id_vars = id_var, by_ref = TRUE)
  }
}
