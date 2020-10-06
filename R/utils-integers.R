

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert integer vector into raw vector (unpacking 4-raw-values per integer)
#'
#' Adapted from code stolen from \code{minitypes} package
#'
#' @param int32_vec vector of raw values
#' @param endian little or big?
#' @param ... other arguments ignored
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
int32_to_raw <- function(int32_vec, endian = c('little', 'big'), ...) {
  stopifnot(is.integer(int32_vec))
  endian <- match.arg(endian)

  writeBin(int32_vec, raw(), size = 4L, useBytes = TRUE, endian = endian)
}

