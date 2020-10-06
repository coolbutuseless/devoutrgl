

`%||%` <- function (x, y) {
  if (is.null(x)) y else x
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Simpler version of 'modifyList'
#'
#' @param old,new lists
#'
#' @return updated version of old list
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
update_list <- function (old, new) {
  for (i in names(new)) old[[i]] <- new[[i]]
  old
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Darken a hex colour by the given amount
#' Stolen from \url{https://benjaminlmoore.wordpress.com/2014/03/18/guardian-data-blog-uk-elections/}
#'
#' @param hex_colour strings e.g. "#345678"
#' @param amount fraction to darken by. default 0.15
#'
#' @return darkened hex colours
#'
#' @importFrom grDevices col2rgb rgb
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
darken_colour <- function(hex_colour, amount = 0.15) {
  rgb(t(col2rgb(hex_colour) * (1 - amount)), maxColorValue = 255)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert a devices colour back into an RGB colour
#'
#' @param col 4 element RGBA colour with values in range [0-255]
#'
#' @importFrom grDevices rgb
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
col2hex <- function(col) {
  rgb(col[1], col[2], col[3], col[4], maxColorValue = 255)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Extract alpha from a hex colour
#'
#' @param hex hex colour with alpha i.e. '#123456ff'
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
hex2alpha <- function(hex) {
  if (nchar(hex) != 9) {
    return(1)
  }
  alpha <- substr(hex, 8, 9)
  strtoi(alpha, 16)/255
}

