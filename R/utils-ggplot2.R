

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Theme for rgl plots which turns off some white rectangles for nicer render
#'
#' @param ... arguments ignored
#'
#' @return some standard theme adjustments
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
theme_rgl <- function(...) {
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    ggplot2::theme(
      legend.background = ggplot2::element_blank(),
      legend.key        = ggplot2::element_blank(),
      plot.background   = ggplot2::element_blank(),
      panel.background  = ggplot2::element_blank(),
      complete = FALSE
    )
  } else {
    message("I don't even know what you're doing to try and use this without having ggplot2 installed. sorry.")
    NULL
  }
}
