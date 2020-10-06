

get_routine <- function(routine, M) {
  if (routine == 1) {
    par3dinterp(
      times = (0:3) * 2,
      userMatrix = list(
        M,
        rotate3d(M, pi, 1, 0, 0),
        rotate3d(M, pi, 0, 1, 0),
        M
      )
    )
  } else if (routine == 2) {

    par3dinterp(
      times = (0:3) * 2,
      userMatrix = list(
        M,
        rotate3d(M, -pi/2, 1, 0, 0),
        rotate3d(M, pi, 0, 0, 1),
        M
      )
    )
  } else {
    stop("No such routine: ", routine)
  }
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' A few demo animation routines
#'
#' This is just a wrapper around \code{rgl} functions \code{play3d()},
#' \code{spin3d()} and \code{par3dinter()}
#'
#' @param routine default: 1
#' @param duration duration. default 5
#'
#' @import rgl
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
animate_scene <- function(routine = 1, duration = 6) {

  M <- par3d('userMatrix')
  on.exit(par3d(userMatrix = M))

  f <- get_routine(routine, M)

  play3d(f, duration = duration)

}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' A few demo animation routines
#'
#' This is just a wrapper around \code{rgl} functions \code{play3d()},
#' \code{spin3d()} and \code{par3dinter()}
#'
#' @param gif_name name of gif output file
#' @param routine default: 1
#' @param duration duration. default 5
#' @param fps frames per second. default 10
#'
#' @import rgl
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
save_animated_scene <- function(gif_name = "ggrgl.gif", routine = 1, duration = 6, fps = 10) {

  M <- par3d('userMatrix')
  on.exit(par3d(userMatrix = M))

  f <- get_routine(routine, M)

  gif_frames <- movie3d(
    f,
    duration = duration,
    verbose  = FALSE
  )

  if (requireNamespace('magick', quietly = TRUE) && requireNamespace('gifski', quietly = TRUE)) {
    magick::image_write_gif(gif_frames, gif_name, delay = 1/fps)
  } else {
    warning("The following packages must be installed to save animations: magick, gifski")
  }

  invisible(gif_frames)
}





if (FALSE) {

  library(rgl)
  plot3d( cube3d(col = "green") )
  animate_scene()

  zz <- save_animated_scene()

  rgl.close()

}
