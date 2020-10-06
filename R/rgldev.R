

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Set up the debug names for a given verbosity level
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
all_debug_levels       <- list()
all_debug_levels[[ 1]] <- c('element summary')
all_debug_levels[[ 2]] <- c('raster')
all_debug_levels[[ 3]] <- c('raster dims')
all_debug_levels[[99]] <- c('device calls')



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Graphics device for RGL display
#'
#' Uses \code{devout::rdevice()}.
#'
#' @param filename Filename to save output to. Default: NULL (display only).
#'        Currently only PNG (use '.png' as suffix) and WebGL (use '.html' as
#'        suffix) output supported.  Consider using \code{show_window = FALSE}
#'        when only saving to file, to avoid opening an opengl window on
#'        the screen.
#' @param width,height Screen or output dimensions in inches. Default 8x6
#' @param dpi Resolution. Default 72 (for screen display). A higher value
#'        (e.g. 150 or 300) is recommended when saving to PNG.
#' @param fov Field of view. Default: 0, indicates an isometric view where objects
#'        do not dimish with size into the distance. For regular perspective,
#'        try values of 30, or really anythin in the range [0, 180].
#' @param view One of 'front', 'flat'. This will be ignored if \code{view3d_args}
#'        is set.
#' @param view_angle rotation angle of view in degrees. Only used if \code{view} is
#'        'front' or 'flat'.  Default: 30
#' @param view_flat_angle angle to tip view when view is 'flat'.
#' @param zoom zoom level. default 0.66. Larger values make the plot appear
#'        further from the screen.
#' @param view3d_args full viewpoint specification as a list of arguments to
#'        \code{rgl::view3d()}.  Default: NULL
#' @param background Set the initial background. This is a list of arguments
#'        to a call to \code{rgl::rgl.bg()}
#' @param close_window Close window when \code{dev.off()} called? Default: FALSE
#'        (leave window open).
#' @param show_window Show rendering window? Default: display window only if
#'        \code{interactive()} is TRUE.
#' @param zscale Global Z scaling. Default: 1.  This is used as the argument
#'        in a call to \code{rgl::aspect3d(1, 1, zscale)}
#' @param light Set the light position. This is a list of arguments to a call
#'        to  \code{rgl::rgl.light()}.
#' @param circle_segments Circles in \code{devoutrgl} are rendered as
#'        polygons with this number of segments. Default: 10
#' @param verbosity Set the verbosity level. Default 0 (be quiet), values up
#'        to 99 for maximum verbosity.
#' @param polygon_offset_delta incremental change in \code{polygon_offset} after
#'        every element is rendered.  Adjust this value to try and
#'        minimise issues with z-fighting of overlapping elements.
#' @param ... other args passed in to device 'rdata'
#'
#' @import devout
#' @import gdtools
#' @importFrom stats setNames
#' @importFrom grDevices as.raster
#' @export
#'
#'
#' @examples
#' \dontrun{
#' library(devoutrgl)
#' library(ggplot2)
#'
#' p <- ggplot(mtcars) +
#'   geom_density(aes(mpg, fill=as.factor(cyl)), colour = '#ffffff00') +
#'   theme(legend.position = 'none') +
#'   theme_bw()
#'
#'
#' rgl(
#'   fov = 30
#' )
#' p
#' invisible(dev.off())
#' }
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rgldev <- function(
  filename              = NULL,
  width                 = 8,
  height                = 6,
  dpi                   = 72,
  fov                   = 0,
  view                  = c('front', 'flat'),
  view_angle            = 0,
  view_flat_angle       = -45,
  zoom                  = 0.800,
  view3d_args           = NULL,
  light                 = list(theta = 20, phi = 20),
  background            = list(color = 'white'),
  zscale                = 1,
  close_window          = FALSE,
  circle_segments       = 10,
  show_window           = interactive(),
  verbosity             = 0,
  polygon_offset_delta  = 0.1,
  ...) {


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # look-up a list of names to match the verbosity level
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  debug_items <- unlist(all_debug_levels[seq_len(verbosity)])


  devout::rdevice(
    rgl_callback,
    filename              = filename,
    width                 = width  * dpi/72,
    height                = height * dpi/72,
    dpi                   = dpi,
    fov                   = fov,
    view                  = view,
    view_angle            = view_angle,
    view_flat_angle       = view_flat_angle,
    zoom                  = zoom,
    view3d_args           = view3d_args,
    light                 = light,
    background            = background,
    zscale                = zscale,
    close_window          = close_window,
    circle_segments       = circle_segments,
    show_window           = show_window,
    debug_items           = debug_items,
    polygon_offset_delta  = polygon_offset_delta,
    ...,
    device_name = 'rgl'
  )
}
