



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Names of all the elements supported by rgl
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
element_names <- c(
  "line", "polyline", "circle", "rect", "text", "polygon", "path", "raster", "raster_quad"
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# When the device is opened
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rgl_open <- function(args, state) {




  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # RGL R6 document
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  state$rdata$rgl <- RGLDoc$new(
    width                = state$dd$right,
    height               = state$dd$bottom,
    fov                  = state$rdata$fov    %||% 0,
    view                 = state$rdata$view,
    view_angle           = state$rdata$view_angle  %||% 0,
    view_flat_angle      = state$rdata$view_flat_angle  %||% -45,
    zoom                 = state$rdata$zoom %||% 0.800,
    view3d_args          = state$rdata$view3d_args,
    zscale               = state$rdata$zscale %||% 1,
    light                = state$rdata$light  %||% list(),
    useNULL              = !isTRUE(state$rdata$show_window),
    background           = state$rdata$background,
    polygon_offset_delta = state$rdata$polygon_offset_delta %||% 0.03
  )


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Some global state
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  state$rdata$ground <- 0

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Try and adjust for DPI
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  dpi <- state$rdata$dpi %||% 72
  state$dd$ipr <- c(1/dpi, 1/dpi)


  state
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# When the device is closed
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rgl_close <- function(args, state) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # If we got a filename, then save the state of the RGL window to either
  # PNG or WebGL depending on the suffix
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  filename <- state$rdata$filename
  if (!is.null(filename)) {
    state$rdata$rgl$save(filename)
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Only if explicitly requested will the rgl window get closed
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (isTRUE(state$rdata$close_window)) {
    rgl::rgl.close()
  }

  state
}




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   #        #
#   #
#   #       ##    # ##    ###
#   #        #    ##  #  #   #
#   #        #    #   #  #####
#   #        #    #   #  #
#   #####   ###   #   #   ###
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rgl_line <- function(args, state) {

  elem <- 'line'

  state$rdata$rgl$line(
    x1       = args$x1,
    x2       = args$x2,
    y1       = state$dd$bottom - args$y1,
    y2       = state$dd$bottom - args$y2,
    z        = state$rdata$ground,
    colour   = col2hex(state$gc$col),
    lwd      = state$gc$lwd
  )

  state
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  ####                  #
#  #   #                 #
#  #   #   ###    ###   ####
#  ####   #   #  #   #   #
#  # #    #####  #       #
#  #  #   #      #   #   #  #
#  #   #   ###    ###     ##
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rgl_rect <- function(args, state) {

  elem <- 'rect'

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Calculate extents of rect in the correct order
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  xmin <- min(c(args$x0, args$x1))
  xmax <- max(c(args$x0, args$x1))
  ymin <- min(c(args$y0, args$y1))
  ymax <- max(c(args$y0, args$y1))

  ymin <- state$dd$bottom - ymin
  ymax <- state$dd$bottom - ymax


  fill <- state$gc$fill
  z <- state$rdata$ground

  state$rdata$rgl$rect(
    xmin, ymax, xmax, ymin, z = z,
    colour = col2hex(state$gc$col),
    fill   = col2hex(fill),
    lwd    = state$gc$lwd,
    alpha  = state$gc$fill[4]/255
  )

  state
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  ####           ##            ##      #
#  #   #           #             #
#  #   #   ###     #    #   #    #     ##    # ##    ###
#  ####   #   #    #    #   #    #      #    ##  #  #   #
#  #      #   #    #    #  ##    #      #    #   #  #####
#  #      #   #    #     ## #    #      #    #   #  #
#  #       ###    ###       #   ###    ###   #   #   ###
#                       #   #
#                        ###
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rgl_polyline <- function(args, state) {

  elem <- 'polyline'

  state$rdata$rgl$polyline(
    xs     = args$x,
    ys     = state$dd$bottom - args$y,
    z      = state$rdata$ground,
    colour = col2hex(state$gc$col),
    lwd    = state$gc$lwd
  )

  state
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  ####           ##
#  #   #           #
#  #   #   ###     #    #   #   ## #   ###   # ##
#  ####   #   #    #    #   #  #  #   #   #  ##  #
#  #      #   #    #    #  ##   ##    #   #  #   #
#  #      #   #    #     ## #  #      #   #  #   #
#  #       ###    ###       #   ###    ###   #   #
#                       #   #  #   #
#                        ###    ###
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rgl_polygon <- function(args, state) {
  # message("polygon")
  elem <- 'polygon'

  z      <- state$rdata$ground

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Draw a polygon
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  state$rdata$rgl$polygon(
    x        = args$x,
    y        = state$dd$bottom - args$y,
    z        = z,
    colour   = col2hex(state$gc$col),
    fill     = col2hex(state$gc$fill),
    alpha    = state$gc$fill[4]/255,
    lwd      = state$gc$lwd
  )

  # polygons_df <- data.frame(
  #   x        = args$x,
  #   y        = state$dd$bottom - args$y,
  #   z        = z,
  #   group    = 1L,
  #   subgroup = 1L
  # )
  #
  # state$rdata$rgl$path(
  #   polygons_df,
  #   colour   = col2hex(state$gc$col),
  #   fill     = col2hex(state$gc$fill),
  #   alpha    = state$gc$fill[4]/255,
  #   lwd      = state$gc$lwd
  # )



  state
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  ####           #     #
#  #   #          #     #
#  #   #   ###   ####   # ##
#  ####       #   #     ##  #
#  #       ####   #     #   #
#  #      #   #   #  #  #   #
#  #       ####    ##   #   #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rgl_path <- function(args, state) {
  # message("path. npoly = ", args$npoly)
  elem <- 'path'

  extents <- c(0, cumsum(args$nper))

  xs <- args$x
  ys <- state$dd$bottom - args$y

  col    <- col2hex(state$gc$col)
  fill   <- col2hex(state$gc$fill)
  z      <- state$rdata$ground

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # This is where we'll assemble the data.frames for each polygon.
  # Data.frame should have x, y, group, subgroup
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  polygons_list <- list()

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # For each polygon in the path, capture its data.frame and add it to the list
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  for (poly in seq_len(args$npoly)) {
    lower     <- extents[poly     ] + 1L
    upper     <- extents[poly + 1L]
    x         <- xs[lower:upper]
    y         <- ys[lower:upper]

    polygon_df <- data.frame(
      x        = x,
      y        = y,
      z        = z,
      group    = 1,
      subgroup = args$npoly + 1 - poly,
      stringsAsFactors = FALSE
    )
    polygons_list[[poly]] <- polygon_df
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Bind all the polygons together
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  polygons_df <- do.call(rbind, polygons_list)
  polygons_df$z <- z

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Add it as a path
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  state$rdata$rgl$path(polygons_df, fill = fill, colour = NA, lwd = state$gc$lwd)

  state
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   ###     #                   ##
#  #   #                         #
#  #       ##    # ##    ###     #     ###
#  #        #    ##  #  #   #    #    #   #
#  #        #    #      #        #    #####
#  #   #    #    #      #   #    #    #
#   ###    ###   #       ###    ###    ###
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rgl_circle <- function(args, state) {

  elem <- 'circle'

  state$rdata$rgl$circle(
    x      = args$x,
    y      = state$dd$bottom - args$y,
    z      = state$rdata$ground,
    r      = args$r,
    colour = col2hex(state$gc$col),
    alpha  = state$gc$col[4]/255,
    circle_segments = state$rdata$circle_segments %||% 10
  )

  state
}





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' ASCII callback for the rdevice
#'
#' @param device_call name of device function call
#' @param args arguments to device function call
#' @param state list of rdata, dd and gc. Some or all of which may be NULL
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rgl_callback <- function(device_call, args, state) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Verbosity
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if ('device calls' %in% state$rdata$debug_items) {
    if (!device_call %in% c('strWidth', 'size', 'clip', 'mode', 'metricInfo', 'strWidthUTF8', 'textUTF8')) {
      message(device_call)
    }
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Anything we're not handling, just return() straight away
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  state <- switch(
    device_call,
    "open"         = rgl_open      (args, state),
    "close"        = rgl_close     (args, state),
    "line"         = rgl_line      (args, state),
    "polyline"     = rgl_polyline  (args, state),
    "circle"       = rgl_circle    (args, state),
    "rect"         = rgl_rect      (args, state),
    "text"         = rgl_text      (args, state),
    'strWidth'     = rgl_strWidth  (args, state),
    "textUTF8"     = rgl_text      (args, state),
    'strWidthUTF8' = rgl_strWidth  (args, state),
    'polygon'      = rgl_polygon   (args, state),
    'metricInfo'   = rgl_metricInfo(args, state),
    'path'         = rgl_path      (args, state),
    'raster'       = rgl_raster    (args, state),
    {
      # if (!device_call %in% c('strWidth', 'size', 'clip', 'mode', 'metricInfo')) {
      #   print(device_call);
      # }
      state
    }
  )

  state
}


