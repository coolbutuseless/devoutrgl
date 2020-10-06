

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Calculate and return the string width in the current state
#
# device_StrWidth should return the width of the given
# string in DEVICE units.
#
# graphics parameters that should be honoured (if possible):
#   font, cex, ps
#
# @param str string
#
# @return Optionally return 'width' the display width of the string in device units (numeric).
#         If not returned then a default value is used i.e. (strlen(str) + 2) * 72
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rgl_strWidth <- function(args, state) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Use the DPI to scale the font appropriately
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  dpi      <- 1/state$dd$ipr[1]
  fontsize <- state$gc$cex * state$gc$ps * dpi/72

  metrics <- gdtools::str_metrics(
    args$str,
    fontname = "sans",
    fontsize = fontsize,
    bold     = FALSE,
    italic   = FALSE,
    fontfile = ""
  )

  state$width <- metrics[['width']]

  state
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Ask for some metrics
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rgl_metricInfo <- function(args, state) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Use the DPI to scale the font appropriately
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  dpi      <- 1/state$dd$ipr[1]
  fontsize <- state$gc$cex * state$gc$ps * dpi/72

  cint <- abs(args$c)
  str  <- intToUtf8(cint)

  metrics  <- gdtools::str_metrics(
    str,
    fontname = "sans",
    fontsize = fontsize,
    bold     = FALSE,
    italic   = FALSE,
    fontfile = ""
  )

  state$ascent  <-  metrics[['ascent' ]]
  state$descent <-  metrics[['descent']]
  state$width   <-  metrics[['width'  ]]

  state
}




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rgl_text_helper <- function(str, x, y, z, rot, colour = 'black', state = NULL, manual = FALSE,
                            orientation = 'xy') {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Since we're rendering fonts to small images, they don't look so great under
  # zoom, so increase the resolution to compensate.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  scale     <- 3
  dpi       <- 1/state$dd$ipr[1]
  font_size <- state$gc$cex * state$gc$ps * scale * dpi/72

  if (!manual) {
    colour <- col2hex(state$gc$col)
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create some text as a PNG file
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  info <- create_text_as_png(
    text      = str,
    font      = 'sans',
    font_size = font_size,
    colour    = colour
  )

  if (is.null(info)) {
    # message("magick text couldn't be created for some reason: ", deparse(str))
    return(state)
  }

  text_width  <- info$width  / scale
  text_height <- info$height / scale

  if (isTRUE(manual)) {
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # This is a manual call that avoided coord scaling within the graphics device
    # so we need to manually scale here.
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    device_width  <- state$dd$right  - abs(state$dd$right  - state$dd$clipRight) - state$dd$clipLeft
    device_height <- state$dd$bottom - abs(state$dd$bottom - state$dd$clipTop)   - state$dd$clipBottom
    offset_x      <- state$dd$clipLeft
    offset_y      <- state$dd$bottom - state$dd$clipTop

    # txt <- sprintf("%5s  (%6.1f, %6.1f, %6.1f) %s", "orig", x, y, z, str)
    # message(txt)

    x <- x * device_width  - text_width /2 + offset_x
    y <- y * device_height - text_height/2 + offset_y
    z <- z

    # ToDo 2020-06-27 Need to undo the y offset if orientation is in the xz plane

    if (orientation == 'xz') {
      option <- 1
    } else {
      option <- 0
    }
  } else {
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # this text request passed through the graphics device, and all x + y
    # values have been scaled appropriately.
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    x <- x
    y <- state$dd$bottom - y
    z <- z
    option <- 0
  }

  # txt <- sprintf("%5s  (%6.1f, %6.1f, %6.1f) %s", manual, x, y, z, str)
  # message(txt)


  state$rdata$rgl$image_as_quad(
    png_filename = info$png_filename,
    x            = x,
    y            = y,
    z            = z,
    width        = text_width,
    height       = text_height,
    alpha        = state$gc$col[4]/255,
    angle        = -rot,
    option       = option
  )
}




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Draw Text using imagemagick text
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rgl_text <- function(args, state) {

  elem <- 'text'

  rgl_text_helper(
    str          = args$str,
    x            = args$x,
    y            = args$y,
    z            = state$rdata$ground + 1,
    rot          = args$rot,
    state        = state,
    manual       = FALSE,
    orientation  = 'xy'
  )

  state
}

