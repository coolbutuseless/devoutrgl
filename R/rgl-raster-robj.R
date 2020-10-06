


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Render an 'robj' encoded in a device raster
#
# @param args the arguments to device raster
# @param state the state passed to device raster
# @param robj the R object encoded in the raster.  In {ggrgl} this is
#        currently only ever an R 'call' object, with maybe some informatio in
#        a 'meta' attribute.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rgl_raster_robj <- function(args, state, robj) {

  elem <- 'raster'
  if (is.null(robj)) {return(state)}


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # What's the size and shape of the current window we're rendering into?
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  width    <- state$dd$right  - abs(state$dd$right  - state$dd$clipRight) - state$dd$clipLeft
  height   <- state$dd$bottom - abs(state$dd$bottom - state$dd$clipTop)   - state$dd$clipBottom
  offset_x <- state$dd$clipLeft
  offset_y <- state$dd$bottom - state$dd$clipTop


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # evaluate_robj
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  evaluate_robj(args, state, robj, width, height, offset_x, offset_y)


  state
}





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 'devoutrgl' received a raster call which contained a cryogenic captured call
#
# if the 'meta' information says to scale coords, then scale the raw x,y coords
# provided into the current viewport.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
render_call <- function(args, state, cc, width, height, offset_x, offset_y) {

  # message("Rendering: ", deparse(class(cc)))

  func_name <- cc[[1]]
  arg_list  <- as.list(cc[-1])
  meta      <- attr(cc, 'meta', exact = TRUE)

  scale_coords <- meta$scale_coords %||% TRUE

  if (scale_coords) {
    if (inherits(arg_list$x, "mesh3d")) {
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # The first argument is a 'mesh3d' object.
      # Scale mesh (x,y) coords to correct location
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      arg_list$x$vb[1,] <- arg_list$x$vb[1,] * width  + offset_x
      arg_list$x$vb[2,] <- arg_list$x$vb[2,] * height + offset_y
    } else if (func_name == 'rgl_text_helper') {
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Scaling for text happens within rgl_text_helper.
      # Don't do any coordinate transformation here
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    } else {
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Scale x/y coordinates to required size
      # ToDo still need to deal with case where first argument 'x' is
      # actually a matrix with xyz columns.
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      if (!is.null(arg_list$x)) { arg_list$x <- arg_list$x * width  + offset_x }
      if (!is.null(arg_list$y)) { arg_list$y <- arg_list$y * height + offset_y }
    }
  }


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # If the 'meta' information contains a texture, then this should be a raster
  # object. We will save this raster as a PNG image, and then
  # use it as the image to supply the texture for this piece of geometry
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (!is.null(meta$texture)) {
    ras <- meta$texture

    if (!is.null(ras)) {
      if (inherits(ras, 'raster')) {
        png_filename <- tempfile(fileext = ".png")
        gdtools::raster_write(ras, png_filename, width=ncol(ras), height=nrow(ras))
        message(png_filename)
        arg_list$texture <- png_filename
      } else {
        warning("devoutrgl: meta texture has t obe a raster, not ", deparse(class(ras)))
      }
    }
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Update the call with modified arguments
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  cc <- cryogenic::modify_call(cc, update = arg_list)


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Rendering text is its own special thing.  We don't want a 'state',
  # and we don't want any material args in the default
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (func_name == 'rgl_text_helper') {
    cc <- cryogenic::modify_call(cc, update = list(state = state))
    state$rdata$rgl$add_call(cc, default_args = list())
  } else {
    state$rdata$rgl$add_call(cc)
  }
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Evaluate the robj.
#
# robj may be a standard R call. in which case evaluate it.
# robj may be a list of calls. In which case evaluate all of them.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
evaluate_robj <- function(args, state, robj, width, height, offset_x, offset_y) {
  if (is.call(robj)) {
    render_call(args, state, robj, width, height, offset_x, offset_y)
  } else if (is.list(robj)) {
    lapply(robj, function(x) {
      evaluate_robj(args, state, x, width, height, offset_x, offset_y)
    })
  } else if (is.null(robj)) {
    # do nothing. but do it quietly
  } else {
    message("devoutrgl::evaluate_robj() - Can't evaluate: ", deparse(class(robj)), "   <---------------")
  }


}
