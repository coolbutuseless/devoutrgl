


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Add a raster representing a plain image to the output
# First convert the device raster to a PNG, save it, and then
# have RGL load it as a texture to a quad.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rgl_raster_png <- function(args, state) {

  elem <- 'raster'

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Draw the appropriate kind of raster.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  raster_data <- convert_device_raster_to_png(args, debug_items = state$rdata$debug_items)
  arr <- raster_data$array_data

  state$rdata$rgl$image_as_quad(
    png_filename = raster_data$png_filename,
    x            = args$x,
    y            = state$dd$bottom - args$y,
    z            = state$rdata$ground,
    width        = args$width,
    height       = args$height,
    alpha        = 1
  )

  state
}




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Helper function to convert device raster data to a PNG file
#'
#' @param args arguments to a 'raster' device call
#' @param flip_y whether to flip the image vertically. default: FALSE
#' @param debug_items character vector of names of things to debug
#'
#' @return list with 'png_filename' and the RGBA 'array_data' (values in [0, 1])
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
convert_device_raster_to_png <- function(args, flip_y = FALSE, debug_items = c()) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Turn the integer values from a call to 'raster' into an array
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  arr <- convert_device_raster_to_array(args$raster, args$w, args$h)


  if ('raster dims' %in% debug_items) {
    message(sprintf("Raster: x: %.2f  y: %.2f  %ix%i", args$x, args$y, args$w, args$h))
    message("Raster dims: ", deparse(dim(arr)))
    message("Raster alpha summary:")
    print(summary(as.vector(arr[,,4])))
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Sometimes image should be y-flipped just because of r raster stupidity
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (flip_y) {
    arr <- arr[rev(seq(nrow(arr))), , , drop=FALSE]
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Same the array as a PNG
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ras           <- grDevices::as.raster(arr)
  png_filename <- tempfile(fileext = ".png")
  gdtools::raster_write(ras, png_filename, width=args$w, height=args$h)

  if ('raster' %in% debug_items) {
    message("Raster PNG: ", png_filename)
  }

  list(
    png_filename = png_filename,
    array_data   = arr
  )
}




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert a device raster to an array
#'
#' @param device_raster raster passed into device. array of ints which
#'        contain ABGR pixels packed 4-bytes-to-an-int
#' @param w,h data dimensions
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
convert_device_raster_to_array <- function(device_raster, w, h) {
  values <- as.integer(int32_to_raw(device_raster))/255
  arr    <- array(values, c(4, w, h))
  arr    <- aperm(arr, c(3, 2, 1))
  arr
}






