





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Render a text string to a PNG file
#'
#' @param text text
#' @param font_size font size
#' @param font 'sans'
#' @param colour 'black'
#' @param angle 0
#'
#' @return list of meta-information about the png. width, height, png_filename
#'
#' @import magick
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
create_text_as_png <- function(text, font_size = 30, font='sans', colour='black', angle = 0) {

  stopifnot(length(text) == 1)

  N <- nchar(text)
  if (N == 0) {
    return(NULL)
  }
  width  <- ceiling(N * font_size)
  height <- ceiling(2 * font_size)


  # message("font_size: ", font_size, " ", width, "x", height)


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #  - Create a huge transparent canvas
  #  - Annotate with the text in the desired font
  #  - Trim off all the empty space
  #  - But leave a small image border to aovid some texture artefacts when the
  #    image is mapped onto a quad.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  im <- magick::image_blank(width = width, height = height)
  im <- magick::image_annotate(im, text, font = font, size = font_size, color = colour)
  im <- magick::image_trim(im)
  im <- magick::image_border(im, color='none', geometry = '2x2')

  orig_info <- magick::image_info(im)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Rotate?
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (angle != 0) {
    im <- magick::image_rotate(im, angle)
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Save the PNG to a file because RGL can only use textures which
  # exist as PNG files on a a filesystem
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  png_filename <- tempfile(fileext = ".png")
  magick::image_write(im, png_filename)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create a data structure of information and actual PNG to return
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  info <- magick::image_info(im)
  info$png_filename <- png_filename
  info$png          <- list(im)
  info$orig_height <- orig_info$height
  info$orig_width  <- orig_info$width


  info
}



if (FALSE) {
  info <- create_text_as_png(text = "crap fantastic of the third kind", angle = 45, font_size = 50)
  info$png[[1]]
}
