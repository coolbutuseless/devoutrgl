


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Standard default material
#'
#' @import triangular
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
standard_material <- list(
  color           = c("blue"), # Was 'white'
  alpha           = c(1.0),
  lit             = FALSE,     # was 'TRUE'
  ambient         = "black",
  specular        = "black",   # was 'white'
  emission        = "black",
  shininess       = 50.0,
  smooth          = TRUE,      # maybe set FALSE
  texture         = NULL,
  textype         = "rgb",
  texmipmap       = FALSE,
  texminfilter    = "linear",
  texmagfilter    = "linear",
  texenvmap       = FALSE,
  front           = "fill",
  back            = "fill",
  size            = 3.0,
  lwd             = 1.0,
  fog             = TRUE,
  point_antialias = TRUE,
  line_antialias  = TRUE,
  depth_mask      = TRUE,
  depth_test      = "less",
  polygon_offset  = c(0.0, 0.0)#,
  # lty             = 'dashed'
)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Class for creating an opengl scene with rgl.
#'
#' @import R6
#' @import rgl
#' @import decido
#' @import cryogenic
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
RGLDoc <- R6::R6Class(
  "RGLDoc",

  public = list(

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' @field default_material default material
    #' @field width,height dimensions of rgl window
    #' @field polygon_offset,polygon_offset_delta current polygon offset, and
    #'        the delta to adjust the offset by every time an element is drawn
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    default_material     = NULL,
    width                = NULL,
    height               = NULL,
    polygon_offset       = NULL,
    polygon_offset_delta = NULL,

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' @description initialise
    #'
    #' @param fov field of view. default: 0 isometric view. Other good values are 30 and 45.
    #' @param width,height dimensions of viewing window. default 800x600
    #' @param zscale scale factor applied to z axis. default 1.
    #' @param view One of 'front', 'flat'. This will be ignored if \code{view3d_args}
    #'        is set.
    #' @param view_angle rotation angle of view in degrees. Only used if \code{view} is
    #'        'front' or 'flat'.  Default: 0
    #' @param view_flat_angle angle to tip view when view is 'flat'.
    #' @param view3d_args full viewpoint specification as a list of arguments to
    #'        \code{rgl::view3d()}.  Default: NULL
    #' @param zoom zoom level. default 0.800. Larger values make the plot appear
    #'        further from the screen.
    #' @param light list of arguments to \code{rgl.light()} to define the
    #'        light for this scene.
    #' @param background list of arguments to \code{rgl.bg()} to define the
    #'        background for these scene
    #' @param default_material default material for objects
    #' @param polygon_offset_delta incremental change in \code{polygon_offset} after
    #'        every element is rendered.  Adjust this value to try and
    #'        minimise issues with z-fighting of overlapping elements.
    #' @param ... other arguments passed to \code{rgl::open3d()}
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    initialize = function(fov                  = 30,
                          width                = 800,
                          height               = 600,
                          zscale               = 1,
                          view                 = c('front', 'flat'),
                          view_angle           = 0,
                          view_flat_angle      = -45,
                          zoom                 = 0.800,
                          view3d_args          = NULL,
                          light                = list(theta=0, phi=0),
                          background           = list(color = 'white'),
                          default_material     = standard_material,
                          polygon_offset_delta = 0.03,
                          ...) {

      view <- match.arg(view)

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Open the vindow
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      self$width  <- width
      self$height <- height

      open3d(FOV = fov, windowRect = c(0, 0, width, height), ...)
      aspect3d(x=1, y=1, z=zscale)

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Set the light
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      rgl.clear('lights')
      do.call(rgl.light, light)

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Set the background
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      do.call(rgl.bg, background)

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Set the view
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      if (!is.null(view3d_args)) {
        if (!'fov'  %in% names(view3d_args)) { view3d_args$fov  <- fov  }
        if (!'zoom' %in% names(view3d_args)) { view3d_args$zoom <- zoom }
        do.call(view3d, view3d_args)
      } else if (view == 'front') {
        rgl::view3d(view_angle, 0, fov = fov, zoom = zoom)
      } else if (view == 'flat') {
        rgl::view3d(0, 0, fov = fov, zoom = zoom,
               userMatrix =
                 rgl::rotationMatrix(view_flat_angle * pi/180, 1, 0, 0) %*%
                 rgl::rotationMatrix(view_angle      * pi/180, 0, 0, 1))
      }

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Set the material
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      stopifnot(is.list(default_material))
      self$default_material <- default_material

      self$polygon_offset       <- 0
      self$polygon_offset_delta <- polygon_offset_delta

      invisible(self)
    },


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' Add a circle
    #'
    #' @param x,y,z coords
    #' @param r radius
    #' @param colour,color colour
    #' @param alpha alpha
    #' @param circle_segments number of segments to render for a circle
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    circle = function(x, y, z, r, colour, color=colour, alpha = 1,
                      circle_segments = 30) {

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Draw a circle as a polygon with number of sides = 'circle_segments'
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      angle <- seq(0, 360, length.out = circle_segments) * pi/180
      xs <- x + r * cos(angle)
      ys <- y + r * sin(angle)

      self$polygon(
        x        = xs,
        y        = ys,
        z        = z,
        colour   = colour,
        fill     = colour,
        alpha    = alpha,
        lwd      = 0
      )

      invisible(self)
    },


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' Add an arbitrary call to this document
    #'
    #' @param cc standard R call object
    #' @param default_args default args
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    add_call = function(cc, default_args = standard_material) {
      cryogenic::evaluate_call(cc, default = default_args)

      invisible(self)
    },


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' Add a sphere
    #'
    #' @param x,y,z coords
    #' @param r radius
    #' @param colour,color colour
    #' @param alpha alpha
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    sphere = function(x, y, z, r, colour, color=colour, alpha = 1) {

      args <- list(
        x        = x,
        y        = y,
        z        = z,
        color    = colour,
        fill     = colour,
        alpha    = alpha,
        lwd      = 0,
        r        = r
      )
      args <- modifyList(self$default_material, args)
      do.call(rgl::spheres3d, args)

      invisible(self)
    },


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' Add a point
    #'
    #' @param x,y,z coords
    #' @param r radius
    #' @param colour,color colour
    #' @param alpha alpha
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    point = function(x, y, z, r, colour, color=colour, alpha = 1) {

      self$polygon_offset <- self$polygon_offset - self$polygon_offset_delta

      args <- list(
        x        = x,
        y        = y,
        z        = z,
        color    = colour,
        fill     = colour,
        alpha    = alpha,
        lwd      = 0,
        polygon_offset = self$polygon_offset
      )
      args <- modifyList(self$default_material, args)
      do.call(rgl::points3d, args)


      invisible(self)
    },


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' Add a rect
    #'
    #' @param x1,y1,x2,y2,z coords
    #' @param colour,color,fill colour
    #' @param lwd line width i.e. size
    #' @param alpha alpha
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    rect = function(x1, y1, x2, y2, z, colour, fill, lwd=3, color=colour,
                    alpha = 1) {

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Construct vector of x,y coords for 4 vertices
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      x <- c(x1, x2, x2, x1)
      y <- c(y1, y1, y2, y2)

      self$polygon_offset <- self$polygon_offset - self$polygon_offset_delta

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Draw a quad
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      args <- list(
        x        = x,
        y        = y,
        z        = z,
        color    = fill,
        alpha    = alpha,
        polygon_offset = self$polygon_offset
      )
      args <- modifyList(self$default_material, args)
      do.call(rgl::quads3d, args)


      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Draw the border separately
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      if (lwd > 0 && hex2alpha(colour) != 0) {

        x <- c(x1, x2, x2, x1, x1)
        y <- c(y1, y1, y2, y2, y1)
        z <- z + 0.01

        args <- list(
          x        = x,
          y        = y,
          z        = z,
          color    = colour,
          alpha    = hex2alpha(colour),
          lwd      = lwd,
          polygon_offset = self$polygon_offset
        )
        args <- modifyList(self$default_material, args)
        do.call(rgl::lines3d, args)

      }


      invisible(self)
    },


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' Add a line
    #'
    #' @param x1,y1,x2,y2,z coords
    #' @param colour,color colour
    #' @param lwd line width i.e. size
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    line = function(x1, y1, x2, y2, z, colour, lwd=3, color=colour) {
      alpha <- hex2alpha(colour)

      self$polygon_offset <- self$polygon_offset - self$polygon_offset_delta

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Draw the line
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      args <- list(
        x        = c(x1, x2),
        y        = c(y1, y2),
        z        = z,
        color    = colour,
        lwd      = lwd,
        alpha    = alpha,
        polygon_offset = self$polygon_offset
      )
      args <- modifyList(self$default_material, args)
      do.call(rgl::segments3d, args)


      invisible(self)
    },


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' Add a polyline
    #'
    #' @param xs,ys,z coords
    #' @param colour,color colour
    #' @param lwd line width i.e. size
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    polyline = function(xs, ys, z, colour, lwd=3, color=colour) {

      alpha <- hex2alpha(colour)

      self$polygon_offset <- self$polygon_offset - self$polygon_offset_delta

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Draw the polyline
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      args <- list(
        x        = xs,
        y        = ys,
        z        = z,
        color    = colour,
        lwd      = lwd,
        alpha    = alpha,
        polygon_offset = self$polygon_offset
      )
      args <- modifyList(self$default_material, args)
      do.call(rgl::lines3d, args)


      invisible(self)
    },


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' Add a polygon
    #'
    #' @param xs,ys,z coords
    #' @param colour,fill colour
    #' @param alpha alpha
    #' @param lwd line width i.e. size
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    polygon_old = function(xs, ys, z=0, colour, fill, alpha = 1, lwd=3) {

      self$polygon_offset <- self$polygon_offset - self$polygon_offset_delta

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Triangulate the polygon using 'decido::earcut'.  This is about 1000x
      # faster than relying on 'rgl::triangulate()' or 'rgl::polygon3d'
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      p   <- cbind(xs, ys, z)
      idx <- decido::earcut(p[, 1:2])
      p   <- p[idx,]

      args <- list(
        x        = p,
        color    = fill,
        fill     = TRUE,
        alpha    = alpha,
        lwd      = lwd,
        polygon_offset = self$polygon_offset
      )
      args <- modifyList(self$default_material, args)
      do.call(rgl::triangles3d, args)


      args <- list(
        x        = xs,
        y        = ys,
        z        = z,
        color    = colour,
        lwd      = lwd,
        alpha    = alpha,
        polygon_offset = self$polygon_offset
      )
      args <- modifyList(self$default_material, args)
      do.call(rgl::lines3d, args)


      invisible(self)
    },


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' Add a polygon
    #'
    #' @param xs,ys,z coords
    #' @param colour,fill colour
    #' @param alpha alpha
    #' @param lwd line width i.e. size
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    polygon = function(xs, ys, z=0, colour, fill, alpha = 1, lwd=3) {

      self$polygon_offset <- self$polygon_offset - self$polygon_offset_delta

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Triangulate the complex path polygon using triangular
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      df <- data.frame(x = xs, y = ys, group = 1L, subgroup = 1L)
      triangles_df <- triangular::decompose(df)


      args <- list(
        x        = triangles_df$x,
        y        = triangles_df$y,
        z        = z,
        color    = fill,
        fill     = TRUE,
        alpha    = alpha,
        lwd      = lwd,
        polygon_offset = self$polygon_offset
      )
      args <- modifyList(self$default_material, args)
      do.call(rgl::triangles3d, args)


      args <- list(
        x        = xs,
        y        = ys,
        z        = z,
        color    = colour,
        lwd      = lwd,
        alpha    = alpha,
        polygon_offset = self$polygon_offset
      )
      args <- modifyList(self$default_material, args)
      do.call(rgl::lines3d, args)


      invisible(self)
    },



    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' Add a path
    #'
    #' @param df polygon specification
    #' @param colour,fill colour
    #' @param alpha alpha
    #' @param lwd line width i.e. size
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    path = function(df, colour, fill, alpha = 1, lwd=3) {

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Sanity check
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      stopifnot(is.data.frame(df))
      stopifnot('x'        %in% colnames(df))
      stopifnot('y'        %in% colnames(df))
      stopifnot('z'        %in% colnames(df))
      stopifnot('group'    %in% colnames(df))
      stopifnot('subgroup' %in% colnames(df))

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # offset the polygons with every draw call
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      self$polygon_offset <- self$polygon_offset - self$polygon_offset_delta

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Triangulate the complex path polygon using triangular
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      triangles_df <- triangular::decompose(df)
      triangles_df$z <- df$z[1]

      args <- list(
        x        = triangles_df$x,
        y        = triangles_df$y,
        z        = triangles_df$z,
        color    = fill,
        fill     = TRUE,
        alpha    = alpha,
        lwd      = lwd,
        polygon_offset = self$polygon_offset
      )
      args <- modifyList(self$default_material, args)
      do.call(rgl::triangles3d, args)


      triangles_list <- split(triangles_df, list(triangles_df$idx))

      if (!is.na(colour)) {
        for (triangle_df in triangles_list) {
          args <- list(
            x        = c(triangle_df$x, triangle_df$x[1]),
            y        = c(triangle_df$y, triangle_df$y[1]),
            z        = c(triangle_df$z, triangle_df$z[1]),
            color    = colour,
            lwd      = lwd,
            alpha    = alpha,
            polygon_offset = self$polygon_offset
          )
          args <- modifyList(self$default_material, args)
          do.call(rgl::lines3d, args)
        }
      }

      invisible(self)
    },


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' Add text
    #'
    #' @param text text to write
    #' @param x,y,z coords
    #' @param colour,color colour
    #' @param cex character expansion values
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    text = function(text, x, y, z=0, colour, cex = 1) {

      self$polygon_offset <- self$polygon_offset - self$polygon_offset_delta

      args <- list(
        x        = x,
        y        = y,
        z        = z,
        texts    = text,
        cex      = cex,
        adj      = c(0, 0),
        polygon_offset = self$polygon_offset
      )
      args <- modifyList(self$default_material, args)
      do.call(rgl::text3d, args)

      invisible(self)
    },


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' Render an image onto a quad
    #'
    #' @param png_filename filename of PNG to add to image
    #' @param z height
    #' @param x,y display position
    #' @param width,height display width and height
    #' @param alpha overall alpha
    #' @param angle rotation angle of quad around the z axis
    #' @param option generic option to control rendering
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    image_as_quad = function(png_filename, x, y, z, width, height, alpha = 1,
                             angle = 0, option = 0) {

      # message("image as quad alpha: ", alpha, "  option: ", option)

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Height sometimes given as negative. Dealing with it using a sledgehammer,
      # but this may cause other issues later on. Beware!
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      height <- abs(height)

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Quad coordinates in screen space
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      if (option == 1) {
        # paralell to xz plane
        x0  <- c(x, x+width,  x+width,        x)
        y0  <- c(y,       y,        y,        y)
        z0  <- c(z,       z, z+height, z+height)
      } else {
        # parallel to xy plane
        x0  <- c(x, x+width, x+width, x)
        y0  <- c(y, y, y+height, y+height)
        z0  <- z
      }

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Rotate quad around the z-axis
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      if (angle != 0) {
        theta <- angle * pi/180

        x0 <- x0 - x
        y0 <- y0 - y
        x1 <- x0 *  cos(theta) + y0 * sin(theta)
        y1 <- x0 * -sin(theta) + y0 * cos(theta)
        x1 <- x1 + x
        y1 <- y1 + y
      } else {
        x1 <- x0
        y1 <- y0
      }

      xyz <- cbind(x1, y1, z0)

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Texture coordinates in Quad space
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      texcoords <- cbind(
        c(0, 1, 1, 0),
        c(0, 0, 1, 1),
        0
      )

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Draw a textured quad
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      self$polygon_offset <- self$polygon_offset - self$polygon_offset_delta

      args <- list(
        x         = xyz,
        texture   = png_filename,
        texcoords = texcoords,
        textype   = 'rgba',
        color     = "white",
        alpha     = alpha,
        polygon_offset = self$polygon_offset
      )
      args <- modifyList(self$default_material, args)
      do.call(rgl::quads3d, args)

      invisible(self)
    },


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' Save as PNG or WebGL based upon suffix
    #'
    #' @param filename filename
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    save = function(filename) {
      if (endsWith(tolower(filename), ".png")) {
        rgl::snapshot3d(filename)
      } else if (endsWith(tolower(filename), ".html")) {
        rgl::writeWebGL(filename = filename)
      } else {
        warning("Only PNG/HTML output supported. Don't know how to handle: ", filename)
      }

      invisible(self)
    }

  )
)



