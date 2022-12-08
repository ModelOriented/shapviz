#===========================================================================
# Helper functions to produce the beeswarm plots
#===========================================================================

# Example
# ggplot(iris, aes(Species, Sepal.Width)) +
#   geom_point(position = position_bee(), aes(color = Species))

# Beeswarm position
position_bee <- function(width = NULL, adjust = NULL) {
  ggplot2::ggproto(NULL, PositionBee, width = width, adjust = adjust)
}

PositionBee <- ggplot2::ggproto(
  "PositionBee",
  ggplot2::Position,
  required_aes = c("x", "y"),

  setup_params = function(self, data) {
    list(
      width = if (!is.null(self$width)) self$width else ggplot2::resolution(data$x, zero = FALSE) * 0.4,
      adjust = if (!is.null(self$adjust)) self$adjust else 0.5
    )
  },

  compute_layer = function(self, data, params, layout) {
    x_jit <- ave2(data$y, g = data$x, FUN = shifter, adjust = params$adjust)
    ggplot2::transform_position(data, function(x) x + x_jit * params$width)
  }
)

# Shift values according to their density in the unit interval by quasi-random numbers
shifter <- function(y, ...) {
  if (length(y) == 1L) {
    return(0)
  }
  dens <- stats::density(y, ...)
  dens_y <- dens[["y"]] / max(dens[["y"]])
  shift <- halton_sequence(length(y))[rank(y, ties.method = "first")] - 0.5
  2 * shift * stats::approx(dens[["x"]], dens_y, xout = y)[["y"]]
}

# "stats::ave" for grouping variable "g" and additional arguments ...
ave2 <- function(x, g = NULL, FUN = mean, ...) {
  if (is.null(g)) {
    x[] <- FUN(x, ...)
  } else {
    split(x, g) <- lapply(split(x, g), FUN, ...)
  }
  x
}

# First n values of the Halton sequence
halton_sequence <- function(n, b = 2) {
  vapply(seq_len(n), halton, FUN.VALUE = 0.0)
}

# i-th element of Holton sequence
# https://en.wikipedia.org/wiki/Halton_sequence
halton <- function(i, b = 2) {
  f <- 1
  r <- 0
  while (i > 0) {
    f <- f / b
    r <- r + f * (i %% b)
    i <- trunc(i / b)
  }
  r
}
