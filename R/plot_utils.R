# Helper functions used in different places

# ncol < 2 treats the special case of constant feature values (e.g., if n = 1)
.get_color_scale <- function(viridis_args, bar = TRUE, ncol = 2L) {
  if (bar) {
    viridis_args_plus <-
      list(
        breaks = if (ncol >= 2L) 0:1 else 0.5,
        labels = if (ncol >= 2L) c("Low", "High") else "Avg"
      )
  } else {
    viridis_args_plus <- list(guide = "none")
  }
  return(do.call(ggplot2::scale_color_viridis_c, c(viridis_args, viridis_args_plus)))
}

# Rotate and align colorbar title
.rotate_colorbar_title <- function() {
  ggplot2::theme(
    legend.title = ggplot2::element_text(angle = 90, hjust = 0.5, vjust = 0.5),
    legend.title.position = "left",
    legend.margin = ggplot2::margin(r = 0.1, l = 0.1, unit = "lines")
  )
}

# Slimmer colorbars with less space
.slim_colorbar <- function(height = 1.2, width = 0.5) {
  ggplot2::theme(
    legend.box.spacing = grid::unit(0, "lines"),
    legend.key.spacing = grid::unit(0.3, "lines"),
    legend.key.width = grid::unit(width, "lines"),
    legend.key.height = grid::unit(height, "lines"),
    legend.ticks.length = ggplot2::rel(0.3)
  )
}
