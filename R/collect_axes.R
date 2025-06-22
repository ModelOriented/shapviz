# Functions that help to derive collection info for patchwork

# Same logic for axes and axis_titles
.collect_xy <- function(z) {
  if (z$x && z$y) {
    return("collect")
  }
  if (z$x) {
    return("collect_x")
  }
  if (z$y) {
    return("collect_y")
  }
  return("keep")
}

# Takes list of ggplots and determines collection info for patchwork
.collect <- function(plot_list) {
  plot_list <- lapply(plot_list, ggplot2::ggplot_build)
  ll <- lapply(plot_list, ggplot2::get_labs) # Version 3.5.2

  titles_unique <- axes_unique <- list()
  for (z in c("x", "y", "colour")) {
    temp <- lapply(plot_list, ggplot2::get_guide_data, aesthetic = z)
    axes_unique[[z]] <- .all_identical(temp, ignore_null = TRUE)

    titles_unique[[z]] <- .all_identical(
      lapply(ll, `[[`, z),
      ignore_null = (z == "colour")
    )
  }

  out <- list(
    axis_titles = .collect_xy(titles_unique),
    axes = .collect_xy(axes_unique),
    guides = if (titles_unique$colour && axes_unique$colour) "collect" else "keep"
  )

  return(out)
}

# Check if the elements in z (list or vector) are all identical, ignoring NULLs
.all_identical <- function(z, ignore_null = TRUE) {
  if (ignore_null) {
    z <- z[!vapply(z, is.null, logical(1L))]
  }
  n <- length(z)
  if (n <= 1L) {
    return(TRUE)
  }
  return(all(vapply(z[-1L], FUN = identical, z[[1L]], FUN.VALUE = logical(1L))))
}
