#' SHAP Dependence Plot
#'
#' Scatterplot of the SHAP values of a feature against its feature values.
#' If SHAP interaction values are available, setting `interactions = TRUE` allows
#' to focus on pure interaction effects (multiplied by two) or on pure main effects.
#' By default, the feature on the color scale is selected via SHAP interactions
#' (if available) or an interaction heuristic, see [potential_interactions()].
#'
#' @importFrom rlang .data
#'
#' @param object An object of class "(m)shapviz".
#' @param v Column name of feature to be plotted. Can be a vector/list if `object` is
#'   of class "shapviz".
#' @param color_var Feature name to be used on the color scale to investigate
#'   interactions. The default ("auto") uses SHAP interaction values (if available),
#'   or a heuristic to select the strongest interacting feature. Set to `NULL` to not
#'   use the color axis. Can be a vector/list if `object` is of class "shapviz".
#' @param color Color to be used if `color_var = NULL`. Can be a vector/list if `v`
#'   is a vector.
#' @param viridis_args List of viridis color scale arguments, see
#'   `?ggplot2::scale_color_viridis_c`. The default points to the global option
#'   `shapviz.viridis_args`, which corresponds to
#'   `list(begin = 0.25, end = 0.85, option = "inferno")`.
#'   These values are passed to `ggplot2::scale_color_viridis_*()`.
#'   For example, to switch to a standard viridis scale, you can either change the
#'   default via `options(shapviz.viridis_args = list())`, or set
#'   `viridis_args = list()`. Only relevant if `color_var` is not `NULL`.
#' @param jitter_width The amount of horizontal jitter. The default (`NULL`) will
#'   use a value of 0.2 in case `v` is discrete, and no jitter otherwise.
#'   (Numeric variables are considered discrete if they have at most 7 unique values.)
#'   Can be a vector/list if `v` is a vector.
#' @param interactions Should SHAP interaction values be plotted? Default is `FALSE`.
#'   Requires SHAP interaction values. If `color_var = NULL` (or is equal to `v`),
#'   the pure main effect of `v` is visualized. Otherwise, twice the SHAP interaction
#'   values between `v` and the `color_var` are plotted.
#' @param ih_nbins,ih_color_num,ih_scale,ih_adjusted Interaction heuristic (ih)
#'   parameters used to select the color variable, see [potential_interactions()].
#'   Only used if `color_var = "auto"` and if there are no SHAP interaction values.
#' @param share_y Should y axis be shared across subplots? The default is FALSE.
#'   Has no effect if `ylim` is passed. Only for multiple plots.
#' @param ylim A vector of length 2 with manual y axis limits applied to all plots.
#' @param ... Arguments passed to [ggplot2::geom_jitter()].
#' @returns An object of class "ggplot" (or "patchwork") representing a dependence plot.
#' @examples
#' dtrain <- xgboost::xgb.DMatrix(
#'   data.matrix(iris[, -1]),
#'   label = iris[, 1], nthread = 1
#' )
#' fit <- xgboost::xgb.train(data = dtrain, nrounds = 10, nthread = 1)
#' x <- shapviz(fit, X_pred = dtrain, X = iris)
#' sv_dependence(x, "Petal.Length")
#' sv_dependence(x, "Petal.Length", color_var = "Species")
#' sv_dependence(x, "Petal.Length", color_var = NULL)
#' sv_dependence(x, c("Species", "Petal.Length"), share_y = TRUE)
#' sv_dependence(x, "Petal.Width", color_var = c("Species", "Petal.Length"))
#'
#' # SHAP interaction values/main effects
#' x2 <- shapviz(fit, X_pred = dtrain, X = iris, interactions = TRUE)
#' sv_dependence(x2, "Petal.Length", interactions = TRUE)
#' sv_dependence(
#'   x2, c("Petal.Length", "Species"),
#'   color_var = NULL, interactions = TRUE
#' )
#' sv_dependence(
#'   x2, "Petal.Length",
#'   color_var = colnames(iris[-1]), interactions = TRUE,
#'   share_y = TRUE
#' )
#' @export
#' @seealso [potential_interactions()]
sv_dependence <- function(object, ...) {
  UseMethod("sv_dependence")
}

#' @describeIn sv_dependence
#'   Default method.
#' @export
sv_dependence.default <- function(object, ...) {
  stop("No default method available.")
}

#' @describeIn sv_dependence
#'   SHAP dependence plot for "shapviz" object.
#' @export
sv_dependence.shapviz <- function(
    object,
    v,
    color_var = "auto",
    color = "#3b528b",
    viridis_args = getOption("shapviz.viridis_args"),
    jitter_width = NULL,
    interactions = FALSE,
    ih_nbins = NULL,
    ih_color_num = TRUE,
    ih_scale = FALSE,
    ih_adjusted = FALSE,
    share_y = FALSE,
    ylim = NULL,
    ...) {
  nv <- length(v)
  stopifnot(
    nv >= 1L,
    is.null(ylim) || (length(ylim) == 2L && is.numeric(ylim))
  )
  if (nv == 1L && length(color_var) <= 1L) {
    p <- .one_dependence_plot(
      object = object,
      v = v,
      color_var = color_var,
      color = color,
      viridis_args = viridis_args,
      jitter_width = jitter_width,
      interactions = interactions,
      ih_nbins = ih_nbins,
      ih_color_num = ih_color_num,
      ih_scale = ih_scale,
      ih_adjusted = ih_adjusted,
      ylim = ylim,
      ...
    )$p
    return(p)
  }

  if (is.null(color_var)) {
    color_var <- replicate(nv, NULL)
  }
  if (is.null(jitter_width)) {
    jitter_width <- replicate(nv, NULL)
  }
  out_list <- mapply(
    FUN = .one_dependence_plot,
    v = v,
    color_var = color_var,
    color = color,
    jitter_width = jitter_width,
    MoreArgs = list(
      object = object,
      viridis_args = viridis_args,
      interactions = interactions,
      ih_nbins = ih_nbins,
      ih_color_num = ih_color_num,
      ih_scale = ih_scale,
      ih_adjusted = ih_adjusted,
      ylim = ylim,
      ...
    ),
    SIMPLIFY = FALSE
  )

  plot_list <- lapply(out_list, `[[`, "p")

  # Add titles if v varies
  plot_list <- add_titles(plot_list, nms = if (nv > 1L) v) # see sv_waterfall()

  # If share_y is TRUE, apply the limits
  if (isTRUE(share_y) && is.null(ylim)) {
    ylim <- .share_y(lapply(out_list, `[[`, "shap_range"))
    plot_list <- .add_ylim(plot_list, ylim = ylim)
  }

  # Collect axis titles, axes and guides
  collect <- .collect(out_list, v = v, ylim = ylim)
  p <- patchwork::wrap_plots(
    plot_list,
    axis_titles = collect$axis_titles, axes = collect$axes, guides = collect$guides
  )

  return(p)
}


#' @describeIn sv_dependence
#'   SHAP dependence plot for "mshapviz" object.
#' @export
sv_dependence.mshapviz <- function(
    object,
    v,
    color_var = "auto",
    color = "#3b528b",
    viridis_args = getOption("shapviz.viridis_args"),
    jitter_width = NULL,
    interactions = FALSE,
    ih_nbins = NULL,
    ih_color_num = TRUE,
    ih_scale = FALSE,
    ih_adjusted = FALSE,
    share_y = FALSE,
    ylim = NULL,
    ...) {
  stopifnot(
    length(v) == 1L,
    length(color_var) <= 1L,
    is.null(ylim) || (length(ylim) == 2L && is.numeric(ylim))
  )

  out_list <- lapply(
    object,
    FUN = .one_dependence_plot,
    # Argument list (simplify via match.call() or some rlang magic?)
    v = v,
    color_var = color_var,
    color = color,
    viridis_args = viridis_args,
    jitter_width = jitter_width,
    interactions = interactions,
    ih_nbins = ih_nbins,
    ih_color_num = ih_color_num,
    ih_scale = ih_scale,
    ih_adjusted = ih_adjusted,
    ylim = ylim,
    ...
  )

  plot_list <- lapply(out_list, `[[`, "p")
  plot_list <- add_titles(plot_list, nms = names(object)) # see sv_waterfall()
  if (isTRUE(share_y) && is.null(ylim)) {
    ylim <- .share_y(lapply(out_list, `[[`, "shap_range"))
    plot_list <- .add_ylim(plot_list, ylim = ylim)
  }

  # Collect axis titles, axes and guides
  collect <- .collect(out_list, v = v, ylim = ylim)
  p <- patchwork::wrap_plots(
    plot_list,
    axis_titles = collect$axis_titles, axes = collect$axes, guides = collect$guides
  )

  return(p)
}

# Helper functions

# Checks if z is discrete. The default equals ggplot2:::is.discrete()
.is_discrete <- function(z, n_unique = 0L) {
  is.factor(z) || is.character(z) || is.logical(z) || (length(unique(z)) <= n_unique)
}

# Calculates a "range" for discrete or continuous variables
.general_range <- function(x) {
  if (.is_discrete(x)) {
    return(sort(unique(x)))
  }
  return(range(x, na.rm = TRUE))
}

# Check if the elements in z (list or vector) are all identical, ignoring NULLs
.all_identical <- function(z) {
  z <- z[!vapply(z, is.null, logical(1L))]
  n <- length(z)
  if (n <= 1L) {
    return(TRUE)
  }
  return(all(vapply(z[-1L], FUN = identical, z[[1L]], FUN.VALUE = logical(1L))))
}

# Based on list of SHAP value ranges, calculate the shared y-axis limits
# Note that this could be calculated directly from the shapviz object,
# except when interactions = TRUE.
.share_y <- function(range_list) {
  r <- range(unlist(range_list))
  return(grDevices::extendrange(r, f = 0.02)) # ggplot uses 0.05 by default
}

# Apply non-NULL ylim to list of ggplots
.add_ylim <- function(plot_list, ylim) {
  return(lapply(plot_list, function(p) p + ggplot2::ylim(ylim)))
}

# Derives info for the axes and guides of the combined plot
# z equals out_list in the calling function
.collect <- function(z, v, ylim) {
  axis_titles <- axes <- guides <- "keep"

  # Determine axis_titles collection info
  y_labs <- vapply(z, `[[`, "y_lab", FUN.VALUE = character(1L))
  nlab <- length(unique(y_labs))
  nvu <- length(unique(v))

  if (nvu == 1L && nlab == 1L) {
    axis_titles <- "collect"
  } else if (nvu == 1L) {
    axis_titles <- "collect_x"
  } else if (nlab == 1L) {
    axis_titles <- "collect_y"
  }

  # Determine axes collection info
  same_v_range <- .all_identical(lapply(z, `[[`, "v_range"))
  if (!is.null(ylim)) {
    same_shap_range <- TRUE
  } else {
    same_shap_range <- .all_identical(lapply(z, `[[`, "shap_range"))
  }

  if (same_shap_range && same_v_range) {
    axes <- "collect"
  } else if (same_shap_range) {
    axes <- "collect_y"
  } else if (same_v_range) {
    axes <- "collect_x"
  }

  # Determine guides collection info
  has_keys <- vapply(z, `[[`, "color_key", FUN.VALUE = logical(1L))
  color_vars <- lapply(z, `[[`, "color_var")[has_keys]
  same_color_range <- .all_identical(lapply(z, `[[`, "color_range"))

  if (length(unique(color_vars)) <= 1L && same_color_range) {
    guides <- "collect"
  }

  return(list(axis_titles = axis_titles, axes = axes, guides = guides))
}

# Returns a list with the following elements:
# - p: the ggplot object
# - color_var: the feature used for coloring (or NULL)
# - color_key: whether a color key is present (TRUE/FALSE)
# - y_lab: the y-axis label
# - shap_range: The range of the visualized SHAP values
# - color_range: the range (or unique values) or NULL of the color variable
# - v_range: The range (or unique values) of the v variable
.one_dependence_plot <- function(
    object,
    v,
    color_var,
    color,
    viridis_args,
    jitter_width,
    interactions,
    ih_nbins,
    ih_color_num,
    ih_scale,
    ih_adjusted,
    # share_y = FALSE, not(!) to be passed
    ylim = NULL,
    ...) {
  S <- get_shap_values(object)
  X <- get_feature_values(object)
  S_inter <- get_shap_interactions(object)
  nms <- colnames(object)
  stopifnot(
    v %in% nms,
    is.null(color_var) || (color_var %in% c("auto", nms))
  )
  if (isTRUE(interactions) && is.null(S_inter)) {
    stop("No SHAP interaction values available in 'object'.")
  }

  # Set jitter value
  if (is.null(jitter_width)) {
    jitter_width <- 0.2 * .is_discrete(X[[v]], n_unique = 7L)
  }

  # Set color value if "auto"
  if (!is.null(color_var) && color_var == "auto" && !("auto" %in% nms)) {
    scores <- potential_interactions(
      object,
      v,
      nbins = ih_nbins,
      color_num = ih_color_num,
      scale = ih_scale,
      adjusted = ih_adjusted
    )
    # 'scores' can be NULL, or a sorted vector like c(0.1, 0, -0.01, NA)
    # Thus, let's take the first positive one (or NULL)
    scores <- scores[!is.na(scores) & scores > 0] # NULL stays NULL
    color_var <- if (length(scores) >= 1L) names(scores)[1L]
  }
  if (isTRUE(interactions)) {
    if (!is.null(color_var) && color_var == v) {
      color_var <- NULL
    }
    if (is.null(color_var)) { # we want to show the main effect
      y_lab <- "SHAP main effect"
      s <- S_inter[, v, v]
    } else {
      y_lab <- "SHAP interaction"
      s <- S_inter[, v, color_var] + S_inter[, color_var, v] # symmetry
    }
  } else {
    y_lab <- "SHAP value"
    s <- S[, v]
  }

  # Create data.frame with SHAP values and features values of v (no color yet)
  dat <- data.frame(s, X[[v]])
  colnames(dat) <- c("shap", v)

  color_key <- !is.null(color_var)

  # No color axis if color_var is NULL
  if (!color_key) {
    color_range <- NULL
    p <- ggplot2::ggplot(dat, ggplot2::aes(x = .data[[v]], y = shap)) +
      ggplot2::geom_jitter(color = color, width = jitter_width, height = 0, ...) +
      ggplot2::ylab(y_lab)
  } else {
    dat[[color_var]] <- X[[color_var]]
    color_range <- .general_range(dat[[color_var]])
    if (.is_discrete(dat[[color_var]])) {
      vir <- ggplot2::scale_color_viridis_d
    } else {
      vir <- ggplot2::scale_color_viridis_c
    }
    if (is.null(viridis_args)) {
      viridis_args <- list()
    }
    p <- ggplot2::ggplot(
      dat, ggplot2::aes(x = .data[[v]], y = shap, color = .data[[color_var]])
    ) +
      ggplot2::geom_jitter(width = jitter_width, height = 0, ...) +
      ggplot2::ylab(y_lab) +
      do.call(vir, viridis_args) +
      ggplot2::theme(legend.box.spacing = grid::unit(0, "pt"))
  }
  if (!is.null(ylim)) {
    p <- p + ggplot2::ylim(ylim)
  }
  out <- list(
    p = p,
    color_var = color_var,
    color_key = color_key,
    y_lab = y_lab,
    shap_range = range(s),
    color_range = color_range,
    v_range = .general_range(X[[v]])
  )

  return(out)
}
