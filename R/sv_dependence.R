#' SHAP Dependence Plot
#'
#' Scatterplot of the SHAP values of a feature against its feature values.
#' If SHAP interaction values are available, setting `interactions = TRUE` allows
#' to focus on pure interaction effects (multiplied by two) or on pure main effects.
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
#'   Requires SHAP interaction values. If `color_var = NULL` (or it is equal to `v`),
#'   the pure main effect of `v` is visualized. Otherwise, twice the SHAP interaction
#'   values between `v` and the `color_var` are plotted.
#' @param ih_nbins,ih_color_num,ih_scale Interaction heuristic (ih) parameters used to
#'   select the color variable, see [potential_interactions()].
#'   Only used if `color_var = "auto"` and if there are no SHAP interaction values.
#' @param ... Arguments passed to [ggplot2::geom_jitter()].
#' @returns An object of class "ggplot" (or "patchwork") representing a dependence plot.
#' @examples
#' \dontrun{
#' dtrain <- xgboost::xgb.DMatrix(
#'   data.matrix(iris[, -1]), label = iris[, 1], nthread = 1
#' )
#' fit <- xgboost::xgb.train(data = dtrain, nrounds = 10, nthread = 1)
#' x <- shapviz(fit, X_pred = dtrain, X = iris)
#' sv_dependence(x, "Petal.Length")
#' sv_dependence(x, "Petal.Length", color_var = "Species")
#' sv_dependence(x, "Petal.Length", color_var = NULL)
#' sv_dependence(x, c("Species", "Petal.Length"))
#' sv_dependence(x, "Petal.Width", color_var = c("Species", "Petal.Length"))
#'
#' # SHAP interaction values/main effects
#' x2 <- shapviz(fit, X_pred = dtrain, X = iris, interactions = TRUE)
#' sv_dependence(x2, "Petal.Length", interactions = TRUE)
#' sv_dependence(
#'   x2, c("Petal.Length", "Species"), color_var = NULL, interactions = TRUE
#' )
#' }
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
sv_dependence.shapviz <- function(object, v, color_var = "auto", color = "#3b528b",
                                  viridis_args = getOption("shapviz.viridis_args"),
                                  jitter_width = NULL, interactions = FALSE,
                                  ih_nbins = NULL, ih_color_num = TRUE,
                                  ih_scale = FALSE, ...) {
  p <- length(v)
  if (p > 1L || length(color_var) > 1L) {
    if (is.null(color_var)) {
      color_var <- replicate(p, NULL)
    }
    if (is.null(jitter_width)) {
      jitter_width <- replicate(p, NULL)
    }
    plot_list <- mapply(
      FUN = sv_dependence,
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
        ...
      ),
      SIMPLIFY = FALSE
    )
    nms <- if (length(v) > 1L) v
    plot_list <- add_titles(plot_list, nms = nms)  # see sv_waterfall()
    return(patchwork::wrap_plots(plot_list))
  }

  S <- get_shap_values(object)
  X <- get_feature_values(object)
  S_inter <- get_shap_interactions(object)
  nms <- colnames(object)
  stopifnot(
    v %in% nms,
    is.null(color_var) || (color_var %in% c("auto", nms))
  )
  if (interactions && is.null(S_inter)) {
    stop("No SHAP interaction values available in 'object'.")
  }

  # Set jitter value
  if (is.null(jitter_width)) {
    jitter_width <- 0.2 * .is_discrete(X[[v]], n_unique = 7L)
  }

  # Set color value
  if (!is.null(color_var) && color_var == "auto" && !("auto" %in% nms)) {
    scores <- potential_interactions(
      object, v, nbins = ih_nbins, color_num = ih_color_num, scale = ih_scale
    )
    color_var <- names(scores)[1L]  # NULL if p = 1L
  }
  if (isTRUE(interactions)) {
    if (is.null(color_var)) {
      color_var <- v
    }
    if (color_var == v) {
      y_lab <- "SHAP main effect"
    } else {
      y_lab <- "SHAP interaction"
    }
    s <- S_inter[, v, color_var]
    if (color_var != v) {
      s <- 2 * s  # Off-diagonals need to be multiplied by 2 for symmetry reasons
    }
  } else {
    y_lab <- "SHAP value"
    s <- S[, v]
  }
  dat <- data.frame(s, X[[v]])
  colnames(dat) <- c("shap", v)
  if (is.null(color_var) || color_var == v) {
    p <- ggplot2::ggplot(dat, ggplot2::aes(x = .data[[v]], y = shap)) +
      ggplot2::geom_jitter(color = color, width = jitter_width, height = 0, ...) +
      ggplot2::ylab(y_lab)
    return(p)
  }
  dat[[color_var]] <- X[[color_var]]
  if (.is_discrete(dat[[color_var]], n_unique = 0L)) {  # only if non-numeric
    vir <- ggplot2::scale_color_viridis_d
  } else {
    vir <- ggplot2::scale_color_viridis_c
  }
  if (is.null(viridis_args)) {
    viridis_args <- list()
  }
  ggplot2::ggplot(
    dat, ggplot2::aes(x = .data[[v]], y = shap, color = .data[[color_var]])
  ) +
    ggplot2::geom_jitter(width = jitter_width, height = 0, ...) +
    ggplot2::ylab(y_lab) +
    do.call(vir, viridis_args) +
    ggplot2::theme(legend.box.spacing = grid::unit(0, "pt"))
}

#' @describeIn sv_dependence
#'   SHAP dependence plot for "mshapviz" object.
#' @export
sv_dependence.mshapviz <- function(object, v, color_var = "auto", color = "#3b528b",
                                   viridis_args = getOption("shapviz.viridis_args"),
                                   jitter_width = NULL, interactions = FALSE,
                                   ih_nbins = NULL, ih_color_num = TRUE,
                                   ih_scale = FALSE, ...) {
  stopifnot(
    length(v) == 1L,
    length(color_var) <= 1L
  )
  plot_list <- lapply(
    object,
    FUN = sv_dependence,
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
    ...
  )
  plot_list <- add_titles(plot_list, nms = names(object))  # see sv_waterfall()
  patchwork::wrap_plots(plot_list)
}

#' Interaction Strength
#'
#' Returns vector of interaction strengths between variable `v` and all other variables,
#' see Details.
#'
#' If SHAP interaction values are available, the interaction strength
#' between feature `v` and another feature `v'` is measured by twice their
#' mean absolute SHAP interaction values.
#'
#' Otherwise, we use a heuristic calculated as follows to calculate interaction strength
#' between `v` and each other "color" feature `v':
#' 1. If `v` is numeric, it is binned into `nbins` bins.
#' 2. Per bin, the SHAP values of `v` are regressed onto `v`.
#' 3. Per bin, the adjusted R-squared is calculated.
#' 4. The adjusted R-squared are averaged over bins, weighted by the bin size.
#'
#' Set `scale = TRUE` to multiply the adjusted R-squared by the within-bin variance
#' of the SHAP values. This will put higher weight to bins with larger scatter.
#'
#' Set `color_num = FALSE` to *not* turn the values of `v` into numerics.
#'
#' @param obj An object of class "shapviz".
#' @param v Variable name for which potential SHAP interactions are to be computed.
#' @param nbins Into how many quantile bins should a numeric `v` be binned?
#'   The default `NULL` equals the smaller of \eqn{n/20} and \eqn{\sqrt n} (rounded up),
#'   where \eqn{n} is the sample size. Ignored if `obj` contains SHAP interactions.
#' @param color_num Should other ("color") features `v'` be converted to numeric,
#'   even if they are factors/characters? Default is `TRUE`.
#'   Ignored if `obj` contains SHAP interactions.
#' @param scale Should adjusted R-squared be multiplied with the sample variance of
#'   within-bin SHAP values? The default is `FALSE`.
#'   Ignored if `obj` contains SHAP interactions.
#' @returns A named vector of decreasing interaction strengths.
#' @export
#' @seealso [sv_dependence()]
potential_interactions <- function(obj, v, nbins = NULL,
                                   color_num = TRUE, scale = FALSE) {
  stopifnot(is.shapviz(obj))
  S <- get_shap_values(obj)
  S_inter <- get_shap_interactions(obj)
  X <- get_feature_values(obj)
  nms <- colnames(obj)
  v_other <- setdiff(nms, v)
  stopifnot(v %in% nms)
  if (ncol(obj) <= 1L) {
    return(NULL)
  }

  # Simple case: we have SHAP interaction values
  if (!is.null(S_inter)) {
    return(sort(2 * colMeans(abs(S_inter[, v, ]))[v_other], decreasing = TRUE))
  }

  # Complicated case: calculate heuristic per color variable
  if (is.null(nbins)) {
    nbins <- ceiling(min(sqrt(nrow(X)), nrow(X) / 20))
  }
  out <- vapply(
    X[v_other],  # data.frame is a list
    FUN = heuristic,
    FUN.VALUE = 1.0,
    s = S[, v],
    bins = .fast_bin(X[[v]], nbins = nbins),
    color_num = color_num,
    scale = scale
  )
  sort(out, decreasing = TRUE, na.last = TRUE)
}

# Helper functions

# Checks if z is discrete
.is_discrete <- function(z, n_unique) {
  is.factor(z) || is.character(z) || is.logical(z) || (length(unique(z)) <= n_unique)
}

# Like as.numeric(), but can deal with factor variables
.as_numeric <- function(z) {
  if (is.numeric(z)) {
    return(z)
  }
  if (is.character(z)) {
    z <- factor(z)
  }
  as.numeric(z)
}

# Bins discrete z into integer valued bins
.fast_bin <- function(z, nbins) {
  if (.is_discrete(z, n_unique = nbins)) {
    return(z)
  }
  q <- stats::quantile(z, seq(0, 1, length.out = nbins + 1L), na.rm = TRUE)
  findInterval(z, unique(q), rightmost.closed = TRUE)
}

#' Interaction Heuristic
#'
#' Internal function used to calculate the interaction heuristics described in
#' [potential_interactions()].
#'
#' @noRd
#' @keywords internal
#'
#' @inheritParams potential_interactions
#' @param color Feature values of the "color" feature.
#' @param s SHAP values of `v`.
#' @returns A single number.
heuristic <- function(color, s, bins, color_num, scale) {
  if (isTRUE(color_num)) {
    color <- .as_numeric(color)
  }
  color <- split(color, bins)
  s <- split(s, bins)
  M <- mapply(heuristic_in_bin, color = color, s = s, MoreArgs = list(scale = scale))
  stats::weighted.mean(M[1L, ], M[2L, ], na.rm = TRUE)
}

#' Interaction Heuristic in Bin
#'
#' Internal function used to calculate the within-bin heuristic used in `heuristic()`.
#' See `heuristic()` for details.
#'
#' @noRd
#' @keywords internal
#'
#' @inheritParams heuristic
#' @returns
#'   A (1x2) matrix. The first value equals the (possibly) scaled adjusted R-squared,
#'   the second the number of used observations.
heuristic_in_bin <- function(color, s, scale = FALSE) {
  suppressWarnings(
    tryCatch(
      {
        z <- stats::lm(s ~ color)
        r <- z$residuals
        var_y <- stats::var(z$fitted.values + r)
        var_r <- sum(r^2) / z$df.residual  # var(r) would give non-adjusted R2
        stat <- 1 - var_r / var_y
        if (scale) {
          stat <- stat * var_y  # = var_y - var_r
        }
        cbind(stat = stat, n = length(r))
      },
      error = function(e) return(cbind(stat = NA, n = 0))
    )
  )
}
