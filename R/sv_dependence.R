#' SHAP Dependence Plot
#'
#' Scatter plot of the SHAP values of a feature against its feature values.
#' If SHAP interaction values are available, setting \code{interactions = TRUE} allows
#' to focus on pure interaction effects (multiplied by two) or on pure main effects.
#'
#' @importFrom rlang .data
#' @param object An object of class "(m)shapviz".
#' @param v Column name of feature to be plotted.
#' @param color_var Feature name to be used on the color scale to investigate interactions.
#' The default ("auto") uses SHAP interaction values (if available) or a heuristic to
#' select the strongest interacting feature. Set to \code{NULL} to not use the color axis.
#' @param color Color to be used if \code{color_var = NULL}.
#' @param viridis_args List of viridis color scale arguments, see
#' \code{?ggplot2::scale_color_viridis_c()}. The default points to the global
#' option \code{shapviz.viridis_args}, which corresponds to
#' \code{list(begin = 0.25, end = 0.85, option = "inferno")}.
#' These values are passed to \code{ggplot2::scale_color_viridis_*()}.
#' For example, to switch to a standard viridis scale, you can either change the default
#' with \code{options(shapviz.viridis_args = NULL)} or set \code{viridis_args = NULL}.
#' Only relevant if \code{color_var} is not \code{NULL}.
#' @param jitter_width The amount of horizontal jitter. The default (\code{NULL}) will
#' use a value of 0.2 in case \code{v} is discrete, and no jitter otherwise.
#' (Numeric variables are considered discrete if they have at most 7 unique values.)
#' @param interactions Should SHAP interaction values be plotted? Default is \code{FALSE}.
#' Requires SHAP interaction values. If \code{color_var = NULL} (or it is equal to
#' \code{v}), the pure main effect of \code{v} is visualized. Otherwise, twice the SHAP
#' interaction values between \code{v} and the \code{color_var} are plotted.
#' @param ... Arguments passed to \code{geom_jitter()}.
#' @return An object of class \code{ggplot} (or "patchwork") representing a dependence plot.
#' @examples
#' dtrain <- xgboost::xgb.DMatrix(data.matrix(iris[, -1L]), label = iris[, 1L])
#' fit <- xgboost::xgb.train(data = dtrain, nrounds = 50L, nthread = 1L)
#' x <- shapviz(fit, X_pred = dtrain, X = iris)
#' sv_dependence(x, "Petal.Length")
#' sv_dependence(x, "Petal.Length", color_var = "Species")
#' sv_dependence(x, "Petal.Length", color_var = NULL)
#'
#' # SHAP interaction values
#' x2 <- shapviz(fit, X_pred = dtrain, X = iris, interactions = TRUE)
#' sv_dependence(x2, "Petal.Length", interactions = TRUE)
#' sv_dependence(x2, "Petal.Length", color_var = NULL, interactions = TRUE)
#'
#' # Show main effect of "Petal.Length" for setosa and virginica separately
#' mx <- c(
#'   setosa = x2[x2$X$Species == "setosa"],
#'   virginica = x2[x2$X$Species == "virginica"]
#' )
#' sv_dependence(mx, "Petal.Length", color_var = NULL, interactions = TRUE)
#' @export
#' @seealso \code{\link{potential_interactions}}
sv_dependence <- function(object, ...) {
  UseMethod("sv_dependence")
}

#' @describeIn sv_dependence Default method.
#' @export
sv_dependence.default <- function(object, ...) {
  stop("No default method available.")
}

#' @describeIn sv_dependence SHAP dependence plot for "shapviz" object.
#' @export
sv_dependence.shapviz <- function(object, v, color_var = "auto", color = "#3b528b",
                                  viridis_args = getOption("shapviz.viridis_args"),
                                  jitter_width = NULL, interactions = FALSE, ...) {
  S <- get_shap_values(object)
  X <- get_feature_values(object)
  S_inter <- get_shap_interactions(object)
  nms <- colnames(object)
  stopifnot(
    length(v) == 1L,
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
    scores <- potential_interactions(object, v)
    color_var <- names(scores)[1L]  # NULL if p = 1L
  }
  if (isTRUE(interactions)) {
    if (is.null(color_var)) {
      color_var <- v
    }
    if (color_var == v) {
      y_lab <- "SHAP main effect"
    } else {
      y_lab <- "SHAP interaction value"
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
    p <- ggplot(dat, aes(x = .data[[v]], y = shap)) +
      geom_jitter(color = color, width = jitter_width, height = 0, ...) +
      ylab(y_lab)
    return(p)
  }
  dat[[color_var]] <- X[[color_var]]
  if (.is_discrete(dat[[color_var]], n_unique = 0L)) {  # only if non-numeric
    vir <- scale_color_viridis_d
  } else {
    vir <- scale_color_viridis_c
  }
  if (is.null(viridis_args)) {
    viridis_args <- list(NULL)
  }
  ggplot(dat, aes(x = .data[[v]], y = shap, color = .data[[color_var]])) +
    geom_jitter(width = jitter_width, height = 0, ...) +
    ylab(y_lab) +
    do.call(vir, viridis_args)
}

#' @describeIn sv_dependence SHAP dependence plot for "mshapviz" object.
#' @export
sv_dependence.mshapviz <- function(object, v, color_var = "auto", color = "#3b528b",
                                   viridis_args = getOption("shapviz.viridis_args"),
                                   jitter_width = NULL, interactions = FALSE, ...) {
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
    ...
  )
  plot_list <- add_titles(plot_list, nms = names(object))  # see sv_waterfall()
  patchwork::wrap_plots(plot_list)
}

#' Interaction Strength
#'
#' Returns the interaction strengths between variable \code{v} and all other variables.
#' If SHAP interaction values are available, interaction strength
#' between feature \code{v} and another feature \code{v'} is measured by twice their
#' mean absolute SHAP interaction values. Otherwise, we use as heuristic the
#' squared correlation between feature values of \code{v'} and
#' SHAP values of \code{v}, averaged over (binned) values of \code{v}.
#' A numeric \code{v} with more than \code{n_bins} unique values is binned into
#' quantile bins.
#' Currently \code{n_bins} equals the smaller of n/20 and sqrt(n), where n is the
#' sample size.
#' The average squared correlation is weighted by the number of non-missing feature
#' values in the bin. Note that non-numeric color features are turned to numeric
#' by calling \code{data.matrix}, which does not necessarily make sense.
#'
#' @param obj An object of type "shapviz".
#' @param v Variable name.
#' @return A named vector of decreasing interaction strengths.
#' @export
#' @seealso \code{\link{sv_dependence}}
potential_interactions <- function(obj, v) {
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

  # Complicated case: we need to rely on correlation based heuristic
  r_sq <- function(s, x) {
    suppressWarnings(stats::cor(s, data.matrix(x), use = "p")^2)
  }
  n_bins <- ceiling(min(sqrt(nrow(X)), nrow(X) / 20))
  v_bin <- .fast_bin(X[[v]], n_bins = n_bins)
  s_bin <- split(S[, v], v_bin)
  X_bin <- split(X[v_other], v_bin)
  w <- do.call(rbind, lapply(X_bin, function(z) colSums(!is.na(z))))
  cor_squared <- do.call(rbind, mapply(r_sq, s_bin, X_bin, SIMPLIFY = FALSE))
  sort(colSums(w * cor_squared, na.rm = TRUE) / colSums(w), decreasing = TRUE)
}

# Helper functions

.is_discrete <- function(z, n_unique) {
  is.factor(z) || is.character(z) || is.logical(z) || (length(unique(z)) <= n_unique)
}

# Bins z into integer valued bins, but only if discrete
.fast_bin <- function(z, n_bins) {
  if (.is_discrete(z, n_unique = n_bins)) {
    return(z)
  }
  q <- stats::quantile(z, seq(0, 1, length.out = n_bins + 1L), na.rm = TRUE)
  findInterval(z, unique(q), rightmost.closed = TRUE)
}

