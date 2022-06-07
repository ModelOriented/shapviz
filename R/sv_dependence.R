#' SHAP Dependence Plot
#'
#' Creates a scatterplot of SHAP values of feature \code{v} against values of \code{v}.
#' A second variable, \code{color_var}, can be selected to be used on the color scale.
#' This allows to gain an impression of possible interaction effects.
#' Set \code{color_var} to \code{"auto"} in order to select
#' the color feature with seemingly strongest interaction based on a simple heuristic.
#' For discrete \code{v}, horizontal scatter is added by default.
#'
#' @importFrom rlang .data
#' @param object An object of class "shapviz".
#' @param v Column name of feature to be plotted.
#' @param color_var Feature name to be used on the color scale to investigate interactions.
#' The default is \code{NULL} (no color feature). An experimental option is "auto",
#' which selects - by a simple heuristic - a variable with seemingly strongest interaction.
#' @param color Color to be used if \code{color_var = NULL}.
#' @param jitter_width The amount of horizontal jitter. The default (\code{NULL}) will
#' use a value of 0.1 in case \code{v} is a factor, logical, or character variable, and
#' no jitter otherwise.
#' @param ... Arguments passed to \code{geom_jitter()}.
#' @return An object of class \code{ggplot}.
#' @export
#' @seealso \code{\link{potential_interactions}}.
#' @examples
#' dtrain <- xgboost::xgb.DMatrix(data.matrix(iris[, -1]), label = iris[, 1])
#' fit <- xgboost::xgb.train(data = dtrain, nrounds = 50)
#' x <- shapviz(fit, X_pred = dtrain, X = iris[, -1])
#' sv_dependence(x, "Petal.Length")
#' sv_dependence(x, "Petal.Length", color_var = "Species")
#' sv_dependence(x, "Species", color_var = "auto")
sv_dependence <- function(object, ...) {
  UseMethod("sv_dependence")
}

#' @describeIn sv_dependence Default method.
#' @export
sv_dependence.default <- function(object, ...) {
  stop("No default method available.")
}

#' @describeIn sv_dependence SHAP dependence plot for shp object.
#' @export
sv_dependence.shapviz <- function(object, v, color_var = NULL, color = "#3b528b",
                                  jitter_width = NULL, ...) {
  S <- get_shap_values(object)
  X <- get_feature_values(object)
  stopifnot(
    v %in% colnames(S),
    is.null(color_var) || (color_var %in% c("auto", colnames(S)))
  )

  # Set jitter value
  if (is.null(jitter_width)) {
    jitter_width <- 0.1 * .is_discrete(X[[v]])
  }

  # Set color value
  if (!is.null(color_var) && color_var == "auto" && !("auto" %in% colnames(S))) {
    scores <- potential_interactions(object, v)
    color_var <- names(scores)[1L]
  }
  dat <- data.frame(S[, v], X[[v]])
  colnames(dat) <- c("shap", v)
  if (is.null(color_var)) {
    p <- ggplot(dat, aes(x = .data[[v]], y = shap)) +
      geom_jitter(color = color, width = jitter_width, ...) +
      ylab(paste("SHAP value of", v))
    return(p)
  }
  dat[[color_var]] <- X[[color_var]]
  vir <- if (.is_discrete(dat[[color_var]])) {
    vir <- scale_color_viridis_d
  } else {
    vir <- scale_color_viridis_c
  }
  ggplot(dat, aes(x = .data[[v]], y = shap, color = .data[[color_var]])) +
    geom_jitter(width = jitter_width, ...) +
    ylab(paste("SHAP value of", v)) +
    do.call(vir, getOption("shapviz.viridis_args"))
}


#' Strongest Interaction
#'
#' This function tries to detect the approximately strongest interacting feature with
#' \code{v}. It works by calculating an average squared correlation between
#' the SHAP values of \code{v} and each feature across values of \code{v}.
#' To this purpose, a numeric \code{v} with more than \code{n_bins} unique values
#' is binned into that many quantile bins.
#' Currently \code{n_bins} equals the smaller of n/20 and sqrt(n), where n is the
#' sample size.
#' The average squared correlation is weighted by the number of non-missing feature
#' values in the bin. Note that non-numeric color features are turned to numeric
#' by calling \code{data.matrix}, which does not necessarily make sense.
#'
#' @param obj An object of type "shapviz".
#' @param v Variable name.
#' @return A named vector of average squared correlations, sorted in decreasing order.
#' @export
#' @seealso \code{\link{sv_dependence}}.
potential_interactions <- function(obj, v) {
  stopifnot(is.shapviz(obj))
  S <- get_shap_values(obj)
  X <- get_feature_values(obj)
  stopifnot(v %in% colnames(X))

  if (ncol(S) < 2L) {
    return(NULL)
  }

  r_sq <- function(s, x) {
    suppressWarnings(stats::cor(s, data.matrix(x), use = "p")^2)
  }

  # Calculate average squared correlation between SHAP value and color across bins
  n_bins <- ceiling(min(sqrt(nrow(X)), nrow(X) / 20))
  v_bin <- .fast_bin(X[[v]], n_bins = n_bins)
  s_bin <- split(S[, v], v_bin)
  X_bin <- split(X[setdiff(colnames(X), v)], v_bin)
  w <- do.call(rbind, lapply(X_bin, function(z) colSums(!is.na(z))))
  cor_squared <- do.call(rbind, mapply(r_sq, s_bin, X_bin, SIMPLIFY = FALSE))
  sort(colSums(w * cor_squared, na.rm = TRUE) / colSums(w), decreasing = TRUE)
}

# Helper functions

# Same as unexported function of ggplot2
.is_discrete <- function(z) {
  is.factor(z) || is.character(z) || is.logical(z)
}

# Bins z into integer valued bins, but only if discrete
.fast_bin <- function(z, n_bins) {
  if (.is_discrete(z) || length(unique(z)) <= n_bins) {
    return(z)
  }
  q <- stats::quantile(z, seq(0, 1, length.out = n_bins + 1L), na.rm = TRUE)
  findInterval(z, unique(q), rightmost.closed = TRUE)
}

