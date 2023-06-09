#' 2D SHAP Dependence Plot
#'
#' @description
#' Scatterplot of two features, showing the sum of their SHAP values on the color scale.
#' This allows to visualize the combined effect of two features, including interactions.
#' A typical application are models with latitude and longitude as features (plus
#' maybe other regional features that can be passed via `add_vars`).
#'
#' If SHAP interaction values are available, setting `interactions = TRUE` allows
#' to focus on pure interaction effects (multiplied by two).
#'
#' @importFrom rlang .data
#'
#' @inheritParams sv_dependence
#' @inheritParams sv_importance
#' @param x Feature name for x axis. Can be a vector/list if `object` is
#'   of class "shapviz".
#' @param y Feature name for y axis. Can be a vector/list if `object` is
#'   of class "shapviz".
#' @param jitter_height Similar to `jitter_width` for vertical scatter.
#' @param interactions Should SHAP interaction values be plotted? The default (`FALSE`)
#'   will show the rowwise sum of the SHAP values of `x` and `y`. If `TRUE`, will
#'   use twice the SHAP interaction value (requires SHAP interactions).
#' @param add_vars Optional vector of feature names, whose SHAP values should be added
#'   to the sum of the SHAP values of `x` and `y` (only if `interactions = FALSE`).
#'   A use case would be a model with geographic x and y coordinates, along with some
#'   additional locational features like distance to the next train station.
#' @param ... Arguments passed to [ggplot2::geom_jitter()].
#' @returns An object of class "ggplot" (or "patchwork") representing a dependence plot.
#' @examples
#' \dontrun{
#' dtrain <- xgboost::xgb.DMatrix(data.matrix(iris[, -1]), label = iris[, 1])
#' fit <- xgboost::xgb.train(data = dtrain, nrounds = 10, nthread = 1)
#' sv <- shapviz(fit, X_pred = dtrain, X = iris)
#' sv_dependence2D(sv, x = "Petal.Length", y = "Species")
#' sv_dependence2D(sv, x = c("Petal.Length", "Species"), y = "Sepal.Width")
#'
#' # SHAP interaction values
#' sv2 <- shapviz(fit, X_pred = dtrain, X = iris, interactions = TRUE)
#' sv_dependence2D(sv2, x = "Petal.Length", y = "Species", interactions = TRUE)
#' sv_dependence2D(
#'   sv2, x = "Petal.Length", y = c("Species", "Petal.Width"), interactions = TRUE
#' )
#'
#' # mshapviz object
#' mx <- split(sv, f = iris$Species)
#' sv_dependence2D(mx, x = "Petal.Length", y = "Sepal.Width")
#' }
#' @export
#' @seealso [sv_dependence()]
sv_dependence2D <- function(object, ...) {
  UseMethod("sv_dependence2D")
}

#' @describeIn sv_dependence2D
#'   Default method.
#' @export
sv_dependence2D.default <- function(object, ...) {
  stop("No default method available.")
}

#' @describeIn sv_dependence2D
#'   2D SHAP dependence plot for "shapviz" object.
#' @export
sv_dependence2D.shapviz <- function(object, x, y,
                                    viridis_args = getOption("shapviz.viridis_args"),
                                    jitter_width = NULL, jitter_height = NULL,
                                    interactions = FALSE, add_vars = NULL, ...) {
  p <- max(length(x), length(y))
  if (p > 1L) {
    if (is.null(jitter_width)) {
      jitter_width <- replicate(p, NULL)
    }
    if (is.null(jitter_height)) {
      jitter_height <- replicate(p, NULL)
    }
    plot_list <- mapply(
      FUN = sv_dependence2D,
      x = x,
      y = y,
      jitter_width = jitter_width,
      jitter_height = jitter_height,
      MoreArgs = list(
        object = object,
        viridis_args = viridis_args,
        interactions = interactions,
        ...
      ),
      SIMPLIFY = FALSE
    )
    return(patchwork::wrap_plots(plot_list))
  }

  S <- get_shap_values(object)
  X <- get_feature_values(object)
  S_inter <- get_shap_interactions(object)
  nms <- colnames(object)
  stopifnot(
    x %in% nms,
    y %in% nms,
    is.null(add_vars) || all(add_vars %in% nms)
  )
  if (interactions && is.null(S_inter)) {
    stop("No SHAP interaction values available in 'object'.")
  }

  # Set jitter value
  if (is.null(jitter_width)) {
    jitter_width <- 0.2 * .is_discrete(X[[x]], n_unique = 7L)
  }
  if (is.null(jitter_height)) {
    jitter_height <- 0.2 * .is_discrete(X[[y]], n_unique = 7L)
  }

  # Color variable
  if (!interactions) {
    s <- rowSums(S[, c(x, y, add_vars)])
  } else {
    s <- S_inter[, x, y]
    if (x != y) {
      s <- 2 * s  # Off-diagonals need to be multiplied by 2 for symmetry reasons
    }
  }
  dat <- data.frame(SHAP = s, X[, c(x, y)], check.names = FALSE)
  vir <- ggplot2::scale_color_viridis_c
  if (is.null(viridis_args)) {
    viridis_args <- list()
  }
  ggplot2::ggplot(dat, ggplot2::aes(x = .data[[x]], y = .data[[y]], color = SHAP)) +
    ggplot2::geom_jitter(width = jitter_width, height = jitter_height, ...) +
    do.call(vir, viridis_args) +
    ggplot2::theme(legend.box.spacing = grid::unit(0, "pt"))
}

#' @describeIn sv_dependence2D
#'   2D SHAP dependence plot for "mshapviz" object.
#' @export
sv_dependence2D.mshapviz <- function(object, x, y,
                                     viridis_args = getOption("shapviz.viridis_args"),
                                     jitter_width = NULL, jitter_height = NULL,
                                     interactions = FALSE, add_vars = NULL, ...) {
  stopifnot(
    length(x) == 1L,
    length(y) == 1L
  )
  plot_list <- lapply(
    object,
    FUN = sv_dependence2D,
    # Argument list (simplify via match.call() or some rlang magic?)
    x = x,
    y = y,
    viridis_args = viridis_args,
    jitter_width = jitter_width,
    jitter_height = jitter_height,
    interactions = interactions,
    add_vars = add_vars,
    ...
  )
  plot_list <- add_titles(plot_list, nms = names(object))  # see sv_waterfall()
  patchwork::wrap_plots(plot_list)
}

