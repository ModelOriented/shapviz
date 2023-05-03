#' SHAP Interaction Plot
#'
#' Plots a beeswarm plot for each feature pair. Diagonals represent the main effects,
#' while off-diagonals show interactions (multiplied by two due to symmetry).
#' The colors on the beeswarm plots represent min-max scaled feature values.
#' Non-numeric features are transformed to numeric by calling [data.matrix()] first.
#' The features are sorted in decreasing order of usual SHAP importance.
#'
#' @param object An object of class "(m)shapviz" containing element `S_inter`.
#' @param kind Set to "no" to simply return the matrix of average absolute SHAP
#'   interactions (or a list of such matrices in case of object of class "mshapviz").
#'   The default is "beeswarm".
#' @param alpha Transparency of the beeswarm dots. Defaults to 0.3.
#' @param ... Arguments passed to [ggplot2::geom_point()]. For instance,
#'   passing `size = 1` will produce smaller dots.
#' @inheritParams sv_importance
#' @returns A "ggplot" (or "patchwork") object, or - if `kind = "no"` - a named
#'   numeric matrix of average absolute SHAP interactions sorted by the average
#'   absolute SHAP values (or a list of such matrices in case of "mshapviz" object).
#' @examples
#' dtrain <- xgboost::xgb.DMatrix(data.matrix(iris[, -1L]), label = iris[, 1L])
#' fit <- xgboost::xgb.train(data = dtrain, nrounds = 50L, nthread = 1L)
#' x <- shapviz(fit, X_pred = dtrain, X = iris, interactions = TRUE)
#' sv_interaction(x)
#' sv_interaction(x, max_display = 2L, size = 3, alpha = 0.1)
#' sv_interaction(x, kind = "no")
#' @seealso [sv_importance()]
#' @export
sv_interaction <- function(object, ...) {
  UseMethod("sv_interaction")
}

#' @describeIn sv_interaction
#'   Default method.
#' @export
sv_interaction.default <- function(object, ...) {
  stop("No default method available.")
}

#' @describeIn sv_interaction
#'   SHAP interaction plot for an object of class "shapviz".
#' @export
sv_interaction.shapviz <- function(object, kind = c("beeswarm", "no"),
                                   max_display = 7L, alpha = 0.3,
                                   bee_width = 0.3, bee_adjust = 0.5,
                                   viridis_args = getOption("shapviz.viridis_args"),
                                   color_bar_title = "Row feature value", ...) {
  kind <- match.arg(kind)
  if (is.null(get_shap_interactions(object))) {
    stop("No SHAP interaction values available.")
  }
  ord <- names(.get_imp(get_shap_values(object)))
  object <- object[, ord]

  if (kind == "no") {
    mat <- apply(abs(get_shap_interactions(object)), 2:3, mean)
    off_diag <- row(mat) != col(mat)
    mat[off_diag] <- mat[off_diag] * 2  # compensate symmetry
    return(mat)
  }

  if (ncol(object) > max_display) {
    ord <- ord[seq_len(max_display)]
    object <- object[, ord]
  }

  # Prepare data.frame for beeswarm
  S_inter <- get_shap_interactions(object)
  X <- .scale_X(get_feature_values(object))
  X_long <- as.data.frame.table(X)
  df <- transform(
    as.data.frame.table(S_inter, responseName = "value"),
    Variable1 = factor(Var2, levels = ord),
    Variable2 = factor(Var3, levels = ord),
    color = X_long$Freq  #  Correctly recycled along the third dimension of S_inter
  )

  # Compensate symmetry
  mask <- df[["Variable1"]] != df[["Variable2"]]
  df[mask, "value"] <- 2 * df[mask, "value"]

  ggplot(df, aes(x = value, y = "1")) +
    geom_vline(xintercept = 0, color = "darkgray") +
    geom_point(
      aes(color = color),
      position = position_bee(width = bee_width, adjust = bee_adjust),
      alpha = alpha,
      ...
    ) +
    facet_grid(Variable1 ~ Variable2, switch = "y") +
    labs(x = "SHAP value", y = element_blank(), color = color_bar_title) +
    .get_color_scale(
      viridis_args = viridis_args,
      bar = !is.null(color_bar_title),
      ncol = length(unique(X_long$Freq))
    ) +
    theme(
      panel.spacing = grid::unit(0.2, "lines"),
      legend.box.spacing = grid::unit(0, "pt"),
      axis.ticks.y = element_blank(),
      axis.text.y = element_blank()
    )
}

#' @describeIn sv_interaction
#'   SHAP interaction plot for an object of class "mshapviz".
#' @export
sv_interaction.mshapviz <- function(object, kind = c("beeswarm", "no"),
                                    max_display = 7L, alpha = 0.3,
                                    bee_width = 0.3, bee_adjust = 0.5,
                                    viridis_args = getOption("shapviz.viridis_args"),
                                    color_bar_title = "Row feature value", ...) {
  kind <- match.arg(kind)

  plot_list <- lapply(
    object,
    FUN = sv_interaction,
    # Argument list (simplify via match.call() or some rlang magic?)
    kind = kind,
    max_display = max_display,
    alpha = alpha,
    bee_width = bee_width,
    bee_adjust = bee_adjust,
    viridis_args = viridis_args,
    color_bar_title = color_bar_title,
    ...
  )
  if (kind == "no") {
    return(plot_list)
  }
  plot_list <- add_titles(plot_list, nms = names(object))  # see sv_waterfall()
  patchwork::wrap_plots(plot_list)
}
