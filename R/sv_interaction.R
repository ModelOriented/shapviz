#' SHAP Interaction Plot
#'
#' @description
#' Creates a beeswarm plot or a barplot of SHAP interaction values/main effects.
#'
#' In the beeswarm plot (`kind = "beeswarm"`), diagonals represent the main effects,
#' while off-diagonals show SHAP interactions (multiplied by two due to symmetry).
#' The color axis represent min-max scaled feature values.
#' Non-numeric features are transformed to numeric by calling [data.matrix()] first.
#' The features are sorted in decreasing order of usual SHAP importance.
#'
#' The barplot (`kind = "bar"`) shows average absolute SHAP interaction values
#' and main effects for each feature pair.
#' Again, due to symmetry, the interaction values are multiplied by two.
#'
#' @param object An object of class "(m)shapviz" containing element `S_inter`.
#' @param kind Set to "no" to return the matrix of average absolute SHAP
#'   interactions (or a list of such matrices in case of object of class "mshapviz").
#'   Due to symmetry, off-diagonals are multiplied by two. The default is "beeswarm".
#' @param alpha Transparency of the beeswarm dots. Defaults to 0.3.
#' @param ... Arguments passed to [ggplot2::geom_point()]. For instance,
#'   passing `size = 1` will produce smaller dots.
#' @inheritParams sv_importance
#' @returns A "ggplot" (or "patchwork") object, or - if `kind = "no"` - a named
#'   numeric matrix of average absolute SHAP interactions sorted by the average
#'   absolute SHAP values (or a list of such matrices in case of "mshapviz" object).
#' @examples
#' dtrain <- xgboost::xgb.DMatrix(
#'   data.matrix(iris[, -1]),
#'   label = iris[, 1], nthread = 1
#' )
#' fit <- xgboost::xgb.train(data = dtrain, nrounds = 10, nthread = 1)
#' x <- shapviz(fit, X_pred = dtrain, X = iris, interactions = TRUE)
#' sv_interaction(x, kind = "no")
#' sv_interaction(x, max_display = 2, size = 3)
#' sv_interaction(x, kind = "bar")
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
sv_interaction.shapviz <- function(
    object,
    kind = c("beeswarm", "bar", "no"),
    max_display = 15L - 8 * (kind == "beeswarm"),
    alpha = 0.3,
    bee_width = 0.3,
    bee_adjust = 0.5,
    viridis_args = getOption("shapviz.viridis_args"),
    color_bar_title = "Row feature value",
    sort_features = TRUE,
    fill = "#fca50a",
    bar_width = 2 / 3,
    ...) {
  kind <- match.arg(kind)
  if (is.null(get_shap_interactions(object))) {
    stop("No SHAP interaction values available.")
  }

  # Sort features by SHAP importance first (irrelevant for kind = "bee")
  ord <- names(.get_imp(get_shap_values(object), sort_features = sort_features))
  object <- object[, ord]

  # Calculate average absolute SHAP interactions
  M <- apply(abs(get_shap_interactions(object)), MARGIN = 2:3, FUN = mean)
  M <- M + t(M) - diag(diag(M)) # Off-diagonals twice

  if (kind == "no") {
    return(M)
  }

  if (kind == "bar") {
    # Turn to long format and make feature pair names
    imp_df <- transform(
      as.data.frame.table(M, responseName = "value"),
      feature = ifelse(Var1 == Var2, as.character(Var1), paste(Var1, Var2, sep = ":"))
    )
    if (sort_features) {
      imp_df <- imp_df[order(imp_df$value, decreasing = TRUE), ]
      imp_df <- transform(imp_df, feature = factor(feature, levels = rev(feature)))
    }
    if (nrow(imp_df) > max_display) {
      imp_df <- imp_df[seq_len(max_display), ]
    }

    p <- ggplot2::ggplot(imp_df, ggplot2::aes(x = value, y = feature)) +
      ggplot2::geom_bar(fill = fill, width = bar_width, stat = "identity", ...) +
      ggplot2::labs(x = "mean(|SHAP interaction value|)", y = ggplot2::element_blank())

    return(p)
  }

  # kind == "bee"
  if (ncol(object) > max_display) {
    ord <- ord[seq_len(max_display)]
    object <- object[, ord]
  }

  S_inter <- get_shap_interactions(object)
  X <- .scale_X(get_feature_values(object))
  X_long <- as.data.frame.table(X)
  df <- transform(
    as.data.frame.table(S_inter, responseName = "value"),
    Variable1 = factor(Var2, levels = ord),
    Variable2 = factor(Var3, levels = ord),
    color = X_long$Freq #  Correctly recycled along the third dimension of S_inter
  )

  # Compensate symmetry
  mask <- df[["Variable1"]] != df[["Variable2"]]
  df[mask, "value"] <- 2 * df[mask, "value"]

  p <- ggplot2::ggplot(df, ggplot2::aes(x = value, y = "1")) +
    ggplot2::geom_vline(xintercept = 0, color = "darkgray") +
    ggplot2::geom_point(
      ggplot2::aes(color = color),
      position = position_bee(width = bee_width, adjust = bee_adjust),
      alpha = alpha,
      ...
    ) +
    ggplot2::facet_grid(Variable1 ~ Variable2, switch = "y") +
    ggplot2::labs(
      x = "SHAP value", y = ggplot2::element_blank(), color = color_bar_title
    ) +
    .get_color_scale(
      viridis_args = viridis_args,
      bar = !is.null(color_bar_title),
      ncol = length(unique(X_long$Freq))
    ) +
    ggplot2::theme(
      panel.spacing = grid::unit(0.2, "lines"),
      legend.box.spacing = grid::unit(0, "pt"),
      axis.ticks.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank()
    )
  return(p)
}

#' @describeIn sv_interaction
#'   SHAP interaction plot for an object of class "mshapviz".
#' @export
sv_interaction.mshapviz <- function(
    object,
    kind = c("beeswarm", "bar", "no"),
    max_display = 7L,
    alpha = 0.3,
    bee_width = 0.3,
    bee_adjust = 0.5,
    viridis_args = getOption("shapviz.viridis_args"),
    color_bar_title = "Row feature value",
    sort_features = TRUE,
    fill = "#fca50a",
    bar_width = 2 / 3,
    ...) {
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
    sort_features = sort_features,
    fill = fill,
    bar_width = bar_width,
    ...
  )
  if (kind == "no") {
    return(plot_list)
  }
  plot_list <- add_titles(plot_list, nms = names(object)) # see sv_waterfall()
  p <- patchwork::wrap_plots(plot_list) +
    patchwork::plot_layout(axis_titles = "collect", guides = "collect")

  return(p)
}
