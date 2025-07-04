% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sv_force.R
\name{sv_force}
\alias{sv_force}
\alias{sv_force.default}
\alias{sv_force.shapviz}
\alias{sv_force.mshapviz}
\title{SHAP Force Plot}
\usage{
sv_force(object, ...)

\method{sv_force}{default}(object, ...)

\method{sv_force}{shapviz}(
  object,
  row_id = 1L,
  max_display = 6L,
  fill_colors = c("#f7d13d", "#a52c60"),
  format_shap = getOption("shapviz.format_shap"),
  format_feat = getOption("shapviz.format_feat"),
  contrast = TRUE,
  bar_label_size = 3.2,
  show_annotation = TRUE,
  annotation_size = 3.2,
  ...
)

\method{sv_force}{mshapviz}(
  object,
  row_id = 1L,
  max_display = 6L,
  fill_colors = c("#f7d13d", "#a52c60"),
  format_shap = getOption("shapviz.format_shap"),
  format_feat = getOption("shapviz.format_feat"),
  contrast = TRUE,
  bar_label_size = 3.2,
  show_annotation = TRUE,
  annotation_size = 3.2,
  ...
)
}
\arguments{
\item{object}{An object of class "(m)shapviz".}

\item{...}{Arguments passed to \code{\link[ggfittext:geom_fit_text]{ggfittext::geom_fit_text()}}.
For example, \code{size = 9} will use fixed text size in the bars and \code{size = 0}
will altogether suppress adding text to the bars.}

\item{row_id}{Subset of observations to plot, typically a single row number.
If more than one row is selected, SHAP values are averaged, and feature values
are shown only when they are unique.}

\item{max_display}{Maximum number of features (with largest absolute SHAP values)
should be plotted? If there are more features, they will be collapsed to one
feature. Set to \code{Inf} to show all features.}

\item{fill_colors}{A vector of exactly two fill colors: the first for positive
SHAP values, the other for negative ones.}

\item{format_shap}{Function used to format SHAP values. The default uses the
global option \code{shapviz.format_shap}, which equals to
\code{function(z) prettyNum(z, digits = 3, scientific = FALSE)} by default.}

\item{format_feat}{Function used to format numeric feature values. The default uses
the global option \code{shapviz.format_feat}, which equals to
\code{function(z) prettyNum(z, digits = 3, scientific = FALSE)} by default.}

\item{contrast}{Logical flag that detemines whether to use white text in dark arrows.
Default is \code{TRUE}.}

\item{bar_label_size}{Size of text used to describe bars
(via \code{\link[ggrepel:geom_text_repel]{ggrepel::geom_text_repel()}}).}

\item{show_annotation}{Should "f(x)" and "E(f(x))" be plotted? Default is \code{TRUE}.}

\item{annotation_size}{Size of the annotation text (f(x)=... and E(f(x))=...).}
}
\value{
An object of class "ggplot" (or "patchwork") representing a force plot.
}
\description{
Creates a force plot of SHAP values of one observation. If multiple
observations are selected, their SHAP values and predictions are averaged.
}
\details{
f(x) denotes the prediction on the SHAP scale, while E(f(x)) refers to the
baseline SHAP value.
}
\section{Methods (by class)}{
\itemize{
\item \code{sv_force(default)}: Default method.

\item \code{sv_force(shapviz)}: SHAP force plot for object of class "shapviz".

\item \code{sv_force(mshapviz)}: SHAP force plot for object of class "mshapviz".

}}
\examples{
dtrain <- xgboost::xgb.DMatrix(
  data.matrix(iris[, -1]),
  label = iris[, 1], nthread = 1
)
fit <- xgboost::xgb.train(data = dtrain, nrounds = 20, nthread = 1)
x <- shapviz(fit, X_pred = dtrain, X = iris[, -1])
sv_force(x)
sv_force(x, row_id = 65, max_display = 3, size = 9, fill_colors = 4:5)

# Aggregate over all observations with Petal.Length == 1.4
sv_force(x, row_id = x$X$Petal.Length == 1.4)

# Two observations separately
sv_force(c(x[1, ], x[2, ])) +
  patchwork::plot_layout(ncol = 1)
}
\seealso{
\code{\link[=sv_waterfall]{sv_waterfall()}}
}
