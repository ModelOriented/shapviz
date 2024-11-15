

### RAW README example ####
library(shapviz)
library(shapr)

fit <- lm(Sepal.Length ~ ., data = iris)
x <- shapr(iris, fit)
explanation <- shapr::explain(
  iris, approach = "ctree", explainer = x, prediction_zero = mean(iris$Sepal.Length)
)
shp <- shapviz(explanation)
sv_importance(shp)
sv_dependence(shp, "Sepal.Width")



##### Test with CRAN version of shapr to identify expected behavior####

#install.packages("shapr")

library(shapr)

fit <- lm(Sepal.Length ~ ., data = iris)
x <- shapr(iris, fit)
explanation <- shapr::explain(
  iris, approach = "ctree", explainer = x, prediction_zero = mean(iris$Sepal.Length)
)
shp <- shapviz(explanation)
sv_imp <- sv_importance(shp)
sv_dep <- sv_dependence(shp, "Sepal.Width")

save(fit, x, explanation,shp,sv_imp,sv_dep, file = "shapr_cran_test.RData")


#### Modified script with GitHub development version ####

#library(shapr)
remotes::install_github("NorskRegnesentral/shapr")

fit <- lm(Sepal.Length ~ ., data = iris)
x <- shapr(iris, fit)
explanation <- shapr::explain(
  iris, approach = "ctree", explainer = x, prediction_zero = mean(iris$Sepal.Length)
)
shp <- shapviz(explanation)
sv_importance(shp)
sv_dependence(shp, "Sepal.Width")



