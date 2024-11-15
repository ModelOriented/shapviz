

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

install.packages("shapr")
install.packages("shapviz")
library(shapr)
library(shapviz)


fit <- lm(Sepal.Length ~ ., data = iris)
x <- shapr(iris, fit)
explanation <- shapr::explain(
  iris, approach = "ctree", explainer = x, prediction_zero = mean(iris$Sepal.Length)
)
shp <- shapviz(explanation)
sv_imp <- sv_importance(shp)
sv_dep <- sv_dependence(shp, "Sepal.Width")

saveRDS(list(fit = fit,
             x = x,
             explanation = explanation,
             shp = shp,
             sv_imp = sv_imp,
             sv_dep = sv_dep),
        file = "shapr_shapviz_cran_test.rds")


#### Modified script with GitHub development version ####

#library(shapr)
remotes::install_github("NorskRegnesentral/shapr")
remotes::install_github("martinju/shapviz")

library(shapr)
library(shapviz)

fit <- lm(Sepal.Length ~ ., data = iris)
explanation <- shapr::explain(model = fit,
                              x_train = iris[-1],
                              x_explain = iris[-1],
                              approach = "ctree",
                              phi0 = mean(iris$Sepal.Length)
)

shp <- shapviz(explanation)
sv_imp <- sv_importance(shp)
sv_dep <- sv_dependence(shp, "Sepal.Width")

saveRDS(list(fit = fit,
             explanation = explanation,
             shp = shp,
             sv_imp = sv_imp,
             sv_dep = sv_dep),
        file = "shapr_shapviz_GH_test.rds")




#### Check whether the results are identical:

cran_test <- readRDS("shapr_shapviz_cran_test.rds")
gh_test <- readRDS("shapr_shapviz_GH_test.rds")

all.equal(cran_test$shp,gh_test$shp) # Differences in the compued Shapley values

cran_test$explanation$dt
gh_test$explanation$shapley_values_est

vdiffr::write_svg(cran_test$sv_dep, "sv_dep_cran.svg")
vdiffr::write_svg(gh_test$sv_dep, "sv_dep_gh.svg")

diffviewer::visual_diff("sv_dep_cran.svg", "sv_dep_gh.svg")

# Some difference due to randomness in the algorithm

##########

##### Test with CRAN version of shapr to identify expected behavior####

install.packages("shapr")
install.packages("shapviz")
library(shapr)
library(shapviz)


fit <- lm(Sepal.Length ~ ., data = iris)
x <- shapr(iris, fit)
explanation <- shapr::explain(
  iris, approach = "ctree", explainer = x, prediction_zero = mean(iris$Sepal.Length), sample = FALSE
)
shp <- shapviz(explanation)
sv_imp <- sv_importance(shp)
sv_dep <- sv_dependence(shp, "Sepal.Width")

saveRDS(list(fit = fit,
             x = x,
             explanation = explanation,
             shp = shp,
             sv_imp = sv_imp,
             sv_dep = sv_dep),
        file = "shapr_shapviz_cran_test_nosample.rds")



#### Modified script with GitHub development version ####

#library(shapr)
remotes::install_github("NorskRegnesentral/shapr")
#remotes::install_github("martinju/shapviz")

library(shapr)
library(shapviz)

fit <- lm(Sepal.Length ~ ., data = iris)
explanation <- shapr::explain(model = fit,
                              x_train = iris[-1],
                              x_explain = iris[-1],
                              approach = "ctree",
                              ctree.sample = FALSE,
                              phi0 = mean(iris$Sepal.Length)
)

shp <- shapviz(explanation)
sv_imp <- sv_importance(shp)
sv_dep <- sv_dependence(shp, "Sepal.Width")

saveRDS(list(fit = fit,
             explanation = explanation,
             shp = shp,
             sv_imp = sv_imp,
             sv_dep = sv_dep),
        file = "shapr_shapviz_GH_test_nosample.rds")


#### Check again wheter  the results are identical:

cran_test <- readRDS("shapr_shapviz_cran_test_nosample.rds")
gh_test <- readRDS("shapr_shapviz_GH_test_nosample.rds")

cran_test$explanation$dt
gh_test$explanation$shapley_values_est

all.equal(cran_test$shp,gh_test$shp)

vdiffr::write_svg(cran_test$sv_dep, "sv_dep_cran.svg")
vdiffr::write_svg(gh_test$sv_dep, "sv_dep_gh.svg")

diffviewer::visual_diff("sv_dep_cran.svg", "sv_dep_gh.svg")



