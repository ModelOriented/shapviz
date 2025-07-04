# =============================================================================
# Put together the package
# =============================================================================

# WORKFLOW: UPDATE EXISTING PACKAGE
# 1) Modify package content and documentation.
# 2) Increase package version in "use_description" below.
# 3) Go through this script and carefully answer "no" if a "use_*" function
#    asks to overwrite the existing files. Don't skip that function call.
# devtools::load_all()

library(usethis)

# Sketch of description file
use_description(
  fields = list(
    Title = "SHAP Visualizations",
    Version = "0.10.1",
    Description = "Visualizations for SHAP (SHapley Additive exPlanations),
    such as waterfall plots, force plots, various types of importance plots,
    dependence plots, and interaction plots.
    These plots act on a 'shapviz' object created from a matrix of SHAP
    values and a corresponding feature dataset. Wrappers for the R packages
    'xgboost', 'lightgbm', 'fastshap', 'shapr', 'h2o', 'treeshap', 'DALEX',
    and 'kernelshap' are added for convenience.
    By separating visualization and computation, it is possible to display
    factor variables in graphs, even if the SHAP values are calculated by a model
    that requires numerical features. The plots are inspired by those provided by
    the 'shap' package in Python, but there is no dependency on it.",
    `Authors@R` =
      "c(person('Michael', family = 'Mayer', role = c('aut', 'cre'), email = 'mayermichael79@gmail.com'),
       person('Adrian', family = 'Stando', role = 'ctb', email = 'adrian.j.stando@gmail.com'))",
    Depends = "R (>= 3.6.0)"
  ),
  roxygen = TRUE
)

use_package("stats", "Imports")
use_package("utils", "Imports")
use_package("rlang", "Imports", min_version = "0.3.0")
use_package("grid", "Imports")
use_package("ggplot2", "Imports", min_version = "3.5.2")
use_package("gggenes", "Imports")
use_package("ggfittext", "Imports", min_version = "0.8.0")
use_package("ggrepel", "Imports")
use_package("patchwork", "Imports", min_version = "1.3.0")
use_package("xgboost", "Imports")

use_package("fastshap", "Enhances")
use_package("h2o", "Enhances")
use_package("lightgbm", "Enhances")

use_gpl_license(2)

# Your files that do not belong to the package itself (others are added by "use_* function")
use_build_ignore(c("^packaging.R$", "[.]Rproj$", "^logo.png$"), escape = FALSE)

# If your code uses the pipe operator %>%
# use_pipe()

# If your package contains data. Google how to document
# miami <- OpenML::getOMLDataSet(43093)$data
use_data(miami, overwrite = TRUE)

# Add short docu in Markdown (without running R code)
use_readme_md()

# Longer docu in RMarkdown (with running R code). Often quite similar to readme.
use_vignette("basic_use")

# If you want to add unit tests
use_testthat()
# use_test("plots.R")
# use_test("interface.R")

# On top of NEWS.md, describe changes made to the package
use_news_md()

# Add logo
use_logo("logo.png")

# If package goes to CRAN: infos (check results etc.) for CRAN
use_cran_comments()

use_github_links(overwrite = TRUE) # use this if this project is on github

# Github actions
# use_github_action("check-standard")
# use_github_action("test-coverage")
# use_github_action("pkgdown")

# Revdep
use_revdep()

# =============================================================================
# Finish package building (can use fresh session)
# =============================================================================

library(devtools)

document()
test()
check(manual = TRUE, cran = TRUE, vignettes = TRUE)
build(vignettes = FALSE)
# build(binary = TRUE)
install(upgrade = FALSE)

# Run only if package is public(!) and should go to CRAN
if (FALSE) {
  check_win_devel()
  check_rhub()
  check_rhub(platforms = "debian-gcc-devel")

  # Takes long
  revdepcheck::revdep_check(num_workers = 4, bioc = FALSE)

  # Wait until above checks are passed without relevant notes/warnings
  # then submit to CRAN
  devtools::release()
}
