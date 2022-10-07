# shapviz 0.2.2

Dear CRAN team

This small update fixes a problem that occur with R versions < 4.1:

I was using `apply(, ..., simplify = FALSE)`, which would fail for earlier R version. I was not aware of this.

## Checks

### check(manual = TRUE, cran = TRUE) 

-> warning on pdf compression.

### check_rhub()

* checking package dependencies ... NOTE
  'h2o', 'lightgbm'
Packages which this enhances but not available for checking:
* checking for detritus in the temp directory ... NOTE
Found the following files/directories:
  'lastMiKTeXException'

### check_win_devel()

OK
