# shapviz 0.9.2

## Resubmission 7

No effect of setDTthreads(2). Removing now all tests with xgboost.

## Resubmission 6

Test timing not fixed, still. Now testing to import {data.table} and setDTthreads(2) pre checks.

## Resubmission 5

Now trying 

Sys.setenv(DT_NUM_THREADS = 1)
Sys.setenv("TESTTHAT_CPUS" = 1)
options(Ncpus = 1)


## Resubmission 4

Trying to set Sys.setenv(DT_NUM_THREADS = 2) in the unit tests to fix the crazy Debian behaviour.

## Resubmission 3

Moving one single nthread = 1 into param = list(). Setting nrounds = 1 in all tests. If this does not help, I will need to delete most unit tests.

## Resubmission 2

Setting nthread = 1 in unit tests and vignettes. Hope this fixes the problems.

## Resubmission 1

Fixing problems with curly braces in .rd files.

### Original message

Hello CRAN team

{shapviz} already got 2 reverse dependencies, which look okay.

The update has mainly added more flexibility of the importance plots for multi-output models.

## Checks look good

### check(manual = TRUE, cran = TRUE)

- checking HTML version of manual ... NOTE
  Skipping checking HTML validation: no command 'tidy' found

### RHub (usual notes)

* checking package dependencies ... NOTE
Packages which this enhances but not available for checking:
  'fastshap', 'h2o', 'lightgbm'
* checking HTML version of manual ... NOTE
Skipping checking HTML validation: no command 'tidy' found
Skipping checking math rendering: package 'V8' unavailable

### Winbuilder()

Status: OK

## Reverse dependencies (2)

- OK: 2
- BROKEN: 0
