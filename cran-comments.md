# Resubmission

Examples taking too long on Linux. 

- I have now reduced the number of examples.
- And the number of boosting rounds.

# shapviz 0.9.0

Hello CRAN team

- I have added a beautiful house price dataset with 14000 transactions from Miami in 2016. A friend of mine has created it (Prof. Steven Bourassa).
- There is a new plot `sv_dependence2D()` that is ideal for visualization of geographic components.
- Added new vignette on SHAP analyses for models with geographic components.

## Checks look good

### check(manual = TRUE, cran = TRUE) 

> checking data for ASCII and uncompressed saves ... OK
   WARNING
  'qpdf' is needed for checks on size reduction of PDFs

> checking HTML version of manual ... NOTE
  Skipping checking HTML validation: no command 'tidy' found

### RHub

* checking package dependencies ... NOTE
Packages which this enhances but not available for checking:
  'fastshap', 'h2o', 'lightgbm'
* checking HTML version of manual ... NOTE
Skipping checking HTML validation: no command 'tidy' found
Skipping checking math rendering: package 'V8' unavailable


### Winbuilder()

Status: OK
