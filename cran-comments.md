# Re-Submission of shapviz 0.5.0

A pre-check failed:

```
Examples with CPU time > 2.5 times elapsed time
               user system elapsed ratio
sv_dependence 1.268  0.032   0.472 2.754
```

I have now set the parameter `nthread = 1` in fitting the XGBoost models in the
examples, hoping to solve that issue.

## Original message

Dear CRAN team. "shapviz" now offers plots for SHAP interaction values - a feature that was on the wishlist for quite some time.

## Checks

### check(manual = TRUE, cran = TRUE) 

-> WARNING
   'qpdf' is needed for checks on size reduction of PDFs

### check_rhub()

rhub notes

NOTES:
* checking package dependencies ... NOTE
Packages which this enhances but not available for checking:
  'h2o', 'lightgbm'
* checking for detritus in the temp directory ... NOTE
Found the following files/directories:
  'lastMiKTeXException'
  
### check_win_devel()

winbuilder: Status: OK
