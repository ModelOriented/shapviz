# Re-Submission of {shapviz} 0.7.0

This re-submission fixes non-standard links in the new vignette (and the README).

## Original message

Dear CRAN team. 

- {shapviz} can now deal with multiclass models or SHAP values of multiple models. Hurray ;).
- Many additional features
- New contributor
- Additional vignette
- New home: github/ModelOriented/shapviz

## Checks

### check(manual = TRUE, cran = TRUE) 

- WARNING: 'qpdf' is needed for checks on size reduction of PDFs
- Note: unable to verify current time

### check_rhub()

* checking package dependencies ... NOTE
Packages which this enhances but not available for checking:
  'h2o', 'lightgbm'
* checking examples ... [20s/81s] NOTE
Examples with CPU (user + system) or elapsed time > 5s
                user system elapsed
shapviz        2.563  0.149  10.403
sv_waterfall   2.473  0.004  10.154
sv_force       2.378  0.015   9.855
sv_interaction 1.936  0.006   7.870
sv_dependence  1.929  0.010   8.092
* checking HTML version of manual ... NOTE
Skipping checking HTML validation: no command 'tidy' found


### check_win_devel()

Status: OK

