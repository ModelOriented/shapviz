# Early submission of shapviz 0.4.0

Hello CRAN team. I am aware that the latest release of this package is not too long ago. 

Still, I have received an email of CRAN, warning about the possible removal of one of its dependencies ("ggbeeswarm"). Even if the ggbeeswarm author has now promised to fix the problem, I took the opportunity to remove ggbeeswarm and all its dependencies from my package.

## Checks

### check(manual = TRUE, cran = TRUE) 

-> Usual WARNING
   'qpdf' is needed for checks on size reduction of PDFs

### check_rhub() and check_win_devel()

Winbuilder: Status: OK

rhub: 0 errors ✔ | 0 warnings ✔ | 0 notes ✔
