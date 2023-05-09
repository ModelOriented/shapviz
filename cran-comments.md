# {shapviz} 0.8.0

Hello CRAN team

- {shapviz} will work more smoothly for upcoming {fastshap} versions. I did this change together with the {fastshap} maintainer (Brandon Greenwell).
- Better help files
- Switched from "import ggplot2" to "ggplot2::function" code style

## Checks look good

### check(manual = TRUE, cran = TRUE) 

* checking for future file timestamps ... NOTE
  unable to verify current time

* checking HTML version of manual ... NOTE
  Skipping checking HTML validation: no command 'tidy' found

### check_rhub(): Some usual notes...

* checking package dependencies ... NOTE
Packages which this enhances but not available for checking:
  'fastshap', 'h2o', 'lightgbm'
* checking HTML version of manual ... NOTE
Skipping checking math rendering: package 'V8' unavailable
* checking for non-standard things in the check directory ... NOTE
Found the following files/directories:
  ''NULL''
* checking for detritus in the temp directory ... NOTE
Found the following files/directories:
  'lastMiKTeXException'

### check_win_devel()

Status: OK
