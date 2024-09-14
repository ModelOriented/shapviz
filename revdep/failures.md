# fastshap

<details>

* Version: 0.1.1
* GitHub: https://github.com/bgreenwell/fastshap
* Source code: https://github.com/cran/fastshap
* Date/Publication: 2024-02-22 22:00:02 UTC
* Number of recursive dependencies: 90

Run `revdepcheck::revdep_details(, "fastshap")` for more info

</details>

## In both

*   R CMD check timed out
    

*   checking package dependencies ... NOTE
    ```
    Package which this enhances but not available for checking: 'lightgbm'
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: 'lightgbm'
    ```

# flowml

<details>

* Version: 
* GitHub: https://github.com/ModelOriented/shapviz
* Source code: NA
* Number of recursive dependencies: 0

</details>

## Error before installation

### Devel

```

  There are binary versions available but the source versions are later:
                binary   source needs_compilation
abind            1.4-5    1.4-8             FALSE
gam             1.22-4   1.22-5              TRUE
ps               1.7.7    1.8.0              TRUE
RcppArmadillo 14.0.0-1 14.0.2-1              TRUE

  Binaries will be installed
package 'ABCanalysis' successfully unpacked and MD5 sums checked
...
package 'pls' successfully unpacked and MD5 sums checked
package 'plyr' successfully unpacked and MD5 sums checked
package 'png' successfully unpacked and MD5 sums checked
package 'prettyunits' successfully unpacked and MD5 sums checked
package 'pROC' successfully unpacked and MD5 sums checked
package 'processx' successfully unpacked and MD5 sums checked
package 'prodlim' successfully unpacked and MD5 sums checked
package 'progress' successfully unpacked and MD5 sums checked
package 'progressr' successfully unpacked and MD5 sums checked
package 'promises' successfully unpacked and MD5 sums checked


Warning: unable to access index for repository https://bioconductor.org/packages/3.18/bioc/bin/windows/contrib/4.4:
  cannot open URL 'https://bioconductor.org/packages/3.18/bioc/bin/windows/contrib/4.4/PACKAGES'
Warning: unable to access index for repository https://bioconductor.org/packages/3.18/bioc/bin/windows/contrib/4.4:
  cannot open URL 'https://bioconductor.org/packages/3.18/bioc/bin/windows/contrib/4.4/PACKAGES'
Warning: unable to access index for repository https://bioconductor.org/packages/3.18/data/annotation/bin/windows/contrib/4.4:
  cannot open URL 'https://bioconductor.org/packages/3.18/data/annotation/bin/windows/contrib/4.4/PACKAGES'
Warning: unable to access index for repository https://bioconductor.org/packages/3.18/data/annotation/bin/windows/contrib/4.4:
  cannot open URL 'https://bioconductor.org/packages/3.18/data/annotation/bin/windows/contrib/4.4/PACKAGES'
Warning: unable to access index for repository https://bioconductor.org/packages/3.18/data/experiment/bin/windows/contrib/4.4:
  cannot open URL 'https://bioconductor.org/packages/3.18/data/experiment/bin/windows/contrib/4.4/PACKAGES'
...
2: In download.file(url, destfile, method, mode = "wb", ...) :
  downloaded length 247004746 != reported length 266665380
3: In download.file(url, destfile, method, mode = "wb", ...) :
  URL 'https://cran.rstudio.com/bin/windows/contrib/4.4/h2o_3.44.0.3.zip': Timeout of 60 seconds was reached
4: In download.file(url, destfile, method, mode = "wb", ...) :
  URL 'https://cran.rstudio.com/bin/windows/contrib/4.4/h2o_3.44.0.3.zip': Timeout of 60 seconds was reached
Warning in download.packages(pkgs, destdir = tmpd, available = available,  :
  download of package 'h2o' failed
Warning in download.packages(pkgs, destdir = tmpd, available = available,  :
  download of package 'h2o' failed


```
### CRAN

```

  There are binary versions available but the source versions are later:
                binary   source needs_compilation
abind            1.4-5    1.4-8             FALSE
gam             1.22-4   1.22-5              TRUE
ps               1.7.7    1.8.0              TRUE
RcppArmadillo 14.0.0-1 14.0.2-1              TRUE

  Binaries will be installed
package 'ABCanalysis' successfully unpacked and MD5 sums checked
...
package 'pls' successfully unpacked and MD5 sums checked
package 'plyr' successfully unpacked and MD5 sums checked
package 'png' successfully unpacked and MD5 sums checked
package 'prettyunits' successfully unpacked and MD5 sums checked
package 'pROC' successfully unpacked and MD5 sums checked
package 'processx' successfully unpacked and MD5 sums checked
package 'prodlim' successfully unpacked and MD5 sums checked
package 'progress' successfully unpacked and MD5 sums checked
package 'progressr' successfully unpacked and MD5 sums checked
package 'promises' successfully unpacked and MD5 sums checked


Warning: unable to access index for repository https://bioconductor.org/packages/3.18/bioc/bin/windows/contrib/4.4:
  cannot open URL 'https://bioconductor.org/packages/3.18/bioc/bin/windows/contrib/4.4/PACKAGES'
Warning: unable to access index for repository https://bioconductor.org/packages/3.18/bioc/bin/windows/contrib/4.4:
  cannot open URL 'https://bioconductor.org/packages/3.18/bioc/bin/windows/contrib/4.4/PACKAGES'
Warning: unable to access index for repository https://bioconductor.org/packages/3.18/data/annotation/bin/windows/contrib/4.4:
  cannot open URL 'https://bioconductor.org/packages/3.18/data/annotation/bin/windows/contrib/4.4/PACKAGES'
Warning: unable to access index for repository https://bioconductor.org/packages/3.18/data/annotation/bin/windows/contrib/4.4:
  cannot open URL 'https://bioconductor.org/packages/3.18/data/annotation/bin/windows/contrib/4.4/PACKAGES'
Warning: unable to access index for repository https://bioconductor.org/packages/3.18/data/experiment/bin/windows/contrib/4.4:
  cannot open URL 'https://bioconductor.org/packages/3.18/data/experiment/bin/windows/contrib/4.4/PACKAGES'
...
2: In download.file(url, destfile, method, mode = "wb", ...) :
  downloaded length 247004746 != reported length 266665380
3: In download.file(url, destfile, method, mode = "wb", ...) :
  URL 'https://cran.rstudio.com/bin/windows/contrib/4.4/h2o_3.44.0.3.zip': Timeout of 60 seconds was reached
4: In download.file(url, destfile, method, mode = "wb", ...) :
  URL 'https://cran.rstudio.com/bin/windows/contrib/4.4/h2o_3.44.0.3.zip': Timeout of 60 seconds was reached
Warning in download.packages(pkgs, destdir = tmpd, available = available,  :
  download of package 'h2o' failed
Warning in download.packages(pkgs, destdir = tmpd, available = available,  :
  download of package 'h2o' failed


```
