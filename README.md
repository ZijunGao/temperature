# README of R-package temperature
## A tool to provide a more comprehensive view of body temperature :smile:

## Install and load the package from R.
The devtools package provides install_github() that enables installing packages from GitHub.
```
devtools::install_github("ZijunGao/temperature") # if you don't have the package devtools, run "install.packages("devtools")" to install it.
library("temperature")
```

## Use the package from R
There are three major usages of the package: compute the percentile of the body temperature for a group of subjects, compute the body temperature at a given percentile for a group of subjects, and visualize the body temperature distribution of a given subject across the day. 

We include toy examples in the vignette. To access the vignette, run the following code in R. An independent page of vignette options will show up.
```
browseVignettes("temperature")
```

To call the visualization tool, just run the following code in R. An independent page for visualization will pop up.
```
temp_app()
```

## Contact
For more details of the package, please contact @ZijunGao through zijungao@stanford.edu.

