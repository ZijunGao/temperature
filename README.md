# README of R-package temperature
*A tool to provide a more comprehensive view of body temperature :smile:*

## Install and load the package from R.
The devtools package provides install_github() that enables installing packages from GitHub.
```
# if you don't have the package "devtools", run "install.packages("devtools")" to install it.
devtools::install_github("ZijunGao/temperature", build_vignettes = TRUE) 
library("temperature")
```

## Use the package from R
The package has three major usages: 
*  compute the percentile of a given body temperature for a group of subjects;
*  compute the body temperature at a given percentile for a group of subjects;
*  visualize the body temperature distribution of a given subject across the day. 

We include toy examples in the vignette. To access the vignette, run the following code in R. A webpage of vignette options (*HTML*, *source*, *R code*) will show up. Click *HTML*. 
```
browseVignettes("temperature")
```

To call the visualization app, run the following code in R. An independent webpage of the visualization app will pop up.
```
temp_app()
```

## Contact
For more details of the package, please contact @ZijunGao through zijungao@stanford.edu.

