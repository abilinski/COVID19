# covid.epi R Package

This package contains a deterministic compartmental model model of coronavirus-19 transmission. 

After cloning this repository locally, the package can be loaded or installed
using the devtools package. Install devtools if you don't already have it with
`install.packages("devtools")` in R.

Loading the package: 

    # make sure you're in the R package directory: 
    # getwd() should return something like: 
    # ~/path/to/COVID19/1 - R Package/

    # if getwd() doesn't show the right path, navigate there using setwd() to 
    # set your R session's working directory to the folder containing 
    # this R package.
    
    devtools::load_all()

Installing the package: 

    # The same goes as above, make sure you're in the R package folder,
    # then run: 

    devtools::install()

The main difference between `devtools::load_all()` and `devtools::install()`
is that `devtools::install()` makes the package available for loading via 
`library(covid.epi)` in any R session, whereas `devtools::load_all()` makes 
the functions available from the R package for your current R session only, 
which is useful for developers of the package to quickly test out new features
and package improvements.

# COVID19 Epi Shiny App

After running `devtools::load_all()` or installing the package as described above,
run `runApp()` (or to be extra specific, `covid.epi::runApp()`) and this will 
launch the Shiny app we have been building that allows users to simulate 
intervention scenarios for the COVID19 epidemic.

