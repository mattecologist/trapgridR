
<!-- README.md is generated from README.Rmd. Please edit that file -->

# trapgridR

<!-- badges: start -->

<!-- badges: end -->

The following is a demonstration of an interace with the java program
“TrapGrid” (ref) using R. Calling this program from R allows for
multiple simulations to be run, and the results to be easily collated
and summarised.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("mattecologist/trapgridR")
```

## Example

``` r
library(trapgridR)
#> Loading required package: rJava
```

There is a simple function to make a regular grid across a given
area

``` r
make_regular_grid("my_grid")
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />

    #> [1] "Trapping grid  my_grid written"

``` r
model1 <- trapgridR(filepath="my_grid", nDays = 14, nFlies = 100, nSim=10, D=10^5)
```

``` r
library (ggplot2)
ggplot(model1$simRuns, aes(Day, 1-Cumulative.Escape.Probability, colour=SimRun, group=SimRun))+
  geom_line()+
  theme_dark()+
  scale_x_continuous(expand=c(0.01,0))+
  scale_y_continuous(expand=c(0,0.01))
```

<img src="man/figures/README-plot of simulation results-1.png" width="100%" />

``` r
ggplot (model1$flyLoc, aes(as.integer(X), as.integer(Y),  colour=as.integer(Simulation.Number)))+
  geom_point()+
  facet_wrap(.~as.integer(Day))+
  scale_colour_viridis_c()+
  theme_dark()
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" />
