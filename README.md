
<!-- README.md is generated from README.Rmd. Please edit that file -->

# trapgridR

<!-- badges: start -->

<!-- badges: end -->

trapgridR is an interace for the java program “TrapGrid” (Manoukis et
al. 2015) using R. Calling this program from R allows for multiple
simulations to be run, and the results to be easily collated and
summarised.

## Installation

Prior to installation, make sure you have `{rJava}` installed and
working. This may require installation of Java first. For example on
Ubuntu:

`sudo apt-get install -y default-jre`

`sudo apt-get install -y default-jdk`

`sudo R CMD javareconf`

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

There is a simple function to make a regular grid across a given area

``` r
make_regular_grid("my_grid")
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />

    #> [1] "Trapping grid  my_grid written"
    #>       [,1] [,2] [,3]
    #>  [1,]    0    0 0.02
    #>  [2,]    0   80 0.02
    #>  [3,]    0  160 0.02
    #>  [4,]    0  240 0.02
    #>  [5,]    0  320 0.02
    #>  [6,]   80    0 0.02
    #>  [7,]   80   80 0.02
    #>  [8,]   80  160 0.02
    #>  [9,]   80  240 0.02
    #> [10,]   80  320 0.02
    #> [11,]  160    0 0.02
    #> [12,]  160   80 0.02
    #> [13,]  160  160 0.02
    #> [14,]  160  240 0.02
    #> [15,]  160  320 0.02
    #> [16,]  240    0 0.02
    #> [17,]  240   80 0.02
    #> [18,]  240  160 0.02
    #> [19,]  240  240 0.02
    #> [20,]  240  320 0.02
    #> [21,]  320    0 0.02
    #> [22,]  320   80 0.02
    #> [23,]  320  160 0.02
    #> [24,]  320  240 0.02
    #> [25,]  320  320 0.02
    #> [26,]  400    0 0.02
    #> [27,]  400   80 0.02
    #> [28,]  400  160 0.02
    #> [29,]  400  240 0.02
    #> [30,]  400  320 0.02
    #> [31,]  480    0 0.02
    #> [32,]  480   80 0.02
    #> [33,]  480  160 0.02
    #> [34,]  480  240 0.02
    #> [35,]  480  320 0.02
    #> [36,]  560    0 0.02
    #> [37,]  560   80 0.02
    #> [38,]  560  160 0.02
    #> [39,]  560  240 0.02
    #> [40,]  560  320 0.02

Or make a random grid of at least a minimum distance apart, that can
include perimeter traps or not

``` r
make_random_grid(n.traps=5, gridname="my_grid", perim=TRUE)
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />

    #>       [,1] [,2] [,3]
    #>  [1,]   82    0 0.02
    #>  [2,]  489 2000 0.02
    #>  [3,] 2000 1604 0.02
    #>  [4,]    0 1289 0.02
    #>  [5,] 1283 1017 0.02
    #>  [6,] 1647 1886 0.02
    #>  [7,] 1164  466 0.02
    #>  [8,]  514  159 0.02
    #>  [9,] 1166 1567 0.02

Simulations can be setup in R and results stored as objects

``` r
model1 <- trapgridR(filepath="my_grid", nDays = 14, nFlies = 100, nSim=20, D=10^5)
```

Simulation results can be plotted from the model objects.

``` r
library (ggplot2)
ggplot(model1$simRuns, aes(Day, 1-Cumulative.Escape.Probability))+
  geom_line(aes(group=SimRun), colour=viridis::viridis(3)[1], alpha=0.3)+
  geom_smooth(method="loess", colour=viridis::viridis(3)[2])+
  theme_dark()+
  scale_x_continuous(expand=c(0.01,0))+
  scale_y_continuous(expand=c(0,0.01))+
  theme_minimal()
#> `geom_smooth()` using formula 'y ~ x'
```

<img src="man/figures/README-plot of simulation results-1.png" width="100%" />
The 1-cumulative escape probability can be thought of as the detection
probability - which only reaches around 0.25 after 2 weeks in this
example.

Additionally, fly locations from the simulation can be plotted to
examine movement patterns….

``` r
ggplot (model1$flyLoc, aes(as.integer(X), as.integer(Y),  colour=as.integer(Simulation.Number)))+
  geom_point()+
  facet_wrap(.~as.integer(Day))+
  scale_colour_viridis_c()+
  theme_minimal()
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="100%" />

…..which is a bit messy, so using `{gganimate}` can make an animation:

``` r
library (gganimate)
#> No renderer backend detected. gganimate will default to writing frames to separate files
#> Consider installing:
#> - the `gifski` package for gif output
#> - the `av` package for video output
#> and restarting the R session
ggplot (model1$flyLoc[model1$flyLoc$Simulation.Number==1:4,], aes(as.integer(X), as.integer(Y),  colour=Simulation.Number))+
  geom_point()+
  transition_time(as.integer(Day)) +
  scale_colour_viridis_d()+
  theme_minimal()+
  facet_wrap(~Simulation.Number, nrow = 2)
#> Warning: No renderer available. Please install the gifski, av, or magick package
#> to create animated output
#> NULL
```

which isn’t really that useful, except to show each fly is being tracked
through each simulation :)

We’ve included functions so that actualy trap arrangements can be easily
be used in R, and are implementing further changes to the model that
allow specification of further biological and trapping parameters.

# References

Manoukis, Nicholas C., Brian Hall, and Scott M. Geib. “A Computer Model
of Insect Traps in a Landscape.” Scientific Reports 4, no. 1 (May 2015).
<https://doi.org/10.1038/srep07015>.
