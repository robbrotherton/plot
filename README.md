
<!-- README.md is generated from README.Rmd. Please edit that file -->

# plot

<!-- badges: start -->
<!-- badges: end -->

This is an R package I use to help make pen-plotter art using R. Since
my pen plotter uses G-code, the package has some function to turn a
dataframe containing cartesian coordinates into G-code for plotting. It
also includes functions to help preview art before plotting.

## Installation

You can install the development version of plot from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("robbrotherton/plot")
```

## Example

Let’s make some art.

``` r
library(plot)
#> 
#> Attaching package: 'plot'
#> The following object is masked from 'package:graphics':
#> 
#>     frame

# It's a triangle

df <- data.frame(x = c(10, 100, 200, 10),
                 y = c(70, 220, 70, 70))
```

Let’s see how it might look in a frame.

``` r

frame(df, matt_size = "8x10")
#> No group variable present.
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />

Now we can turn the data into g-code to use with the pen plotter.

``` r

plotting_time(df)
#> [1] 00:00:46
```

``` r
write_gcode(df, filename = here::here("test.gcode"))
```
