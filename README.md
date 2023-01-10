
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Colors for all! <img src='inst/img/cols4all_logo.png' align="right" height="139" />

The **cols4all** is a new R package for selecting color palettes. “Color
for all” refers to our mission that colors should be usable for not just
people with normal color vision, but also for people with color vision
deficiency. Currently, this package contains palettes from several
popular and lesser known color palette series: [Color
Brewer](https://colorbrewer2.org),
[Viridis](http://bids.github.io/colormap/),
[Kovesi](https://colorcet.com/), [Paul
Tol](https://personal.sron.nl/~pault/),
[Scico](https://www.fabiocrameri.ch/colourmaps/),
[Carto](https://carto.com/carto-colors/),
[Tableau](https://www.tableau.com/about/blog/2016/7/colors-upgrade-tableau-10-56782),
[Wes Anderson](https://github.com/karthik/wesanderson), and
[Seaborn](https://seaborn.pydata.org/tutorial/color_palettes.html). Own
palettes series can be added as well.

Color palettes are well organized and made consistent with each other.
Moreover, they are scored on several aspects: color-blind-friendliness,
the presence of intense colors (which should be avoided), the overall
aesthetic harmony, and how many different hues are used. Finally, for
each color palette a color for missing values is assigned, which is
especially important for spatial data visualization. Currently we
support several types: *categorical* (qualitative) palettes,
*sequential* palettes, *diverging* palettes, and *bivariate* palettes
(divided into four subtypes).

## Installation

**cols4all** will be available on CRAN in January or February this year
(2023). Until then it can be installed using:

``` r
# colorspace 2.1 required:
install.packages("colorspace", repos = "https://R-Forge.R-project.org")

# development (github) version of cols4all
install.packages("remotes")
remotes::install_github("mtennekes/cols4all", dependencies = TRUE)
```

## Getting started

Load the package:

``` r
library(cols4all)
```

The main tool is a dashboard, which is started with:

``` r
c4a_gui()
```

![<https://user-images.githubusercontent.com/2444081/210850914-cdb8a128-1b8a-4900-94d0-dfa6ca449585.png>](https://user-images.githubusercontent.com/2444081/210850914-cdb8a128-1b8a-4900-94d0-dfa6ca449585.png)

What palettes are available?

``` r
# All loaded palettes
c4a_palettes()
#>   [1] "misc.r3"                                
#>   [2] "misc.r4"                                
#>   [3] "misc.ggplot2"                           
#>   [4] "misc.okabe"                             
#>   [5] "hcl.grays"                              
#>   [6] "hcl.light_grays"                        
#>   [7] "hcl.blues2"                             
#>   [8] "hcl.blues3"                             
#>   [9] "hcl.purples2"                           
#>  [10] "hcl.purples3"                           
...
```

``` r
# Diverging palettes from the 'hcl' series
c4a_palettes(type = "div", series = "hcl")
#>  [1] "hcl.blue_red1"    "hcl.blue_red2"    "hcl.blue_red3"    "hcl.red_green"   
#>  [5] "hcl.purple_green" "hcl.purple_brown" "hcl.green_brown"  "hcl.blue_yellow2"
#>  [9] "hcl.blue_yellow3" "hcl.green_orange" "hcl.cyan_magenta"
```

Give me the colors!

``` r
# Select the palette "kelly" with 7 colors
c4a("kelly", 7)
#> [1] "#F2F3F4" "#222222" "#F3C300" "#875692" "#F38400" "#A1CAF1" "#BE0032"

# select purple green palette from the hcl series:
c4a("hcl.purple_green", 11)
#>  [1] "#492050" "#82498C" "#B574C2" "#D2A9DB" "#E8D4ED" "#F1F1F1" "#C8E1C9"
#>  [8] "#91C392" "#4E9D4F" "#256C26" "#023903"

# get the associated color for missing values
c4a_na("hcl.purple_green")
#> [1] "#868686"
```

## Overview of functions

Main functions:

- `c4a_gui` Dashboard for analyzing the palettes
- `c4a` Get the colors from a palette

Palette names and properties:

- `c4a_palettes` Get available palette names
- `c4a_series` Get available series names
- `c4a_citation` Show how to cite palettes (with bibtex code).
- `c4a_info` Get information from a palette, such as type and maximum
  number of colors
- `.P` Environment via which palette names can be browsed with
  auto-completion (using `$`)

Importing and exporting palettes:

- `c4a_data` Build color palette data
- `c4a_load` Load color palette data
- `c4a_sysdata_import` Import system data
- `c4a_sysdata_export` Export system data

ggplot2

- `scale_<aesthetic>_<mapping>_c4a_<type>`
  e.g. `scale_color_continuous_c4a_div` Add scale to ggplot2.

## Related R packages

The foundation of this package is another R package:
[**colorspace**](https://colorspace.r-forge.r-project.org/). We use this
package to analyse colors. For this purpose and specifically for color
blind friendliness checks, we also use
[**colorblindcheck**](https://github.com/Nowosad/colorblindcheck).

There are a few other pacakges with a large collection of color
palettes, in particular [**pals**](https://kwstat.github.io/pals/) and
[**paletteer**](https://github.com/EmilHvitfeldt/paletteer). There are a
few features that distinguishes **cols4all** from those packages:

- Color palettes are characterized and analysed. Properties such as
  color blindness, fairness (whether colors stand out about equally),
  and contrast are determined for each palette.

- Bivariate color palettes are available (besides the three main palette
  types: categorical, sequential, and diverging).

- Own color palettes can be loaded and analysed.

- Color for missing values are made explicit.

- Palettes are made consistent with each other to enable comparison. For
  instance, black and white are (by default) removed from categorical
  palettes. Another standard that we adapt to is that all sequential
  palettes go from light to dark and not the other way round.

- There is native support for **ggplot2** and **tmap** (as of the
  upcoming version 4).

- There are a couple of exporting options, including (bibtex) citation.

## Feedback welcome!

- Is everything working as expected?

- Do you miss certain palettes?

- Do you have ideas for improvement how to measure palette properties?

Let us know! (via github issues)
