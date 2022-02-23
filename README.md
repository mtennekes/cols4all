
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Colors for all!

The **cols4all** is an R package in development for selecting color
palettes. “Color for all” refers to our mission that colors should be
usable for not just people with normal color vision, but also for people
with color vision deficiency. This package contains palettes from
several popular color palette series, such as Color Brewer, Viridis, and
Kovesi, but also the lesser known [palettes from Paul
Tol](https://personal.sron.nl/~pault/). Own palettes series can be added
as well. Color palettes are well organized. Currently we support three
types: *categorical* (qualitative) palettes, *sequential* palettes, and
*diverging* palettes. In the near future, more palette types will be
added, namely *cyclic*, *bivariate*, and *hierarchical*.

## Installation

**cols4all** will be available on CRAN soon! Until then it can be
installed using:

``` r
# install.packages("devtools")
devtools::install_github("mtennekes/cols4all")
```

``` r
getwd()
#> [1] "/home/mtes/git/cols4all"
devtools::load_all(".")
#> ℹ Loading cols4all
#> Loading required package: colorspace
```

## Get started

Navigation through the color palettes is easy:

``` r
library(cols4all)
c4a_gui()
```

![categorical
palettes)](https://user-images.githubusercontent.com/2444081/155185594-a52c361f-7113-40fe-82ea-94e782c27be4.png)
![sequential
palettes)](https://user-images.githubusercontent.com/2444081/155185615-9356443d-8a18-40a6-bc9e-d2e0d8e26eb4.png)
![diverging
palettes)](https://user-images.githubusercontent.com/2444081/155185638-3d05a045-c794-4eab-b994-88c0b58196e2.png)

Selecting a palette is easy. Palette names are in the format
<series>.<palette_name>. So all letters are in small caps, and
underscores are used to separate words.

``` r
# Select the palette "kelly" with 7 colors
c4a("kelly", 7)
#> [1] "#F3C300" "#875692" "#F38400" "#A1CAF1" "#BE0032" "#C2B280" "#848482"

# find names of hcl palettes that are diverging
c4a_palettes(type = "div", series = "hcl")
#>  [1] "hcl.blue_red"     "hcl.blue_red2"    "hcl.blue_red3"    "hcl.red_green"   
#>  [5] "hcl.purple_green" "hcl.purple_brown" "hcl.green_brown"  "hcl.blue_yellow2"
#>  [9] "hcl.blue_yellow3" "hcl.green_orange" "hcl.cyan_magenta"

# select purple green palette from the hcl series:
c4a("hcl.purple_green", 11)
#>  [1] "#481F50" "#81488C" "#B473C1" "#D2A9DA" "#E7D3EC" "#F1F1F1" "#C7E0C9"
#>  [8] "#91C392" "#4D9D4E" "#256C26" "#013902"

# get the associated color for missing values
c4a_na("hcl.purple_green")
#> [1] "#818181"
```

## Related R packages

The foundation of this package is another R package:
[**colorspace**](https://colorspace.r-forge.r-project.org/). We use this
package to analyse colors. For this purpose, we also use
[**colorblindcheck**](https://github.com/Nowosad/colorblindcheck).

There are a few features that distinguishes **cols4all** from other
(great) color palette packages, in particular
[**pals**](https://kwstat.github.io/pals/) and
[**paletteer**](https://github.com/EmilHvitfeldt/paletteer):

-   Colors for missing values are made explicit. For this, either a
    greyscale color that is already contained in the palette is used, or
    a greyscale color is selected that is distinguisable, also for
    people with color vision deficiency.

-   Palettes are made consistent with each other. For instance,
    sequential palettes (except for spectral/rainbow palettes) all start
    with the lightest color and end with the darkest color. Furthermore,
    black and white are removed from most categorical palettes for
    practical reasons: white or light grey is almost always used as
    background color and black for annotation.

-   With the gui (see below), a user is able to analyse characteristics
    of color palettes, such as color-blind-friendliness, harmony, and
    the presence of intense colors.

-   It will be possible to submit an own series of color palettes.

There will be native support for **ggplot2** and **tmap** (as of the
upcoming version 4)
