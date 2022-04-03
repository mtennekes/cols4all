
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

|                                                                                                    Categorical                                                                                                    |                                                                                                    Sequential                                                                                                     |                                                                                                      Diverging                                                                                                      |
|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------:|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------:|:-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------:|
| [![](https://user-images.githubusercontent.com/2444081/161420545-68c007d1-640e-47f1-84e3-1e22c7b74e20.png)](https://user-images.githubusercontent.com/2444081/161420545-68c007d1-640e-47f1-84e3-1e22c7b74e20.png) | [![](https://user-images.githubusercontent.com/2444081/161420541-1c2dfefc-0451-47ed-badd-d5ecc339ead0.png)](https://user-images.githubusercontent.com/2444081/161420541-1c2dfefc-0451-47ed-badd-d5ecc339ead0.png) | [![](https://user-images.githubusercontent.com/2444081/161420543-62bc6947-df35-43e2-8277-35b2dbc223ca.png)]((https://user-images.githubusercontent.com/2444081/161420543-62bc6947-df35-43e2-8277-35b2dbc223ca.png)) |

|                                                                                                Bivariate (seq x seq)                                                                                                |                                                                                                Bivariate (seq x cat)                                                                                                |                                                                                            Bivariate (seq x desaturated)                                                                                            |
|:-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------:|:-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------:|:-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------:|
| [![](https://user-images.githubusercontent.com/2444081/161420551-84475286-5394-4379-abfc-d765567c2db6.png)]((https://user-images.githubusercontent.com/2444081/161420551-84475286-5394-4379-abfc-d765567c2db6.png)) | [![](https://user-images.githubusercontent.com/2444081/161420550-dc0ac110-ddea-4f02-8d47-cbb6221cca4f.png)]((https://user-images.githubusercontent.com/2444081/161420550-dc0ac110-ddea-4f02-8d47-cbb6221cca4f.png)) | [![](https://user-images.githubusercontent.com/2444081/161420549-8130172f-9047-48c3-b8f5-1a3e7ae7c3b3.png)]((https://user-images.githubusercontent.com/2444081/161420549-8130172f-9047-48c3-b8f5-1a3e7ae7c3b3.png)) |

Color palettes are well organized and made consistent with each other.
Moreover, they are scored on several aspects: color-blind-friendliness,
the presence of intense colors (which should be avoided), the overall
aesthetic harmony, and how many different hues are used. Finally, for
each color palette a color for missing values is assigned, which is
especially important for spatial data visualization. Currently we
support several types: *categorical* (qualitative) palettes,
*sequential* palettes, *diverging* palettes, and *bivariate* palettes
(divided into three subtypes).

## Installation

**cols4all** will be available on CRAN soon. Until then it can be
installed using:

``` r
# install.packages("devtools")
remotes::install_github("mtennekes/cols4all")
```

## Getting started

Navigation through the color palettes is easy via the GUI shown above,
which is started with:

``` r
library(cols4all)
c4a_gui()
```

Selecting a palette is easy:

``` r
# Select the palette "kelly" with 7 colors
c4a("kelly", 7)
#> [1] "#F2F3F4" "#222222" "#F3C300" "#875692" "#F38400" "#A1CAF1" "#BE0032"

# find names of hcl palettes that are diverging
c4a_palettes(type = "div", series = "hcl")
#>  [1] "hcl.blue_red1"    "hcl.blue_red2"    "hcl.blue_red3"    "hcl.red_green"   
#>  [5] "hcl.purple_green" "hcl.purple_brown" "hcl.green_brown"  "hcl.blue_yellow2"
#>  [9] "hcl.blue_yellow3" "hcl.green_orange" "hcl.cyan_magenta"

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

-   `c4a_gui` GUI (shiny app) to see and analyse the palettes
-   `c4a` Get the colors of a palette

Palette names and properties:

-   `c4a_palettes` Get available palette names
-   `c4a_series` Get available series names
-   `c4a_info` Get palette information (such as type and maximum number
    of colors)
-   `c4a_citation` Show how to cite palettes (with bibtex code).
-   `.P` Environment via which palette names can be browsed with
    auto-completion (using `$`)

Importing and exporting palettes:

-   `c4a_palettes_add` Add color palettes
-   `c4a_palettes_remove` Remove color palettes
-   `c4a_sysdata_import` Import system data
-   `c4a_sysdata_export` Export system data

ggplot2

-   `scale_<aesthetic>_<mapping>_c4a_<type>`
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

-   The GUI enables users to analyse characteristics of color palettes,
    such as color-blind-friendliness, harmony, and the presence of
    intense colors.

-   There is native support for **ggplot2** and **tmap** (as of the
    upcoming version 4).

-   It will be possible to submit an own series of color palettes.

## Feedback welcome!

-   Is everything working as expected?

-   Do you miss certain palettes or series?

-   Do you have ideas for improvement how to measure palette properties?

Let us know! (via github issues)
