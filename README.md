# Colors for all!

The **cols4all** is an R package that is used to select proper color palettes. Color for all refers to the most important property that a color palette should have: it should be usable for not just people with normal color vision, but also for people with color vision deficiency.

The **cols4all** package contains palettes from many popular color palette series, such as brewer, viridis, and kovesi, but also the lesser known palettes from Paul Tol.

There are a few features that distinguishes **cols4all** from other (great) meta-color-palette package (**paletteer** and **pals**):

* Colors for missing values are made explicit. For this, either a greyscale color that is already contained in the palette is used, or a greyscale color is selected that is distinguisable, also for people with color vision deficiency.

* Palettes are made consistent with each other. For instance, sequential palettes (except rainbow type palettes) all start with the lightest color and end with the darkest color.

* With the gui (see below), a user is able to analyse characteristics of color palettes, such as color-blind-friendliness, harmony, and the presence of intense colors.

* It will be possible to submit an own series of color palettes. You can think of a certain house style of your organization.

There will be native support for **ggplot** and **tmap** (as of the upcoming version 4)


## Installation

**cols4all** will soon be available on CRAN. Until then it can be installed using:

```{r, eval=FALSE}
# install.packages("devtools")
devtools::install_github("mtennekes/cols4all")
```

## GUI

The central function `c4a_gui` starts a graphical user interface (shiny app): 

![c4a_gui(alt text))](https://user-images.githubusercontent.com/2444081/155034066-ed8e2441-ca67-4feb-a9a1-8f4a350b14a9.png)

