<style>
ul {
    list-style-type: none;
    margin-left: 0px;
}
</style>

### Introduction

Many charts and thematic maps use color palettes to visualize certain data. There are a lot of palettes to choose from: most visualization software tools have their own palettes and there are many other stand-alone sets of color palettes. To make life easy for R users, there are a couple of packages that have a collection of a large number of palettes, most notably `paletteer` with 2569 (!) palettes. However, people often cannot see the trees through the forest so therefore stick with default palettes or palettes from the popular collections, such as ColorBrewer.

The `cols4all` also contains a collection of palettes, but with the central question: which palettes are good and why? There is no simple answer, since there are many aspects to take into account. In `cols4all` we examine three main aspects:

- First and most importantly, is a palette suitable for colorblind people? About 5% of all humans have a color vision deficiency, and we want to include them; hence the package name "colors for all".
- Secondly, does a palette consist of colors that go well with each other, or as we call it, is a palette "harmonic"? Often, you would like the colors to stand out about equally, or to put it the other way round: a palette that contains both vivid and dull (grayish) colors is considered 'disharmonic' since some colors stand out more than others.
- Thirdly, does a palette potentially suffer from visual illusions, and if so how to solve those?

Color palettes also are important in areas other than data visualization, such as arts, web design, and fashion. The same aspects apply as well here, but obviously different criteria apply: the colors of a dress don't have to be distinguished by colorblind people, websites often contain colors that stand out more than others, and there are paintings that purposely use visual illusions.


### RGB and HCL

The human eye has three types of cones, which are sensitive to light of different wavelengths: long, medium, and short. Their peak sensitivities (of 565, 540, and 440 nm respectively) correspond to the primary colors red, green, and blue. In computer graphics, a color is usually defined as RGB, which is a mixture of those three primary colors. RGB colors are often represented in hex-format (e.g. #FF00FF), where the first two characters specify red (in hex, so FF means 255), the third and fourth green (00 means 0) and the last two blue. So #FF00FF is purple. Formally, the color space is known as _sRGB_.

To analyse color palettes, we also use the polarLUV color space, which is a transformation of the CIELUV space. In this space, a color is defined by three variables: 

* **Hue** The 'color'. e.g. blue, green, yellow, etc. It is defined as a polar coordinate, so takes values from 0 to 360 degrees.
* **Chroma** The vividness or intensity of the color. The higher, the more colorful. The maximum chroma depends on the hue. Grayscale colors have 0 chroma.
* **Luminance** The amount of light emitted from an object (e.g. a computer screen). It is similar to _brightness_, although the latter is a relative measure.


### Colorblind friendliness

First of all, we use the distance measure between two colors A and B from CIE as defined in 2000 (see Lindbloom, Bruce Justin. Delta E (CIE 2000).).

For simplicity, we apply two labels: colorblind friendly <font size="4">&#9786;</font> and colorblind unfriendly <font size="4">&#128064;</font>. A palette without such label is classified in between. 
Our definition of these labels depend on the palette type:


* (**categorical**) The minimum distance metric (`min_dist`) is the minimum distance between any two colors for any color vision deficiency type. 
  - <font size="4">&#9786;</font> (colorblind friendly): `min_dist >= 10`
  - <font size="4">&#128064;</font> (colorblind unfriendly): `min_dist <= 2`
* (**sequential**) The minimum step metric (`min_step`) is used, which is the smallest distance between any two adjacent colors (for any color vision deficiency type).
  - <font size="4">&#9786;</font> (colorblind friendly): `min_step >= 5`
  - <font size="4">&#128064;</font> (colorblind unfriendly): `min_dist <= 1`
Note that this metric is not very useful when a continuous scale is used. In that case, it is recommended to analyse the palette with say 7 or 9 colors.
* (**diverging**) The `min_step` metric used in a similar way as for sequential palettes. In addition, the inter wing distance metric (`inter_wing_dist`) is used, which is the minimum color distance between any color in the left wing to any color in the right wing of the palette (for any color vision deficiency type). 
  - <font size="4">&#9786;</font> (colorblind friendly): `min_step >= 5` and `inter_wing_dist >= 10`
  - <font size="4">&#128064;</font> (colorblind unfriendly): `min_dist <= 1` or `inter_wing_dist <= 4`
* (**bivariate (seq x seq)**) Three sequential palettes are extracted: a) the first column, b) the bottom row, and c) the diagonal. These are combined to form three diverging palettes: ab, ac, and bc. For each of these diverging palette, the same checks are applied as described above for regular diverging palettes.
* (**bivariate (seq x cat)**) For each row, the same checks apply as for a categorical palette
* (**bivariate (seq x div)**) Three sequential palettes are extracted: a) the first column, b) the middle column column, and c) the last column. These are combined to form three diverging palettes: ab, ac, and bc. For each of these diverging palette, the same checks are applied as described above for regular diverging palettes.
* (**bivariate (seq x desaturated)**) A diverging palette is formed from the first and the last column. The same checks are applied as described above for regular diverging palettes.

The threshold numbers can be configured via options (to do).

### HCL analysis


Regarding the used **hues**, there is no simple good and bad. However, for quantitative palettes we distinguish 

* (**sequential**) 
  - Single hue palettes (&#128396;) which are recommended for quantitative analysis, but harder to distinguish colors
  - Spectral (&#127752;) which are easy to distinguish colors, but less suitable for quantitative analysis
* (**diverging**)
  - Two-hue palettes (&#x262F;) each wing has its own distinct hue, which is highly recommended
  - Spectral (&#127752;) which are easy to distinguish colors, but less suitable for quantitative analysis
 

Regarding the used **chroma** / **vividness**, we label a color palette

- **vivid** (<font size="4">&#x1f576;</font>) if it contains a color with a chroma value of 100 or higher
- **pastel** (<font size="4">&#10057;</font>) if all colors have chroma values of at most 70

These threshold numbers can be configured via options (to do).

Pastel (low chroma) colors are recommended for *space-filling visualizations*, like maps and bar charts. Vivid (high chroma) colors for *small objects*, such as dots, lines, and text labels.

We also use the **chroma** and **luminance** values of palette colors to find out whether some colors stand out more than other colors. Ideally, a categorical palette should contain colors that stand out about equally (for otherwise, one color will draw more attention than another, which may bias our perception and interpretation of the shown data). Colors with a high chroma value stand out more than less chromatic colors. Furthermore, against a bright background, dark colors (low luminance) stand out more, while bright colors (high luminance) stand out more against a dark background. Luminance is only considered for categorical palettes, because quantitative palettes (sequential and diverging) often map a numeric variable to luminance.

Let us define the luminance range (`Lrange`) as the maximum minus the minimum luminance value of the palette and the chroma range (`Crange`) as the maximum chroma value minus the minimum chroma value.

Regarding **Harmony** we consider two metrics: the luminance range (`Lrange`) and the chroma range (`Crange`). We call a palette:

- **harmonic** (<font size="4">🎵</font>) if `Crange <= 50` and (for categorical palettes only) `Lrange <= 30`
- **disharmonic** (<font size="4">&#92601;</font>) if `Crange <= 80` or (for categorical palettes only) `Lrange >= 50`

Note that harmonic color palettes are usually not color blind friendly. Furthermore, when the luminance values of the colors are about equal, the contrast of those colors is low, which requires the use of border lines (see below).

### Contrast

The border between two colored shapes appears **wobbly** when the colors are equally luminant (bright), no matter what hue (red, blue, etc.) they have. This visual illusion is called **equiluminance**.

The **contrast ratio** is a measure for equiluminance, calculated as `CR = (L1 + 0.05) / (L2 + 0.05)`, where `L1` and `L2` are the luminances (normalized between 0 and 1) of the lighter and darker colors, respectively. Note that the minimum contrast ratio is 1 and the maximum 21.

In the overview table, there are three flags that indicate low contrast (`CR < 1.2`):

- <font size="1">&#127937;</font>: Between two colors of the palettes
- <font size="1">&#127987;</font>: Between one color of the palette and white
- <font size="1">&#127988;</font>: Between one color of the palette and black

The go-to solution to prevent wobbly borders is by using black or white (depending of the lightness of the colors) **border lines**."




### 3D Blues

Pure blue colors, by which we mean a 0 or very low number for the R (red) and G (green) channels and a high number for the B (blue) channel may cause a visual illusion called **chromostereopsis**. This effect is especially prominent when blue objects are shown next to red objects against a black background.

The table column "3D Blues" indicates when a palette contains a pure blue color. The used symbol is B7#9 (which refers to a music chord used in blues music). 

