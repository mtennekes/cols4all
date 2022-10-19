#### **Basics**

The human eye has three types of cones, which are sensitive to light of different wavelengths: long, medium, and short. Their peak sensitivities (of 565, 540, and 440 nm respectively) correspond to the primary colors red, green, and blue. In computer graphics, a color is usually defined as RGB, which is a mixture of those three primary colors. RGB colors are often represented in hex-format (e.g. #FF00FF), where the first two characters specify red (in hex, so FF means 255), the third and fourth green (00 means 0) and the last two blue. So #FF00FF is purple. Formally, the color space is known as _sRGB_.

To analyse color palettes, we also use the polarLUV color space, which is a transformation of the CIELUV space. In this space, a color is defined by three variables: 

* **Hue** The 'color'. e.g. blue, green, yellow, etc. It is defined as a polar coordinate, so takes values from 0 to 360 degrees.
* **Chroma** The vividness or intensity of the color. The higher, the more colorful. The maximum chroma depends on the hue. Grayscale colors have 0 chroma.
* **Luminance** The amount of light emitted from an object (e.g. a computer screen). It is similar to _brightness_, although the latter is a relative measure.


#### **Colorblind friendliness** & **Hues**

First of all, we use the distance measure between two colors A and B from CIE as defined in 2000 (see Lindbloom, Bruce Justin. Delta E (CIE 2000).).

We define **colorblind friendliness** as:

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

Regarding the used **hues**, we distinguish 

* (**sequential**) 
  - Single hue palettes (&#128396;) which are recommended for quantitative analysis, but harder to distinguish colors
  - Spectral (&#127752;) which are easy to distinguish colors, but less suitable for quantitative analysis
* (**diverging**)
  - Two-hue palettes (&#x262F;) each wing has its own distinct hue, which is highly recommended
  - Spectral (&#127752;) which are easy to distinguish colors, but less suitable for quantitative analysis
 
##### Tab panel "Colorblind Friendliness & Hues"

In the plot at the top of the panel, the palette colors are shown in the original colors, and simulated for the three types of color vision deficiency.

The **triangle color spaces** at the left-hand side show all hue-chroma combinations (with luminance being maximized) for people with normal color vision and simulated for the three types of color vision deficiency (details: the space is CIE xyY space in the sRGB gamut). The central white point (known as D65) is marked with a + symbol. The palette colors are marked with numbers. Note that these colors might be brighter than the original colors. The aim of this plot is to analyse the hue.

The lines shown in the simulated triangle color spaces are called **confusion lines**. They indicate which colors appear the same (regarding hue, so ignoring brightness and intensity) for color blind people.

The **distance matrices** show which colors are similar to each other for people with normal color vision and for people with color vision deficiency. The used symbols are:

- &#9632 (extremely close): `dist <= 2`
- &#9650 (very close): `2 < dist <= 5` 
- &#9650 (close): `5 < dist <= 10`

You can click on the matrix to select two colors. On the right hand side of the screen are examples of maps using the two selected colors.


#### **Vividness and Harmony** 

Regarding **Vividness**: a color palette is called

- **vivid** (<font size="4">&#x1f576;</font>) if it contains a color with a chroma value of 100 or higher
- **pastel** (<font size="4">&#10057;</font>) if all colors have chroma values of at most 70

Pastel (low chroma) colors are recommended for *space-filling visualizations*, like maps and bar charts. Vivid (high chroma) colors for *small objects*, such as dots, lines, and text labels.

Regarding **Harmony** we consider two metrics: the luminance range (`Lrange`) and the chroma range (`Crange`). We call a palette:

- **harmonic** (<font size="4">🎵</font>) if `Crange <= 50` and (for categorical palettes only) `Lrange <= 30`
- **disharmonic** (<font size="4">&#92601;</font>) if `Crange <= 80` or (for categorical palettes only) `Lrange >= 50`

Why? Because it is recommended to use a palette of colors that stand out about equally (for otherwise, one color will draw more attention than another, which may bias our perception and interpretation of the shown data). Colors with a high chroma value stand out more than less chromatic colors. Furthermore, against a bright background, dark colors (low luminance) stand out more, while bright colors (high luminance) stand out more against a dark background. Luminance is only considers for categorical palettes, because quantitative palettes (sequential and diverging) often use luminance to distinguish colors.

Note that harmonic color palettes are usually not color blind friendly. Furthermore, when the luminance values of the colors are about equal, the contrast of those colors is low, which requires the use of border lines (see below).

##### Tab panel "Vividness & Harmony"

In the chroma-luminance (CL) plot, the chroma and luminance values for all palette colors are shown. The dashed vertical lines divide chroma into three classes: low, medium, and high. The white box indicates the harmony.

The dashed vertical lines divide chroma into three classes: low (&#10057;), medium, and high (&#x1f576;).

#### **Contrast**

The border between two colored shapes appears **wobbly** when the colors are equally luminant (bright), no matter what hue (red, blue, etc.) they have. This visual illusion is called **equiluminance**.

The **contrast ratio** is a measure for equiluminance, calculated as `CR = (L1 + 0.05) / (L2 + 0.05)`, where `L1` and `L2` are the luminances (normalized between 0 and 1) of the lighter and darker colors, respectively. Note that the minimum contrast ratio is 1 and the maximum 21.

The go-to solution to prevent wobbly borders is by using black or white (depending of the lightness of the colors) **border lines**."

##### Tab panel "Contrast"

The contrast ratio matrix shows the contrast ratio for each pair of colors. The used symbols are:

- &#9632 (extremely low): `CR <= 1.2`
- &#9650 (very low): `1.2 < CR <= 1.5` 
- &#9650 (low): `1.5 < CR <= 2.0`

When colors with a low contrast ratio are shown next to each other, border lines are recommended to prevent wobbly (unstable) borders.

You can click on the matrix to select two colors. The example charts and the optical illusion art illustrate how severe this problem is, and how border lines can help.
