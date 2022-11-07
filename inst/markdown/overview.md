<style>
ul {
    list-style-type: none;
    margin-left: 0px;
}
</style>


#### _Top bar_

The settings will determine what color palettes are shown in the main table

- **Palette type** Categorical palettes are used for visualizing categorical (unordered) data. For numeric and ordered data sequential and diverging are commonly used, where the latter is used when there is a clear middle (neutral) category.
- **Palette series** The series determines what the palettes have in common. Typically it is the source (e.g. brewer stands for ColorBrewer), but in some cases it is a collection of specific palettes: e.g. poly stands for polychrome categorical palettes.
- **Number of colors** Number of colors in the palette. Only palettes are shown that support this number.
- **Range** For sequential and diverging palettes, the range determines from which two extremes the palette is created.


#### _Main table




#### _Tab panel_

In the plot at the top of the panel, the palette colors are shown in the original colors, and simulated for the three types of color vision deficiency.

The **triangle color spaces** at the left-hand side show all hue-chroma combinations (with luminance being maximized) for people with normal color vision and simulated for the three types of color vision deficiency (details: the space is CIE xyY space in the sRGB gamut). The central white point (known as D65) is marked with a + symbol. The palette colors are marked with numbers. Note that these colors might be brighter than the original colors. The aim of this plot is to analyse the hue.

The lines shown in the simulated triangle color spaces are called **confusion lines**. They indicate which colors appear the same (regarding hue, so ignoring brightness and intensity) for color blind people.

The **distance matrices** show which colors are similar to each other for people with normal color vision and for people with color vision deficiency. The used symbols are:

- &#9632; (extremely close): `dist <= 2`
- &#9650; (very close): `2 < dist <= 5` 
- &#9650; (close): `5 < dist <= 10`

You can click on the matrix to select two colors. On the right hand side of the screen are examples of maps using the two selected colors.


#### _Tab panel_

In the chroma-luminance (CL) plot, the chroma and luminance values for all palette colors are shown. The dashed vertical lines divide chroma into three classes: low, medium, and high. The white box indicates the harmony.

The dashed vertical lines divide chroma into three classes: low (&#10057;), medium, and high (&#x1f576;).


#### _Tab panel_

The contrast ratio matrix shows the contrast ratio for each pair of colors. The used symbols are:

- <font size="3">&#9632;</font> (extremely low): `CR <= 1.2`
- <font size="2">&#9650;</font> (very low): `1.2 < CR <= 1.5` 
- <font size="1">&#9679;</font> (low): `1.5 < CR <= 2.0`

When colors with a low contrast ratio are shown next to each other, border lines are recommended to prevent wobbly (unstable) borders.

You can click on the matrix to select two colors. The example charts and the optical illusion art illustrate how severe this problem is, and how border lines can help.
