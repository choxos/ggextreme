# ggextreme

<!-- badges: start -->
[![R-CMD-check](https://github.com/choxos/ggextreme/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/choxos/ggextreme/actions/workflows/R-CMD-check.yaml)
[![License: MIT](https://img.shields.io/badge/license-MIT-blue.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

Presentation quality charts built on **ggplot2** that the package itself does
not provide. There are two so far:

* **Bar chart races**: an animation of a ranking that changes over time, of
  the kind used to summarize long panels in talks, teaching material and
  journal supplements.
* **Interactive causal diagrams**: a directed acyclic graph in which every
  node and arrow carries its rationale and references, shown on hover and
  opened in full on click.

Both are drawn as ordinary `ggplot` objects. Nothing is hidden behind a
separate rendering engine, so a frame or a diagram can be inspected, modified
or saved on its own.

![A bar chart race of the Quality of Care Index for orofacial clefts in fifteen countries, 1990 to 2019](man/figures/README-race.gif)

## Installation

```r
# install.packages("remotes")
remotes::install_github("choxos/ggextreme")
```

Writing output requires an encoder: **gifski** or **magick** for GIF, **av**
or an `ffmpeg` binary for MP4. Images on the bars require **magick**.

## Bar chart races

`ggrace()` takes long data with one row per entity per time point, and three
bare column names for the value, the label and the time.

```r
library(ggextreme)

race <- ggrace(
  clefts_qci,
  value = qci,
  name = country,
  time = year,
  top_n = 15,
  duration = 15,
  title = "Quality of care for orofacial clefts",
  caption = "Source: Sofi-Mahmudi et al. 2025, PLOS ONE 20(1): e0317267"
)

race_frame(race, 200)          # one frame, as a ggplot
animate_race(race, "race.mp4") # draw every frame and encode
```

`time` may be numeric or a `Date`. Each entity and time pair must appear
once; a repeat is an error rather than a silent average. The encoder is
chosen from the file extension, and frames are drawn across cores by default.

Selected arguments:

| argument | effect |
| --- | --- |
| `top_n` | number of bars visible at once |
| `duration`, `fps`, `end_pause` | length in seconds, frame rate, hold on the final frame |
| `swap` | seconds a bar takes to move into a new rank |
| `group` | color bars by category and draw a legend |
| `palette`, `breaks` | bar colors; gridline positions |
| `label_value`, `label_time` | formatters for the bar numbers and the time label |
| `images` | pictures placed at the end of the bars |
| `timeline`, `play_button`, `card` | optional chrome around the plot |
| `width`, `res` | output size; the layout scales with `width` |

### Coloring by group

Passing a `group` column colors the bars by category rather than
individually and draws a legend above the axis. Each entity must belong to
exactly one category; a factor keeps the legend in the order of its levels.

```r
ggrace(
  clefts_qci, qci, country, year,
  group = region,
  legend_title = "Region",
  top_n = 15
)
```

The card grows to make room for the legend, wrapping onto more rows when the
categories do not fit across it. `legend = FALSE` keeps the coloring and
drops the legend.

### Images on the bars

`images` takes image file paths named by entity. Pictures are cropped to a
circle and right aligned just inside the end of each bar; entities without an
image simply get none. A circular flag for every ISO 3166-1 country, plus
Kurdistan, is bundled, so country races need no extra files.

```r
key <- unique(clefts_qci[c("country", "iso")])
flags <- setNames(race_flags(key$iso), key$country)

ggrace(clefts_qci, qci, country, year, top_n = 15, images = flags)
```

Any image works, not only flags. Pass paths to logos, portraits or crests in
the same way.

### Design notes

Three choices govern how the animation reads. They are set out in full in
`vignette("how-the-animation-works")`.

* **Values are interpolated on a uniform time grid.** Bars grow at a steady
  rate, and unevenly spaced observations play at their true relative speed.
* **Rank is not interpolated.** Every frame is ranked on its own values, and
  a bar that changes rank eases into the new position over `swap` seconds.
  Bars therefore rest in place and trade positions in one short move rather
  than drifting for a whole time step.
* **The geometry is fixed.** The label column has a constant width and the
  panel edges are constants, so the chart does not shift sideways when the
  longest name enters or leaves the visible window. Colors are assigned once
  across the whole field, so an entity keeps its color when it drops out and
  returns.

## Interactive causal diagrams

`ggcausal()` draws a DAG from two data frames: `edges`, with one row per
arrow, and `nodes`, with one row per variable. A `rationale` and
`references` column on either one explains why that node or arrow is in the
diagram. Hovering shows the rationale; clicking opens a panel with the full
text and clickable references, which also works on touch screens.

```r
dag <- ggcausal(cleft_dag$edges, cleft_dag$nodes, legend_title = "Role")
dag                           # interactive widget
graph_save(dag, "dag.html")   # a single file for a supplement
graph_save(dag, "dag.png")    # a static figure
```

[![An illustrative causal diagram for maternal smoking and orofacial clefts](man/figures/README-dag.png)](https://choxos.github.io/ggextreme/articles/causal-diagrams.html)

GitHub cannot run the widget, so the image above is static. The
[interactive version](https://choxos.github.io/ggextreme/articles/causal-diagrams.html)
is on the package website.

Nodes are colored by `role`, with fixed colors for exposures, outcomes,
confounders, mediators, colliders, instruments and unobserved variables.
The layout is layered so that every arrow points the same way, and an arrow
that skips a layer bends around the boxes in between; `x` and `y` columns
place the boxes by hand instead. Any other column in either data frame
appears as a labeled field. The widget embeds a web copy of Lato and works in
R Markdown, Quarto, 'pkgdown' and 'shiny'.

## Bundled data

`clefts_qci` gives the Quality of Care Index for orofacial clefts in fifteen
countries from 1990 to 2019. The index is a composite of four secondary
indices derived from Global Burden of Disease estimates, summarized by
principal component analysis and rescaled from 0 to 100.

> Sofi-Mahmudi A, Shamsoddin E, Khademioore S, Khazaei Y, Vahdati A,
> Tovani-Palone MR (2025). Global, regional, and national survey on burden
> and Quality of Care Index (QCI) of orofacial clefts: Global burden of
> disease systematic analysis 1990-2019. *PLOS ONE* 20(1): e0317267.
> <https://doi.org/10.1371/journal.pone.0317267>

`cleft_dag` is a small illustrative causal diagram for maternal smoking and
orofacial clefts. Its rationales were written for the package as a teaching
example, and every reference it cites was checked against PubMed.

## License

MIT. The package bundles the Lato typeface, and a web subset of it for the
interactive graphs, under the SIL Open Font License (`inst/fonts/OFL.txt`),
and country flag artwork from the flag-icons project under the MIT License,
with two exceptions noted in `inst/extdata/flags/SOURCE.txt`.
