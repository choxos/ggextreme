# Draw an interactive choropleth map over time

Colors every region of a map by a measure, with one map per measure side
by side, and a slider and play button under them that step through the
years. All the maps show the same year, so a region's measures read
together: hovering over a country outlines it on every map and lists its
value and rank on each measure that year, and clicking it opens its
whole series under the maps. The year can be dragged to or played
through, and the shades change in place. Ranks count from the highest
value that year, so for a measure where lower is better, such as
mortality, rank 1 is the worst.

## Usage

``` r
ggchoropleth(
  data,
  region,
  time,
  values,
  map = "world",
  map_id = NULL,
  at = NULL,
  ncol = NULL,
  palette = NULL,
  interval = 0.6,
  title = NULL,
  caption = NULL,
  family = "Lato"
)
```

## Arguments

- data:

  A data frame with a row for each region and time.

- region:

  Column of `data` naming the region: a code or an English name for the
  world map, or a value of `map_id` for an 'sf' map.

- time:

  Column of `data` giving the time, usually the year.

- values:

  Columns of `data` to map, as a character vector, one map each. Names
  label the maps; without them, the columns' `label` attributes or names
  are used.

- map:

  `"world"`, or an 'sf' object of polygons.

- map_id:

  For an 'sf' map, the column holding the keys `region` matches.
  Defaults to the first character or factor column.

- at:

  The time shown first, and in static copies. Defaults to the last.

- ncol:

  Maps per row. Defaults to two, or one for a single measure.

- palette:

  Colors for the maps, one entry per measure, in order or named by map
  label. One color gives a sequential scale; two give a diverging scale,
  the first for values below zero and the second for values above.

- interval:

  Seconds each time is shown while playing.

- title, caption:

  Title above the maps and note below them.

- family:

  Font family. The package ships Lato and registers it on load.

## Value

An object of class `ggchoropleth`, which prints as an interactive
widget. Use
[`graph_widget()`](https://choxos.github.io/ggextreme/reference/graph_widget.md),
[`graph_plot()`](https://choxos.github.io/ggextreme/reference/graph_widget.md)
or
[`graph_save()`](https://choxos.github.io/ggextreme/reference/graph_widget.md)
for the widget, a static ggplot of the year `at` or a file, and
[`animate_choropleth()`](https://choxos.github.io/ggextreme/reference/animate_choropleth.md)
to play the years as an animation.

## Details

The default map is the world: Natural Earth's 1:50m countries in the
Equal Earth projection, bundled with the package. Regions are matched by
ISO 3166-1 alpha-3 or alpha-2 code, or by English name, allowing for
case, accents and punctuation and for the forms the WHO and the Global
Burden of Disease study use, such as "Iran (Islamic Republic of)". Rows
for places that are not on the map, such as world regions and income
groups, are left out with a message naming them.

Any other map can be given as an 'sf' object of polygons, projected as
it should be drawn, with `map_id` naming the column that `region`
matches. A map in longitude and latitude is scaled so a degree of
longitude has its true length at the middle of the map. Detailed
boundaries make a heavy page, so simplify them first, for example with
[`sf::st_simplify()`](https://r-spatial.github.io/sf/reference/geos_unary.html).

Each measure has its own color scale, fixed across the years, so a
change of shade is a change of value. A measure with values on both
sides of zero, such as a change since the first year, gets a diverging
scale centered on zero. Shades are a color at increasing strength, drawn
with transparency, so a map suits a light or a dark page.

## Examples

``` r
qci <- clefts_qci_world
first <- qci$qci[qci$year == 1990][match(qci$iso3, qci$iso3[qci$year == 1990])]
qci$change <- qci$qci - first
ggchoropleth(qci, iso3, year,
             values = c("Quality of Care Index" = "qci",
                        "Change since 1990" = "change"),
             title = "Quality of care for orofacial clefts")
```
