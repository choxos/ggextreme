# Draw an interactive swimmer plot

Draws one lane per patient: a bar for the time on treatment or on study,
marks for the events along it, such as responses, progression and death,
and an arrow for patients still ongoing. Hovering over a lane shows the
patient's group, the columns named in `hover`, the first time of each
event and whether they are ongoing, and fades the other lanes. Clicking
it opens the patient's events in order under the plot, beside their full
record. Buttons under the widget reorder the lanes, which slide into
place.

## Usage

``` r
ggswimmer(
  data,
  id,
  end,
  events = NULL,
  group = NULL,
  ongoing = NULL,
  start = NULL,
  hover = NULL,
  sort = c("duration", "group", "response", "data"),
  palette = NULL,
  ongoing_label = "Ongoing",
  xlab = "Time",
  legend = TRUE,
  title = NULL,
  caption = NULL,
  family = "Lato"
)
```

## Arguments

- data:

  A data frame with one row per patient.

- id:

  Column of `data` identifying the patient.

- end:

  Column of `data`, or an expression of its columns, giving the time
  each bar ends.

- events:

  Optional data frame with one row per event: the patient's id in a
  column named as `id` is in `data`, the time in `time` and what
  happened in `event`.

- group:

  Optional column of `data`, such as the arm, that colors the bars.

- ongoing:

  Optional logical column of `data`, or an expression of its columns,
  marking patients still ongoing at `end`, drawn with an arrow.

- start:

  Optional column of `data` giving the time each bar starts. Defaults to
  zero.

- hover:

  Names of columns of `data` shown in each lane's hover card.

- sort:

  Order of the lanes in a static copy and when the widget opens:
  `"duration"`, the longest first; `"group"`; `"response"`, those with a
  complete response first, then a partial one; or `"data"`, the order of
  `data`.

- palette:

  Colors for the groups, unnamed in level order or named by group.

- ongoing_label:

  What the arrow means, for the legend and the hover card, such as
  `"On treatment"` or `"Alive at last follow-up"`.

- xlab:

  Axis label. Include the unit, such as `"Months since first dose"`.

- legend:

  Draw a legend of the groups and events above the plot.

- title, caption:

  Title above the plot and note below it.

- family:

  Font family. The package ships Lato and registers it on load.

## Value

An object of class `ggswimmer`, which prints as an interactive widget.
Use
[`graph_widget()`](https://choxos.github.io/ggextreme/reference/graph_widget.md),
[`graph_plot()`](https://choxos.github.io/ggextreme/reference/graph_widget.md)
or
[`graph_save()`](https://choxos.github.io/ggextreme/reference/graph_widget.md)
for the widget, a static ggplot or a file.

## Details

Events are drawn by their wording: a complete response as a star, a
partial response or other response as a triangle, progression, relapse
or recurrence as a diamond, and death as a cross, with any other event
as a circle or square in its own color.

## Examples

``` r
if (requireNamespace("survival", quietly = TRUE)) {
  aml <- subset(survival::myeloid, id <= 30)
  month <- 30.44
  events <- rbind(
    data.frame(id = aml$id, time = aml$crtime / month, event = "Complete response"),
    data.frame(id = aml$id, time = aml$txtime / month, event = "Transplant"),
    data.frame(id = aml$id, time = aml$rltime / month, event = "Relapse"),
    data.frame(id = aml$id, time = ifelse(aml$death == 1, aml$futime / month, NA),
               event = "Death")
  )
  events <- events[!is.na(events$time), ]
  ggswimmer(aml, id, futime / month, events = events, group = trt,
            ongoing = death == 0, hover = c("sex", "flt3"),
            ongoing_label = "Alive at last follow-up",
            xlab = "Months since randomization")
}
```
