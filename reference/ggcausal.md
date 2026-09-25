# Draw an interactive causal diagram

Draws a directed acyclic graph (DAG) in which every node and every arrow
carries its own justification. Hovering over a node shows its role and
the reason it is in the diagram; hovering over an arrow shows the
assumed direction of effect and the reason for it. Clicking either opens
a panel under the diagram with the full rationale and clickable
references, which also makes the diagram usable on touch screens.

## Usage

``` r
ggcausal(
  edges,
  nodes = NULL,
  direction = c("right", "down"),
  palette = NULL,
  legend = TRUE,
  legend_title = NULL,
  title = NULL,
  caption = NULL,
  family = "Lato",
  paths = FALSE,
  exposure = NULL,
  outcome = NULL,
  adjust = character(0)
)
```

## Arguments

- edges:

  A data frame with one row per arrow and columns `from` and `to` naming
  its ends. Optional columns: `rationale`, the reason for the arrow and
  its direction, and `references`. Any other column is shown as a
  labeled field.

- nodes:

  A data frame with one row per node and a `name` column matching `from`
  and `to`. Optional columns: `label`, the text in the box, which may
  contain line breaks and defaults to `name`; `role`; `rationale`;
  `references`; and `x` and `y` to place the box by hand, in grid units
  with `y` pointing up. Any other column is shown as a labeled field.
  When `NULL`, the nodes are taken from `edges`.

- direction:

  Which way the arrows point: `"right"` or `"down"`.

- palette:

  Colors for the roles, as a vector named by role. Roles left out keep
  their default color.

- legend:

  Draw a legend of the roles above the diagram.

- legend_title:

  Text in front of the legend, such as `"Role"`.

- title, caption:

  Title above the diagram and note below it.

- family:

  Font family. The package ships Lato and registers it on load.

- paths:

  Show which paths between the exposure and the outcome are open or
  blocked for an adjustment set. The widget then lets the reader choose
  the exposure and outcome, click variables to adjust for them, and read
  why each path is open or blocked, whether the set is sufficient and
  which minimal sets would be.

- exposure, outcome:

  Names of the exposure and the outcome. Default to the nodes whose role
  is `exposure` and `outcome`.

- adjust:

  Names of the nodes adjusted for at the start.

## Value

An object of class `ggcausal`, which prints as an interactive widget.
Use
[`graph_widget()`](https://choxos.github.io/ggextreme/reference/graph_widget.md),
[`graph_plot()`](https://choxos.github.io/ggextreme/reference/graph_widget.md)
or
[`graph_save()`](https://choxos.github.io/ggextreme/reference/graph_widget.md)
for the widget, a static ggplot or a file. The fields `width` and
`height` give the natural size in inches.

## Details

The layout is layered, so every arrow points the same way, left to right
or top down. An arrow that skips a layer is routed around the boxes in
between on a smooth curve. Give `x` and `y` columns in `nodes` to place
the boxes yourself; arrows are then drawn straight.

Nodes are colored by `role`. The roles `exposure`, `outcome`,
`confounder`, `mediator`, `collider`, `instrument`, `unobserved` and
`latent` have fixed colors, and the last two are drawn with a dashed
border. Any other role is allowed and takes the next color from
[`race_palette()`](https://choxos.github.io/ggextreme/reference/race_palette.md).

`references` may be a list column with one character vector per row, or
text with several references separated by `|` or a line break. A
reference that contains a URL is linked to it; otherwise one that
contains a DOI is linked to `https://doi.org/`.

## Adjustment paths

A path between the exposure and the outcome is causal when every arrow
on it points from the exposure toward the outcome, and biasing
otherwise. A path is blocked when it passes through a variable that is
adjusted for, unless that variable is a collider, where two arrows meet
head to head; a collider blocks a path until it, or one of its
descendants, is adjusted for, which opens it. An adjustment set is
sufficient by the backdoor criterion when it blocks every biasing path
and contains no descendant of the exposure (Pearl 2009). Unobserved and
latent variables cannot be adjusted for. Adjustment is conditioning, not
intervention, and a diagram gives no size of effect.

## Examples

``` r
dag <- ggcausal(cleft_dag$edges, cleft_dag$nodes, legend_title = "Role")
dag

# Only the arrows are required.
ggcausal(data.frame(from = c("A", "A", "B"), to = c("B", "C", "C")))
```
