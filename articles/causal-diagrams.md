# Interactive causal diagrams

A directed acyclic graph (DAG) sets out the causal assumptions behind an
analysis: which variables are included, and which way each effect is
thought to run. A review of DAGs in applied health research found wide
variation in how they are reported and recommended making them more
transparent ([Tennant et
al. 2021](https://doi.org/10.1093/ije/dyaa213)). A printed diagram has
little room for the reasoning behind each node and arrow.
[`ggcausal()`](https://choxos.github.io/ggextreme/reference/ggcausal.md)
keeps that reasoning with the diagram. Every node and every arrow
carries its own rationale and references, shown when the reader hovers
over it and opened in full, with clickable links, when they click.

``` r

library(ggextreme)
```

## A first diagram

The package includes a small illustrative DAG for a study of maternal
smoking and orofacial clefts in the child.

``` r

dag <- ggcausal(
  cleft_dag$edges,
  cleft_dag$nodes,
  legend_title = "Role",
  title = "Maternal smoking and orofacial clefts"
)
dag
```

Hover over a box or an arrow to see why it is there. Click it to open
the full rationale and the references under the diagram; click it again,
or the close button, to hide them. Clicking works on touch screens,
where there is no hover.

## The data

A diagram is two data frames. `edges` has one row per arrow and is the
only required input:

``` r

cleft_dag$edges[c("from", "to")]
#>      from      to
#> 1     ses smoking
#> 2     ses  folate
#> 3     ses   cleft
#> 4 smoking   cleft
#> 5  folate   cleft
#> 6   genes   cleft
#> 7 smoking   birth
#> 8   cleft   birth
```

`nodes` has one row per variable, with a `name` matching `from` and
`to`:

``` r

cleft_dag$nodes[c("name", "label", "role")]
#>      name                   label              role
#> 1     ses   Socioeconomic\nstatus        confounder
#> 2 smoking        Maternal smoking          exposure
#> 3  folate Folic acid\nsupplements protective factor
#> 4   genes Genetic\nsusceptibility        unobserved
#> 5   cleft         Orofacial cleft           outcome
#> 6   birth              Live birth          collider
```

A few column names have a meaning of their own.

| column       | in    | effect                                         |
|--------------|-------|------------------------------------------------|
| `from`, `to` | edges | the ends of the arrow                          |
| `name`       | nodes | the identifier used in `from` and `to`         |
| `label`      | nodes | text in the box, which may contain line breaks |
| `role`       | nodes | sets the color, and appears in the legend      |
| `rationale`  | both  | why the node or arrow is in the diagram        |
| `references` | both  | sources for the rationale                      |
| `x`, `y`     | nodes | place the box by hand                          |

Every other column is shown as a labeled field. The example has a
`timing` column on its nodes, which appears as “Timing” in the hover
card and the panel. This is the place for anything a reader might want
to check, such as how a variable was measured.

## References

`references` holds one or more references per row. Give several either
as a list column or as text separated by `|` or a line break. Semicolons
are left alone because Vancouver style uses them inside a single
reference.

``` r

cleft_dag$nodes$references[2]
#> [1] "Little J, Cardy A, Munger RG. Tobacco smoking and oral clefts: a meta-analysis. Bull World Health Organ. 2004;82(3):213-8. https://pmc.ncbi.nlm.nih.gov/articles/PMC2585921/"
```

A reference that contains a URL is linked to it. Otherwise one that
contains a DOI, with or without a `doi:` prefix, is linked to
`https://doi.org/`. All text is escaped, so characters such as `<` and
`&` appear as written.

## Roles

`role` is free text. Eight roles have fixed colors: `exposure`,
`outcome`, `confounder`, `mediator`, `collider`, `instrument`,
`unobserved` and `latent`. The last two are drawn with a dashed border.
Any other role, such as the `protective factor` in the example, takes
the next color from
[`race_palette()`](https://choxos.github.io/ggextreme/reference/race_palette.md).
Override any of them with a named `palette`:

``` r

ggcausal(
  cleft_dag$edges, cleft_dag$nodes,
  palette = c(exposure = "#1B6CA8", outcome = "#B8413A")
)
```

## Layout

By default the nodes are placed in layers so that every arrow points the
same way, left to right or, with `direction = "down"`, top down. An
arrow that skips a layer is routed around the boxes in between on a
smooth curve.

``` r

ggcausal(cleft_dag$edges, cleft_dag$nodes, direction = "down", legend = FALSE)
```

For full control, give `x` and `y` columns in `nodes`. They are grid
positions with `y` pointing up, and the arrows between them are
straight.

``` r

nodes <- data.frame(
  name = c("confounder", "exposure", "outcome"),
  role = c("confounder", "exposure", "outcome"),
  x = c(1, 0, 2),
  y = c(1, 0, 0)
)
edges <- data.frame(
  from = c("confounder", "confounder", "exposure"),
  to = c("exposure", "outcome", "outcome")
)
ggcausal(edges, nodes)
```

## Adjustment paths

A diagram is drawn to decide what to adjust for. With `paths = TRUE`,
[`ggcausal()`](https://choxos.github.io/ggextreme/reference/ggcausal.md)
finds every path between the exposure and the outcome and says, for the
variables adjusted for, which ones stay open. A path is causal when
every arrow on it points away from the exposure, and biasing otherwise.
Adjusting for a variable blocks a path through it, except at a collider,
where two arrows meet head to head: a collider blocks its path until it,
or one of its consequences, is adjusted for, which opens it.

``` r

ggcausal(cleft_dag$edges, cleft_dag$nodes, legend_title = "Role", paths = TRUE)
```

In the widget, click a variable to adjust for it; it is boxed, the
arrows recolor, and the panel under the diagram says whether the set is
sufficient by the backdoor criterion, lists every path with the reason
it is open or blocked, and offers the minimal sufficient sets, which
apply with one click. Hover over a path in the list to find it in the
diagram. The menus change the exposure and the outcome, and the switch
next to them turns clicks back to opening each variable’s rationale.
Unobserved variables cannot be adjusted for.

Here the two backdoor paths through socioeconomic status are open, and
adjusting for it closes both. Adjusting for live birth as well opens the
path through it, since live birth is a collider and a consequence of the
exposure:

``` r

both <- ggcausal(cleft_dag$edges, cleft_dag$nodes, paths = TRUE,
                 adjust = c("ses", "birth"))
both$adjustment$verdict
#> [1] "Adjusting for Socioeconomic status, Live birth is not sufficient: 1 biasing path stays open, and Live birth is a consequence of the exposure and should not be adjusted for."
both$paths
#>                                                                                 path
#> 1                                                 Maternal smoking → Orofacial cleft
#> 2                                    Maternal smoking → Live birth ← Orofacial cleft
#> 3 Maternal smoking ← Socioeconomic status → Folic acid supplements → Orofacial cleft
#> 4                          Maternal smoking ← Socioeconomic status → Orofacial cleft
#>      kind  status                                                reason
#> 1  causal    open the effect to estimate; nothing on it is adjusted for
#> 2 biasing    open        adjusting for the collider Live birth opens it
#> 3 biasing blocked          adjusting for Socioeconomic status blocks it
#> 4 biasing blocked          adjusting for Socioeconomic status blocks it
```

`exposure` and `outcome` default to the nodes with those roles. The
verdict is only as good as the arrows drawn, and says nothing about the
size of the effect.

## Other forms

The diagram prints as a widget in the RStudio viewer, in R Markdown and
Quarto documents, and on ‘pkgdown’ sites. Three functions give its other
forms:

- [`graph_widget()`](https://choxos.github.io/ggextreme/reference/graph_widget.md)
  returns the htmlwidget, for ‘shiny’ or
  [`htmlwidgets::saveWidget()`](https://rdrr.io/pkg/htmlwidgets/man/saveWidget.html).
- [`graph_plot()`](https://choxos.github.io/ggextreme/reference/graph_widget.md)
  returns the underlying ggplot, a static diagram for a manuscript.
- [`graph_save()`](https://choxos.github.io/ggextreme/reference/graph_widget.md)
  writes the widget to a single `.html` file, which can go in a
  supplement, or a static `.png` at its natural size.

``` r

graph_save(dag, "dag.html")
graph_save(dag, "dag.png", res = 300)
```

The diagram is laid out at a fixed size so that every box fits its
label. The widget scales down to fit a narrow page; a static copy should
be drawn at `dag$width` by `dag$height` inches, which is what
[`graph_save()`](https://choxos.github.io/ggextreme/reference/graph_widget.md)
does.
