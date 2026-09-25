# Draw an interactive Kaplan-Meier plot

Draws Kaplan-Meier curves by group with confidence bands, censoring
marks and a table of the numbers at risk, followed by the hazard ratios
and the proportional hazards tests. Hovering anywhere along the time
axis reads every group at that time: survival with its confidence
interval, the number at risk, the events so far and the hazard ratio
against the reference group at that time, while the matching column of
the risk table lights up. Clicking opens the full readout for that time
under the plot. Hovering over a curve shows that group's summary, and
clicking it opens the group's survival at each break.

## Usage

``` r
ggkm(
  formula,
  data,
  type = c("survival", "risk"),
  hr_time = c("schoenfeld", "log", "linear", "constant", "none"),
  ph_tests = FALSE,
  rmst = NULL,
  risk_table = TRUE,
  conf_int = TRUE,
  breaks = NULL,
  reference = NULL,
  palette = NULL,
  xlab = "Time",
  ylab = NULL,
  legend = TRUE,
  legend_title = NULL,
  title = NULL,
  caption = NULL,
  family = "Lato"
)
```

## Arguments

- formula:

  A formula of the form `Surv(time, status) ~ group`, with right
  censored survival times and a single grouping variable.

- data:

  A data frame holding the variables in `formula`.

- type:

  `"survival"` for the survival probability, or `"risk"` for the
  cumulative probability of the event.

- hr_time:

  How the hazard ratio at each time is estimated: `"schoenfeld"`,
  `"log"`, `"linear"`, `"constant"` or `"none"`.

- ph_tests:

  Add the hazard ratios and proportional hazards tests, in a collapsed
  section under the plot. The time interaction models grow with the
  number of events, so they are skipped with a message for very large
  data.

- rmst:

  Restricted mean survival time: `NULL` for none, the prespecified
  horizon \\\tau\\ on the time scale, or `TRUE` for the end of follow-up
  in the group followed least.

- risk_table:

  Show the numbers at risk.

- conf_int:

  Shade the confidence bands.

- breaks:

  Times for the axis ticks and the risk table. Defaults to about eight
  evenly spaced times.

- reference:

  The group the hazard ratios compare against. Defaults to the first
  level.

- palette:

  Colors for the groups, unnamed in level order or named by group. The
  reference group defaults to a neutral gray.

- xlab, ylab:

  Axis labels. `xlab` also names the time in the hover card, so include
  its unit, such as `"Years since randomization"`.

- legend:

  Draw a legend of the groups above the plot.

- legend_title:

  Text in front of the legend.

- title, caption:

  Title above the plot and note below it.

- family:

  Font family. The package ships Lato and registers it on load.

## Value

An object of class `ggkm`, which prints as an interactive widget. Use
[`graph_widget()`](https://choxos.github.io/ggextreme/reference/graph_widget.md),
[`graph_plot()`](https://choxos.github.io/ggextreme/reference/graph_widget.md)
or
[`graph_save()`](https://choxos.github.io/ggextreme/reference/graph_widget.md)
for the widget, a static ggplot or a file, and
[`animate_km()`](https://choxos.github.io/ggextreme/reference/animate_km.md)
to draw the curves over follow-up as an animation. With
`ph_tests = TRUE`, the field `ph` holds the proportional hazards table
as a data frame.

## Details

The hazard ratio at a time comes from `hr_time`. The default,
`"schoenfeld"`, smooths the scaled Schoenfeld residuals of the Cox model
against time, which estimates the log hazard ratio as a function of time
(Grambsch and Therneau 1994), the curve
[`survival::plot.cox.zph()`](https://rdrr.io/pkg/survival/man/plot.cox.zph.html)
draws. `"log"` and `"linear"` take it from a Cox model with an
interaction between the group and log time or time, and `"constant"`
shows the Cox estimate at every time.

With `rmst`, the plot shades the area under each curve up to a horizon
\\\tau\\ and tabulates the restricted mean survival time, the mean time
alive (or free of the event) within \\\tau\\, for each group, with its
difference from the reference group. RMST and its standard error are
those
[`survival::survfit()`](https://rdrr.io/pkg/survival/man/survfit.html)
reports for each group; the difference assumes the groups are
independent, as in a randomized comparison. The widget adds a slider to
explore other horizons, up to the end of follow-up in the group followed
least, while the prespecified horizon stays marked, so the horizon
reported is the one planned rather than the most favorable one.

With `ph_tests = TRUE`, a section under the plot, collapsed until the
reader opens it, gives the Cox hazard ratios, the log-rank test, the
Grambsch and Therneau test for each comparison and overall, and the
group by time and group by log time interactions with their joint Wald
tests, with a note on what each test asks. The table is also returned as
the field `ph`. It belongs to the widget, so static copies from
[`graph_plot()`](https://choxos.github.io/ggextreme/reference/graph_widget.md)
and
[`graph_save()`](https://choxos.github.io/ggextreme/reference/graph_widget.md)
leave it out.

## Examples

``` r
if (requireNamespace("survival", quietly = TRUE)) {
  colon <- subset(survival::colon, etype == 2)
  colon$years <- colon$time / 365.25
  km <- ggkm(survival::Surv(years, status) ~ rx, data = colon,
             ph_tests = TRUE, xlab = "Years since randomization")
  km
  km$ph
}
#>                    test     comparison                    estimate
#> 1             Cox model     Lev vs Obs        HR 0.97 (0.78, 1.21)
#> 2             Cox model Lev+5FU vs Obs        HR 0.69 (0.55, 0.87)
#> 3         Log-rank test     All groups                            
#> 4  Schoenfeld residuals     Lev vs Obs                            
#> 5  Schoenfeld residuals Lev+5FU vs Obs                            
#> 6  Schoenfeld residuals         Global                            
#> 7          Group × time     Lev vs Obs     -0.071 per unit of time
#> 8          Group × time Lev+5FU vs Obs     -0.073 per unit of time
#> 9          Group × time          Joint                            
#> 10     Group × log time     Lev vs Obs -0.164 per unit of log time
#> 11     Group × log time Lev+5FU vs Obs -0.264 per unit of log time
#> 12     Group × log time          Joint                            
#>           statistic           p
#> 1         z = -0.24 0.809174301
#> 2         z = -3.13 0.001747549
#> 3  χ² = 11.68, df 2 0.002904348
#> 4         χ² = 0.19 0.666290517
#> 5         χ² = 0.66 0.416696850
#> 6   χ² = 1.48, df 2 0.476943445
#> 7         z = -1.06 0.287630028
#> 8         z = -1.03 0.305010710
#> 9   χ² = 1.51, df 2 0.469594021
#> 10        z = -1.14 0.253301079
#> 11        z = -1.75 0.080646010
#> 12  χ² = 3.15, df 2 0.207143210
```
