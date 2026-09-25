# Package index

## Building a race

Turn a panel of observations into frames, then draw or encode them.

- [`ggrace()`](https://choxos.github.io/ggextreme/reference/ggrace.md) :
  Build a bar chart race
- [`race_frame()`](https://choxos.github.io/ggextreme/reference/race_frame.md)
  : Draw one frame of a bar chart race
- [`race_size()`](https://choxos.github.io/ggextreme/reference/race_size.md)
  : The pixel size of a race frame
- [`animate_race()`](https://choxos.github.io/ggextreme/reference/animate_race.md)
  : Render a bar chart race to a file

## Appearance

Colors, theme and the images that sit on the bars.

- [`race_palette()`](https://choxos.github.io/ggextreme/reference/race_palette.md)
  : Colors used by the bar chart race
- [`theme_race()`](https://choxos.github.io/ggextreme/reference/theme_race.md)
  : The bare theme a race frame is drawn on
- [`race_flags()`](https://choxos.github.io/ggextreme/reference/race_flags.md)
  : Bundled circular country flags
- [`race_flag_codes()`](https://choxos.github.io/ggextreme/reference/race_flag_codes.md)
  : Countries the bundled flags cover

## Causal diagrams

Draw a DAG whose nodes and arrows explain themselves.

- [`ggcausal()`](https://choxos.github.io/ggextreme/reference/ggcausal.md)
  : Draw an interactive causal diagram

## Network plots

Draw the network of a network meta-analysis from arm level data.

- [`ggnma()`](https://choxos.github.io/ggextreme/reference/ggnma.md) :
  Draw an interactive network plot for a network meta-analysis

## Meta-analysis

Forest plots and league tables for fitted models.

- [`ggmeta()`](https://choxos.github.io/ggextreme/reference/ggmeta.md) :
  Draw an interactive forest plot for a meta-analysis
- [`animate_meta()`](https://choxos.github.io/ggextreme/reference/animate_meta.md)
  : Animate a cumulative meta-analysis
- [`ggfunnel()`](https://choxos.github.io/ggextreme/reference/ggfunnel.md)
  : Draw an interactive funnel plot for a meta-analysis
- [`ggleague()`](https://choxos.github.io/ggextreme/reference/ggleague.md)
  : Draw an interactive league table for a network meta-analysis

## Confidence in a network meta-analysis

Judge every estimate of a network meta-analysis in CINeMA’s six domains,
and see what lies behind each judgment.

- [`cinema_judge()`](https://choxos.github.io/ggextreme/reference/cinema_judge.md)
  : Judge confidence in the results of a network meta-analysis
- [`cinema_league()`](https://choxos.github.io/ggextreme/reference/cinema_league.md)
  : Draw a league table with a CINeMA confidence profile in every cell
- [`cinema_contribution()`](https://choxos.github.io/ggextreme/reference/cinema_contribution.md)
  : Draw where each network estimate's evidence comes from, study by
  study
- [`cinema_clinical()`](https://choxos.github.io/ggextreme/reference/cinema_clinical.md)
  : Draw network estimates against a range of little difference
- [`cinema_incoherence()`](https://choxos.github.io/ggextreme/reference/cinema_incoherence.md)
  : Draw direct, indirect and network estimates side by side
- [`cinema_network()`](https://choxos.github.io/ggextreme/reference/cinema_network.md)
  : Draw a network plot with a strand for every study, colored by its
  judgment

## Survival and trials

Kaplan-Meier plots with a linked risk table, proportional hazards tests
and restricted mean survival, swimmer plots of each patient’s course
with a waterfall and trajectories, and responder thresholds.

- [`ggkm()`](https://choxos.github.io/ggextreme/reference/ggkm.md) :
  Draw an interactive Kaplan-Meier plot
- [`animate_km()`](https://choxos.github.io/ggextreme/reference/animate_km.md)
  : Animate a Kaplan-Meier plot
- [`ggswimmer()`](https://choxos.github.io/ggextreme/reference/ggswimmer.md)
  : Draw an interactive swimmer plot
- [`ggresponder()`](https://choxos.github.io/ggextreme/reference/ggresponder.md)
  : Draw an interactive responder threshold plot

## Tests, bias and robustness

Explore the cutoff of a diagnostic test, the strength of unmeasured
confounding and a multiverse of analyses.

- [`ggdiagnostic()`](https://choxos.github.io/ggextreme/reference/ggdiagnostic.md)
  : Draw an interactive diagnostic threshold explorer
- [`ggsensitivity()`](https://choxos.github.io/ggextreme/reference/ggsensitivity.md)
  : Draw an interactive bias and tipping point explorer
- [`ggmultiverse()`](https://choxos.github.io/ggextreme/reference/ggmultiverse.md)
  : Draw an interactive multiverse of analyses

## Prediction models

Nomograms of regression models, with a handle per predictor and the
prediction computed in the page.

- [`ggnomogram()`](https://choxos.github.io/ggextreme/reference/ggnomogram.md)
  : Draw an interactive nomogram for a regression model
- [`nomogram_predict()`](https://choxos.github.io/ggextreme/reference/nomogram_predict.md)
  : Predict from a nomogram

## Maps

Choropleth maps that step or play through the years, with measures side
by side.

- [`ggchoropleth()`](https://choxos.github.io/ggextreme/reference/ggchoropleth.md)
  : Draw an interactive choropleth map over time
- [`animate_choropleth()`](https://choxos.github.io/ggextreme/reference/animate_choropleth.md)
  : Animate a choropleth map

## Using a graph

Turn any of the interactive graphs into a widget, a ggplot or a file.

- [`graph_widget()`](https://choxos.github.io/ggextreme/reference/graph_widget.md)
  [`graph_plot()`](https://choxos.github.io/ggextreme/reference/graph_widget.md)
  [`graph_save()`](https://choxos.github.io/ggextreme/reference/graph_widget.md)
  [`knit_print(`*`<ggx_graph>`*`)`](https://choxos.github.io/ggextreme/reference/graph_widget.md)
  [`knit_print(`*`<ggrace>`*`)`](https://choxos.github.io/ggextreme/reference/graph_widget.md)
  : Use an interactive graph as a widget, a ggplot or a file

## Data

- [`clefts_qci`](https://choxos.github.io/ggextreme/reference/clefts_qci.md)
  : Quality of Care Index for orofacial clefts, 1990 to 2019
- [`clefts_qci_world`](https://choxos.github.io/ggextreme/reference/clefts_qci_world.md)
  : Quality of Care Index for orofacial clefts in every country, 1990 to
  2019
- [`cleft_dag`](https://choxos.github.io/ggextreme/reference/cleft_dag.md)
  : An illustrative causal diagram for maternal smoking and orofacial
  clefts
- [`psoriasis_nma`](https://choxos.github.io/ggextreme/reference/psoriasis_nma.md)
  : Arm level data from five trials in plaque psoriasis

## Package

- [`ggextreme`](https://choxos.github.io/ggextreme/reference/ggextreme-package.md)
  [`ggextreme-package`](https://choxos.github.io/ggextreme/reference/ggextreme-package.md)
  : ggextreme: Bar Chart Races and Interactive Plots for Clinical
  Research
