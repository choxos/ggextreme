# ggextreme: Bar Chart Races and Interactive Plots for Clinical Research

Presentation quality charts built on 'ggplot2' that the package itself
does not provide. The bar chart race interpolates values on a uniform
time grid, ranks every frame on its own values and eases bars into new
positions, so a reordering field stays readable. Frames are ordinary
'ggplot2' objects laid out on a fixed pixel grid, which keeps the axis
and label column from drifting between them, and are encoded to GIF or
MP4. Circular images, such as the bundled country flags, can be placed
at the end of each bar. Causal diagrams are drawn as directed acyclic
graphs whose nodes and arrows each carry a rationale and references,
shown on hover and opened with clickable links on click, through
'ggiraph'; the same diagram is also available as a static 'ggplot2'
object. Network plots for network meta-analysis are drawn from arm level
data, with the baseline characteristics and outcomes of every arm shown
side by side on click. Forest plots for 'metafor' and 'meta' fits carry
each study's record and risk of bias traffic lights, and replay a
cumulative meta-analysis as an animation; league tables for 'netmeta'
fits show the direct and indirect evidence behind every estimate.
Kaplan-Meier plots read survival and the hazard ratio at any time under
the pointer, beside a risk table and proportional hazards tests.
Choropleth maps of the world or of any 'sf' map step or play through the
years, with several measures side by side for the same year.

## See also

Useful links:

- <https://choxos.github.io/ggextreme/>

- <https://github.com/choxos/ggextreme>

- Report bugs at <https://github.com/choxos/ggextreme/issues>

## Author

**Maintainer**: Ahmad Sofi-Mahmudi <ahmad.pub@gmail.com>

Authors:

- Ahmad Sofi-Mahmudi <ahmad.pub@gmail.com>
