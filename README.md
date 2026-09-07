# ggextreme

Presentation grade charts that ggplot2 does not ship with. The first one is a
bar chart race built to match the [flourish.studio](https://flourish.studio)
design: smooth continuous overtaking, a fixed label column, a top axis that
rescales with the field, the big translucent year in the corner and a
timeline with a moving marker.

Every frame is an ordinary `ggplot` object, so nothing is hidden behind a
rendering engine you cannot inspect.

## Install

```r
# install.packages("remotes")
remotes::install_github("choxos/ggextreme")
```

Writing output needs an encoder: `gifski` or `magick` for GIF, `av` or an
`ffmpeg` binary for MP4.

## Use

```r
library(ggextreme)

phones <- as.data.frame.table(datasets::WorldPhones, responseName = "phones")
names(phones)[1:2] <- c("year", "region")
phones$year <- as.numeric(as.character(phones$year))

race <- ggrace(
  phones, value = phones, name = region, time = year,
  top_n = 7,
  duration = 20,
  title = "Telephones in use, 1951 to 1961",
  caption = "Source: AT&T"
)

race_frame(race, 200)          # inspect one frame
animate_race(race, "phones.mp4")
```

`ggrace()` takes long data: one row per entity per time point, with bare
column names for the value, the label and the time. `time` may be numeric or
a `Date`. Everything else has a default that reproduces the reference design.

Useful arguments:

| argument | what it does |
| --- | --- |
| `top_n` | how many bars are visible at once |
| `duration`, `fps`, `end_pause` | length in seconds, frame rate, hold on the last frame |
| `swap` | seconds a bar takes to move into a new rank |
| `palette` | a color vector, or one named by entity |
| `label_value`, `label_time` | formatters for the bar numbers and the big time label |
| `timeline`, `play_button`, `card` | turn off the timeline strip, the pause button, or the card and its shadow |
| `width`, `res` | output size; the whole layout scales with `width` |

`animate_race()` picks its encoder from the file extension and draws frames
across cores by default, about a third of a second per frame on eight cores.
A 30 second race at 30 fps is 900 frames, so roughly four minutes.

## How the motion works

The reference does two different things, and copying only one of them is
what makes most R bar races look wrong. `ggrace()` does both:

* Values are interpolated on a **uniform real time grid**, so bars grow at a
  steady rate and the timeline marker moves at constant speed even when the
  keyframes are unevenly spaced.
* **Position is tweened, not interpolated.** Every frame is ranked on its own
  values, and a bar that changes rank eases into the new slot over `swap`
  seconds, defaulting to the fifth of a second the reference takes. Bars hold
  their slot and trade places in one quick move instead of drifting between
  slots for a whole time step.
* Ranks are **clamped to `top_n + 1`**, so an entity far down the field waits
  just below the visible window and enters from the bottom edge rather than
  flying in from off screen.
* The label column has a **fixed width** and the panel geometry never depends
  on which entities are currently visible, so nothing drifts sideways between
  frames.
* Colors are assigned once over the whole field, so an entity keeps its color
  when it leaves and comes back.

## Design provenance

The layout is not an approximation. Card width, bar pitch, gutters, rule
weights, tick lengths, ink colors and font sizes were all measured off a
reference recording frame by frame, and the package ships Lato, the font the
reference uses. `R/layout.R` holds those measurements in one place; change
the numbers there to restyle the whole chart.

## License

MIT. Lato is bundled under the SIL Open Font License, see
`inst/fonts/OFL.txt`.
