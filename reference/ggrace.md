# Build a bar chart race

Turns a long panel of `name`, `time`, `value` observations into a bar
chart race and returns one row per entity per frame. Draw a single frame
with
[`race_frame()`](https://choxos.github.io/ggextreme/reference/race_frame.md)
or write the whole animation with
[`animate_race()`](https://choxos.github.io/ggextreme/reference/animate_race.md).

## Usage

``` r
ggrace(
  data,
  value,
  name,
  time,
  top_n = 10,
  duration = 25,
  fps = 60,
  end_pause = 2,
  swap = 0.2,
  palette = NULL,
  title = NULL,
  caption = NULL,
  breaks = scales::breaks_extended(4),
  label_value = scales::label_comma(accuracy = 1),
  label_time = NULL,
  images = NULL,
  image_size = 0.87,
  timeline = TRUE,
  play_button = TRUE,
  card = TRUE,
  width = 736,
  res = 100,
  family = "Lato"
)
```

## Arguments

- data:

  A data frame in long format.

- value, name, time:

  Bare column names holding the bar length, the bar label, and the time
  point. `time` may be numeric or a `Date`. Each `name` and `time` pair
  must be unique.

- top_n:

  Number of bars visible at once.

- duration:

  Length of the animation in seconds, excluding `end_pause`.

- fps:

  Frames per second.

- end_pause:

  Seconds to hold the final frame.

- swap:

  Seconds a bar takes to move to a new rank. Bars hold their position
  and change places in one quick eased move rather than drifting the
  whole way between one time point and the next.

- palette:

  Colors for the bars. Either an unnamed vector, recycled over the
  entities in alphabetical order, or a vector named by entity. Defaults
  to
  [`race_palette()`](https://choxos.github.io/ggextreme/reference/race_palette.md).

- title, caption:

  Card title and the source note under the timeline.

- breaks:

  A function taking the axis range and returning the gridline positions,
  or a numeric vector of fixed positions. Breaks past the current
  maximum are dropped, so the axis fills in as the field grows.

- label_value:

  A function formatting the number printed after each bar.

- label_time:

  A function formatting the large time label in the corner. Defaults to
  the floor of the interpolated time for numeric input and the year for
  dates.

- images:

  Pictures to sit at the end of each bar, as a character vector of image
  file paths named by entity. Entities with no image get none.
  [`race_flags()`](https://choxos.github.io/ggextreme/reference/race_flags.md)
  returns paths to the bundled country flags. Needs the magick package.

- image_size:

  Image diameter as a fraction of the bar height. Images are cropped to
  a circle and right aligned just inside the end of the bar.

- timeline:

  Draw the timeline strip with the moving marker.

- play_button:

  Draw the round pause button next to the timeline.

- card:

  Draw the chart on a white card with a drop shadow over a light page.
  Set to `FALSE` for a plain white background.

- width:

  Output width in pixels. Frames are laid out for this width, so it also
  fixes every font size.

- res:

  Output resolution in pixels per inch.

- family:

  Font family. The package ships Lato and registers it on load.

## Value

An object of class `ggrace`.

## Details

Values move linearly and ranks do not. Every frame is ranked on its own
interpolated values, and a bar whose rank changes eases into its new
slot over `swap` seconds. Bars therefore rest in place and trade
positions in one short move, rather than drifting for a whole time step,
which is what keeps a crowded field readable while it reorders.

Ranks are clamped to `top_n + 1`. An entity far down the field therefore
waits just below the visible window and slides in from the bottom edge
instead of flying up from off screen, which is what makes entries and
exits read cleanly.

The x axis carries no headroom. The longest bar always reaches the right
edge of the plotting area and the axis maximum is the largest value in
the current frame, so gridlines drift as the field grows.

## Examples

``` r
phones <- as.data.frame.table(datasets::WorldPhones, responseName = "phones")
names(phones)[1:2] <- c("year", "region")
phones$year <- as.numeric(as.character(phones$year))

race <- ggrace(phones, phones, region, year, top_n = 7, duration = 5)
race
#> <ggrace>
#>  entities: 7 (top 7 shown)
#>  keyframes: 7 
#>  frames:   300 at 60 fps

# Frames are set in Lato, which only devices that understand registered
# fonts can use, so draw them with ragg rather than the default device.
file <- tempfile(fileext = ".png")
ragg::agg_png(file, width = race$width, height = 500, units = "px",
               res = race$res)
print(race_frame(race, 60))
dev.off()
#> agg_record_1e7c4a4aeb5e 
#>                       2 
# \donttest{
animate_race(race, tempfile(fileext = ".gif"), cores = 1)
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |                                                                      |   1%
#>   |                                                                              |=                                                                     |   1%
#>   |                                                                              |=                                                                     |   2%
#>   |                                                                              |==                                                                    |   2%
#>   |                                                                              |==                                                                    |   3%
#>   |                                                                              |==                                                                    |   4%
#>   |                                                                              |===                                                                   |   4%
#>   |                                                                              |===                                                                   |   5%
#>   |                                                                              |====                                                                  |   5%
#>   |                                                                              |====                                                                  |   6%
#>   |                                                                              |=====                                                                 |   7%
#>   |                                                                              |=====                                                                 |   8%
#>   |                                                                              |======                                                                |   8%
#>   |                                                                              |======                                                                |   9%
#>   |                                                                              |=======                                                               |  10%
#>   |                                                                              |========                                                              |  11%
#>   |                                                                              |========                                                              |  12%
#>   |                                                                              |=========                                                             |  12%
#>   |                                                                              |=========                                                             |  13%
#>   |                                                                              |==========                                                            |  14%
#>   |                                                                              |==========                                                            |  15%
#>   |                                                                              |===========                                                           |  15%
#>   |                                                                              |===========                                                           |  16%
#>   |                                                                              |============                                                          |  16%
#>   |                                                                              |============                                                          |  17%
#>   |                                                                              |============                                                          |  18%
#>   |                                                                              |=============                                                         |  18%
#>   |                                                                              |=============                                                         |  19%
#>   |                                                                              |==============                                                        |  19%
#>   |                                                                              |==============                                                        |  20%
#>   |                                                                              |==============                                                        |  21%
#>   |                                                                              |===============                                                       |  21%
#>   |                                                                              |===============                                                       |  22%
#>   |                                                                              |================                                                      |  22%
#>   |                                                                              |================                                                      |  23%
#>   |                                                                              |================                                                      |  24%
#>   |                                                                              |=================                                                     |  24%
#>   |                                                                              |=================                                                     |  25%
#>   |                                                                              |==================                                                    |  25%
#>   |                                                                              |==================                                                    |  26%
#>   |                                                                              |===================                                                   |  27%
#>   |                                                                              |===================                                                   |  28%
#>   |                                                                              |====================                                                  |  28%
#>   |                                                                              |====================                                                  |  29%
#>   |                                                                              |=====================                                                 |  30%
#>   |                                                                              |======================                                                |  31%
#>   |                                                                              |======================                                                |  32%
#>   |                                                                              |=======================                                               |  32%
#>   |                                                                              |=======================                                               |  33%
#>   |                                                                              |========================                                              |  34%
#>   |                                                                              |========================                                              |  35%
#>   |                                                                              |=========================                                             |  35%
#>   |                                                                              |=========================                                             |  36%
#>   |                                                                              |==========================                                            |  36%
#>   |                                                                              |==========================                                            |  37%
#>   |                                                                              |==========================                                            |  38%
#>   |                                                                              |===========================                                           |  38%
#>   |                                                                              |===========================                                           |  39%
#>   |                                                                              |============================                                          |  39%
#>   |                                                                              |============================                                          |  40%
#>   |                                                                              |============================                                          |  41%
#>   |                                                                              |=============================                                         |  41%
#>   |                                                                              |=============================                                         |  42%
#>   |                                                                              |==============================                                        |  42%
#>   |                                                                              |==============================                                        |  43%
#>   |                                                                              |==============================                                        |  44%
#>   |                                                                              |===============================                                       |  44%
#>   |                                                                              |===============================                                       |  45%
#>   |                                                                              |================================                                      |  45%
#>   |                                                                              |================================                                      |  46%
#>   |                                                                              |=================================                                     |  47%
#>   |                                                                              |=================================                                     |  48%
#>   |                                                                              |==================================                                    |  48%
#>   |                                                                              |==================================                                    |  49%
#>   |                                                                              |===================================                                   |  50%
#>   |                                                                              |====================================                                  |  51%
#>   |                                                                              |====================================                                  |  52%
#>   |                                                                              |=====================================                                 |  52%
#>   |                                                                              |=====================================                                 |  53%
#>   |                                                                              |======================================                                |  54%
#>   |                                                                              |======================================                                |  55%
#>   |                                                                              |=======================================                               |  55%
#>   |                                                                              |=======================================                               |  56%
#>   |                                                                              |========================================                              |  56%
#>   |                                                                              |========================================                              |  57%
#>   |                                                                              |========================================                              |  58%
#>   |                                                                              |=========================================                             |  58%
#>   |                                                                              |=========================================                             |  59%
#>   |                                                                              |==========================================                            |  59%
#>   |                                                                              |==========================================                            |  60%
#>   |                                                                              |==========================================                            |  61%
#>   |                                                                              |===========================================                           |  61%
#>   |                                                                              |===========================================                           |  62%
#>   |                                                                              |============================================                          |  62%
#>   |                                                                              |============================================                          |  63%
#>   |                                                                              |============================================                          |  64%
#>   |                                                                              |=============================================                         |  64%
#>   |                                                                              |=============================================                         |  65%
#>   |                                                                              |==============================================                        |  65%
#>   |                                                                              |==============================================                        |  66%
#>   |                                                                              |===============================================                       |  67%
#>   |                                                                              |===============================================                       |  68%
#>   |                                                                              |================================================                      |  68%
#>   |                                                                              |================================================                      |  69%
#>   |                                                                              |=================================================                     |  70%
#>   |                                                                              |==================================================                    |  71%
#>   |                                                                              |==================================================                    |  72%
#>   |                                                                              |===================================================                   |  72%
#>   |                                                                              |===================================================                   |  73%
#>   |                                                                              |====================================================                  |  74%
#>   |                                                                              |====================================================                  |  75%
#>   |                                                                              |=====================================================                 |  75%
#>   |                                                                              |=====================================================                 |  76%
#>   |                                                                              |======================================================                |  76%
#>   |                                                                              |======================================================                |  77%
#>   |                                                                              |======================================================                |  78%
#>   |                                                                              |=======================================================               |  78%
#>   |                                                                              |=======================================================               |  79%
#>   |                                                                              |========================================================              |  79%
#>   |                                                                              |========================================================              |  80%
#>   |                                                                              |========================================================              |  81%
#>   |                                                                              |=========================================================             |  81%
#>   |                                                                              |=========================================================             |  82%
#>   |                                                                              |==========================================================            |  82%
#>   |                                                                              |==========================================================            |  83%
#>   |                                                                              |==========================================================            |  84%
#>   |                                                                              |===========================================================           |  84%
#>   |                                                                              |===========================================================           |  85%
#>   |                                                                              |============================================================          |  85%
#>   |                                                                              |============================================================          |  86%
#>   |                                                                              |=============================================================         |  87%
#>   |                                                                              |=============================================================         |  88%
#>   |                                                                              |==============================================================        |  88%
#>   |                                                                              |==============================================================        |  89%
#>   |                                                                              |===============================================================       |  90%
#>   |                                                                              |================================================================      |  91%
#>   |                                                                              |================================================================      |  92%
#>   |                                                                              |=================================================================     |  92%
#>   |                                                                              |=================================================================     |  93%
#>   |                                                                              |==================================================================    |  94%
#>   |                                                                              |==================================================================    |  95%
#>   |                                                                              |===================================================================   |  95%
#>   |                                                                              |===================================================================   |  96%
#>   |                                                                              |====================================================================  |  96%
#>   |                                                                              |====================================================================  |  97%
#>   |                                                                              |====================================================================  |  98%
#>   |                                                                              |===================================================================== |  98%
#>   |                                                                              |===================================================================== |  99%
#>   |                                                                              |======================================================================|  99%
#>   |                                                                              |======================================================================| 100%
#> 
# }
```
