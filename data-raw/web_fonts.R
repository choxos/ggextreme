# The web copy of Lato embedded in the interactive graphs.
#
# The bundled TTFs cover far more of Unicode than a diagram label needs, so
# the widget ships a WOFF2 subset: Latin, Greek, general punctuation, arrows
# and a few math signs. Glyph metrics are unchanged by subsetting, so text
# measured in R fits its box in the browser. Needs fontTools with brotli.
#
# Run with: source("data-raw/web_fonts.R")

ranges <- paste(
  "U+0020-007E", "U+00A0-024F", "U+0370-03FF", "U+2000-206F", "U+2190-21FF",
  "U+2212", "U+2264", "U+2265", "U+2248", "U+00B1",
  sep = ","
)
for (weight in c("Regular", "Bold")) {
  system2("pyftsubset", c(
    file.path("inst/fonts", paste0("Lato-", weight, ".ttf")),
    paste0("--unicodes=", ranges),
    "--flavor=woff2",
    "--layout-features=*",
    paste0("--output-file=inst/www/lato-", tolower(weight), ".woff2")
  ))
}
