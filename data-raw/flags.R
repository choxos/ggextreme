# Circular country flags, one SVG per ISO 3166-1 alpha-2 code.
#
# The artwork comes from the flag-icons project (MIT, Panayiotis Lipiridis),
# by way of the gt package, which uses the same set for gt::fmt_flag(). It is
# copied out here at build time so ggextreme carries the files itself and
# needs no dependency on gt at run time.
#
# Run with: source("data-raw/flags.R")

stopifnot(requireNamespace("gt", quietly = TRUE))

flags <- as.data.frame(get("flag_tbl", asNamespace("gt")))
dir <- "inst/extdata/flags"
unlink(dir, recursive = TRUE)
dir.create(dir, recursive = TRUE)

for (i in seq_len(nrow(flags))) {
  writeLines(flags$country_flag[i],
             file.path(dir, paste0(tolower(flags$country_code_2[i]), ".svg")))
}

lookup <- data.frame(
  code = tolower(flags$country_code_2),
  code3 = tolower(flags$country_code_3),
  country = flags$country_name,
  stringsAsFactors = FALSE
)

# Kurdistan is not an ISO 3166-1 country and so is not in the gt set. Its flag
# comes from Wikimedia Commons (public domain) and is wrapped in the same
# 512 by 512 circular mask the other flags use: scaled to cover the square,
# centered, then masked.
kurdistan <- paste(
  readLines("https://upload.wikimedia.org/wikipedia/commons/3/35/Flag_of_Kurdistan.svg",
            warn = FALSE),
  collapse = "\n"
)
inner <- sub("(?s)^.*?<svg[^>]*>", "", kurdistan, perl = TRUE)
inner <- sub("(?s)</svg>\\s*$", "", inner, perl = TRUE)
# Take the largest square window centered on the source flag and mask it with
# a circle, all in the source coordinate system, so no transform is involved
# and any renderer places the sun in the middle.
w <- as.numeric(sub('(?s).*<svg[^>]*width="([0-9.]+)".*', "\\1", kurdistan, perl = TRUE))
h <- as.numeric(sub('(?s).*<svg[^>]*height="([0-9.]+)".*', "\\1", kurdistan, perl = TRUE))
side <- min(w, h)
writeLines(sprintf(paste0(
  '<svg xmlns="http://www.w3.org/2000/svg" width="512" height="512" ',
  'viewBox="%g %g %g %g"><mask id="a">',
  '<circle cx="%g" cy="%g" r="%g" fill="#fff"/></mask>',
  '<g mask="url(#a)">%s</g></svg>'
), (w - side) / 2, (h - side) / 2, side, side,
   w / 2, h / 2, side / 2, inner), file.path(dir, "krd.svg"))

lookup <- rbind(lookup, data.frame(code = "krd", code3 = "krd",
                                   country = "Kurdistan"))

write.csv(lookup, file.path(dir, "countries.csv"), row.names = FALSE)

cat(nrow(lookup), "flags,", round(sum(file.size(list.files(dir, full.names = TRUE))) / 1024), "KB\n")
