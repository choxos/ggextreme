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

# Iran, as a plain green, white and red tricolour without the central emblem
# or the marginal script.
writeLines(paste0(
  '<svg xmlns="http://www.w3.org/2000/svg" width="512" height="512" ',
  'viewBox="0 0 512 512"><mask id="a">',
  '<circle cx="256" cy="256" r="256" fill="#fff"/></mask>',
  '<g mask="url(#a)">',
  '<path fill="#239f40" d="M0 0h512v170.7H0z"/>',
  '<path fill="#fff" d="M0 170.7h512v170.6H0z"/>',
  '<path fill="#da0000" d="M0 341.3h512V512H0z"/>',
  '</g></svg>'
), file.path(dir, "ir.svg"))

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

writeLines(c(
  "Flag artwork",
  "============",
  "",
  "One circular SVG per ISO 3166-1 alpha-2 country code, plus Kurdistan.",
  "countries.csv maps codes and country names to those files.",
  "",
  "Most of the artwork comes from the flag-icons project",
  "(https://github.com/lipis/flag-icons), by way of the gt package, which",
  "uses the same set for gt::fmt_flag(). flag-icons is released under the MIT",
  "License, Copyright (c) 2013 Panayiotis Lipiridis.",
  "",
  "Two files differ. krd.svg is the flag of Kurdistan, from Wikimedia",
  "Commons, cropped to a square and masked with a circle. ir.svg is the plain",
  "green, white and red tricolour of Iran, drawn for this package.",
  "",
  "See data-raw/flags.R for the script that generates all of them."
), file.path(dir, "SOURCE.txt"))

cat(nrow(lookup), "flags,", round(sum(file.size(list.files(dir, full.names = TRUE))) / 1024), "KB\n")
