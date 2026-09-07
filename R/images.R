#' Bundled circular country flags
#'
#' Returns file paths to circular flag images, ready to hand to the `images`
#' argument of [ggrace()]. The package ships one flag per ISO 3166-1 country,
#' plus Kurdistan, mostly drawn from the same artwork `gt::fmt_flag()` uses.
#' See `inst/extdata/flags/SOURCE.txt` for the provenance.
#'
#' Lookup tries, in order: the two letter code, the three letter code, the
#' full country name, and finally a unique partial match on the name. Names
#' follow the World Bank style used by 'gt', so a few common spellings do not
#' match: pass the code for those, and use [race_flag_codes()] to find it.
#'
#' @param country Country names or ISO codes, in any mix.
#'
#' @return A character vector of file paths, named by the input.
#' @export
#'
#' @examples
#' race_flags(c("br", "Iran", "Kenya"))
#'
#' # "Turkey" and "South Korea" are spelled differently in the table, so pass
#' # their codes instead.
#' race_flags(c("tr", "kr"))
race_flags <- function(country) {
  if (!length(country)) return(stats::setNames(character(0), character(0)))
  tab <- flag_lookup()
  key <- tolower(trimws(as.character(country)))

  idx <- match(key, tab$code)
  idx <- ifelse(is.na(idx), match(key, tab$code3), idx)
  idx <- ifelse(is.na(idx), match(key, tolower(tab$country)), idx)
  # Anything three characters or shorter is a code, not a name, so it must
  # match exactly. A longer string may name the start of a country, but only
  # when exactly one country starts that way.
  for (i in which(is.na(idx) & nchar(key) > 3)) {
    hit <- which(startsWith(tolower(tab$country), key[i]))
    if (length(hit) == 1) idx[i] <- hit
  }
  if (anyNA(idx)) {
    rlang::abort(paste0(
      "No flag for: ", paste(unique(country[is.na(idx)]), collapse = ", "),
      ". See race_flag_codes() for the names and codes that are available."
    ))
  }

  paths <- system.file("extdata", "flags", paste0(tab$code[idx], ".svg"),
                       package = "ggextreme")
  stats::setNames(paths, as.character(country))
}

#' Countries the bundled flags cover
#'
#' @return A data frame with the two letter code, the three letter code and
#'   the country name of every bundled flag.
#' @export
#'
#' @examples
#' head(race_flag_codes())
#' subset(race_flag_codes(), grepl("Korea", country))
race_flag_codes <- function() {
  flag_lookup()
}

flag_lookup <- function() {
  file <- system.file("extdata", "flags", "countries.csv", package = "ggextreme")
  if (!nzchar(file)) rlang::abort("The bundled flags are missing.")
  utils::read.csv(file, stringsAsFactors = FALSE)
}

# Internals -------------------------------------------------------------

# Images are read once, at twice the size they are drawn at, so the renderer
# downsamples rather than stretching a small picture up.
prepare_images <- function(images, px, entities = NULL) {
  if (is.null(images) || !length(images)) return(NULL)
  if (is.null(names(images)) || any(!nzchar(names(images)))) {
    rlang::abort("`images` must be named by the entity each image belongs to.")
  }
  if (!is.null(entities)) {
    images <- images[names(images) %in% entities]
    if (!length(images)) return(NULL)
  }
  if (!requireNamespace("magick", quietly = TRUE)) {
    rlang::abort(c(
      "Drawing images on the bars needs the magick package.",
      i = 'Install it with install.packages("magick").'
    ))
  }
  keys <- names(images)
  paths <- as.character(images)
  missing <- paths[!file.exists(paths)]
  if (length(missing)) {
    rlang::abort(paste0("Cannot find image file: ",
                        paste(utils::head(missing, 3), collapse = ", ")))
  }
  stats::setNames(lapply(paths, read_circle, px = px), keys)
}

read_circle <- function(path, px) {
  im <- if (grepl("\\.svg$", path, ignore.case = TRUE)) {
    if (!requireNamespace("rsvg", quietly = TRUE)) {
      rlang::abort(c(
        "Reading an SVG image needs the rsvg package.",
        i = 'Install it with install.packages("rsvg"), or use a PNG.'
      ))
    }
    # Width only, so a picture that is not square keeps its proportions and
    # is cropped below rather than squashed.
    magick::image_read_svg(path, width = px)
  } else {
    magick::image_read(path)
  }
  im <- magick::image_resize(im, sprintf("%dx%d^", px, px))
  im <- magick::image_crop(im, sprintf("%dx%d", px, px), gravity = "center")
  circle_mask(grDevices::as.raster(im))
}

circle_mask <- function(r) {
  rows <- (row(r) - (nrow(r) + 1) / 2) / (nrow(r) / 2)
  cols <- (col(r) - (ncol(r) + 1) / 2) / (ncol(r) / 2)
  r[rows^2 + cols^2 > 1] <- "#FFFFFF00"
  r
}

# One layer holding every image on the frame. The panel spans the whole page
# with no expansion, so a card coordinate divided by the page size is exactly
# its position in npc, and no coordinate transform is needed.
image_layer <- function(df, rasters, d, page_w, page_h) {
  if (!nrow(df)) return(NULL)
  children <- do.call(grid::gList, lapply(seq_len(nrow(df)), function(i) {
    grid::rasterGrob(
      rasters[[df$key[i]]],
      x = grid::unit(df$x[i] / page_w, "npc"),
      y = grid::unit(df$y[i] / page_h, "npc"),
      width = grid::unit(d / page_w, "npc"),
      height = grid::unit(d / page_h, "npc"),
      interpolate = TRUE
    )
  }))
  annotation_custom(grid::gTree(children = children), -Inf, Inf, -Inf, Inf)
}
