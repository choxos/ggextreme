.onLoad <- function(libname, pkgname) {
  register_lato(system.file("fonts", package = pkgname))
  invisible()
}

# The charts are set in Lato, so the package ships it. Registration is
# skipped when Lato is already available, either as a real system font or
# because another package registered it first.
register_lato <- function(dir) {
  plain <- file.path(dir, "Lato-Regular.ttf")
  bold <- file.path(dir, "Lato-Bold.ttf")
  if (!file.exists(plain) || !file.exists(bold)) return(invisible(FALSE))
  known <- c(systemfonts::system_fonts()$family,
             systemfonts::registry_fonts()$family)
  if ("Lato" %in% known) return(invisible(FALSE))
  try(systemfonts::register_font("Lato", plain = plain, bold = bold),
      silent = TRUE)
  invisible(TRUE)
}
