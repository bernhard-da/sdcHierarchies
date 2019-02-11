.onAttach <- function(lib, pkg) {
  options(useFancyQuotes = FALSE)
  txt <- paste(
    "Package", shQuote(pkg),
    utils::packageVersion(pkg),
    "has been loaded."
  )
  packageStartupMessage(txt)
}
