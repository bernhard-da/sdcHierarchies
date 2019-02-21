#' Export a hierarchy into a file
#'
#' This function allows to write nested hierarchies into files
#' on your disk.
#'
#' @inherit hier_convert
#' @param path (character) relative or absolute path where results should
#' be written to
#' @param verbose (logical) additional results
#' @export
#' @examples
#' h <- hier_create(rootnode = "Total", leaves = LETTERS[1:2])
#' h <- hier_add(h, node = "A", leaves = c("a1", "a2"))
#' h <- hier_add(h, node = "B", leaves = c("b1", "b2"))
#' h <- hier_add(h, node = "b1", leaves = "b1a")
#' hier_display(h)
#'
#' # export as input for tauArgus
#' hier_export(h, format = "argus", path = "h.hrc")
hier_export <- function(tree, format="df", path, verbose=FALSE) {
  .check_path  <- function(path) {
    if (file.exists(path)) {
      stop(paste("File", shQuote(path), "already exists!"), call. = FALSE)
    }
    res <- file.create(path, showWarnings = FALSE)
    if (!res) {
      err <- paste("File", shQuote(path), "could not be created!")
      err <- paste(err, "Please provide another path")
      stop(err, call. = FALSE)
    }
    res <- file.remove(path)
    return(invisible(TRUE))
  }

  stopifnot(is_scalar_character(path))
  .check_path(path)
  res <- hier_convert(tree, format = format)

  if (verbose) {
    cat(paste("Output is written to", shQuote(path), "\n"))
  }
  if (format %in% c("df", "dt")) {
    write.table(res, file = path, sep = ";", row.names = FALSE)
  }
  if (format == "json") {
    cat(res, sep = "\n", file = path)
  }
  if (format == "code") {
    cat(res, sep = "\n", file = path)
  }
  if (format == "argus") {
    df <- data.frame(
      inp = attributes(res)$sout,
      stringsAsFactors = FALSE
    )
    write.table(
      df$inp,
      file = path,
      sep = " ",
      row.names = FALSE,
      col.names = FALSE,
      quote = FALSE,
      eol = "\r\n"
    )
  }
  if (format == "sdc") {
    saveRDS(res, file = path)
  }
  return(invisible(res))
}
