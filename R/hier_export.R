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
#' @md
#' @examples
#' h <- hier_create(root = "Total", nodes = LETTERS[1:2])
#' h <- hier_add(h, root = "A", nodes = c("a1", "a2"))
#' h <- hier_add(h, root = "B", nodes = c("b1", "b2"))
#' h <- hier_add(h, root = "b1", nodes = "b1a")
#' hier_display(h)
#'
#' # export as input for tauArgus
#' hier_export(h, as = "argus", path = file.path(tempdir(), "h.hrc"))
hier_export <- function(tree, as="df", path, verbose=FALSE) {
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
  res <- hier_convert(tree, as = as)

  if (verbose) {
    cat(paste("Output is written to", shQuote(path), "\n"))
  }
  if (as %in% c("df", "dt")) {
    write.table(res, file = path, sep = ";", row.names = FALSE)
  }
  if (as == "json") {
    cat(res, sep = "\n", file = path)
  }
  if (as == "code") {
    cat(res, sep = "\n", file = path)
  }
  if (as == "argus") {
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
  if (as == "sdc") {
    saveRDS(res, file = path)
  }
  return(invisible(res))
}
