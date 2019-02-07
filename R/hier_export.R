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
#' ## for examples, see hier_vignette()
hier_export <- function(h, format="df", path, verbose=FALSE) {
  check_file  <- function(path) {
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
  check_file(path)
  res <- hier_convert(h, format = format, verbose = verbose)

  if (verbose) {
    cat(paste("Output is written to", shQuote(path), "\n"))
  }
  if (format %in% c("df", "dt")) {
    write.table(df, file = path, sep = ";", row.names = FALSE)
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
