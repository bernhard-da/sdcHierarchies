#' sdcHier_convert
#'
#' @inherit sdcHier_add
#' @param format (character) specifying the export format. possible choices are:
#' - **"data.frame"**: a `data.frame` with two columns. The first columns contains a string containing as many `@` as the level
#' of the node in the string (e.g `@` corresponds to the overall total while `@@` would be all codes contributing to the total.
#' The second column contains the names of the levels.
#' - **"json"**: json format
#' @export
#' @md
#' @examples
#' ## for examples, see ?sdcHier_create
sdcHier_convert <- function(h, format="data.frame") {
  h_is_valid(h)

  stopifnot(is_scalar_character(format))
  stopifnot(format %in% c("data.frame","json"))

  if (format=="data.frame") {
    res <- sdcHier_info(h)

    df <- do.call("rbind", lapply(res, function(x) {
      data.frame(level=paste(rep("@", x$level), collapse=""), name=x$name, stringsAsFactors=FALSE)
    }))
    rownames(df) <- NULL
    return(df)
  }
  if (format=="json") {
    # todo
  }
  stop(paste("Error in sdcHier_convert()"), call.=FALSE)
}
