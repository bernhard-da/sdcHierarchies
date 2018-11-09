# internally used for shinyApps

#' sdcHier_import
#'
#' returns a tree from various input sources
#'
#' @param inp an import from which should be converted. This could either
#' be a character-string (json).
#' @param from (character) from which format should be imported.
#' Possible choices are:
#' - *"json"*
#' @param tot_lab TODO
#'
#' @return a (nested) hierarchy suitable as input
#' for `sdcHier_xyz()`-functions
#' @md
#' @export
#' @examples
#' ## not yet
sdcHier_import <- function(inp, from="json", tot_lab=NULL) {
  convert.from.json <- function(json, totLab=NULL) {
    tab <- fromJSON(json)
    if (length(tab)==0) {
      return(sdcHier_create("rootnode"))
    }
    tab <- tab[,c(2,1)]
    colnames(tab) <- c("from","to")
    if (!is.null(totLab)) {
      tab$from[tab$from=="#"] <- totLab
    } else {
      tab$from[tab$from=="#"] <- "rootnode"
    }
    tt <- FromDataFrameNetwork(tab)
    class(tt) <- c(class(tt), "sdcHier")
    tt
  }

  convert.from.data.frame <- function(df, totLab=NULL) {
    stopifnot(is.data.frame(df))
    stopifnot(ncol(df)==2)


    colnames(df) <- c("levels", "labs")
    rr <- unique(unlist(strsplit(df$level, "")))
    stopifnot(length(rr)==1, rr=="@")

    df$labs <- as.character(df$labs)


    # TODO
  }

  stopifnot(is_scalar_character(from))
  stopifnot(from %in% c("json"))
  if (!is.null(tot_lab)) {
    stopifnot(is_scalar_character(tot_lab))
  }

  if (from=="json") {
    return(convert.from.json(json=inp, totLab=tot_lab))
  }
  stop("err in sdcHier_import()", call.=FALSE)
}
