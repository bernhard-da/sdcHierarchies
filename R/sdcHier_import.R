#' sdcHier_import
#'
#' returns a tree from various input sources
#'
#' @param inp an import from which should be converted. This could either
#' be a character-string (json) or a \code{data.frame}.
#' @param from (character) from which format should be imported.
#' Possible choices are:
#' \itemize{
#' \item \strong{"json"}: json-encoded string should be converted
#' \item \strong{"data.frame"}: a \code{data.frame} in \code{@;level}-format will be converted
#' }
#' @param tot_lab optional name of overall total
#' @return a (nested) hierarchy
#' @export
#' @examples
#' ## not yet
sdcHier_import <- function(inp, from="json", tot_lab=NULL) {
  convert.from.json <- function(json, totLab=NULL) {
    totlab_from_attr <- function(json, tot_lab) {
      if (!is.null(tot_lab)) {
        return(tot_lab)
      }
      tot_lab <- attributes(json)$totlev
      if (is.null(tot_lab) | tot_lab=="") {
        tot_lab <- "rootnode"
      }
      tot_lab
    }
    tab <- fromJSON(json)
    if (length(tab)==0) {
      return(sdcHier_create(tot_lab=totlab_from_attr(json, tot_lab)))
    }
    tab <- tab[,c(2,1)]
    colnames(tab) <- c("from","to")
    if (!is.null(totLab)) {
      tab$from[tab$from=="#"] <- totLab
    } else {
      tab$from[tab$from=="#"] <- totlab_from_attr(json, tot_lab)
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
    stopifnot(df$levels[1]=="@")
    stopifnot(sum(df$levels[1]=="@")==1)
    df$labs <- as.character(df$labs)

    dd <- sdcHier_create(tot_lab=df$labs[1])
    if (nrow(df)==1) {
      return(dd)
    }
    df$hier <- nchar(df$levels)
    df$index <- 1:nrow(df)
    for (i in 2:nrow(df)) {
      cur_hier <- df[i, "hier"]
      prev_hier <- df[(i-1), "hier"]
      if (cur_hier==prev_hier) {
        # sibling
        nn <- FindNode(dd, df$labs[i-1])
        nn$AddSibling(df$labs[i])
      } else {
        if (cur_hier>prev_hier) {
          nn <- FindNode(dd, df$labs[i-1])
          nn$AddChild(df$labs[i])
        } else {
          ii <- max(which(df$hier[1:i]==(cur_hier-1)))
          nn <- FindNode(dd, df$labs[ii])
          nn$AddChild(df$labs[i])
        }
      }
    }
    return(dd)
  }

  stopifnot(is_scalar_character(from))
  stopifnot(from %in% c("json","data.frame"))
  if (!is.null(tot_lab)) {
    stopifnot(is_scalar_character(tot_lab))
  }

  if (from=="json") {
    return(convert.from.json(json=inp, totLab=tot_lab))
  }
  if (from=="data.frame") {
    return(convert.from.data.frame(df=inp))
  }
  stop("err in sdcHier_import()", call.=FALSE)
}
