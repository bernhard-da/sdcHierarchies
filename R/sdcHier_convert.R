#' sdcHier_convert
#'
#' @inherit sdcHier_add
#' @param format (character) specifying the export format. possible choices are:
#' - **"data.frame"**: a `data.frame` with two columns. The first columns contains a string containing as many `@` as the level
#' of the node in the string (e.g `@` corresponds to the overall total while `@@` would be all codes contributing to the total.
#' The second column contains the names of the levels.
#' - **"json"**: json format suitable as input for shiny Tree
#' @export
#' @md
#' @examples
#' ## for examples, see ?sdcHier_create
sdcHier_convert <- function(h, format="data.frame") {
  h_to_json <- function(h) {
    write.json.row <- function(id, parent, text, opened=TRUE, disabled=FALSE, selected=FALSE) {
      stopifnot(is_scalar_character(id))
      stopifnot(is_scalar_character(parent))
      stopifnot(is_scalar_character(text))
      op <- ifelse(opened==TRUE, "true","false")
      dis <- ifelse(disabled==TRUE, "true","false")
      sel <- ifelse(selected==TRUE, "true","false")
      js <- paste0("{",dQuote("id"),":",dQuote(id),",",dQuote("parent"),":",dQuote(parent),",",dQuote("text"),":",dQuote(text))
      js <- paste0(js, ",",dQuote("state"),":{",dQuote("opened"),":",op,",",dQuote("disabled"),":",dis,",",dQuote("selected"),":",sel,"}}")
      js
    }

    df <- ToDataFrameTypeCol(h)

    if (!is.data.frame(df)) {
      js <- paste0('[]')
      return(js)
    }

    df[[1]] <- "#"
    js <- "["
    for (i in 2:ncol(df)) {
      sub <- unique(df[,c(i-1, i)])
      sub <- sub[!is.na(sub[[2]]),]
      for (j in 1:nrow(sub)) {
        js <- paste0(js, write.json.row(id=sub[[2]][j], parent=sub[[1]][j], text=sub[[2]][j]),",")
      }
    }
    js <- paste0(js,"]")
    sub(",\\]","\\]", js)
  }

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
    return(h_to_json(h))
  }
  stop(paste("Error in sdcHier_convert()"), call.=FALSE)
}
