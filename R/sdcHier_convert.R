#' sdcHier_convert
#'
#' convert nested hierarchies into other data structures
#'
#' @inherit sdcHier_add
#' @param format (character) specifying the export format. possible choices are:
#' \itemize{
#' \item \strong{"data.frame"}: a \code{data.frame} with two columns. The first columns contains a string
#' containing as many \code{@} as the level of the node in the string (e.g \code{@} corresponds to the overall
#' total while \code{@@} would be all codes contributing to the total. The second column contains the names
#' of the levels.
#' \item \strong{"json"}: json format suitable as input for shiny Tree
#' \item \strong{"code"}: code required to generate the hierarchy
#' }
#' @export
#' @examples
#' ## for examples, see ?sdcHier_create
sdcHier_convert <- function(h, format="data.frame") {
  # to data.frame
  h_to_df <- function(h) {
    res <- sdcHier_info(h)

    if (length(res)==6 && names(res)[1]=="exists") {
      return(data.frame(level="@", name=res$name, stringsAsFactors=FALSE))
    }
    df <- do.call("rbind", lapply(res, function(x) {
      data.frame(level=paste(rep("@", x$level), collapse=""), name=x$name, stringsAsFactors=FALSE)
    }))
    rownames(df) <- NULL
    return(df)
  }

  # node to json
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

  # node to code
  h_to_code <- function(h) {
    all_names <- sdcHier_nodenames(h)
    code <- "library(sdcHierarchies)"
    code <- c(code, paste0("d <- sdcHier_create(tot_lab=",shQuote(all_names[1]),")"))
    all_names <- all_names[-c(1)]
    info <- sdcHier_info(h, node_labs=all_names)

    runInd <- TRUE
    while(runInd) {
      lev <- all_names[1]
      cur_info <- info[[lev]]
      nn <- c(lev, cur_info$siblings)
      all_names <- setdiff(all_names, nn)

      s1 <- shQuote(cur_info$parent)
      s2 <- paste0("c(",paste0(shQuote(nn), collapse=","),")")
      code <- c(code, paste0("sdcHier_add(d, refnode=",s1,", node_labs=",s2,")"))
      if (length(all_names)==0) {
        runInd <- FALSE
      }
    }
    code <- c(code, "print(d)")
    cat(code, sep="\n")
    return(invisible(code))
  }

  h_is_valid(h)

  stopifnot(is_scalar_character(format))
  stopifnot(format %in% c("data.frame","json","code"))

  if (format=="data.frame") {
    return(h_to_df(h))
  }
  if (format=="json") {
    return(h_to_json(h))
  }
  if (format=="code") {
    return(h_to_code(h))
  }
  stop(paste("Error in sdcHier_convert()"), call.=FALSE)
}
