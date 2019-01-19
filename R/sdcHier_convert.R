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
#' \item \strong{"argus"}: used to create hrc-files suitable for tau-argus
#' \item \strong{"json"}: json format suitable as input for shiny Tree
#' \item \strong{"code"}: code required to generate the hierarchy
#' }
#' @param verbose (logical) if true, the result of the conversion will not only
#' be (invisibly) returned but also printed in the prompt
#' @export
#' @examples
#' ## for examples, see sdcHier_vignette()
sdcHier_convert <- function(h, format="data.frame", verbose=FALSE) {
  # to data.frame
  h_to_df <- function(h, verbose) {
    res <- sdcHier_info(h)

    if (length(res) == 6 && names(res)[1] == "exists") {
      return(data.frame(level = "@", name = res$name, stringsAsFactors = FALSE))
    }
    df <- do.call("rbind", lapply(res, function(x) {
      data.frame(level = paste(rep("@", x$level), collapse = ""), name = x$name, stringsAsFactors = FALSE)
    }))
    rownames(df) <- NULL
    if (verbose) {
      print(df)
    }
    return(df)
  }

  # node to json
  h_to_json <- function(h, verbose) {
    write_json_row <- function(id, parent, text, opened=TRUE, disabled=FALSE, selected=FALSE) {
      stopifnot(is_scalar_character(id))
      stopifnot(is_scalar_character(parent))
      stopifnot(is_scalar_character(text))
      op <- ifelse(opened == TRUE, "true", "false")
      dis <- ifelse(disabled == TRUE, "true", "false")
      sel <- ifelse(selected == TRUE, "true", "false")
      js <- paste0("{",
        dQuote("id"), ":", dQuote(id), ",",
        dQuote("parent"), ":", dQuote(parent), ",",
        dQuote("text"), ":", dQuote(text), ",",
        dQuote("state"), ":{",
        dQuote("opened"), ":", op, ",",
        dQuote("disabled"), ":", dis, ",",
        dQuote("selected"), ":", sel, "}}")
      js
    }

    df <- ToDataFrameTypeCol(h)

    if (!is.data.frame(df)) {
      js <- paste0("[]")
      attr(js, "totlev") <- df
      return(js)
    }

    totlab <- df[[1]][1]
    df[[1]] <- "#"
    js <- "["
    for (i in 2:ncol(df)) {
      sub <- unique(df[, c(i - 1, i)])
      sub <- sub[!is.na(sub[[2]]), ]
      for (j in 1:nrow(sub)) {
        js <- paste0(js, write_json_row(id = sub[[2]][j], parent = sub[[1]][j], text = sub[[2]][j]), ",")
      }
    }
    js <- paste0(js, "]")
    js <- sub(",\\]", "\\]", js)
    if (verbose) {
      cat(js, sep = "\n")
    }
    attr(js, "totlev") <- as.character(totlab)
    return(js)
  }

  # node to code
  h_to_code <- function(h, verbose) {
    all_names <- sdcHier_nodenames(h)
    code <- "library(sdcHierarchies)"
    code <- c(code, paste0("d <- sdcHier_create(tot_lab=", shQuote(all_names[1]), ")"))
    all_names <- all_names[-c(1)]

    if (length(all_names) > 0) {
      info <- sdcHier_info(h, node_labs = all_names)
      while (length(all_names) > 0) {
        lev <- all_names[1]
        cur_info <- info[[lev]]
        nn <- c(lev, cur_info$siblings)
        all_names <- setdiff(all_names, nn)

        s1 <- shQuote(cur_info$parent)
        s2 <- paste0("c(", paste0(shQuote(nn), collapse = ","), ")")
        code <- c(code, paste0("sdcHier_add(d, refnode=", s1, ", node_labs=", s2, ")"))
      }
    }
    code <- c(code, "print(d)")
    if (verbose) {
      cat(code, sep = "\n")
    }
    return(code)
  }

  # node to argus
  h_to_argus <- function(h, verbose) {
    dforig <- df <- sdcHier_convert(h, "data.frame")
    df <- df[-1, ]
    df$level <- substr(df$level, 3, nchar(df$level))
    sout <-  df$name
    ind_levs <-  df$level != ""
    m1 <- max(nchar(df$level[ind_levs]))
    slev <- sprintf(paste0("%-", m1, "s"), df$level[ind_levs])

    m2 <- max(nchar(df$name[ind_levs]))
    sname <- sprintf(paste0("%", m2, "s"), df$name[ind_levs])

    sout[ind_levs] <- paste(slev, sname)
    if (verbose) {
      print(dforig)
    }
    attr(dforig, "sout") <- sout
    return(dforig)
  }

  stopifnot(is_scalar_character(format))
  stopifnot(format %in% c("data.frame", "json", "argus", "code"))
  stopifnot(is_scalar_logical(verbose))
  h_is_valid(h)


  if (format == "data.frame") {
    res <- h_to_df(h, verbose = verbose)
  }
  if (format == "json") {
    res <- h_to_json(h, verbose = verbose)
  }
  if (format == "code") {
    res <- h_to_code(h, verbose = verbose)
  }
  if (format == "argus") {
    res <- h_to_argus(h, verbose = verbose)
  }

  attr(res, "sdcHier_convert") <- TRUE
  attr(res, "sdcHier_format") <- format
  return(res)
}
