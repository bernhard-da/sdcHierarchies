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
#' \item \strong{"df"}: a \code{data.frame} in \code{@;level}-format will be converted
#' \item \strong{"argus"}: an object exported using \code{\link{sdcHier_convert}} using \code{format = "argus"}
#' \item \strong{"code"}: an object exported using \code{\link{sdcHier_convert}} using \code{format = "code"}
#' \item \strong{"hrc"}: text-files in tau-argus hrc-format
#' \item \strong{"sdc"}: an object exported using \code{\link{sdcHier_convert}} using \code{format = "sdc"}
#' }
#' @param tot_lab optional name of overall total
#' @return a (nested) hierarchy
#' @export
#' @examples
#' ## for examples, see sdcHier_vignette()
sdcHier_import <- function(inp, from="json", tot_lab=NULL) {
  h_from_json <- function(json, tot_lab=NULL) {
    totlab_from_attr <- function(json, tot_lab) {
      if (!is.null(tot_lab)) {
        return(tot_lab)
      }
      tot_lab <- attributes(json)$totlev
      if (is.null(tot_lab) || tot_lab == "") {
        tot_lab <- "rootnode"
      }
      tot_lab
    }
    tab <- fromJSON(json)
    if (length(tab) == 0) {
      return(sdcHier_create(tot_lab = totlab_from_attr(json, tot_lab)))
    }
    tab <- tab[, c(2, 1)]
    colnames(tab) <- c("from", "to")
    if (!is.null(tot_lab)) {
      tab$from[tab$from == "#"] <- tot_lab
    } else {
      tab$from[tab$from == "#"] <- totlab_from_attr(json, tot_lab)
    }
    tt <- FromDataFrameNetwork(tab)
    class(tt) <- c(class(tt), "sdcHier")
    tt
  }
  h_from_df <- function(df, tot_lab=NULL) {
    stopifnot(is.data.frame(df))
    stopifnot(ncol(df) == 2)
    colnames(df) <- c("levels", "labs")
    rr <- unique(unlist(strsplit(df$levels, "")))
    stopifnot(length(rr) == 1, rr == "@")
    stopifnot(df$levels[1] == "@")
    stopifnot(sum(df$levels[1] == "@") == 1)
    df$labs <- as.character(df$labs)

    dd <- sdcHier_create(tot_lab = df$labs[1])
    if (nrow(df) == 1) {
      return(dd)
    }
    df$hier <- nchar(df$levels)
    df$index <- 1:nrow(df)
    for (i in 2:nrow(df)) {
      cur_hier <- df[i, "hier"]
      prev_hier <- df[(i - 1), "hier"]
      if (cur_hier == prev_hier) {
        # sibling
        nn <- FindNode(dd, df$labs[i - 1])
        nn$AddSibling(df$labs[i])
      } else {
        if (cur_hier > prev_hier) {
          nn <- FindNode(dd, df$labs[i - 1])
          nn$AddChild(df$labs[i])
        } else {
          ii <- max(which(df$hier[1:i] == (cur_hier - 1)))
          nn <- FindNode(dd, df$labs[ii])
          nn$AddChild(df$labs[i])
        }
      }
    }
    return(dd)
  }
  h_from_argus <- function(df, tot_lab=NULL) {
    stopifnot(is.data.frame(df))
    stopifnot(attributes(df)$sdcHier_format == "argus")
    return(h_from_df(df, tot_lab = tot_lab))
  }
  h_from_code <- function(code, tot_lab=NULL) {
    stopifnot(is.character(code))
    stopifnot(attributes(code)$sdcHier_convert == TRUE)
    stopifnot(attributes(code)$sdcHier_format == "code")
    code <- paste(code[-c(1, length(code))], collapse = ";")
    return(eval(parse(text = code)))
  }
  h_from_hrc <- function(hrc, tot_lab=NULL) {
    df <- data.frame(inp = readLines(hrc), stringsAsFactors = FALSE)
    r1 <- strsplit(df$inp, " ")
    df$level <- sapply(r1, function(x) {
      if (substr(x[1], 1, 1) == "@") {
        return(x[1])
      } else {
        return("")
      }
    })
    df$names <- sapply(r1, function(x) {
      if (substr(x[1], 1, 1) == "@") {
        return(tail(x, 1))
      } else {
        return(x[1])
      }
    })
    df$inp <- NULL
    df$level <- paste0("@@", df$level)
    if (is.null(tot_lab)) {
      tot_lab <- "Total"
    }
    df <- rbind(data.frame(level = "@", names = tot_lab, stringsAsFactors = FALSE), df)
    return(h_from_df(df))
  }
  h_from_sdc <- function(inp) {
    code_default <- codes_minimal <- id <- key <- NULL
    level <- levs <- name <- setkeyv <- NULL
    stopifnot(is.list(inp))
    stopifnot(attributes(inp)$sdcHier_format == "sdc")

    df <- data.frame(
      levels = inp$codes$level,
      labs = inp$codes$orig,
      stringsAsFactors = FALSE)
    df$levels <- sapply(1:nrow(df), function(x) {
      paste(rep("@", df$levels[x]), collapse = "")
    })

    h <- h_from_df(df)

    bogus <- inp$bogus
    if (!is.null(bogus$bogus_codes)) {
      for (i in 1:length(bogus$bogus_codes)) {
        sdcHier_add(h,
          refnode = bogus$bogus_parents[i],
          node_labs = bogus$bogus_codes[i])
      }
    }
    return(h)
  }

  stopifnot(is_scalar_character(from))
  stopifnot(from %in% c("json", "df", "argus", "hrc", "code", "sdc"))
  if (!is.null(tot_lab)) {
    stopifnot(is_scalar_character(tot_lab))
  }

  if (from == "json") {
    return(h_from_json(json = inp, tot_lab = tot_lab))
  }
  if (from == "df") {
    return(h_from_df(df = inp))
  }
  if (from == "argus") {
    return(h_from_argus(df = inp))
  }
  if (from == "code") {
    return(h_from_code(code = inp))
  }
  if (from == "hrc") {
    return(h_from_hrc(hrc = inp, tot_lab = tot_lab))
  }
  if (from == "sdc") {
    return(h_from_sdc(inp = inp))
  }
  stop("uncaught error in sdcHier_import()", call. = FALSE)
}
