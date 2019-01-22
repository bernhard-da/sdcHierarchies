#' sdcHier_compute
#'
#' computes hierarchies from character-vectors
#'
#' @param dim a character vector containing codes of a hierarchical variables.
#' @param dim_spec an (integerish) vector containing either the length
#' (in terms of characters) for each level or the end-positions of these levels.
#' In the latter-case, one needs to set argument \code{method} to \code{"endpos"}
#' @param tot_lev \code{NULL} or a scalar characer specifying the name of the overall
#' total in case it is not encoded at the first positions of \code{dim}
#' @param method either \code{len} (the default) or \code{endpos}
#' \itemize{
#' \item \code{len}: the number of characters for each of the levels needs to be specified
#' \item \code{endpos}: the end-positions for each levels need to be fixed
#' }
#' @param as_df (logical) if \code{FALSE}, a data.tree is returned, else a data.frame suitable as input for tau-argus
#' @return a hierarchical data structure depending on choice of argument \code{as_df}
#' @export
#' @examples
#' ## nuts: digits 1-2 (nuts1), digit 3 (nut2), digits 4-5 (nuts3)
#' ## all strings have equal length but total is not encoded in these values
#' geo_m <- c(
#'   "01051","01053","01054","01055","01056","01057","01058","01059","01060","01061","01062",
#'   "02000",
#'   "03151","03152","03153","03154","03155","03156","03157","03158","03251","03252","03254","03255",
#'   "03256","03257","03351","03352","03353","03354","03355","03356","03357","03358","03359","03360",
#'   "03361","03451","03452","03453","03454","03455","03456",
#'   "10155")
#' a <- sdcHier_compute(dim=geo_m, dim_spec=c(2,3,5),
#'   tot_lev="Tot", method="endpos")
#' b <- sdcHier_compute(dim=geo_m, dim_spec=c(2,1,2),
#'   tot_lev="Tot", method="len")
#' identical(sdcHier_convert(a, format="df"), sdcHier_convert(b, format="df"))
#'
#' ## return data.frame suitable as input for tau-argus or sdcTable
#' a <- sdcHier_compute(dim=geo_m, dim_spec=c(2,3,5),
#'   tot_lev="Tot", method="endpos", as_df=TRUE)
#' b <- sdcHier_compute(dim=geo_m, dim_spec=c(2,1,2),
#'   tot_lev="Tot", method="len", as_df=TRUE)
#' identical(a, b)
#'
#' ## total is contained in the first 3 positions of the input values
#' ## --> we need to set tot_level to NULL (the default)
#' geo_m_with_tot <- paste0("Tot",geo_m)
#' a <- sdcHier_compute(dim=geo_m_with_tot, dim_spec=c(3,2,1,2),
#'   method="len", as_df=TRUE)
#' b <- sdcHier_compute(dim=geo_m_with_tot, dim_spec=c(3,5,6,8),
#'   method="endpos", as_df=TRUE)
#' identical(a, b)
#'
#' ## second example, unequal strings; overall total not included in input
#' yae_h <- c(
#'   "1.1.1.","1.1.2.",
#'   "1.2.1.","1.2.2.","1.2.3.","1.2.4.","1.2.5.","1.3.1.",
#'   "1.3.2.","1.3.3.","1.3.4.","1.3.5.",
#'   "1.4.1.","1.4.2.","1.4.3.","1.4.4.","1.4.5.",
#'   "1.5.","1.6.","1.7.","1.8.","1.9.","2.","3.")
#' a <- sdcHier_compute(dim=yae_h, dim_spec=c(2,4,6), tot_lev="Tot", method="endpos")
#' b <- sdcHier_compute(dim=yae_h, dim_spec=c(2,2,2), tot_lev="Tot", method="len")
#' identical(sdcHier_convert(a, format="df"), sdcHier_convert(b, format="df"))
#'
#' ## total is contained in the first 3 positions of the input values
#' ## --> we need to set tot_level to NULL (the default)
#' yae_h_with_tot <- paste0("Tot",yae_h)
#' a <- sdcHier_compute(dim=yae_h_with_tot, dim_spec=c(3,2,2,2),
#'   method="len", as_df=TRUE)
#' b <- sdcHier_compute(dim=yae_h_with_tot, dim_spec=c(3,5,7,9),
#'   method="endpos", as_df=TRUE)
#' identical(a, b)
sdcHier_compute <- function(dim, dim_spec, tot_lev=NULL, method="len", as_df=FALSE) {
  # convert endpos to length
  endpos_to_len <- function(end_pos) {
    diff(c(0, end_pos))
  }
  stopifnot(is_scalar_character(method), method %in% c("len", "endpos"))

  if (method == "endpos") {
    dim_len <- endpos_to_len(dim_spec)
  } else {
    dim_len <- dim_spec
  }

  stopifnot(is.character(dim))
  stopifnot(is_integerish(dim_len))
  stopifnot(all(dim_len > 0))

  if (!is.null(tot_lev)) {
    stopifnot(is_scalar_character(tot_lev))
  }

  stopifnot(sum(dim_len) >= max(nchar(dim)))
  if (sum(any(duplicated(dim))) > 0) {
    stop(paste("duplicated values detected in argument", shQuote("dim"), "!"), call. = FALSE)
  }

  tree_depth <- length(dim)
  only_total <- FALSE
  if (is.null(tot_lev)) {
    if (length(dim_len) == 1) {
      only_total <- TRUE
    }
    df <- data.frame(path = substr(dim, 1, dim_len[1]), stringsAsFactors = FALSE)
    if (length(unique(df$path)) > 1) {
      err <- paste("Top-Level should be included in first", dim_len[1], "characters, but >1 values were detected!")
      stop(err, call. = FALSE)
    }

    dim <- substr(dim, dim_len[1] + 1, nchar(dim))
    dim_len <- dim_len[-c(1)]
  } else {
    df <- data.frame(path = rep(tot_lev, tree_depth), stringsAsFactors = FALSE)
    only_total <- FALSE
  }

  # only total specified
  if (only_total == TRUE) {
    nn <- sdcHier_create(as.character(df[1, 1]))
    if (as_df == TRUE) {
      return(sdcHier_convert(nn, format = "df"))
    }
    return(nn)
  }

  cs <- c(0, cumsum(dim_len))
  for (i in 2:length(cs)) {
    from <- cs[i - 1] + 1
    to <- cs[i]
    levs <- substr(dim, from, to)
    ii <- which(levs != "")

    if (length(ii) > 0) {
      df$path[ii] <- paste0(df$path[ii], "/", substr(dim, 1, to)[ii])
    }
  }
  nn <- FromDataFrameTable(df, pathName = "path")
  class(nn) <- c(class(nn), "sdcHier")
  if (as_df == TRUE) {
    return(sdcHier_convert(nn, format = "df"))
  }
  return(nn)
}
