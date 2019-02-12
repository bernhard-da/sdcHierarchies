#' compute a nested hierarchy
#'
#' This function allows to compute a nested hierarchy from an character
#' vector or a (named) list.
#'
#' @param inp a character vector (for methods \code{len} and \code{endpos}
#' containing codes of a hierarchical variables or a list for
#' method \code{list}. In the latter case, the input is expected to be a named list
#' where each list-element contains the codes belonging to the node that has the
#' name of this specific list element. In the examples below, the required input
#' formats are further explained.
#' @param dim_spec an (integerish) vector containing either the length
#' (in terms of characters) for each level or the end-positions of these levels.
#' In the latter-case, one needs to set argument \code{method} to \code{"endpos"}.
#' This argument is ignored in case the hierarchy should be created from a
#' named list.
#' @param tot_lev \code{NULL} or a scalar characer specifying the name of the overall
#' total in case it is not encoded at the first positions of \code{dim}
#' @param method either \code{len} (the default) or \code{endpos}
#' \itemize{
#' \item \code{len}: the number of characters for each of the levels needs to be specified
#' \item \code{endpos}: the end-positions for each levels need to be fixed
#' \item \code{list}: the end-positions for each levels need to be fixed
#' }
#' @param as_df (logical) if \code{FALSE}, a data.tree is returned, else a data.frame suitable as input for tau-argus
#' @return a hierarchical data structure depending on choice of argument \code{as_df}
#' @export
#' @examples
#' ## Example Regional Codes (NUTS)
#' # digits 1-2 (len=2, endpos=2) --> level 1
#' # digit 3 (len=1, endpos=3) --> level 2
#' # digits 4-5 (len=2, endpos=5) -> level 3
#' ## all strings have equal length but total is not encoded in these values
#' geo_m <- c(
#'   "01051", "01053", "01054", "01055",
#'   "01056", "01057", "01058", "01059", "01060",
#'   "01061", "01062",
#'   "02000",
#'   "03151", "03152", "03153", "03154", "03155", "03156", "03157", "03158",
#'   "03251", "03252", "03254", "03255", "03256", "03257",
#'   "03351", "03352", "03353", "03354", "03355",
#'   "03356", "03357", "03358", "03359",
#'   "03360", "03361",
#'   "03451", "03452", "03453", "03454", "03455", "03456",
#'   "10155")
#'
#' a <- hier_compute(
#'   inp = geo_m,
#'   dim_spec = c(2, 3, 5),
#'   tot_lev = "Tot",
#'   method = "endpos"
#' )
#' b <- hier_compute(
#'   inp = geo_m,
#'   dim_spec = c(2, 1, 2),
#'   tot_lev = "Tot",
#'   method = "len"
#' )
#' identical(
#'   hier_convert(a, format = "df"),
#'   hier_convert(b, format = "df")
#' )
#'
#' ## total is contained in the first 3 positions of the input values
#' ## --> we need to set tot_level to NULL (the default)
#' geo_m_with_tot <- paste0("Tot", geo_m)
#' a <- hier_compute(
#'   inp = geo_m_with_tot,
#'   dim_spec = c(3, 2, 1, 2),
#'   method = "len",
#'   as_df = TRUE
#' )
#' b <- hier_compute(
#'   inp = geo_m_with_tot,
#'   dim_spec = c(3, 5, 6, 8),
#'   method = "endpos",
#'   as_df=TRUE
#' )
#' identical(a, b)
#'
#' ## example where inputs have unequal length
#' ## the overall total is not included in input vector
#' yae_h <- c(
#'   "1.1.1.", "1.1.2.",
#'   "1.2.1.", "1.2.2.", "1.2.3.", "1.2.4.", "1.2.5.", "1.3.1.",
#'   "1.3.2.", "1.3.3.", "1.3.4.", "1.3.5.",
#'   "1.4.1.", "1.4.2.", "1.4.3.", "1.4.4.", "1.4.5.",
#'   "1.5.", "1.6.", "1.7.", "1.8.", "1.9.", "2.", "3.")
#'
#' a <- hier_compute(
#'   inp = yae_h,
#'   dim_spec = c(2, 4, 6),
#'   tot_lev = "Tot",
#'   method = "endpos"
#' )
#' b <- hier_compute(
#'   inp = yae_h,
#'   dim_spec = c(2, 2, 2),
#'   tot_lev = "Tot",
#'   method = "len"
#' )
#' identical(
#'   hier_convert(a, format = "df"),
#'   hier_convert(b, format = "df")
#' )
#'
#' ## Same exampl, but overall total is contained in the first 3 positions
#' ## of the input values --> tot_level needs to be set to NULL (the default)
#' yae_h_with_tot <- paste0("Tot", yae_h)
#' a <- hier_compute(
#'   inp = yae_h_with_tot,
#'   dim_spec = c(3, 2, 2, 2),
#'   method = "len",
#'   as_df = TRUE
#' )
#' b <- hier_compute(
#'   inp = yae_h_with_tot,
#'   dim_spec = c(3, 5, 7, 9),
#'   method = "endpos",
#'   as_df = TRUE
#' )
#' identical(a, b)
#'
#' ## An example using a list as input (same as above)
#' ## Hierarchy: digits 1-2 (nuts1), digit 3 (nut2), digits 4-5 (nuts3)
#' ## The order of the list-elements is not important but the
#' ## names of input-list correspond to (subtotal/level) names
#' geo_ll <- list()
#' geo_ll[["Total"]] <- c("01", "02", "03", "10")
#' geo_ll[["010"]]   <- c(
#'   "01051", "01053", "01054", "01055",
#'   "01056", "01057", "01058", "01059",
#'   "01060", "01061", "01062"
#' )
#' geo_ll[["031"]]   <- c(
#'   "03151", "03152", "03153", "03154",
#'   "03155", "03156", "03157", "03158"
#' )
#' geo_ll[["032"]]   <- c(
#'   "03251", "03252", "03254",
#'   "03255", "03256", "03257"
#' )
#' geo_ll[["033"]]   <- c(
#'   "03351", "03352", "03353", "03354", "03355",
#'   "03356", "03357", "03358", "03359",
#'   "03360", "03361"
#' )
#' geo_ll[["034"]]   <- c(
#'   "03451", "03452", "03453",
#'   "03454", "03455","03456"
#' )
#' geo_ll[["01"]]    <- "010"
#' geo_ll[["02"]]    <- "020"
#' geo_ll[["020"]]   <- "02000"
#' geo_ll[["03"]]    <- c("031", "032", "033", "034")
#' geo_ll[["10"]]    <- "101"
#' geo_ll[["101"]]   <- "10155"
#'
#' d <- hier_compute(
#'   inp = geo_ll,
#'   tot_lev = "Total",
#'   method = "list",
#'   as_df = FALSE
#' ); d
#'
#' ## Reproduce example from above with input defined as named list
#' yae_ll <- list()
#' yae_ll[["Total"]] <- c("1.", "2.", "3.")
#' yae_ll[["1."]] <- paste0("1.", 1:9, ".")
#' yae_ll[["1.1."]] <- paste0("1.1.", 1:2, ".")
#' yae_ll[["1.2."]] <- paste0("1.2.", 1:5, ".")
#' yae_ll[["1.3."]] <- paste0("1.3.", 1:5, ".")
#' yae_ll[["1.4."]] <- paste0("1.4.", 1:6, ".")
#' d <- hier_compute(
#'   inp = yae_ll,
#'   tot_lev = "Total",
#'   method = "list",
#'   as_df = FALSE
#' ); d
hier_compute <- function(
  inp,
  dim_spec=NULL,
  tot_lev=NULL,
  method="len",
  as_df=FALSE
  ) {
  # convert endpos to length
  endpos_to_len <- function(end_pos) {
    diff(c(0, end_pos))
  }

  # compute from a nested (named) list
  from_list <- function(dim, tot_lev, as_df=FALSE) {
    stopifnot(is_scalar_character(tot_lev))
    nn <- names(dim)
    dim_q <- shQuote(substitute(dim))
    if (is.null(nn)) {
      stop(paste("Argument", dim_q, "must be a named list"), call. = FALSE)
    }
    if (sum(nn == "") > 0) {
      e <- paste("Some elements of argument", dim_q, "are not named.")
      stop(e, call. = FALSE)
    }

    if (!tot_lev %in% nn) {
      e <- c(
        "The given name for the overall total",
        shQuote(tot_lev),
        "was not found in the given input list."
      )
      stop(paste(e, collapse = " "), call. = FALSE)
    }

    if (any(duplicated(nn))) {
      stop(paste("Duplicated names in argument", dim_q, "found."), call. = FALSE)
    }

    all_codes <- as.character(unlist(dim))
    if (tot_lev %in% all_codes) {
      t <- shQuote(tot_lev)
      d <- shQuote("dim")
      err <- paste("The overall total", t, "was found in", dim_q)
      stop(err, call. = FALSE)
    }

    nodes <- setdiff(nn, tot_lev)
    ind <- which(!nodes %in% all_codes)
    if (length(ind) > 0) {
      nn <- paste0("- ", nodes[ind], collapse = "\n")
      err <- paste("The following sub-levels were not found as inputs:\n", nn)
      stop(err, call. = FALSE)
    }

    # generate hierarchy
    df <- data.frame(nodes = nn, finished = FALSE)
    cur_nodes <- dim[[tot_lev]]
    d <- hier_create(tot_lab = tot_lev, node_labs = cur_nodes)
    df[df$nodes == tot_lev, "finished"] <- TRUE

    not_finished <- sum(df$finished == FALSE) >= 0
    while (not_finished) {
      todo <- df[df$finished == FALSE, "nodes"]
      levs <- intersect(todo, hier_nodenames(d))
      for (i in seq_along(levs)) {
        cur_nr <- levs[i]
        hier_add(d, refnode = cur_nr, node_labs = as.character(dim[[cur_nr]]))
        df[df$nodes == cur_nr, "finished"] <- TRUE
      }
      not_finished <- sum(df$finished == FALSE) > 0
    }

    if (as_df == TRUE) {
      d <- hier_convert(d, format = "df")
    }
    return(d)
  }

  stopifnot(is_scalar_character(method), method %in% c("len", "endpos", "list"))

  if (method == "list") {
    m <- c(
      "Argument", shQuote("dim_spec"),
      "is ignored when constructing a hierarchy from a nested list."
    )
    message(paste(m, collapse = " "))
    return(from_list(dim = inp, tot_lev = tot_lev, as_df = as_df))
  }

  if (is.null(dim_spec)) {
    e <- c(
      "Argument", shQuote("dim_spec"), "must be",
      "a numeric vector specifying"
    )
    if (method == "len") {
      e <- c(e, "end-positions of each hierarchical level within the input.")
    } else {
      e <- c(e, "the required number of characters for each level.")
    }
    stop(paste(e, collapse = " "), call. = FALSE)
  }

  if (method == "endpos") {
    dim_len <- endpos_to_len(dim_spec)
  } else {
    dim_len <- dim_spec
  }

  stopifnot(is.character(inp))
  stopifnot(is_integerish(dim_len))
  stopifnot(all(dim_len > 0))

  if (!is.null(tot_lev)) {
    stopifnot(is_scalar_character(tot_lev))
  }

  stopifnot(sum(dim_len) >= max(nchar(inp)))
  if (sum(any(duplicated(inp))) > 0) {
    err <- paste("duplicated values detected in argument", shQuote("inp"), "!")
    stop(err, call. = FALSE)
  }

  tree_depth <- length(inp)
  only_total <- FALSE
  if (is.null(tot_lev)) {
    if (length(dim_len) == 1) {
      only_total <- TRUE
    }
    df <- data.frame(
      path = substr(inp, 1, dim_len[1]),
      stringsAsFactors = FALSE
    )
    if (length(unique(df$path)) > 1) {
      err <- paste("Top-Level should be included in first", dim_len[1])
      err <- paste(err, "characters, but >1 values were detected!")
      stop(err, call. = FALSE)
    }

    inp <- substr(inp, dim_len[1] + 1, nchar(inp))
    dim_len <- dim_len[-c(1)]
  } else {
    df <- data.frame(path = rep(tot_lev, tree_depth), stringsAsFactors = FALSE)
    only_total <- FALSE
  }

  # only total specified
  if (only_total == TRUE) {
    nn <- hier_create(as.character(df[1, 1]))
    if (as_df == TRUE) {
      return(hier_convert(nn, format = "df"))
    }
    return(nn)
  }

  cs <- c(0, cumsum(dim_len))
  for (i in 2:length(cs)) {
    from <- cs[i - 1] + 1
    to <- cs[i]
    levs <- substr(inp, from, to)
    ii <- which(levs != "")

    if (length(ii) > 0) {
      df$path[ii] <- paste0(df$path[ii], "/", substr(inp, 1, to)[ii])
    }
  }
  nn <- FromDataFrameTable(df, pathName = "path")
  class(nn) <- c(class(nn), "sdc_hierarchy")
  if (as_df == TRUE) {
    return(hier_convert(nn, format = "df"))
  }
  return(nn)
}
