#' sdcHier_compute_fromList
#'
#' compute a hierarchies from a list input. The name of each list-element refers to a (sub)-total (node) in the hierarchy
#' while the values of the each list-elements to the leafs contributing to this (sub)total (node)
#'
#' @param dim a named list where each list-element containing codes of a hierarchical variables.
#' @param tot_lev a scalar characer specifying the name of the overall total
#' @param as_df (logical) if \code{FALSE}, a data.tree is returned, else a data.frame in \code{@;level}-format
#' @return a hierarchical data structure depending on choice of argument \code{as_df}
#' @export
#' @seealso sdcHier_compute
#' @examples
#' ## nuts: digits 1-2 (nuts1), digit 3 (nut2), digits 4-5 (nuts3)
#' ## the order of the list-elements is not important
#' ## names of input-list correspond to sub-total names; its values to the contributing levels
#' geo_ll <- list()
#' geo_ll[["Total"]] <- c("01","02","03","10")
#' geo_ll[["010"]] <- c("01051","01053","01054","01055","01056","01057",
#'   "01058","01059","01060","01061","01062")
#' geo_ll[["031"]] <- c("03151","03152","03153","03154","03155","03156",
#'   "03157","03158")
#' geo_ll[["032"]] <- c("03251","03252","03254","03255","03256","03257")
#' geo_ll[["033"]] <- c("03351","03352","03353","03354","03355","03356",
#'   "03357","03358","03359","03360","03361")
#' geo_ll[["034"]] <- c("03451","03452","03453","03454","03455","03456")
#' geo_ll[["01"]] <- c("010")
#' geo_ll[["02"]] <- c("020")
#' geo_ll[["020"]] <- c("02000")
#' geo_ll[["03"]] <- c("031","032","033","034")
#' geo_ll[["10"]] <- c("101")
#' geo_ll[["101"]] <- c("10155")
#' d <- sdcHier_compute_fromList(dim=geo_ll, tot_lev="Total", as_df=FALSE); d
#'
#' ## second example; same as in ?sdcHier_compute
#' yae_ll <- list()
#' yae_ll[["Total"]] <- c("1.","2.","3.")
#' yae_ll[["1."]] <- paste0("1.",1:9,".")
#' yae_ll[["1.1."]] <- paste0("1.1.",1:2,".")
#' yae_ll[["1.2."]] <- paste0("1.2.",1:5,".")
#' yae_ll[["1.3."]] <- paste0("1.3.",1:5,".")
#' yae_ll[["1.4."]] <- paste0("1.4.",1:6,".")
#' d <- sdcHier_compute_fromList(dim=yae_ll, tot_lev="Total", as_df=FALSE); d
sdcHier_compute_fromList <- function(dim, tot_lev, as_df=FALSE) {
  stopifnot(is_scalar_character(tot_lev))
  nn <- names(dim)
  if (is.null(nn)) {
    stop(paste("Argument", shQuote(dim), "must be a named list"), call. = FALSE)
  }
  if (sum(nn == "") > 0) {
    stop(paste("Some elements of argument", shQuote(dim), "are not named"), call. = FALSE)
  }
  stopifnot(tot_lev %in% nn)
  if (any(duplicated(nn))) {
    stop(paste("Duplicated names in argument", shQuote(dim), "found"), call. = FALSE)
  }

  all_codes <- as.character(unlist(dim))
  if (tot_lev %in% all_codes) {
    stop(paste("The overall total", shQuote(tot_lev), "was found in argument", shQuote("dim")), call. = FALSE)
  }

  nodes <- setdiff(nn, tot_lev)
  ind <- which(!nodes %in% all_codes)
  if (length(ind) > 0) {
    nn <- paste0("- ", nodes[ind], collapse = "\n")
    stop(paste("The following sub-levels were not found as inputs:\n", nn), call. = FALSE)
  }

  # generate hierarchy
  df <- data.frame(nodes = nn, finished = FALSE)
  cur_nodes <- dim[[tot_lev]]
  d <- sdcHier_create(tot_lab = tot_lev, node_labs = cur_nodes)
  df[df$nodes == tot_lev, "finished"] <- TRUE

  not_finished <- sum(df$finished == FALSE) >= 0
  while (not_finished) {
    todo <- df[df$finished == FALSE, "nodes"]
    levs <- intersect(todo, sdcHier_nodenames(d))
    for (i in seq_along(levs)) {
      cur_nr <- levs[i]
      sdcHier_add(d, refnode = cur_nr, node_labs = as.character(dim[[cur_nr]]))
      df[df$nodes == cur_nr, "finished"] <- TRUE
    }
    not_finished <- sum(df$finished == FALSE) > 0
  }

  if (as_df == TRUE) {
    d <- sdcHier_convert(d, format = "data.frame")
  }
  return(d)
}
