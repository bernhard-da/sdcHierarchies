## helpers
h_is_valid <- function(h) {
  if (!"sdcHier" %in% class(h)) {
    stop(paste("not a valid sdcHierarchy-obj generated with", shQuote("h_create()")), call.=FALSE)
  }
  invisible(TRUE)
}

h_node_exists <- function(h, node_lab) {
  h_is_valid(h)
  if (is.null(FindNode(h, node_lab))) {
    return(FALSE)
  }
  return(TRUE)
}

# nodeinfo for a specific hierarchy
h_nodeinfo <- function(sdcHier, node_lab) {
  h_is_valid(sdcHier)
  stopifnot(is_scalar_character(node_lab))
  out <- list()
  out$exists <- h_node_exists(sdcHier, node_lab)
  if (out$exists) {
    nn <- FindNode(sdcHier, node_lab)
    out$name <- nn$name
    out$is_rootnode <- nn$isRoot
    out$level <- nn$level
    out$is_leaf <- nn$isLeaf
    out$siblings <- names(nn$siblings)
    out$parent <- nn$parent$name
    out$is_bogus <- length(out$siblings)==0 && out$is_leaf==TRUE
  } else {
    out$name <- NA
    out$is_rootnode <- NA
    out$level <- NA
    out$is_leaf <- NA
    out$siblings <- NA
    out$parent <- NA
    out$is_bogus <- NA
  }
  return(out)
}
