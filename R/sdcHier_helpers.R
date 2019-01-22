## helpers
h_is_valid <- function(h) {
  err <- paste("not a valid sdcHierarchy-obj generated with", shQuote("sdcHier_create()"))
  if (is.null(h)) {
    stop(err, call. = FALSE)
  }
  if (!"sdcHier" %in% class(h)) {
    stop(err, call. = FALSE)
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

# compute the levels of the codes contributing to a specific name
# used in sdcHier_info()
h_min_contributing_codes <- function(sdcHier, node_name) {
  stopifnot(is_scalar_character(node_name))
  nn <- FindNode(sdcHier, node_name)
  if (is.null(nn)) {
    stop(paste("Node", shQuote(node_name), "not found\n"), call. = FALSE)
  }

  if (nn$isLeaf) {
    ## this node has no leaves (is not a sub-total)
    return(NA)
  }

  if (nn$totalCount == 2) {
    ## this node only has one leaf
    return(names(nn$leaves))
  }

  na_char <- "_____NA____"

  df <- ToDataFrameTypeCol(nn)
  df[is.na(df)] <- na_char

  indices <- data.frame(t(apply(df, 1, function(x) {
    x != na_char
  })))
  col_indices <- sapply(1:nrow(df), function(x) {
    tmp <- as.character(df[x, ])
    max(which(tmp != na_char))
  })
  contributing_codes <- sapply(1:nrow(df), function(x) {
    df[x, col_indices[x]]
  })
  contributing_codes
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
    if (out$is_rootnode) {
      out$siblings <- NA
    } else {
      out$siblings <- names(nn$siblings)
    }
    out$contributing_codes <- h_min_contributing_codes(nn, node_name = node_lab)

    ch <- names(nn$children)
    if (length(ch) == 0) {
      ch <- NA
    }
    out$children <- ch
    out$parent <- nn$parent$name
    out$is_bogus <- length(out$siblings) == 0 #&& out$is_leaf == TRUE
  } else {
    out$name <- NA
    out$is_rootnode <- NA
    out$level <- NA
    out$is_leaf <- NA
    out$siblings <- NA
    out$contributing_codes <- NA
    out$children <- NA
    out$parent <- NA
    out$is_bogus <- NA
  }
  return(out)
}
