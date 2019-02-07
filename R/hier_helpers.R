## helpers
h_is_valid <- function(h) {
  fn <- shQuote("hier_create()")
  err <- paste("not a valid sdcHierarchy-obj generated with", fn)
  if (is.null(h)) {
    stop(err, call. = FALSE)
  }
  if (!inherits(h, "sdc_hierarchy")) {
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
# used in hier_info()
h_min_contributing_codes <- function(h, node_name) {
  from <- to <- NULL
  h_is_valid(h)
  stopifnot(is_scalar_character(node_name))

  dt <- as.data.table(ToDataFrameNetwork(h))

  all_codes <- unique(c(dt[, from], dt[, to]))
  if (!node_name %in% all_codes) {
    stop("node not found!\n", call. = FALSE)
  }

  # never in "from"
  index_subtot <- all_codes %in% dt[, from]
  codes_minimal <- all_codes[!index_subtot]

  if (node_name %in% codes_minimal) {
    return(NA) # it is contributing only to it self
  }

  counter <- 1
  tmp <- dt[from == node_name]
  codes <- c()
  cc <- intersect(codes_minimal, tmp[, to])
  if (length(cc) > 0) {
    tmp <- tmp[!to %in% cc]
    codes <- c(codes, cc)
  }
  if (nrow(tmp) == 0) {
    return(codes)
  }

  counter <- counter + 1
  while (nrow(tmp) > 0 || counter > 10000) {
    cc <- intersect(codes_minimal, tmp[, to])
    if (length(cc) > 0) {
      tmp <- tmp[!to %in% cc]
      codes <- c(codes, cc)
    } else {
      tmp <- dt[from %in% tmp[, to]]
      tmp[, from := node_name]
    }
    counter <- counter + 1
  }
  return(codes)
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
    out$contributing_codes <- h_min_contributing_codes(
      sdcHier,
      node_name = node_lab
    )

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
