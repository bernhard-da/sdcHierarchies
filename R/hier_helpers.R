# add the class attribute to the tree
.add_class <- function(tree) {
  class(tree) <- unique(c("sdc_hierarchy", class(tree)))
  tree
}

# initializes an empty tree
.init <- function(rootnode) {
  tree <- data.table(
    root = rootnode,
    leaf = rootnode,
    level = 1
  )
  class(tree) <- unique(c("sdc_hierarchy", class(tree)))
  tree
}

# checks if the given tree is valid
.is_valid <- function(tree) {
  if (!inherits(tree, "sdc_hierarchy")) {
    e <- "The provided input `tree` is not a sdc_hierarchy object."
    stop(e, call. = FALSE)
  }

  # check only one rootnode
  if (nrow(tree) > 0) {
    if (sum(duplicated(tree$leaf)) > 0) {
      stop("non-unique leaf nodes detected!", call. = FALSE)
    }
  }
  TRUE
}

# returns the names of all nodes in the correct order
.all_nodes <- function(tree) {
  .is_valid(tree)
  hier_convert(tree, "dt")$name
}

# returns the name of the rootnode
.rootnode <- function(tree) {
  rcpp_rootnode(tree = tree)
}

# adds multiple rows to an existing tree
.add_nodes <- function(tree, new) {
  tree <- rbind(tree, new)
  tree <- .add_class(tree)
  tree
}

# all direct children of a given leaf in the tree
.children <- function(tree, leaf) {
  stopifnot(rlang::is_scalar_character(leaf))
  rcpp_children(tree = tree, leaf = leaf)
}

# returns number of children for a given leaf in the tree
.nr_children <- function(tree, leaf) {
  length(.children(tree = tree, leaf = leaf))
}

# returns TRUE if the given leaf has no children
.is_leaf <- function(tree, leaf) {
  .nr_children(tree = tree, leaf = leaf) == 0
}

# computes all siblings for each node
.siblings <- function(tree, leaf) {
  .is_valid_leaf(tree, leaf)
  rcpp_siblings(tree = tree, leaf = leaf)
}

# returns number of sibligns for a given leaf in the tree
.nr_siblings <- function(tree, leaf) {
  length(.siblings(tree = tree, leaf = leaf))
}

# checks if a given leaf is valid in the tree
.is_valid_leaf <- function(tree, leaf) {
  stopifnot(rlang::is_scalar_character(leaf))
  if (!rcpp_exists(tree, leaf)) {
    stop("leaf", shQuote(leaf), "does not exist", call. = FALSE)
  }
  invisible(TRUE)
}

# returns TRUE, if a given leaf exists in the tree
.exists <- function(tree, leaf) {
  stopifnot(rlang::is_scalar_character(leaf))
  rcpp_exists(tree, leaf)
}

# returns TRUE if given leaf is the rootnode
.is_rootnode <- function(tree, leaf) {
  .is_valid_leaf(tree, leaf)
  rcpp_is_rootnode(tree = tree, leaf = leaf)
}

# returns path from rootnode to given leaf
.path <- function(tree, leaf) {
  .is_valid_leaf(tree, leaf)
  rcpp_path(tree = tree, leaf = leaf)
}

# numeric level of given leaf in the tree
.level <- function(tree, leaf) {
  .is_valid_leaf(tree, leaf)
  rcpp_level(tree = tree, leaf = leaf)
}

# all levels (numeric of the given tree)
.levels <- function(tree) {
  rcpp_levels(tree = tree)
}

# number of levels
.nr_levels <- function(tree) {
  rcpp_nr_levels(tree = tree)
}

# returns TRUE if it is a bogus (duplicated) leaf
# this is the case if it has no siblings and is a leaf-node
.is_bogus <- function(tree, leaf) {
  .is_valid_leaf(tree, leaf)
  rcpp_is_bogus(tree = tree, leaf = leaf)
}

# returns all bogus_codes
.bogus_codes <- function(tree) {
  rcpp_bogus_codes(tree = tree)
}

# returns name of parent node
.parent <- function(tree, leaf) {
  .is_valid_leaf(tree, leaf)
  rcpp_parent(tree = tree, leaf = leaf)
}

# returns all codes contributing to a specific leaf
.contributing_leaves <- function(tree, leaf) {
  .is_valid_leaf(tree, leaf)
  rcpp_contributing_leaves(tree = tree, leaf = leaf)
}

# sort the tree, top to bottom
.sort <- function(tree) {
  path <- NULL

  # only root node available
  if (nrow(tree) == 1) {
    return(tree)
  }

  nn <- sort(.all_nodes(tree))

  # use / seperated paths to generate correct order
  res <- lapply(nn, function(x) {
    p <- .path(tree, x)
    list(path = p, leaf = tail(p, 1))
  })
  res <- data.table(
    path = sapply(1:length(res), function(x) {
      paste(res[[x]]$path, collapse = "/")
    }),
    leaf = sapply(1:length(res), function(x) {
      res[[x]]$leaf
    })
  )
  setkey(res, path)

  # create a new tree based on this order
  newtree <- list()
  length(newtree) <- nrow(tree)
  ii <- which(tree$root == .rootnode(tree) & is.na(tree$leaf))
  newtree[[1]] <- tree[ii]
  for (i in 1:nrow(res)) {
    ind <- tree$leaf == res$leaf[i]
    newtree[[i]] <- tree[ind]
  }
  newtree <- rbindlist(newtree)
  newtree <- .add_class(newtree)
  attr(newtree, "is_sorted") <- TRUE
  newtree
}

# info about a single leaf in the tree
.info <- function(tree, leaf) {
  stopifnot(rlang::is_scalar_character(leaf))
  rcpp_info(tree = tree, leaf = leaf)
}

# is the tree sorted?
.is_sorted <- function(tree) {
  x <- attr(tree, "is_sorted")
  if (is.null(x)) {
    return(FALSE)
  }
  x == TRUE
}

# data.table with each level being in a sperate column
.tree_to_cols <- function(tree) {
  dt <- lapply(.all_nodes(tree), function(x) {
    data.table(t(.path(tree, x)))
  })
  rbindlist(dt, fill = TRUE)
}

# compute the number of required digits for each level of the tree
.required_digits <- function(tree) {
  dt <- .tree_to_cols(tree)

  # only rootnode
  if (ncol(dt) == 1) {
    return(c(1))
  }

  req_digits <- rep(NA, .nr_levels(tree))
  req_digits[1] <- 1
  for (i in 2:ncol(dt)) {
    tmp <- na.omit(unique(dt[, c(i - 1, i), with = FALSE]))
    s <- split(tmp, tmp[[1]])
    req_digits[i] <- max(nchar(sapply(s, nrow)))
  }
  req_digits
}

# returns TRUE if the code is a minimal code (eg. is required to build the hierarchy)
.is_minimal_code <- function(tree) {
  rcpp_is_minimal_code(tree = tree)
}

# returns names of minimal codes
.minimal_codes <- function(tree) {
  rcpp_minimal_codes(tree = tree)
}

# returns TRUE if the code is a subtotal (not required to build the hierarchy)
.is_subtotal <- function(tree) {
  rcpp_is_subtotal(tree = tree)
}

# returns names of subtotals
.subtotals <- function(tree) {
  rcpp_subtotals(tree = tree)
}

# remove a leaf and all sub-leaves from a tree
.prune <- function(tree, leaf) {
  stopifnot(rlang::is_scalar_character(leaf))
  tree <- rcpp_prune(tree = tree, leaf = leaf)
  tree <- data.table::setalloccol(tree)
  return(tree)
}
