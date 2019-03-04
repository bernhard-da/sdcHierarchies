# add the class attribute to the tree
.add_class <- function(tree) {
  class(tree) <- unique(c("sdc_hierarchy", class(tree)))
  tree
}

# initializes an empty tree
.init <- function(rootnode) {
  tree <- data.table(
    root = rootnode,
    leaf = rootnode
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
      stop("non-unique leaf nodes detected!\n")
    }
  }
  TRUE
}

# checks if a given leaf is valid in the tree
.is_valid_leaf <- function(tree, leaf) {
  stopifnot(is_scalar_character(leaf))
  if (!.exists(tree = tree, leaf = leaf)) {
    stop(paste("leaf", shQuote(leaf), "does not exist!\n"), call. = FALSE)
  }
  return(TRUE)
}

# returns the names of all nodes
.all_nodes <- function(tree) {
  tree$leaf
}

# returns the name of the rootnode
.rootnode <- function(tree) {
  tree$root[tree$leaf == tree$root]
}

# adds multiple rows to an existing tree
.add_nodes <- function(tree, new) {
  tree <- rbind(tree, new)
  tree <- .add_class(tree)
  tree
}

# all direct children of a given leaf in the tree
.children <- function(tree, leaf) {
  ii <- tree$root == leaf
  res <- as.character(tree$leaf[ii])
  if (length(res) == 0) {
    return(character())
  }
  rootnode <- .rootnode(tree)
  if (leaf == rootnode) {
    res <- setdiff(res, rootnode)
  }
  res
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
  ii <- which(leaf == tree$leaf)
  if (length(ii) == 0) {
    stop(paste("leaf", shQuote(leaf), "not found!"), call. = FALSE)
  }
  parent <- tree$root[ii]

  res <- tree$leaf[tree$root == parent]
  res <- setdiff(res, c(leaf, parent))
  if (length(res) == 0) {
    return(character())
  }
  return(res)
}

# returns number of sibligns for a given leaf in the tree
.nr_siblings <- function(tree, leaf) {
  length(.siblings(tree = tree, leaf = leaf))
}

# returns TRUE, if a given leaf exists in the tree
.exists <- function(tree, leaf) {
  leaf %in% .all_nodes(tree)
}

# returns TRUE if given leaf is the rootnode
.is_rootnode <- function(tree, leaf) {
  leaf == .rootnode(tree)
}

# returns path from rootnode to given leaf
.path <- function(tree, leaf) {
  rootn <- .rootnode(tree)
  if (leaf == rootn) {
    return(leaf)
  }

  pathout <- leaf
  ind <- which(tree$leaf == leaf)
  res <- tree$root[ind]

  pathout <- c(res, pathout)
  while (res != rootn) {
    ind <- which(tree$leaf == res)
    res <- tree$root[ind]
    pathout <- c(res, pathout)
  }
  pathout
}

# numeric level of given leaf in the tree
.level <- function(tree, leaf) {
  length(.path(tree = tree, leaf = leaf))
}

# all levels (numeric of the given tree)
.levels <- function(tree) {
  sapply(.all_nodes(tree), function(x) {
    .level(tree = tree, leaf = x)
  })
}

# number of levels
.nr_levels <- function(tree) {
  max(.levels(tree))
}


# returns TRUE if it is a bogus (duplicated) leaf
# this is the case if it has no siblings and is a leaf-node
.is_bogus <- function(tree, leaf) {
  cond1 <- .nr_children(tree = tree, leaf = leaf) < 2
  cond2 <- .nr_siblings(tree = tree, leaf = leaf) == 0
  cond1 & cond2
}

# returns all bogus_codes
.bogus_codes <- function(tree) {
  if (nrow(tree) == 1) {
    return(character(0))
  }
  sort(names(which(sapply(.all_nodes(tree), function(x) {
    .is_bogus(tree = tree, leaf = x)
  }))))
}

# returns name of parent node
.parent <- function(tree, leaf) {
  if (leaf == .rootnode(tree)) {
    return(NA)
  }
  ii <- which(tree$leaf == leaf)
  return(tree$root[ii])
}

# returns all codes contributing to a specific leaf
.contributing_leaves <- function(tree, leaf) {
  # no nodes contribute to a leaf node
  if (.is_leaf(tree = tree, leaf = leaf)) {
    return(leaf)
  }

  # idea: compute for all nodes if they are leaf-nodes
  # for all these codes compute the path to the top and select those
  # where the given "leaf" is part of the path

  nn <- .all_nodes(tree = tree)
  ind <- sapply(nn, function(x) {
    .is_leaf(tree = tree, leaf = x)
  })

  poss <- nn[ind]

  # if some of the possibles are bogus-codes, we need
  # to use their parents
  ind <- which(sapply(poss, function(x) {
    .is_bogus(tree, leaf = x)
  }))
  while (length(ind) > 0) {
    res <- sapply(names(ind), function(x) {
      .parent(tree = tree, leaf = x)
    })
    poss[ind] <- res

    ind <- which(sapply(poss, function(x) {
      .is_bogus(tree, leaf = x)
    }))
  }

  res <- lapply(poss, function(x) {
    .path(tree = tree, leaf = x)
  })
  names(res) <- poss

  # find those paths in which the given leaf occurs
  ind <- sapply(res, function(x) {
    leaf %in% x
  })
  sort(poss[ind])
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
  out <- list()
  out$name <- leaf
  out$is_rootnode <- .is_rootnode(tree = tree, leaf = leaf)
  out$level <- .level(tree = tree, leaf = leaf)
  out$is_leaf <- .is_leaf(tree = tree, leaf = leaf)
  out$siblings <- .siblings(tree = tree, leaf = leaf)
  out$contributing_codes <- .contributing_leaves(tree = tree, leaf = leaf)
  out$children <- .children(tree = tree, leaf = leaf)
  out$parent <- .parent(tree = tree, leaf = leaf)
  out$is_bogus <- .is_bogus(tree = tree, leaf = leaf)
  out
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
  dt <- rbindlist(dt, fill = TRUE)
  dt
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
    key_vars <- paste0("V", c(i - 1, i))
    setkeyv(dt, key_vars)
    agg <- dt[, .N, by = key(dt)]
    req_digits[i] <- nchar(max(agg[!is.na(agg[[2]])]$N))
  }
  req_digits
}

# computes default codes for a given tree
.default_codes <- function(tree, req_digits) {
  if (!.is_sorted(tree)) {
    tree <- .sort(tree)
  }

  dt <- copy(tree)
  dt$leaf[1] <- .rootnode(tree)
  cc <- paste(rep("0", sum(req_digits)), collapse = "")
  codes_default <- rep(cc, nrow(tree))
  names(codes_default) <- dt$leaf

  dt$levs <- .levels(tree)
  dt$id <- 1:nrow(dt)
  dt <- dt[-1]
  cs <- cumsum(req_digits)
  while (nrow(dt) > 0) {
    code <- dt$leaf[1]
    parent <- .parent(tree, leaf = code)
    code <- setdiff(c(code, .siblings(tree, leaf = code)), NA)
    lev <- dt$levs[1]

    ids <- match(code, names(codes_default))

    first <- cs[lev - 1] + 1
    last <- cs[lev]

    # parent string
    ss <- rep(codes_default[parent], length(code))

    old_val <- as.integer(substring(text = ss, first = first, last = last))
    new_val <- sprintf(
      paste("%0", req_digits[lev], "d", sep = ""),
      old_val + 1:length(code)
    )
    substring(ss, first = first, last = last) <- new_val

    codes_default[ids] <- ss
    dt[setdiff(1:nrow(dt), ids)]
    dt <- dt[!dt$id %in% ids]
  }
  codes_default
}

# returns TRUE if the code is a minimal code (eg. is required to build the hierarchy)
.is_minimal_code <- function(tree) {
  sapply(.all_nodes(tree), function(x) {
    .is_leaf(tree, leaf = x)
  })
}

# returns names of minimal codes
.minimal_codes <- function(tree) {
  r <- .is_minimal_code(tree)
  names(r[which(r == TRUE)])
}

# returns TRUE if the code is a subtotal (not required to build the hierarchy)
.is_subtotal <- function(tree) {
  !.is_minimal_code(tree)
}

# returns names of subtotals
.subtotals <- function(tree) {
  s <- .is_subtotal(tree)
  names(s[which(s == TRUE)])
}

# remove a leaf and all sub-leaves from a tree
.prune <- function(tree, leaf) {
  if (!.exists(tree, leaf)) {
    return(tree)
  }
  todos <- leaf
  while (length(todos) > 0) {
    cc <- todos[1]
    todos <- setdiff(c(todos, .children(tree, cc)), NA)
    keep <- !(cc == tree$root | cc == tree$leaf)
    tree <- tree[keep]
    todos <- todos[-1]
  }
  tree
}
