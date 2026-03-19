library(shiny)
library(shinyTree)
library(shinyjs)
library(jsonlite)
library(rlang)
library(shinyBS)
library(sdcHierarchies)
library(data.table)

dim <- getShinyOption(".data")
res <- try(sdcHierarchies:::.is_valid(dim), silent = TRUE)

# start_with_hier: did we start with an existing hierarchy
if (res == TRUE) {
  js <- hier_convert(dim, as = "json")
  start_with_hier <- TRUE
} else {
  js <- NULL
  start_with_hier <- FALSE
}

# converts input$tree to a sdc_hierarchy object
shinytree_to_tree <- function(tree, root = "Total") {
  # Helper to recursively extract paths from the nested list
  get_paths <- function(l, current_path = character()) {
    if (is.null(names(l))) {
      return(paste(current_path, collapse = "/"))
    }

    results <- list()
    for (n in names(l)) {
      new_path <- c(current_path, n)
      # add the current path to results
      results[[n]] <- paste(new_path, collapse = "/")

      # recurse if required
      if (is.list(l[[n]]) && length(l[[n]]) > 0) {
        results <- c(results, get_paths(l[[n]], new_path))
      }
    }
    return(unlist(results))
  }

  # extract and split the paths
  paths <- get_paths(tree)
  if (is.null(paths)) {
    return(hier_create(root = root))
  }

  # Prefix with root
  paths <- paste0(root, "/", paths)

  # build a data.table for level-by-level insertion
  # we can use tstrsplit to handle paths of varying depths safely
  ll <- strsplit(paths, "/")
  dt <- rbindlist(lapply(ll, function(x) data.table(t(x))), fill = TRUE)

  # initialize the hierarchy
  h <- hier_create(root = dt[[1]][1])
  if (ncol(dt) == 1) {
    return(h)
  }

  # iteratively add nodes
  for (i in 2:ncol(dt)) {
    # select current parent (V[i-1]) and current child (V[i])
    parent_col <- paste0("V", i - 1)
    child_col <- paste0("V", i)

    # get unique rows for this level that aren't NA
    new_nodes <- unique(dt[!is.na(get(child_col)), c(parent_col, child_col), with = FALSE])
    setnames(new_nodes, c("root", "leaf"))

    if (nrow(new_nodes) > 0) {
      new_nodes <- new_nodes[!leaf %in% h$leaf]

      if (nrow(new_nodes) > 0) {
        # add level information
        # We calculate level based on the parent's current level in 'h'
        for (j in 1:nrow(new_nodes)) {
          p_node <- new_nodes$root[j]
          c_node <- new_nodes$leaf[j]

          lev <- h$level[h$leaf == p_node] + 1
          h <- sdcHierarchies:::.add_nodes(
            tree = h,
            new = data.table(root = p_node, leaf = c_node, level = lev)
          )
        }
      }
    }
  }
  return(h)
}
