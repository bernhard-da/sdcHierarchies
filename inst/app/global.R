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
shinytree_to_tree <- function(tree, root = NULL) {
  if (is.null(root)) {
    root <- "Total"
  }

  json <- toJSON(tree)

  ul <- names(unlist(fromJSON(json), recursive = TRUE))
  ul <- gsub(".", "/", ul, fixed = TRUE)

  # correct code names (fixing bugs with codes containing dots)
  json <- gsub(":\\[0\\]", "", json)
  codes <- gsub(",", "", json)
  codes <- gsub(":", "", codes)
  codes <- gsub("\\{", "", codes)
  codes <- gsub("\\}", "", codes)
  codes <- unlist(strsplit(codes, '"'))
  codes <- codes[codes != ""]
  codes <- codes[grep("[.]", codes)]
  if (length(codes) > 0) {
    for (i in seq_along(codes)) {
      corr <- codes[i]
      wrong <- sub("[.]", "/", corr)
      ul <- gsub(wrong, corr, ul)
    }
  }
  ul <- paste0(root, "/", ul)


  # create the tree
  ll <- strsplit(ul, "/")
  dt <- lapply(ll, function(x) data.table(t(x)))
  dt <- rbindlist(dt, fill = TRUE)

  tree <- hier_create(root = dt$V1[1])

  if (ncol(dt) == 1) {
    return(tree)
  }

  for (i in 2:ncol(dt)) {
    vars <- paste0("V", c(i - 1):i)
    new <- unique(dt[, vars, with = FALSE])
    new <- new[!is.na(new[[2]])]
    new$level <- tree$level[tree$leaf == new$root[1]] + 1
    setnames(new, c("root", "leaf", "level"))
    tree <- sdcHierarchies:::.add_nodes(tree = tree, new = new)
  }
  tree
}
