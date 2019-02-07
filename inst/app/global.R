library("shiny")
library("shinyTree")
library("shinyjs")
library("data.tree")
library("jsonlite")
library("rlang")
library("shinyBS")

dim <- getShinyOption(".data")
res <- try(h_is_valid(dim), silent = TRUE)

# start_with_hier: did we start with an existing hierarchy
if (res == TRUE) {
  js <- hier_convert(dim, format = "json")
  start_with_hier <- TRUE
} else {
  js <- NULL
  start_with_hier <- FALSE
}

# converts input$tree to node (internally used only)
shinytree_to_node <- function(tree, tot_lab=NULL) {
  json <- toJSON(tree)
  json <- gsub("\\[0\\]", "[]", json)
  ll <- fromJSON(json)
  tt <- data.tree:::as.Node.list(ll)

  aa <- ToDataFrameTypeCol(tt)

  if (!is.null(tot_lab)) {
    aa[[1]] <- tot_lab
  }

  aa$path <- apply(aa, 1, paste, collapse = "/")
  aa <- FromDataFrameTable(aa, pathName = "path")
  class(aa) <- c(class(aa), "sdcHier")
  return(aa)
}
