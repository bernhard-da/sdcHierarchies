rm(list=ls())

options(useFancyQuotes=FALSE)
library(shiny)
library(shinyTree)
library(shinyjs)
library(data.tree)
library(jsonlite)
library(rlang)

data <- getShinyOption(".data")
if (is.null(data)) {
  dd <- sdcHier_create("rootnode")
} else {
  if (h_is_valid(data)) {
    dd <- data
  } else {
    dd <- sdcHier_create("rootnode")
  }
}
rm(data)

# converts input$tree to node (internally used only)
shinytree_to_node <- function(tree, totLab=NULL) {
  json <- toJSON(tree)
  json <- gsub("\\[0\\]", '[]', json)
  ll <- fromJSON(json)
  tt <- data.tree:::as.Node.list(ll)

  aa <- ToDataFrameTypeCol(tt)
  aa[is.na(aa)] <- ""

  if (!is.null(totLab)) {
    aa[[1]] <- "bla"
  }
  aa$path <- apply(aa, 1, paste, collapse="/")
  aa <- FromDataFrameTable(aa, pathName="path")
  class(aa) <- c(class(aa), "sdcHier")
  return(aa)
}

json <- sdcHier_convert(dd, format="json")
