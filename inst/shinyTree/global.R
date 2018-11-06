rm(list=ls())

options(useFancyQuotes = FALSE)
library(shiny)
library(shinyTree)
library(shinyjs)

library(data.tree)
library(sdcTable)
library(jsonlite)
library(rlang)
# dd <- create_node("Total")
# dd <- add_nodes(dd, letters[1:3], reference_node="Total")
# dd <- add_nodes(dd, paste0("a",1:3), reference_node="a")
# dd <- add_nodes(dd, paste0("a1_",1:5), reference_node="a1")
# dd <- add_nodes(dd, paste0("a2_",1:3), reference_node="a2")

dd <- create_node("rootnode")
dd <- add_nodes(dd, "A", reference_node="rootnode")
dd <- add_nodes(dd, "B", reference_node="rootnode")
dd <- add_nodes(dd, "b1", reference_node="B")
dd <- add_nodes(dd, "b1a", reference_node="b1")
dd <- add_nodes(dd, "b1b", reference_node="b1")
dd <- add_nodes(dd, "C", reference_node="rootnode")

#json <- '[{"id":"root","parent":"#","text":"myrootnode","state":{"opened":true,"disabled":false,"selected":false}},
#{"id":"A","parent":"root","text":"A","state":{"opened":true,"disabled":false,"selected":false}},
#{"id":"B","parent":"root","text":"B","state":{"opened":true,"disabled":false,"selected":false}},
#{"id":"b1","parent":"B","text":"b1","state":{"opened":true,"disabled":false,"selected":false}},
#{"id":"b1a","parent":"b1","text":"b1a","state":{"opened":true,"disabled":false,"selected":false}},
#{"id":"b1b","parent":"b1","text":"b1b","state":{"opened":true,"disabled":false,"selected":false}},
#{"id":"C","parent":"root","text":"C","state":{"opened":true,"disabled":false,"selected":false}}]'

# node to json
convert.to.json <- function(dd) {
  write.json.row <- function(id, parent, text, opened=TRUE, disabled=FALSE, selected=FALSE) {
    stopifnot(is_scalar_character(id))
    stopifnot(is_scalar_character(parent))
    stopifnot(is_scalar_character(text))
    op <- ifelse(opened==TRUE, "true","false")
    dis <- ifelse(disabled==TRUE, "true","false")
    sel <- ifelse(selected==TRUE, "true","false")
    js <- paste0("{",dQuote("id"),":",dQuote(id),",",dQuote("parent"),":",dQuote(parent),",",dQuote("text"),":",dQuote(text))
    js <- paste0(js, ",",dQuote("state"),":{",dQuote("opened"),":",op,",",dQuote("disabled"),":",dis,",",dQuote("selected"),":",sel,"}}")
    js
  }

  df <- ToDataFrameTypeCol(dd)

  df[[1]] <- "#"
  js <- "["
  #js <- paste0(js, write.json.row(id=df$level_1[1], parent="#", text=df$level_1[1]))
  for (i in 2:ncol(df)) {
    sub <- unique(df[,c(i-1, i)])
    sub <- sub[!is.na(sub[[2]]),]
    for (j in 1:nrow(sub)) {
      js <- paste0(js, write.json.row(id=sub[[2]][j], parent=sub[[1]][j], text=sub[[2]][j]),",")
    }
  }
  js <- paste0(js,"]")

  sub(",\\]","\\]", js)
}
json <- convert.to.json(dd)


# json to node
convert.from.json <- function(json, totLab=NULL) {
  tab <- fromJSON(json)
  tab <- tab[,c(2,1)]
  colnames(tab) <- c("from","to")
  tab$from[tab$from=="#"] <- "rootnode"
  tt <- FromDataFrameNetwork(tab)
  class(tt) <- c(class(tt), "nodedim")
  tt
}

# input$tree to node
convert.from.tree <- function(tree, totLab=NULL) {
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
  class(aa) <- c(class(aa), "nodedim")
  return(aa)
}


# name of parent
node.find_parent <- function(dd, name) {
  xx <- FindNode(dd, name)
  xx$parent$name
}
