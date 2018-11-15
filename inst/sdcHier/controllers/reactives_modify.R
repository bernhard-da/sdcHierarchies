# do we have a tree with root-node only?
isEmptyTree <- reactive({
  if (curJson()=="[]") {
    return(TRUE)
  }
  return(FALSE)
})

allNodes <- reactive({
  c("rootnode",fromJSON(curJson())$id)
})
