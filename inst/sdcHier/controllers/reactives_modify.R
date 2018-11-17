# do we have a tree with root-node only?
isEmptyTree <- reactive({
  if (json()=="[]") {
    return(TRUE)
  }
  return(FALSE)
})

allNodes <- reactive({
  c("rootnode",fromJSON(json())$id)
})
