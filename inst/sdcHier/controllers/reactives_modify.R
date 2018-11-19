# do we have a tree with root-node only?
isEmptyTree <- reactive({
  if (json()=="[]") {
    return(TRUE)
  }
  return(FALSE)
})

totLevelName <- reactive({
  sdcHier_nodenames(hierarchy())[1]
})

allNodes <- reactive({
  sdcHier_nodenames(hierarchy())
})
