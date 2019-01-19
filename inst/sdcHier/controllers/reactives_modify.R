# do we have a tree with root-node only?
isEmptyTree <- reactive({
  if (json() == "[]") {
    return(TRUE)
  }
  return(FALSE)
})

hierarchy <- reactive({
  js <- json()
  if (is.null(js)) {
    return(NULL)
  }
  sdcHier_import(inp = js, from = "json")
})

totLevelName <- reactive({
  attributes(json())$totlev
})

allNodes <- reactive({
  dd <- hierarchy()
  if (is.null(dd)) {
    return("")
  }
  sdcHier_nodenames(dd)
})
