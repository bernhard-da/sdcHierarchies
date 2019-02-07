# do we have a tree with root-node only?
is_empty_tree <- reactive({
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
  hier_import(inp = js, from = "json")
})

overall_level_name <- reactive({
  attributes(json())$totlev
})

all_nodes <- reactive({
  dd <- hierarchy()
  if (is.null(dd)) {
    return("")
  }
  hier_nodenames(dd)
})
