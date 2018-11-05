library(shiny)
library(shinyTree)
shinyServer(function(input, output, session) {
  curTree <- reactiveVal()
  #ll <- list(
  #  root1 = "",
  #  root2 = list(
  #    SubListA = list(leaf1 = "", leaf2 = list(xx=list(a="",b="")))
  #  )
  #)

  ll <- list("total")
  ll$tot <- list(letters[1:4])
  ll$a$asf <- list("x", "y")
  curTree(ll)



  #ll <- list(name="Total", xx=list())
  #ll$xx <- list()
#
  #ll$xx <- append(ll$xx, list(name1="v1"))
  #ll$xx <- append(ll$xx, list(name2="v2"))

  #ll$root1$children <- list(name="a", name="b")
  #as.Node(ll)


  observeEvent(input$addNode, {
    tt <- curTree()
    nn <- paste0("node_",length(tt)+1)
    tt[[nn]] <- nn
    curTree(tt)
  })

  output$tree <- renderTree({
    curTree()
  })

  output$str <- renderPrint({
    str(input$tree, give.attr = FALSE)
  })
  #output$datatree <- renderPrint({
  #  print(data.tree::as.Node(curTree(), mode="explicit"))
  #})
})
