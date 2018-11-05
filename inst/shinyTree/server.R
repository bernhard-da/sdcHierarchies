shinyServer(function(input, output, session) {
  curTree <- reactiveVal()
  curTree(dd)

  observeEvent(input$what, {
    shinyjs::reset("name_addNode")
    if (input$what=="add") {
      shinyjs::hide("action_delete")
      shinyjs::hide("action_modify")
      shinyjs::show("action_add")
    }
    if (input$what=="delete") {
      shinyjs::hide("action_add")
      shinyjs::hide("action_modify")
      shinyjs::show("action_delete")
    }
    if (input$what=="modify") {
      shinyjs::hide("action_add")
      shinyjs::hide("action_delete")
      shinyjs::show("action_modify")
    }
  })

  #observeEvent(input$addNode, {
  #  tt <- curTree()
  #  nn <- input$name_addNode
  #  tt[[1]][[nn]] <- nn
  #  curTree(tt)
  #})

  output$tree <- renderEmptyTree()

  observe({
    updateTree(session, "tree", data=json)
  })

  output$str <- renderPrint({
    req(input$tree)
    #str(input$tree, give.attr = FALSE)
    convert.from.tree(input$tree)
  })
})
