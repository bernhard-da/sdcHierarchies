shinyServer(function(input, output, session) {
  curTree <- reactiveVal()
  curTree(dd)

  curJson <- reactiveVal(json)

  # update tree if json has changed
  observe({
    json <- curJson()
    updateTree(session, "tree", data=json)
  })

  # update JSON in case hierarchies have been moved/dragged around
  observeEvent(input$tree, {
    req(input$tree)
    curJson(convert.to.json(convert.from.tree(input$tree)))
  })

  allNodes <- reactive({
    c("rootnode",fromJSON(curJson())$id)
  })

  observeEvent(input$what, {
    shinyjs::reset("name_addNode")
    if (input$what=="add") {
      shinyjs::hide("action_delete")
      shinyjs::hide("action_rename")
      shinyjs::hide("action_export")
      shinyjs::show("action_add")
    }
    if (input$what=="delete") {
      shinyjs::hide("action_add")
      shinyjs::hide("action_rename")
      shinyjs::hide("action_export")
      shinyjs::show("action_delete")
    }
    if (input$what=="rename") {
      shinyjs::hide("action_add")
      shinyjs::hide("action_delete")
      shinyjs::hide("action_export")
      shinyjs::show("action_rename")
    }
    if (input$what=="export") {
      shinyjs::hide("action_add")
      shinyjs::hide("action_delete")
      shinyjs::hide("action_rename")
      shinyjs::show("action_export")
    }
  })

  output$tree <- renderEmptyTree()

  # print the data.tree
  output$str <- renderPrint({
    convert.from.json(curJson())
  })

  ## add values
  observe({
    updateSelectInput(session, inputId="selAddNode_ref", choices=allNodes())
    updateSelectInput(session, inputId="seldelNode", choices=setdiff(allNodes(),"rootnode"))
    updateSelectInput(session, inputId="selRenameNode", choices=setdiff(allNodes(),"rootnode"))
  })
  # show/hide addNode-Button
  observe({
    if (input$name_addNode=="") {
      shinyjs::hide("addNode")
    } else {
      if (!input$name_addNode %in% allNodes()) {
        shinyjs::show("addNode")
      }
    }
  })

  observeEvent(input$addNode, {
    json <- curJson()
    if (is.null(json)) {
      return(NULL)
    }
    dd <- convert.from.json(json)
    dd <- add_nodes(dd, reference_node=input$selAddNode_ref, node_labs=input$name_addNode)
    curJson(convert.to.json(dd))
    updateTree(session, "tree", data=curJson())
    updateTextInput(session, inputId="name_addNode", value = "")
  })

  ## delete node
  observeEvent(input$delNode, {
    json <- curJson()
    if (is.null(json)) {
      return(NULL)
    }
    dd <- convert.from.json(json)
    ref <- node.find_parent(dd, name=input$seldelNode)
    dd <- delete_nodes(dd, reference_node=ref, node_labs=input$seldelNode)
    curJson(convert.to.json(dd))
    updateTree(session, "tree", data=curJson())
  })

  ## rename a node
  # show/hide renameNode-Button
  observe({
    if (input$name_renameNode=="") {
      shinyjs::hide("modRename")
    } else {
      if (!input$name_renameNode %in% allNodes()) {
        shinyjs::show("modRename")
      }
    }
  })

  observeEvent(input$modRename, {
    json <- curJson()
    if (is.null(json)) {
      return(NULL)
    }
    dd <- convert.from.json(json)
    dd <- node.rename(dd, from=input$selRenameNode, to=input$name_renameNode)
    curJson(convert.to.json(dd))
    updateTree(session, "tree", data=curJson())
    updateTextInput(session, inputId="name_renameNode", value = "")
  })

  ## export/save
  observeEvent(input$saveToObj, {
    tt <- convert.from.tree(input$tree, totLab="asdf")
    stopApp(returnValue=tt)
  })
})
