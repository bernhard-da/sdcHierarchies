shinyServer(function(input, output, session) {
  curTree <- reactiveVal()
  curTree(dd)

  curJson <- reactiveVal(json)

  # do we have a tree with root-node only?
  isEmptyTree <- reactive({
    if (curJson()=="[]") {
      return(TRUE)
    }
    return(FALSE)
  })

  # update tree if json has changed
  observe({
    json <- curJson()
    updateTree(session, "tree", data=json)

    if (isEmptyTree()) {
      shinyjs::show("row_empty_results")
      shinyjs::hide("row_info_drag")
      shinyjs::hide("row_results")

      shinyjs::hide("seldelNode")
      shinyjs::hide("delNode")
      shinyjs::show("row_msg_delete")

      shinyjs::hide("selRenameNode")
      shinyjs::hide("name_renameNode")
      shinyjs::hide("modRename")
      shinyjs::show("row_msg_rename")

      shinyjs::hide("exportFormat")
      shinyjs::hide("name_exportTot")
      shinyjs::hide("modExport")
      shinyjs::show("row_msg_export")
    } else {
      shinyjs::hide("row_empty_results")
      shinyjs::show("row_info_drag")
      shinyjs::show("row_results")

      shinyjs::show("seldelNode")
      shinyjs::show("delNode")
      shinyjs::hide("row_msg_delete")

      shinyjs::show("selRenameNode")
      shinyjs::show("name_renameNode")
      shinyjs::show("modRename")
      shinyjs::hide("row_msg_rename")

      shinyjs::show("exportFormat")
      shinyjs::show("name_exportTot")
      shinyjs::show("modExport")
      shinyjs::hide("row_msg_export")
    }
  })

  # update JSON in case hierarchies have been moved/dragged around
  observeEvent(input$tree, {
    req(input$tree)
    curJson(sdcHier_convert(shinytree_to_node(input$tree), format="json"))
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
    sdcHier_import(inp=curJson(), tot_lab=NULL)
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
    dd <- sdcHier_import(inp=json, tot_lab=NULL)
    dd <- sdcHier_add(dd, refnode=input$selAddNode_ref, node_labs=input$name_addNode)
    curJson(sdcHier_convert(dd, format="json"))
    updateTree(session, "tree", data=curJson())
    updateTextInput(session, inputId="name_addNode", value = "")
  })

  ## delete node
  observeEvent(input$delNode, {
    json <- curJson()
    if (is.null(json)) {
      return(NULL)
    }
    dd <- sdcHier_import(inp=json, tot_lab=NULL)
    res <- sdcHier_info(dd, node_labs=input$seldelNode)$parent
    dd <- sdcHier_delete(dd, node_labs=input$seldelNode)
    curJson(sdcHier_convert(dd, format="json"))
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
    dd <- sdcHier_import(inp=json, tot_lab=NULL)
    dd <- sdcHier_rename(dd,
      node_labs=input$selRenameNode,
      node_labs_new=input$name_renameNode)

    curJson(sdcHier_convert(dd, format="json"))
    updateTree(session, "tree", data=curJson())
    updateTextInput(session, inputId="name_renameNode", value="")
  })

  ## export/save
  observeEvent(input$exportFormat, {
    updateActionButton(session, inputId="modExport", label=paste("Export to", input$exportFormat))
  })
  # show/hide modExport-Button
  observe({
    if (input$name_exportTot=="") {
      shinyjs::hide("modExport")
    } else {
      if (!input$name_exportTot %in% allNodes()) {
        shinyjs::show("modExport")
      }
    }
  })
  observeEvent(input$modExport, {
    req(input$exportFormat)
    json <- curJson()
    if (is.null(json)) {
      return(NULL)
    }
    dd <- sdcHier_import(inp=json, tot_lab=input$name_exportTot)
    if (input$exportFormat=="data.frame") {
      dd <- sdcHier_convert(dd, format="data.frame")
    }
    stopApp(dd)
  })
})
