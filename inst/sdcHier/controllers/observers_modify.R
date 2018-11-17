# update tree if json has changed
observe({
  if (modify_mode()==TRUE) {
    js <- json()
    updateTree(session, "mytree", data=js)
    dd <- sdcHier_import(inp=js, tot_lab=input$name_exportTot)
    code_modify(sdcHier_convert(dd, format="code"))
  }
})

# update JSON in case hierarchies have been moved/dragged around
observeEvent(input$mytree, {
  req(input$mytree)
  json(sdcHier_convert(shinytree_to_node(input$mytree), format="json"))
})

## update select inputs
observe({
  if (!is.null(json())) {
    updateSelectInput(session, inputId="selAddNode_ref", choices=allNodes())
    updateSelectInput(session, inputId="seldelNode", choices=setdiff(allNodes(),"rootnode"))
    updateSelectInput(session, inputId="selRenameNode", choices=setdiff(allNodes(),"rootnode"))
  }
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
  js <- json()
  if (is.null(js)) {
    return(NULL)
  }
  dd <- sdcHier_import(inp=js, tot_lab=NULL)
  dd <- sdcHier_add(dd, refnode=input$selAddNode_ref, node_labs=input$name_addNode)
  json(sdcHier_convert(dd, format="json"))
  updateTree(session, "mytree", data=json())
  updateTextInput(session, inputId="name_addNode", value="")
})

## delete node
observeEvent(input$delNode, {
  js <- json()
  if (is.null(js)) {
    return(NULL)
  }
  dd <- sdcHier_import(inp=js, tot_lab=NULL)
  res <- sdcHier_info(dd, node_labs=input$seldelNode)$parent
  dd <- sdcHier_delete(dd, node_labs=input$seldelNode)
  json(sdcHier_convert(dd, format="json"))
  updateTree(session, "mytree", data=json())
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
  js <- json()
  if (is.null(js)) {
    return(NULL)
  }
  dd <- sdcHier_import(inp=js, tot_lab=NULL)
  dd <- sdcHier_rename(dd,
    node_labs=input$selRenameNode,
    node_labs_new=input$name_renameNode)

  json(sdcHier_convert(dd, format="json"))
  updateTree(session, "mytree", data=json())
  updateTextInput(session, inputId="name_renameNode", value="")
})

## export/save
observeEvent(input$exportFormat, {
  updateActionButton(session, inputId="modExport", label=paste("Export to", input$exportFormat))
})

# show/hide modExport-Button
observe({
  if (!is.null(json())) {
    if (input$name_exportTot=="") {
      shinyjs::hide("modExport")
    } else {
      if (!input$name_exportTot %in% allNodes()) {
        shinyjs::show("modExport")
      }
    }
  }
})
observeEvent(input$modExport, {
  req(input$exportFormat)
  js <- json()
  if (is.null(js)) {
    return(NULL)
  }
  dd <- sdcHier_import(inp=js, tot_lab=input$name_exportTot)
  if (input$exportFormat=="data.frame") {
    dd <- sdcHier_convert(dd, format="data.frame")
  }
  stopApp(dd)
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

observeEvent(input$reset_btn, {
  json(NULL)
  data(dim)
  hierarchy(NULL)
  shinyjs::reset("tot_is_included")
  shinyjs::reset("method")
  shinyjs::reset("tot_level")
  shinyjs::hide("row_btn_switch")
  modify_mode(FALSE)
})
