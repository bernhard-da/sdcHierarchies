# update tree if json has changed
observe({
  if (modify_mode()==TRUE) {
    js <- json()
    updateTree(session, "mytree", data=js)
    dd <- sdcHier_import(inp=js, tot_lab=totLevelName())
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
    updateSelectInput(session, inputId="seldelNode", choices=setdiff(allNodes(),totLevelName()))
    updateSelectInput(session, inputId="selRenameNode", choices=setdiff(allNodes(),totLevelName()))
    updateSelectInput(session, inputId="selRenameNode", choices=allNodes())
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

# add a new node
observeEvent(input$addNode, {
  js <- json()
  if (is.null(js)) {
    return(NULL)
  }
  dd <- sdcHier_import(inp=js, tot_lab=totLevelName())
  dd <- sdcHier_add(dd, refnode=input$selAddNode_ref, node_labs=input$name_addNode)
  hierarchy(dd)
  json(sdcHier_convert(dd, format="json"))
  updateTextInput(session, inputId="name_addNode", value="")
})

## delete a node
observeEvent(input$delNode, {
  js <- json()
  if (is.null(js)) {
    return(NULL)
  }
  dd <- sdcHier_import(inp=js, tot_lab=totLevelName())
  res <- sdcHier_info(dd, node_labs=input$seldelNode)$parent
  dd <- sdcHier_delete(dd, node_labs=input$seldelNode)
  hierarchy(dd)
  json(sdcHier_convert(dd, format="json"))
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

# rename a node
observeEvent(input$modRename, {
  js <- json()
  if (is.null(js)) {
    return(NULL)
  }
  dd <- sdcHier_import(inp=js, tot_lab=totLevelName())
  dd <- sdcHier_rename(dd,
    node_labs=input$selRenameNode,
    node_labs_new=input$name_renameNode)
  hierarchy(dd)
  json(sdcHier_convert(dd, format="json"))
  updateTextInput(session, inputId="name_renameNode", value="")
})

## export/save
observeEvent(input$exportFormat, {
  ff <- input$exportFormat
  if (!ff=="file") {
    updateActionButton(session, inputId="btn_export", label=paste("Export to", ff))
  }
})

observeEvent(input$exportType, {
  if (input$exportType=="file") {
    shinyjs::hide("row_export_btn")
    shinyjs::show("row_export_dl_btn")
  } else {
    shinyjs::hide("row_export_dl_btn")
    shinyjs::show("row_export_btn")
    updateActionButton(session, inputId="btn_export", label=paste("Export to", input$exportFormat))
  }
})

observeEvent(input$btn_export, {
  req(input$exportFormat)
  js <- json()
  if (is.null(js)) {
    stopApp(NULL)
  }

  dd <- sdcHier_import(inp=js, tot_lab=totLevelName())
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
  shinyjs::hide("btn_switch")
  modify_mode(FALSE)
})
