# update tree if json has changed
observe({
  if (modify_mode() == TRUE) {
    js <- json()
    updateTree(session, "mytree", data = js)
    dd <- sdcHier_import(inp = js, tot_lab = totLevelName())
    code_modify(sdcHier_convert(dd, format = "code"))
  }
})

# update header containing overall total
observe({
  shinyjs::html(id = "header_total", html = totLevelName())
})

# update JSON in case hierarchies have been moved/dragged around
observeEvent(input$mytree, {
  req(input$mytree)
  json(sdcHier_convert(shinytree_to_node(input$mytree), format = "json"))
})

## update select inputs
observe({
  if (!is.null(json())) {
    updateSelectInput(session, inputId = "selAddNode_ref", choices = allNodes())
    updateSelectInput(session, inputId = "seldelNode", choices = setdiff(allNodes(), totLevelName()))
    updateSelectInput(session, inputId = "selRenameNode", choices = setdiff(allNodes(), totLevelName()))
    updateSelectInput(session, inputId = "selRenameNode", choices = allNodes())
  }
})

# enable/disable addNode-Button
observe({
  if (input$name_addNode == "") {
    shinyjs::disable("btn_add")
  } else {
    if (!input$name_addNode %in% allNodes()) {
      shinyjs::enable("btn_add")
    }
  }
})

# add a new node
observeEvent(input$btn_add, {
  js <- json()
  if (is.null(js)) {
    return(NULL)
  }
  dd <- hierarchy()
  dd <- sdcHier_add(dd, refnode = input$selAddNode_ref, node_labs = input$name_addNode)
  json_prev(js)
  json(sdcHier_convert(dd, format = "json"))
  updateTextInput(session, inputId = "name_addNode", value = "")
})

## delete a node
observeEvent(input$btn_delete, {
  js <- json()
  if (is.null(js)) {
    return(NULL)
  }
  dd <- hierarchy()
  res <- sdcHier_info(dd, node_labs = input$seldelNode)$parent
  dd <- sdcHier_delete(dd, node_labs = input$seldelNode)
  json_prev(js)
  json(sdcHier_convert(dd, format = "json"))
})

## rename a node
# show/hide renameNode-Button
observe({
  if (input$name_renameNode == "") {
    shinyjs::disable("btn_rename")
  } else {
    if (!input$name_renameNode %in% allNodes()) {
      shinyjs::enable("btn_rename")
    }
  }
})

# rename a node
observeEvent(input$btn_rename, {
  js <- json()
  if (is.null(js)) {
    return(NULL)
  }
  dd <- hierarchy()
  dd <- sdcHier_rename(dd,
    node_labs = input$selRenameNode,
    node_labs_new = input$name_renameNode)
  json_prev(js)
  json(sdcHier_convert(dd, format = "json"))
  updateTextInput(session, inputId = "name_renameNode", value = "")
})

# update the label of the export button
observeEvent(input$exportFormat, {
  ff <- input$exportFormat
  if (!ff == "file") {
    updateActionButton(session, inputId = "btn_export", label = paste("Export to", ff))
  }
})

observeEvent(input$exportType, {
  if (input$exportType == "file") {
    shinyjs::hide("row_export_btn")
    shinyjs::show("row_export_dl_btn")
  } else {
    shinyjs::hide("row_export_dl_btn")
    shinyjs::show("row_export_btn")
    updateActionButton(session, inputId = "btn_export", label = paste("Export to", input$exportFormat))
  }
})

observeEvent(input$btn_export, {
  req(input$exportFormat)
  js <- json()
  if (is.null(js)) {
    stopApp(NULL)
  }

  dd <- sdcHier_import(inp = js, tot_lab = totLevelName())
  if (input$exportFormat == "data.frame") {
    dd <- sdcHier_convert(dd, format = "data.frame")
  }
  if (input$exportFormat == "argus") {
    dd <- sdcHier_convert(dd, format = "argus")
  }
  if (input$exportFormat == "code") {
    dd <- sdcHier_convert(dd, format = "code")
  }
  stopApp(dd)
})

observeEvent(input$what, {
  shinyjs::hide("action_delete_warning")
  shinyjs::reset("name_addNode")
  if (input$what == "add") {
    shinyjs::hide("action_delete")
    shinyjs::hide("action_rename")
    shinyjs::show("action_add")
  }
  if (input$what == "delete") {
    shinyjs::hide("action_add")
    shinyjs::hide("action_rename")

    if (isEmptyTree()) {
      shinyjs::hide("action_delete")
      shinyjs::show("action_delete_warning")

    } else {
      shinyjs::show("action_delete")
      shinyjs::hide("action_delete_warning")
    }
  }
  if (input$what == "rename") {
    shinyjs::hide("action_add")
    shinyjs::hide("action_delete")
    shinyjs::show("action_rename")
  }
})

observeEvent(input$btn_reset, {
  json(NULL)
  data(dim)
  shinyjs::reset("tot_is_included")
  shinyjs::reset("method")
  shinyjs::reset("tot_level")
  shinyjs::hide("btn_switch")
  modify_mode(FALSE)
})

observe({
  js_prev <- json_prev()
  if (!is.null(js_prev)) {
    shinyjs::enable("btn_undo")
  } else {
    shinyjs::disable("btn_undo")
  }
})

observeEvent(input$btn_undo, {
  js_prev <- json_prev()
  json_prev(NULL)
  json(js_prev)
})
