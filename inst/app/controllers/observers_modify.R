# update tree if json has changed
observe({
  if (modify_mode() == TRUE) {
    js <- json()
    updateTree(session, "mytree", data = js)
    dd <- hier_import(inp = js, root = overall_level_name())
    code_modify(hier_convert(dd, as = "code"))
  }
})

# update header containing overall total
shiny::observe({
  shinyjs::html(id = "header_total", html = overall_level_name())
})

# update JSON in case hierarchies have been moved/dragged around
shiny::observeEvent(input$mytree, {
  req(input$mytree)
  json(
    hier_convert(
      shinytree_to_tree(
        tree = input$mytree,
        root = overall_level_name()
      ),
      as = "json"
    )
  )
})

## update select inputs
observe({
  if (!is.null(json())) {
    shiny::updateSelectInput(
      session,
      inputId = "sel_addnode_ref",
      choices = all_nodes()
    )
    shiny::updateSelectInput(
      session,
      inputId = "sel_delnode",
      choices = setdiff(all_nodes(), overall_level_name())
    )
    shiny::updateSelectInput(
      session,
      inputId = "sel_rename_node",
      choices = setdiff(all_nodes(), overall_level_name())
    )
    shiny::updateSelectInput(
      session,
      inputId = "sel_rename_node",
      choices = all_nodes()
    )
  }
})

# enable/disable addNode-Button
shiny::observe({
  if (input$name_add_node == "") {
    shinyjs::disable("btn_add")
  } else {
    if (!input$name_add_node %in% all_nodes()) {
      shinyjs::enable("btn_add")
    }
  }
})

# add a new node
shiny::observeEvent(input$btn_add, {
  js <- json()
  if (is.null(js)) {
    return(NULL)
  }
  dd <- hierarchy()
  dd <- hier_add(
    dd,
    root = input$sel_addnode_ref,
    nodes = input$name_add_node
  )
  json_prev(js)
  json(hier_convert(dd, as = "json"))
  shiny::updateTextInput(session, inputId = "name_add_node", value = "")
})

## delete a node
shiny::observeEvent(input$btn_delete, {
  js <- json()
  if (is.null(js)) {
    return(NULL)
  }
  dd <- hierarchy()
  res <- hier_info(tree = dd, nodes = input$sel_delnode)$parent
  dd <- hier_delete(dd, nodes = input$sel_delnode)
  json_prev(js)
  json(hier_convert(dd, as = "json"))
})

## rename a node
# show/hide renameNode-Button
shiny::observe({
  if (input$name_rename_node == "") {
    shinyjs::disable("btn_rename")
  } else {
    if (!input$name_rename_node %in% all_nodes()) {
      shinyjs::enable("btn_rename")
    }
  }
})

# rename a node
shiny::observeEvent(input$btn_rename, {
  js <- json()
  if (is.null(js)) {
    return(NULL)
  }
  dd <- hierarchy()

  leaves <- input$name_rename_node
  names(leaves) <- input$sel_rename_node
  dd <- hier_rename(
    tree = dd,
    nodes = leaves
  )
  json_prev(js)
  json(hier_convert(dd, as = "json"))
  shiny::updateTextInput(session, inputId = "name_rename_node", value = "")
})

# update the label of the export button
shiny::observeEvent(input$export_format, {
  ff <- input$export_format
  if (!ff == "file") {
    shiny::updateActionButton(
      session,
      inputId = "btn_export",
      label = paste("Export to", ff)
    )
  }
})

shiny::observeEvent(input$export_type, {
  if (input$export_type == "file") {
    shinyjs::hide("row_export_btn")
    shinyjs::show("row_export_dl_btn")
  } else {
    shinyjs::hide("row_export_dl_btn")
    shinyjs::show("row_export_btn")
    shiny::updateActionButton(
      session,
      inputId = "btn_export",
      label = paste("Export to", input$export_format)
    )
  }
})

shiny::observeEvent(input$btn_export, {
  shiny::req(input$export_format)
  js <- json()
  if (is.null(js)) {
    shiny::stopApp(NULL)
  }

  dd <- hier_import(inp = js, root = overall_level_name())
  if (input$export_format == "data.frame") {
    dd <- hier_convert(dd, as = "df")
  }
  if (input$export_format == "argus") {
    dd <- hier_convert(dd, as = "argus")
  }
  if (input$export_format == "code") {
    dd <- hier_convert(dd, as = "code")
  }
  shiny::stopApp(dd)
})

shiny::observeEvent(input$what, {
  shinyjs::hide("action_delete_warning")
  shinyjs::reset("name_add_node")
  if (input$what == "add") {
    shinyjs::hide("action_delete")
    shinyjs::hide("action_rename")
    shinyjs::show("action_add")
  }
  if (input$what == "delete") {
    shinyjs::hide("action_add")
    shinyjs::hide("action_rename")

    if (is_empty_tree()) {
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

shiny::observeEvent(input$btn_reset, {
  json(NULL)
  data(dim)
  shinyjs::reset("tot_is_included")
  shinyjs::reset("method")
  shinyjs::reset("tot_level")
  shinyjs::hide("btn_switch")
  modify_mode(FALSE)
})

shiny::observe({
  js_prev <- json_prev()
  if (!is.null(js_prev)) {
    shinyjs::enable("btn_undo")
  } else {
    shinyjs::disable("btn_undo")
  }
})

shiny::observeEvent(input$btn_undo, {
  js_prev <- json_prev()
  json_prev(NULL)
  json(js_prev)
})
