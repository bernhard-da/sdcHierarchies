library(shinythemes)
shinyUI(fluidPage(theme = shinytheme("flatly"),
  headerPanel("sdcHier (dyn)"),

  sidebarPanel(
    shinyjs::useShinyjs(),

    radioButtons("what", h3("What do you want to do?"),
        choices=c("Add a Node"="add","Delete a Node"="delete","Rename a Node"="rename","Export Hierarchy"="export")),
    shinyjs::hidden(div(id="action_add",
        selectInput("selAddNode_ref", "Reference-Node", choices=NULL),
        textInput("name_addNode", "Level-Name"),
        actionButton("addNode", "Add new Node")
    )),
    shinyjs::hidden(div(id="action_delete",
      selectInput("seldelNode", "Select Node for Deletion", choices=NULL),
      actionButton("delNode", "Delete selected Node"),
      div(id="row_msg_delete", p("No Nodes available to delete"))
    )),
    shinyjs::hidden(div(id="action_rename",
      selectInput("selRenameNode", "Select Node to Rename", choices=NULL),
      textInput("name_renameNode", "new Label", value=""),
      actionButton("modRename", "Rename selected Node"),
      div(id="row_msg_rename", p("No Nodes available to rename"))
    )),

    shinyjs::hidden(div(id="action_export",
      selectInput("exportFormat", "Format for export", choices=c("node", "data.frame")),
      textInput("name_exportTot", "Node-Name for overall total", value="rootnode"),
      tags$button(id='modExport', type="button",
        class="btn action-button", onclick="setTimeout(function(){window.close();},500);", "Export"),
      div(id="row_msg_export", p("No hierarchy available to export"))
    ))
  ),
  mainPanel(
    fluidRow(
      column(6, align="center", h2("Current Hierarchy (interactive)")),
      column(6, align="center", h2("Output"))
    ),

    fluidRow(
      column(6, align="center", p("Drag-and-drop nodes around!")),
      column(6, align="center", p(""))
    ),
    fluidRow(
      column(6, align="center", div(id="output_tree_dynamic", shinyTree("tree", dragAndDrop=TRUE, theme="proton"))),
      column(6, align="center", div(id="output_table_static", verbatimTextOutput("str")))
    ),

    fluidRow(
      column(6, align="center", div(id="output_tree_dynamic_empty", p("Please start adding nodes!"))),
      column(6, align="center", div(id="output_table_static_empty", p("No hierarchy defined")))
    )
  )
))
