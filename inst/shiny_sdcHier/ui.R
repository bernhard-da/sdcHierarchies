library(shinythemes)
shinyUI(fluidPage(theme = shinytheme("flatly"),
  headerPanel("Create and modify a hierarchy"),

  sidebarPanel(
    shinyjs::useShinyjs(),

    radioButtons("what", h3("What do you want to do?"),
        choices=c("Add a Node"="add","Delete a Node"="delete","Rename a Node"="rename","Export Hierarchy"="export")),
    shinyjs::hidden(div(id="action_add",
        selectInput("selAddNode_ref", "Reference-Node", choices=NULL),
        textInput("name_addNode", "Level-Name"),
        actionButton("addNode", "Add new Node", class="btn-success")
    )),
    shinyjs::hidden(div(id="action_delete",
      selectInput("seldelNode", "Select Node for Deletion", choices=NULL),
      actionButton("delNode", "Delete selected Node", class="btn-success"),
      div(id="row_msg_delete", p("No Nodes available to delete"))
    )),
    shinyjs::hidden(div(id="action_rename",
      selectInput("selRenameNode", "Select Node to Rename", choices=NULL),
      textInput("name_renameNode", "new Label", value=""),
      actionButton("modRename", "Rename selected Node", class="btn-success"),
      div(id="row_msg_rename", p("No Nodes available to rename"))
    )),

    shinyjs::hidden(div(id="action_export",
      selectInput("exportFormat", "Format for export", choices=c("node", "data.frame")),
      textInput("name_exportTot", "Node-Name for overall total", value="rootnode"),
      actionButton("modExport", "Export", class="btn-success"),
      div(id="row_msg_export", p("No hierarchy available to export"))
    ))
  ),
  mainPanel(
    fluidRow(
      column(6, align="center", h2("Current hierarchy (interactive)")),
      column(6, align="center", h2("Output"))
    ),

    fluidRow(id="row_info_drag",
      column(6, align="center", p("Drag-and-drop nodes around!")),
      column(6, align="center", NULL)
    ),
    fluidRow(id="row_results",
      column(6, align="left", shinyTree("tree", dragAndDrop=TRUE, theme="proton")),
      column(6, align="center", verbatimTextOutput("str"))
    ),

    fluidRow(id="row_empty_results",
      column(6, align="center", p("Please start adding nodes!")),
      column(6, align="center", p("No hierarchy defined"))
    ),

    fluidRow(id="row_code",
      column(12, align="left", h2("R-Code")),
      column(12, align="left", verbatimTextOutput("code"))
    )
  )
))
