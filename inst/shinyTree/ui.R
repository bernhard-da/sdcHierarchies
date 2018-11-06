shinyUI(pageWithSidebar(
  headerPanel("Drag-and-Drop shinyTree"),

  sidebarPanel(
    shinyjs::useShinyjs(),
    radioButtons("what", h3("What do you want to do?"), choices=c("add","delete","rename","export")),
    shinyjs::hidden(div(id="action_add",
        selectInput("selAddNode_ref", "Reference-Node", choices=NULL),
        textInput("name_addNode", "Level-Name"),
        actionButton("addNode", "Add a Node")
    )),
    shinyjs::hidden(div(id="action_delete",
      selectInput("seldelNode", "Select Node for Deletion", choices=NULL),
      actionButton("delNode", "Delete a Node")
    )),
    shinyjs::hidden(div(id="action_rename", actionButton("modRename", "Rename a Node"))),
    shinyjs::hidden(div(id="action_export", actionButton("modExport", "Export")))

  ),
  mainPanel(
    h3("Current Hierarchy"),
    shinyTree("tree", dragAndDrop=TRUE, theme="proton"),
    verbatimTextOutput("str"),
    actionButton("saveToObj", "Save Hierarchy to Object")
  )
))
