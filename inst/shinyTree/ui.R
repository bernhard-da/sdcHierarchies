shinyUI(pageWithSidebar(
  headerPanel("Drag-and-Drop shinyTree"),

  sidebarPanel(
    shinyjs::useShinyjs(),
    radioButtons("what", "action", choices=c("add","delete","modify")),
    shinyjs::hidden(div(id="action_add",
        textInput("name_addNode", "Level-Name"),
        actionButton("addNode", "Add a Node")
    )),
    shinyjs::hidden(div(id="action_delete", actionButton("delNode", "Delete a Node"))),
    shinyjs::hidden(div(id="action_modify", actionButton("modNode", "Modify a Node")))
  ),
  mainPanel(
    h3("Current Hierarchy"),
    shinyTree("tree", dragAndDrop=TRUE, theme="proton"),
    verbatimTextOutput("str")#,
    #verbatimTextOutput("datatree")

  )
))
