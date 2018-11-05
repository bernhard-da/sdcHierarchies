library(shiny)
library(shinyTree)
shinyUI(
  pageWithSidebar(
    headerPanel("Drag-and-Drop shinyTree"),

    sidebarPanel(
      actionButton("addNode", "add a node")
    ),
    mainPanel(
      h3("the current tree"),
      shinyTree("tree", dragAndDrop=TRUE, theme="proton"),
      verbatimTextOutput("str")#,
      #verbatimTextOutput("datatree")

    )
  ))
