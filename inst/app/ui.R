ui <- fluidPage(
  titlePanel("Interactive Hierachy Builder"),

  sidebarLayout(
    sidebarPanel(
      div(id="create_root",
          fluidRow(column(12, h3("Initialize the hierarchy"))),
          fluidRow(column(12, textInput("root_name","Label of the total-category","Total"))),
          shinyjs::hidden(fluidRow(id="id_btn_create_root",
            column(12, actionButton("btn_create_root","Create the Hierarchy"))))),

      shinyjs::hidden(div(id="select_action",
          fluidRow(column(12, radioButtons("action", h3("What do you want to do?"),
            choices=c("Add Node(s)"="add","Delete Nodes"="delete", "Modify Nodes"="modify"), selected=character(0)))))),

      shinyjs::hidden(div(id="add_nodes",
        fluidRow(column(12, h3("Add Nodes"))),
        selectInput("refnode_add", "Reference node", choices=NULL),
        shinyjs::hidden(div(id="row_nrchoices", sliderInput("nrchilds", label="how many nodes to add?", min=1, max=5, step=1, value=1))),
        shinyjs::hidden(div(id="row_childs", uiOutput("childs"))),
        shinyjs::hidden(div(id="id_btn_add_nodes", actionButton("btn_add_nodes","Add node(s)")))
      )),
      shinyjs::hidden(div(id="delete_nodes",
        fluidRow(column(12, h3("Delete Nodes"))),
        shinyjs::hidden(div(id="row_delete_impossible", helpText("No nodes can be deleted"))),
        div(id="row_refnode_del", selectInput("refnode_del", "Reference Node", choices=NULL)),
        shinyjs::hidden(div(id="row_node_to_del",selectInput("nodes_to_del", "Nodes to delete", choices=NULL))),
        shinyjs::hidden(div(id="id_btn_del_nodes", actionButton("btn_del_nodes","Delete node")))
      )),
      shinyjs::hidden(div(id="modify_nodes",
        fluidRow(column(12, h3("Modify Nodes")))
      ))
      ),
      mainPanel(
        useShinyjs(),
        fluidRow(column(12, h3("Hierarchy"))),
        fluidRow(column(12, verbatimTextOutput("print_dim"))),
        fluidRow(column(12, h3("Code"))),
        fluidRow(column(12, uiOutput("eval_code"))))
        #grVizOutput("xx"))
  )
)
