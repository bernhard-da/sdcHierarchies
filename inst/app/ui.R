ui <- fluidPage(
  titlePanel("Hello Shiny!"),

  sidebarLayout(
    sidebarPanel(
      div(id="create_root",
          fluidRow(column(12, h3("Create the root-node"))),
          fluidRow(column(12, textInput("root_name","root_name",""))),
          shinyjs::hidden(fluidRow(id="id_btn_create_root",
            column(12, actionButton("btn_create_root","create"))))),

      shinyjs::hidden(div(id="select_action",
          fluidRow(column(12, radioButtons("action", "What do you want to do?",
            choices=c("add","delete","modify"), selected=character(0)))))),

      shinyjs::hidden(div(id="add_nodes",
        fluidRow(column(12, h3("Add Nodes"))),
        selectInput("refnode_add", "Reference node", choices=NULL),
        shinyjs::hidden(div(id="row_nrchoices", sliderInput("nrchilds", label="how many nodes to add?", min=1, max=5, step=1, value=1))),
        shinyjs::hidden(div(id="row_childs", uiOutput("childs"))),
        shinyjs::hidden(div(id="id_btn_add_nodes", actionButton("btn_add_nodes","Add node(s)")))
      )),
      shinyjs::hidden(div(id="delete_nodes",
        fluidRow(column(12, h3("Delete Nodes"))),
        selectInput("refnode_del", "Reference Node", choices=NULL),
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
