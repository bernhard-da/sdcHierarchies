shinyUI(navbarPage("Interactive sdcHierarches",
  tabPanel("Hierarchy",
    useShinyjs(),
    sidebarLayout(
      sidebarPanel(width=4,
        shinyjs::hidden(div(id="sidebar_create",
          fluidRow(
            column(6, align="center", div(id="help_tot_is_included", icon(name="question-circle"))),
            column(6, align="center", div(id="help_method", icon(name="question-circle")))
          ),
          fluidRow(
            column(6, align="center", radioButtons("tot_is_included", label="Overall Total available?", choices=c("yes","no"), inline=TRUE)),
            column(6, align="center", radioButtons("method", label="Specify level-structure by", choices=c("length"="len","endpos"="endpos"), inline=TRUE))
          ),
          shinyjs::hidden(fluidRow(id="row_tot_level",
            column(12, align="center", textInput("tot_level", "Name of overall total category", value=""))
          )),
          bsTooltip(id="help_tot_is_included", placement="top", title="help for tot_is_incl"),
          bsTooltip(id="help_method", placement="top", title="help for method"),

          fluidRow(id="row_nr_levels",
            column(12, align="center", sliderInput("nr_levels", "Number of levels", min=1, max=6, val=1, step=1, ticks=FALSE))
          ),
          fluidRow(id="row_helptxt",
            column(12, align="center", div(id="helptxt", ""))
          ),
          uiOutput("spec"),
          actionButton("createHier", "createHierarchy", class="btn-success")
        )),
        div(id="sidebar_modify",
            radioButtons("what", h4("What do you want to do?"),
              choices=c("Add a Node"="add","Delete a Node"="delete","Rename a Node"="rename")),
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
            )
          )
        )
      ),
      mainPanel(
        shinyjs::hidden(div(id="div_create",
          fluidRow(
            column(6, h1("Original Input"), align="center"),
            column(6, h1("Generated Hierarchy"), align="center")
          ),
          fluidRow(
            column(6, div(id="col_orig", verbatimTextOutput("origDim"), align="center")),
            column(6, div(id="col_generated", verbatimTextOutput("generatedDim"), align="center")),
            shinyjs::hidden(div(id="error_gen", p("Something went wrong, please try other specifications"), align="center"))
          ),
          shinyjs::hidden(fluidRow(id="row_btn_switch",
            column(12, align="left", actionButton("btn_switch", "Switch to modify view", class="btn-success"))
          ))
        )),
        div(id="div_modify",
          fluidRow(column(12, align="left",h1("Modify the Hierarchy"))),
          fluidRow(id="row_reset_btn",
            column(12, align="left", actionButton("reset_btn", "Reset hierarchy", class="btn-danger")),
            tags$br()
          ),
          fluidRow(
            column(6, align="left", h3("Interactive")),
            column(6, align="left", h3("Output"))
          ),
          fluidRow(
            column(6, align="left", shinyTree("mytree", dragAndDrop=TRUE, theme="proton")),
            column(6, align="left", verbatimTextOutput("treeprint"))
          )
        )
      )
    )
  ),
  tabPanel("Code",
    div(id="div_code_hidden", h1("Please create a hierarchy first")),
      shinyjs::hidden(div(id="div_code",
      fluidRow(
        column(12, align="left",h1("Code")),
        column(12, align="left", verbatimTextOutput("requiredCode"))
      ),
      fluidRow(
        column(12, align="left", actionButton("reset_dl_code", "Download the code", class="btn-success"))
      )
    )
  )),
  tabPanel("Export",
    div(id="div_export_hidden", h1("Please create a hierarchy first")),
    shinyjs::hidden(div(id="div_export",
      fluidRow(
        column(12, align="left", h1("Export hierarchy"))
      ),
      fluidRow(
        column(12, align="left", p("Select a output-format and additional options to export the current hierarchy"))
      ),
      fluidRow(
        column(6, align="center", selectInput("exportFormat", "Format for export", choices=c("node", "data.frame"))),
        column(6, align="center", textInput("name_exportTot", "Node-Name for overall total", value="Total"))
      ),
      fluidRow(
        column(12, align="center", actionButton("modExport", "Export", class="btn-success"))
      )
    )
  ))
))
