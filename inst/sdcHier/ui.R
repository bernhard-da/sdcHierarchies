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

          fluidRow(
            column(6, align="center", actionButton("createHier", "createHierarchy", class="btn-success")),
            column(6, align="left", shinyjs::hidden(actionButton("btn_switch", "Switch to modify view", class="btn-primary")))
          )
        )),
        shinyjs::hidden(div(id="sidebar_modify",
            radioButtons("what", h4("What do you want to do?"),
              choices=c("Add a Node"="add","Delete a Node"="delete","Rename a Node"="rename")),
            shinyjs::hidden(div(id="action_add",
              selectInput("selAddNode_ref", "Reference-Node", choices=NULL),
              textInput("name_addNode", "Level-Name"),
              actionButton("addNode", "Add new Node", class="btn-success")
            )),
            shinyjs::hidden(div(id="action_delete",
              selectInput("seldelNode", "Select Node for Deletion", choices=NULL),
              actionButton("delNode", "Delete selected Node", class="btn-success")
            )),
            shinyjs::hidden(div(id="action_delete_warning",
              p("You need to add nodes/levels to your hierarchy before you can delete any.")
            )),
            shinyjs::hidden(div(id="action_rename",
              selectInput("selRenameNode", "Select Node to Rename", choices=NULL),
              textInput("name_renameNode", "new Label", value=""),
              actionButton("modRename", "Rename selected Node", class="btn-success")
            )
          )
        ))
      ),
      mainPanel(
        shinyjs::hidden(div(id="div_create",
          fluidRow(
            column(6, h1("Data (input)"), align="center"),
            column(6, h1("Hierarchy (computed)"), align="center")
          ),
          fluidRow(
            column(6, div(id="col_data", verbatimTextOutput("origDim"), align="center")),
            column(6, div(id="col_hierarchy", verbatimTextOutput("generatedDim"), align="center")),
            shinyjs::hidden(div(id="txt_error_created", p("Something went wrong, please try other specifications"), align="center"))
          )
        )),
        div(id="div_modify",
          fluidRow(column(12, align="left",h1("Modify the Hierarchy"))),
          fluidRow(id="row_reset_btn",
            column(12, align="left", actionButton("reset_btn", "Reset hierarchy", class="btn-danger")),
            tags$br()
          ),
          fluidRow(
            column(6, align="left", h3(id="header_total", "Interactive")),
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
        column(12, align="left",h1("Code"))
      ),
      fluidRow(
        column(12, align="left",p("Below, the code required to re-generate the current hierarchy is shown. Clicking the
          button allows you to save the code to an Rscript."))
      ),
      fluidRow(
        column(12, align="left", verbatimTextOutput("requiredCode"))
      ),
      fluidRow(
        column(12, align="left", downloadButton(outputId="btn_dl_code", label="Download code", class="btn-success"))
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
        column(12, align="left", p("Select a output-format and additional options to export the current hierarchy."),
               p("Once you click on the Button, the result will be returned to your R-prompt. Please make sure that
                  you start the interactive app as follows: ",code("x <- sdcHier(...)"),". In this case, the output will be assigned
                 to object",code("x"),"."))
      ),
      fluidRow(
        column(12, align="left", selectInput("exportFormat", "Format for export", choices=c("node", "data.frame")))
      ),
      fluidRow(
        column(12, align="left", selectInput("exportType", "Return as?", choices=c("object", "file (rds)"="file")))
      ),
      shinyjs::hidden(div(id="row_export_btn",
        column(12, align="left", actionButton("btn_export", "Export", class="btn-success"))
      )),
      shinyjs::hidden(fluidRow(id="row_export_dl_btn",
        column(12, align="left", downloadButton(outputId="btn_export_dl", label="Export to File", class="btn-success"))
      ))
    )
  ))
))
