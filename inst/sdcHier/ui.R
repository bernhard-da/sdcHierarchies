shinyUI(navbarPage("Interactive sdcHierarches",
  tabPanel("Hierarchy",
    useShinyjs(),
    sidebarLayout(
      sidebarPanel(width = 4,
        shinyjs::hidden(div(id = "sidebar_create",
          fluidRow(
            column(6, align = "center", div(id = "help_tot_is_included", icon(name = "question-circle"))),
            column(6, align = "center", div(id = "help_method", icon(name = "question-circle")))
          ),
          fluidRow(
            column(6, align = "center",
              radioButtons(
                inputId = "tot_is_included",
                label = "Overall Total available?",
                choices = c("yes", "no"),
                inline = TRUE
              )
            ),
            column(6, align = "center",
              radioButtons(
                inputId = "method",
                label = "Specify level-structure by",
                choices = c("length" = "len", "endpos" = "endpos"),
                inline = TRUE
              )
            )
          ),
          shinyjs::hidden(fluidRow(id = "row_tot_level",
            column(12, align = "center",
              textInput(
                inputId = "tot_level",
                label = "Name of overall total category",
                value = ""
              )
            )
          )),
          bsTooltip(id = "help_tot_is_included", placement = "top", title = "help for tot_is_incl"),
          bsTooltip(id = "help_method", placement = "top", title = "help for method"),

          fluidRow(id = "row_nr_levels",
            column(12, align = "center",
              sliderInput("nr_levels",
                label = "Number of levels",
                min = 1,
                max = 6,
                val = 1,
                step = 1,
                ticks = FALSE
              )
            )
          ),
          fluidRow(id = "row_helptxt", column(12, align = "center", div(id = "helptxt", ""))),
          uiOutput("spec"),

          fluidRow(
            column(6, align = "center",
              actionButton("createHier",
                label = "createHierarchy",
                class = "btn-success"
              )
            ),
            column(6, align = "left",
              shinyjs::hidden(
                actionButton(
                  inputId = "btn_switch",
                  label = "Switch to modify view",
                  class = "btn-primary"
                )
              )
            )
          )
        )),
        shinyjs::hidden(div(id = "sidebar_modify",
          radioButtons(
            inputId = "what",
            label = h4("What do you want to do?"),
            choices = c("Add a Node" = "add", "Delete a Node" = "delete", "Rename a Node" = "rename")
          ),
          shinyjs::hidden(div(id = "action_add",
            fluidRow(
              column(12, align = "left",
                selectInput(
                  inputId = "selAddNode_ref",
                  label = "Reference-Node",
                  choices = NULL
                )
              )
            ),
            fluidRow(
              column(12, align = "left",
                textInput(
                  inputId = "name_addNode",
                  label = "Level-Name"
                )
              )
            ),
            fluidRow(
              column(12, align = "left",
                shinyjs::disabled(
                  actionButton(
                    inputId = "btn_add",
                    label = "Add new Node",
                    class = "btn-success"
                  )
                )
              )
            )
          )),
          shinyjs::hidden(div(id = "action_delete",
            fluidRow(
              column(12, align = "left",
                selectInput(
                  inputId = "seldelNode",
                  label = "Select Node for Deletion",
                  choices = NULL
                )
              )
            ),
            fluidRow(
              column(12, align = "left",
                actionButton(
                  inputId = "btn_delete",
                  label = "Delete selected Node",
                  class = "btn-success"
                )
              )
            )
          )),
          shinyjs::hidden(div(id = "action_delete_warning",
            p("You need to add nodes/levels to your hierarchy before you can delete any.")
          )),
          shinyjs::hidden(div(id = "action_rename",
            fluidRow(
              column(12, align = "left",
                selectInput(
                  inputId = "selRenameNode",
                  label = "Select Node to Rename",
                  choices = NULL
                )
              )
            ),
            fluidRow(
              column(12, align = "left",
                textInput(
                  inputId = "name_renameNode",
                  label = "new Label",
                  value = ""
                )
              )
            ),
            fluidRow(
              column(12, align = "left",
                shinyjs::disabled(
                  actionButton(
                    inputId = "btn_rename",
                    label = "Rename selected Node",
                    class = "btn-success"
                  )
                )
              )
            )
          ))
        ))
      ),
      mainPanel(
        shinyjs::hidden(div(id = "div_create",
          fluidRow(
            column(6, h1("Data (input)"), align = "center"),
            column(6, h1("Hierarchy (computed)"), align = "center")
          ),
          fluidRow(
            column(6, div(id = "col_data", verbatimTextOutput("original_dim"), align = "center")),
            column(6, div(id = "col_hierarchy", verbatimTextOutput("generated_dim"), align = "center")),
            shinyjs::hidden(
              div(id = "txt_error_created",
                p("Something went wrong, please try other specifications"), align = "center")
            )
          )
        )),
        div(id = "div_modify",
          fluidRow(column(12, align = "left", h1("Modify the Hierarchy"))),
          fluidRow(
            column(12, align = "left",
              shinyjs::hidden(actionButton("btn_reset", "Reset hierarchy", class = "btn-danger")),
              shinyjs::disabled(actionButton("btn_undo", "Undo last action", class = "btn-primary")))
          ),
          fluidRow(
            column(6, align = "left", h3(id = "header_total", "")),
            column(6, align = "left", h3("Output"))
          ),
          fluidRow(
            column(6, align = "left", shinyTree("mytree", dragAndDrop = TRUE, theme = "proton")),
            column(6, align = "left", verbatimTextOutput("treeprint"))
          )
        )
      )
    )
  ),
  tabPanel("Code",
    div(id = "div_code_hidden", h1("Please create a hierarchy first")),
      shinyjs::hidden(div(id = "div_code",
      fluidRow(
        column(12, align = "left",
          h1("Code")
        )
      ),
      fluidRow(
        column(12, align = "left",
          p("Below, the code required to re-generate the current hierarchy is shown. Clicking the button allows you to
            save the code to an Rscript.")
        )
      ),
      fluidRow(
        column(12, align = "left",
          verbatimTextOutput("required_code")
        )
      ),
      fluidRow(
        column(12, align = "left",
          downloadButton(
            outputId = "btn_dl_code",
            label = "Download code",
            class = "btn-success"
          )
        )
      )
    )
  )),
  tabPanel("Export",
    div(id = "div_export_hidden", h1("Please create a hierarchy first")),
    shinyjs::hidden(div(id = "div_export",
      fluidRow(column(12, align = "left", h1("Export results"))),
      fluidRow(
        column(12, align = "left",
          p("Select a output-format and additional options to export the current hierarchy."),
          p("Once you click on the Button, the result will be either saved to disk or returned to your R-prompt
            if you select", code("R-object."),
            "In this case please make sure that you start the interactive app as follows: ",
            code("x <- sdcHier(...)"), ". In this case, the output will be assigned to object", code("x"), ".")
          )
      ),
      fluidRow(
        column(12, align = "left",
          selectInput(
            inputId = "exportFormat",
            label = "Format for export",
            choices = c(
              "sdcHierarchy" = "node",
              "data.frame suitable for sdcTable" = "df",
              "hrc-file for tau-Argus" = "argus",
              "json-encoded string" = "json",
              "a list suitable for sdcTable" = "sdc")
          )
        )
      ),
      fluidRow(
        column(12, align = "left",
          selectInput(
            inputId = "exportType",
            label = "Return the result as?",
            choices = c("R-object", "File" = "file")
          )
        )
      ),
      shinyjs::hidden(div(id = "row_export_btn",
        column(12, align = "left",
          actionButton(
            inputId = "btn_export",
            label = "Export",
            class = "btn-success"
          )
        )
      )),
      shinyjs::hidden(fluidRow(id = "row_export_dl_btn",
        column(12, align = "left",
          downloadButton(
            outputId = "btn_export_dl",
            label = "Export to File",
            class = "btn-success"
          )
        )
      ))
    )
  ))
))
