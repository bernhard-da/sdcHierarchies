shinyUI(navbarPage("Interactive sdcHierarches",
  tabPanel("Hierarchy",
    useShinyjs(),
    sidebarLayout(
      sidebarPanel(width = 4,
        shinyjs::hidden(div(id = "sidebar_create",
          fluidRow(
            column(
              width = 6,
              align = "center",
              div(id = "help_tot_is_included", icon(name = "question-circle"))
            ),
            column(
              width = 6,
              align = "center",
              div(id = "help_method", icon(name = "question-circle"))
            )
          ),
          fluidRow(
            column(
              width = 6,
              align = "center",
              radioButtons(
                inputId = "tot_is_included",
                label = "Overall Total available?",
                choices = c("yes", "no"),
                inline = TRUE
              )
            ),
            column(
              width = 6,
              align = "center",
              radioButtons(
                inputId = "method",
                label = "Specify level-structure by",
                choices = c("length" = "len", "endpos" = "endpos"),
                inline = TRUE
              )
            )
          ),
          shinyjs::hidden(fluidRow(id = "row_tot_level",
            column(
              width = 12,
              align = "center",
              textInput(
                inputId = "tot_level",
                label = "Name of overall total category",
                value = ""
              )
            )
          )),

          help_tot <- paste(c(
            "If selected, the label for the overall total should be",
            "contained within the input vector.<br>",
            "If not selected, a code or label for the global total",
            "can be set below."
          ), collapse = " "),

          shinyBS::bsTooltip(
            id = "help_tot_is_included",
            placement = "right",
            title = help_tot,
            options = list(container = "body")
          ),

          help_method <- paste(c(
            "Endpos: Defines the position at which each level",
            "in the hierarchy ends.<br>",
            "Len: The length (in terms of characters) each hiearchical",
            "level requires in the input vector."
          ), collapse = " "),

          shinyBS::bsTooltip(
            id = "help_method",
            placement = "right",
            title = help_method,
            options = list(container = "body")
          ),

          fluidRow(id = "row_nr_levels",
            column(
              width = 12,
              align = "center",
              shiny::sliderInput("nr_levels",
                label = "Number of levels",
                min = 1,
                max = 6,
                value = 1,
                step = 1,
                ticks = FALSE
              )
            )
          ),
          fluidRow(id = "row_helptxt",
            column(
              width = 12,
              align = "center",
              htmltools::div(id = "helptxt", "")
            )
          ),
          uiOutput("spec"),
          fluidRow(
            column(
              width = 6,
              align = "center",
              shiny::actionButton(
                inputId = "create_hier",
                label = "Create a hierarchy",
                class = "btn-success"
              )
            ),
            column(
              width = 6,
              align = "left",
              shinyjs::hidden(actionButton(
                inputId = "btn_switch",
                label = "Switch to modify view",
                class = "btn-primary"
              ))
            )
          )
        )),
        shinyjs::hidden(htmltools::div(id = "sidebar_modify",
          radioButtons(
            inputId = "what",
            label = htmltools::h4("What do you want to do?"),
            choices = c(
              "Add a Node" = "add",
              "Delete a Node" = "delete",
              "Rename a Node" = "rename"
            )
          ),
          shinyjs::hidden(htmltools::div(id = "action_add",
            fluidRow(
              column(12, align = "left",
                selectInput(
                  inputId = "sel_addnode_ref",
                  label = "Reference-Node",
                  choices = NULL
                )
              )
            ),
            fluidRow(
              column(12, align = "left",
                textInput(
                  inputId = "name_add_node",
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
          shinyjs::hidden(htmltools::div(id = "action_delete",
            fluidRow(
              column(
                width = 12,
                align = "left",
                selectInput(
                  inputId = "sel_delnode",
                  label = "Select Node for Deletion",
                  choices = NULL
                )
              )
            ),
            fluidRow(
              column(
                width = 12,
                align = "left",
                actionButton(
                  inputId = "btn_delete",
                  label = "Delete selected Node",
                  class = "btn-success"
                )
              )
            )
          )),
          shinyjs::hidden(htmltools::div(id = "action_delete_warning",
            p("No nodes / levels available for deletion.")
          )),
          shinyjs::hidden(div(id = "action_rename",
            fluidRow(
              column(
                width = 12,
                align = "left",
                selectInput(
                  inputId = "sel_rename_node",
                  label = "Select Node to Rename",
                  choices = NULL
                )
              )
            ),
            fluidRow(
              column(
                width = 12,
                align = "left",
                textInput(
                  inputId = "name_rename_node",
                  label = "new Label",
                  value = ""
                )
              )
            ),
            fluidRow(
              column(
                width = 12,
                align = "left",
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
            column(
              width = 6,
              h1("Data (input)"),
              align = "center"
            ),
            column(
              width = 6,
              h1("Hierarchy (computed)"),
              align = "center"
            )
          ),
          fluidRow(
            column(
              width = 6,
              div(
                id = "col_data",
                verbatimTextOutput("original_dim"),
                align = "center"
              )
            ),
            column(
              width = 6,
              div(
                id = "col_hierarchy",
                verbatimTextOutput("generated_dim"),
                align = "center"
              )
            ),
            shinyjs::hidden(
              div(
                id = "txt_error_created",
                align = "center",
                p("Something went wrong, please try other specifications")
              )
            )
          )
        )),
        div(id = "div_modify",
          fluidRow(
            column(
              width = 12,
              align = "left",
              h1("Modify the Hierarchy")
            )
          ),
          fluidRow(
            column(
              width = 12,
              align = "left",
              shinyjs::hidden(
                shiny::actionButton(
                  inputId = "btn_reset",
                  label = "Reset hierarchy",
                  class = "btn-danger"
                )
              ),
              shinyjs::disabled(
                shiny::actionButton(
                  inputId = "btn_undo",
                  label = "Undo last action",
                  class = "btn-primary"
                )
              )
            )
          ),
          fluidRow(
            column(
              width = 6,
              align = "left",
              h3(id = "header_total", "")
            ),
            column(
              width = 6,
              align = "left",
              h3("Output")
            )
          ),
          fluidRow(
            column(
              width = 6,
              align = "left",
              shinyTree::shinyTree(
                outputId = "mytree",
                dragAndDrop = TRUE,
                theme = "proton"
              )
            ),
            column(
              width = 6,
              align = "left",
              shiny::verbatimTextOutput("treeprint")
            )
          )
        )
      )
    )
  ),
  tabPanel("Code",
    div(
      id = "div_code_hidden",
      h1("Please create a hierarchy first")
    ),
    shinyjs::hidden(div(id = "div_code",
      fluidRow(
        column(
          width = 12,
          align = "left",
          h1("Code")
        )
      ),
      fluidRow(
        column(
          width = 12,
          align = "left",
          p("Below, the required code to re-generate the hierarchy is shown.
            Clicking the button allows you to save the code into a R-script.")
        )
      ),
      fluidRow(
        column(
          width = 12,
          align = "left",
          shiny::verbatimTextOutput("required_code")
        )
      ),
      fluidRow(
        column(
          width = 12, align = "left",
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
      fluidRow(
        column(
          width = 12,
          align = "left",
          h1("Export results")
        )
      ),
      fluidRow(
        column(
          width = 12,
          align = "left",
          p("Select an output format and additional options that are
             used when exporting the hierarchy."),
          p("Once you click on the button, the result will be either saved to
             disk or returned to your prompt if you select", code("R-object."),
            "In this case please make sure that you start the interactive
             app as follows: ", code("x <- hier_app(...)"),
            ". In this case, the output will be assigned to
             object", code("x"), ".")
        )
      ),
      fluidRow(
        column(
          width = 12,
          align = "left",
          shiny::selectInput(
            inputId = "export_format",
            label = "Format for export",
            choices = c(
              "sdcHierarchy" = "node",
              "data.frame suitable for sdcTable" = "df",
              "hrc-file for tau-Argus" = "argus",
              "json-encoded string" = "json",
              "a list suitable for sdcTable" = "sdc"
            )
          )
        )
      ),
      fluidRow(
        column(
          width = 12,
          align = "left",
          shiny::selectInput(
            inputId = "export_type",
            label = "Return the result as?",
            choices = c("R-object", "File" = "file")
          )
        )
      ),
      shinyjs::hidden(div(id = "row_export_btn",
        column(
          width = 12,
          align = "left",
          shiny::actionButton(
            inputId = "btn_export",
            label = "Export",
            class = "btn-success"
          )
        )
      )),
      shinyjs::hidden(
        fluidRow(
          id = "row_export_dl_btn",
          column(
            width = 12,
            align = "left",
            shiny::downloadButton(
              outputId = "btn_export_dl",
              label = "Export to File",
              class = "btn-success"
            )
          )
        )
      )
    )
  ))
))
