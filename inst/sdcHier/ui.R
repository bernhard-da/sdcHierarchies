shinyUI(fluidPage(theme=shinytheme("flatly"),
  headerPanel("Create a hierarchy by position"),
  sidebarPanel(
    shinyjs::useShinyjs(),
    fluidRow(
      column(4, align="center", div(id="help_tot_is_included", icon(name="question-circle"))),
      column(4, align="center", div(id="help_as_df", icon(name="question-circle"))),
      column(4, align="center", div(id="help_method", icon(name="question-circle")))
    ),
    fluidRow(
      column(4, align="center", radioButtons("tot_is_included", label="Overall Total available?", choices=c("yes","no"), inline=FALSE)),
      column(4, align="center", radioButtons("as_df", label="Result as data.frame?", choices=c("yes", "no"), inline=FALSE)),
      column(4, align="center", radioButtons("method", label="Specify levels", choices=c("by length"="len","by endposition"="endpos"), inline=TRUE))
    ),
    shinyjs::hidden(fluidRow(id="row_tot_level",
      column(12, align="center", textInput("tot_level", "Name of overall total category", value=""))
    )),

    bsTooltip(id="help_tot_is_included", placement="top", title="help for tot_is_incl"),
    bsTooltip(id="help_as_df", placement="top", title="help for as_df"),
    bsTooltip(id="help_method", placement="top", title="help for method"),

    fluidRow(id="row_nr_levels",
      column(12, align="center", sliderInput("nr_levels", "Number of levels", min=1, max=6, val=1, step=1, ticks=FALSE)
    ),
    fluidRow(id="row_helptxt",
      column(12, align="center", div(id="helptxt", ""))
    ),
    uiOutput("spec"),
    actionButton("createHier", "createHierarchy", class="btn-success")
    )
  ),
  mainPanel(
    tabsetPanel(id="panel1",
      tabPanel(title="Hierarchy",
        fluidRow(
         column(6, h3("Original Input"), align="center"),
          column(6, h3("Generated Hierarchy"), align="center")
        ),
        fluidRow(
          column(6, div(id="col_orig", verbatimTextOutput("origDim"), align="center")),
          column(6, div(id="col_generated", verbatimTextOutput("generatedDim"), align="center")),
          shinyjs::hidden(div(id="error_gen", p("Something went wrong, please try other specifications"), align="center"))
        )
      ),
      tabPanel(title="Code",
        div(id="div_code_hidden", h2("Please create a hierarchy first")),
        shinyjs::hidden(div(id="div_code",
          fluidRow(
            column(12, align="left",h2("Code")),
            column(12, align="left", verbatimTextOutput("requiredCode"))
        )))
      ),
      tabPanel(title="Interactive Modification",
        div(id="div_interactive_hidden", h2("Please create a hierarchy first")),
        shinyjs::hidden(div(id="div_interactive",
          fluidRow(
            column(12, align="left",h2("Modify the Hierarchy"))
          )
        ))
      ),
      tabPanel(title="Export",
        div(id="div_export_hidden", h2("Please create a hierarchy first")),
        shinyjs::hidden(div(id="div_export",
          fluidRow(
            column(12, align="left",h2("Export the results"))
          ),
          fluidRow(id="row_export_btn",
            column(12, align="center", actionButton("btn_export", "Export", class="btn-success"))
          )
        ))
      )
    )
  )
))
