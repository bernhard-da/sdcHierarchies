library(shinyBS)
shinyUI(fluidPage(theme = shinytheme("flatly"),
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
      column(12, align="center", sliderInput("nr_levels", "Number of levels", min=1, max=6, val=1, step=1, ticks=FALSE))
    ),

    fluidRow(id="row_helptxt",
      column(12, align="center", div(id="helptxt", ""))
    ),
    uiOutput("spec"),
    actionButton("createHier", "createHierarchy")
  ),
  mainPanel(
    fluidRow(
      column(6, h3("Original Input"), align="center"),
      column(6, h3("Generated Hierarchy"), align="center")
    ),
    fluidRow(
      column(6,
        div(id="col_orig", verbatimTextOutput("origDim"), align="center"),
        div(id="error_gen", p("Something went wrong, please try other specifications"))
      ),
      column(6, div(id="col_generated", verbatimTextOutput("generatedDim"), align="center"))
    ),
    fluidRow(
      column(12, align="center", div(id="id_export_btn", actionButton("btn_export", "Export")))
    )
  )
))
