shinyUI(fluidPage(theme = shinytheme("flatly"),
  headerPanel("sdcHier (by position)"),

  sidebarPanel(
    shinyjs::useShinyjs(),

    radioButtons("tot_is_included", label="Overall Total available?", choices=c("yes", "no"), inline=TRUE),
    radioButtons("as_df", label="Result as data.frame?", choices=c("yes", "no"), inline=TRUE),

    shinyjs::hidden(textInput("tot_level", "Name of overall total category", value="")),


    radioButtons("method", label="How do you want to specify levels?", choices=c("by length"="len","by endposition"="endpos"), inline=TRUE),
    sliderInput("nr_levels", "Number of levels", min=1, max=6, val=1, step=1, ticks=FALSE),
    div(id="helptxt", ""),
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
        div(id="col_orig", tableOutput("origDim"), align="center"),
        div(id="error_gen", p("Something went wrong, please try other specifications"))
      ),
      column(6, div(id="col_generated", tableOutput("generatedDim"), align="center"))
    ),
    fluidRow(
      column(12, align="center", div(id="id_export_btn", actionButton("btn_export", "Export")))
    )
  )
))
