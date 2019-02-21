shinyServer(function(input, output, session) {
  data <- shiny::reactiveVal(dim)
  json <- shiny::reactiveVal(NULL)
  json_prev <- shiny::reactiveVal(NULL)
  code_import <- shiny::reactiveVal(NULL)
  code_modify <- shiny::reactiveVal(NULL)
  modify_mode <- shiny::reactiveVal(FALSE)

  for (file in list.files("controllers")) {
    source(file.path("controllers", file), local = TRUE)
  }

  if (is.null(js)) {
    shinyjs::show("sidebar_create")
    modify_mode(FALSE)
  } else {
    json(js)
    shinyjs::show("sidebar_modify")
    modify_mode(TRUE)
  }

  output$spec <- renderUI({
    if (!modify_mode()) {
      req(input$nr_levels)
      req(input$method)

      labs <- function(nr, method) {
        if (method == "len") {
          return(paste("# digits for level", 1:nr))
        }
        if (method == "endpos") {
          return(paste("Endposition for level", 1:nr))
        }
      }
      vals <- function(nr, method) {
        if (method == "len") {
          return(rep(1, nr))
        }
        if (method == "endpos") {
          return(1:nr)
        }
      }
      ll <- labs(nr = input$nr_levels, method = input$method)
      vv <- vals(nr = input$nr_levels, method = input$method)

      nr_cols <- 2
      nr_rows <- ceiling(input$nr_levels / nr_cols)
      nr_cells <- nr_cols * nr_rows

      res <- lapply(1:input$nr_levels, function(i) {
        numericInput(inputId = paste0("pos", i), label = ll[i],
          min = 1, max = max_nchar(), value = vv[i], step = 1)
      })

      if (length(res) < nr_cells) {
        for (i in (length(res) + 1):nr_cells) {
          res[[i]] <- ""
        }
      }

      out <- NULL
      counter <- 1
      for (i in 1:nr_rows) {
        out <- list(out,
          fluidRow(
            column(6, align = "center", res[[counter]]),
            column(6, align = "center", res[[counter + 1]])
          ))
        counter <- counter + nr_cols
      }
      out
    }
  })

  # outputs create
  output$original_dim <- renderPrint({
    print(data.frame(code = data()), row.names = FALSE)
  })

  # the generated hierarchy
  output$generated_dim <- renderPrint({
    cur_hier <- hierarchy()
    if (!is.null(cur_hier)) {
      print(cur_hier)
    }
  })

  # common outputs
  output$required_code <- renderPrint({
    cat(code_complete(), sep = "\n")
  })

  ## outputs modify tree
  output$mytree <- renderEmptyTree()

  # print the tree
  output$treeprint <- renderPrint({
    js <- json()
    if (!is.null(js)) {
      root <- overall_level_name()
      if (root == "") {
        root <- "Total"
      }
      h <- hier_import(inp = js, root = root)
      hier_display(h)
    }
  })

  # export button
  observeEvent(input$btn_export, {
    stopApp(hierarchy())
  })
})
