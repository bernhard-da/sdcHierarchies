shinyServer(function(input, output, session) {
  data <- reactiveVal(dim)
  json <- reactiveVal(NULL)
  json_prev <- reactiveVal(NULL)
  code_import <- reactiveVal(NULL)
  code_modify <- reactiveVal(NULL)
  modify_mode <- reactiveVal(FALSE)

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

      nrCols <- 2
      nrRows <- ceiling(input$nr_levels / nrCols)
      nrCells <- nrCols * nrRows

      res <- lapply(1:input$nr_levels, function(i) {
        numericInput(inputId = paste0("pos", i), label = ll[i],
          min = 1, max = max_nchar(), value = vv[i], step = 1)
      })

      if (length(res) < nrCells) {
        for (i in (length(res) + 1):nrCells) {
          res[[i]] <- ""
        }
      }

      out <- NULL
      counter <- 1
      for (i in 1:nrRows) {
        out <- list(out,
          fluidRow(
            column(6, align = "center", res[[counter]]),
            column(6, align = "center", res[[counter + 1]])
          ))
        counter <- counter + nrCols
      }
      out
    }
  })

  # outputs create
  output$origDim <- renderPrint({
    print(data.frame(code = data()), row.names = FALSE)
  })

  # the generated hierarchy
  output$generatedDim <- renderPrint({
    cur_hier <- hierarchy()
    if (!is.null(cur_hier)) {
      print(cur_hier)
    }
  })

  # common outputs
  output$requiredCode <- renderPrint({
    cat(code_complete(), sep = "\n")
  })

  ## outputs modify tree
  output$mytree <- renderEmptyTree()

  # print the tree
  output$treeprint <- renderPrint({
    js <- json()
    if (!is.null(js)) {
      tot_lev <- totLevelName()
      if (tot_lev == "") {
        tot_lev <- "Total"
      }
      sdcHier_import(inp = js, tot_lab = tot_lev)
    }
  })

  # export button
  observeEvent(input$btn_export, {
    stopApp(hierarchy())
  })
})
