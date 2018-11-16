shinyServer(function(input, output, session) {
  curDim <- reactiveVal(dim)
  genDim <- reactiveVal(NULL)
  curJson <- reactiveVal(NULL)
  ok_res <- reactiveVal()
  code_import <- reactiveVal()

  for (file in list.files("controllers")) {
    source(file.path("controllers", file), local=TRUE)
  }

  if (is.null(json)) {
    ok_res(FALSE)
  } else {
    ok_res(TRUE)
    curJson(json)
    genDim(sdcHier_import(inp=json, tot_lab=NULL))
  }

  output$spec <- renderUI({
    if (!ok_res()) {
      req(input$nr_levels)
      req(input$method)

      labs <- function(nr, method) {
        if (method=="len") {
          return(paste("# digits for level",1:nr))
        }
        if (method=="endpos") {
          return(paste("Endposition for level",1:nr))
        }
      }
      vals <- function(nr, method) {
        if (method=="len") {
          return(rep(1, nr))
        }
        if (method=="endpos") {
          return(1:nr)
        }
      }
      ll <- labs(nr=input$nr_levels, method=input$method)
      vv <- vals(nr=input$nr_levels, method=input$method)

      nrCols <- 2
      nrRows <- ceiling(input$nr_levels / nrCols)
      nrCells <- nrCols*nrRows

      res <- lapply(1:input$nr_levels, function(i) {
        numericInput(inputId=paste0("pos", i), label=ll[i],
          min=1, max=max_nchar(), value=vv[i], step=1)
      })

      if (length(res)<nrCells) {
        for (i in (length(res)+1):nrCells) {
          res[[i]] <- ""
        }
      }

      out <- NULL
      counter <- 1
      for (i in 1:nrRows) {
        out <- list(out,
          fluidRow(
            column(6, align="center", res[[counter]]),
            column(6, align="center", res[[counter+1]])
          ))
        counter <- counter + nrCols
      }
      out
    }
  })

  # outputs create
  output$origDim <- renderPrint({
    print(data.frame(code=curDim()), row.names=FALSE)
  })
  output$generatedDim <- renderPrint({
      genDim()
  })

  # common outputs
  output$requiredCode <- renderPrint({
    dd <- sdcHier_import(inp=curJson(), tot_lab=input$name_exportTot)
    code_convert <- sdcHier_convert(dd, format="code")
    code <- code_import()
    if (!is.null(code)) {
      code <- c(code, "", "## code to create hierarchy (after modification)", code_convert[-1])
    } else {
      code <- c(code, code_convert)
    }
    cat(code, sep="\n")
  })

  ## outputs modify tree
  output$mytree <- renderEmptyTree()

  # print the tree
  output$treeprint <- renderPrint({
    json <- curJson()
    if (!is.null(json)) {
      tot_lev <- input$tot_level
      if (tot_lev=="") {
        tot_lev <- "Total"
      }
      sdcHier_import(inp=json, tot_lab=tot_lev)
    }
  })

  observeEvent(input$btn_export, {
    stopApp(genDim())
  })
})
