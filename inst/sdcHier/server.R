shinyServer(function(input, output, session) {
  curDim <- reactiveVal(dim)
  genDim <- reactiveVal(NULL)
  curJson <- reactiveVal(NULL)
  ok_res <- reactiveVal()

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

      nrCols <- 3
      nrRows <- ceiling(input$nr_levels / 3)
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
            column(4, align="center", res[[counter]]),
            column(4, align="center", res[[counter+1]]),
            column(4, align="center", res[[counter+2]])
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
    if (ok_res()==TRUE) {
      genDim()
    }
  })

  # common outputs
  output$requiredCode <- renderPrint({
    dd <- sdcHier_import(inp=curJson(), tot_lab=input$name_exportTot)
    cat(sdcHier_convert(dd, format="code"), sep="\n")
  })

  ## outputs modify tree
  output$mytree <- renderEmptyTree()

  # print the tree
  output$treeprint <- renderPrint({
    json <- curJson()
    if (!is.null(json)) {
      sdcHier_import(inp=json, tot_lab=NULL)
    }
  })

  observeEvent(input$btn_export, {
    stopApp(genDim())
  })
})
