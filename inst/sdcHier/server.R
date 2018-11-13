shinyServer(function(input, output, session) {

  for (file in list.files("controllers")) {
    source(file.path("controllers", file), local=TRUE)
  }

  curDim <- reactiveVal(dim)
  genDim <- reactiveVal(NULL)
  curJson <- reactiveVal(NULL)
  ok_res <- reactiveVal()

  if (is.null(json)) {
    ok_res(FALSE)
  } else {
    ok_res(TRUE)
    curJson(json)
    genDim(sdcHier_import(inp=json, tot_lab=NULL))
  }

  # possible number of levels
  observe({
    if (!ok_res()) {
      updateSliderInput(session, inputId="nr_levels", max = max_nchar(), val=max_nchar())
    }
  })

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

  observeEvent(input$tot_is_included, {
    if (input$tot_is_included=="yes") {
      shinyjs::hide("row_tot_level")
    } else {
      shinyjs::show("row_tot_level")
    }
  })


  # should button be shown?
  observe({
    if (!is.null(specs()) && !is.null(max_nchar())) {
      if (input$method=="len") {
        if (sum(specs()) < max_nchar()) {
          shinyjs::hide("createHier")
        } else {
          shinyjs::show("createHier")
        }
      }
      if (input$method=="endpos") {
        if (tail(specs(),1) < max_nchar()) {
          shinyjs::hide("createHier")
        } else {
          shinyjs::show("createHier")
        }
      }
    }
  })


  observeEvent(input$createHier, {
    if (input$tot_is_included=="yes") {
      tot_lev <- NULL
    } else {
      tot_lev <- input$tot_level
    }

    as_df <- FALSE
    if (input$as_df=="yes") {
      as_df <- TRUE
    }
    res <- try(sdcHier_compute(dim=curDim(), dim_spec=specs(), tot_lev=tot_lev, method=input$method, as_df=as_df))
    if (!"try-error" %in% class(res)) {
      genDim(res)
      ok_res(TRUE)
      shinyjs::hide("error_gen")
      shinyjs::show("col_generated")
    } else {
      ok_res(FALSE)
      shinyjs::show("error_gen")
      shinyjs::hide("col_generated")
    }
  })

  output$origDim <- renderPrint({
    print(data.frame(code=curDim()), row.names=FALSE)
  })

  output$generatedDim <- renderPrint({
    if (ok_res()==TRUE) {
      genDim()
    }
  })

  output$requiredCode <- renderPrint({
    if (ok_res()) {
      #if (input$as_df=="yes") {
      #  nn <- sdcHier_import(inp=genDim(), from="data.frame")
      #} else {
        nn <- genDim()
      #}
      cat(sdcHier_convert(nn, format="code"), sep="\n")
    }
  })

  observe({
    if (ok_res()==TRUE) {
      shinyjs::show("div_code"); shinyjs::hide("div_code_hidden")
      shinyjs::show("div_modify"); shinyjs::hide("div_create")
      shinyjs::show("div_export"); shinyjs::hide("div_export_hidden")
    } else {
      shinyjs::hide("div_code"); shinyjs::show("div_code_hidden")
      shinyjs::hide("div_modify"); shinyjs::show("div_create")
      shinyjs::hide("div_export"); shinyjs::show("div_export_hidden")
    }
  })

  observeEvent(input$btn_export, {
    stopApp(genDim())
  })
})
