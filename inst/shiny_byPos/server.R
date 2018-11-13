shinyServer(function(input, output, session) {
  curDim <- reactiveVal(dim)
  genDim <- reactiveVal(NULL)

  max_nchar <- reactive({
    max(nchar(curDim()))
  })

  ok_res <- reactiveVal()
  ok_res(FALSE)


  # possible number of levels
  observe({
    updateSliderInput(session, inputId="nr_levels", max = max_nchar(), val=max_nchar())
  })

  specs <- reactive({
    req(input$nr_levels)
    out <- sapply(1:input$nr_levels, function(i) {
      input[[paste0("pos", i)]]
    })

    xx <- sapply(out, function(x) { is.null(x)})
    if (sum(xx)>0) {
      return(NULL)
    }
    out
  })

  output$spec <- renderUI({
    req(input$nr_levels)
    req(input$method)

    labs <- function(nr, method) {
      if (method=="len") {
        return(paste("# digits for level",1:nr))
      }
      if (method=="endpos") {
        return(paste("endpos for level",1:nr))
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
    lapply(1:input$nr_levels, function(i) {
      numericInput(inputId = paste0("pos", i), label=ll[i],
        min = 1, max = max_nchar(), value = vv[i], step = 1)
    })
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

  observeEvent(input$method, {
    if (input$method=="len") {
      txt <- paste("The sum of the slider inputs must be at least the maximum number (",tags$code(max_nchar()),") of characters in your input vector")
    }
    if (input$method=="endpos") {
      txt <- paste("The slider values must be in ascending order and the maximum value must be larger or equal of the
        maximum number (",tags$code(max_nchar()),") of characters in the input")
    }
    shinyjs::html(id="helptxt", html=as.character(txt))
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
    if (ok_res()==TRUE) {
      if (input$as_df=="yes") {
        nn <- sdcHier_import(inp=genDim(), from="data.frame")
      } else {
        nn <- genDim()
      }
      cat(sdcHier_convert(nn, format="code"), sep="\n")
    }
  })

  observe({
    if (ok_res()==TRUE) {
      shinyjs::show("row_export_btn")
      shinyjs::show("row_code")
    } else {
      shinyjs::hide("row_export_btn")
      shinyjs::hide("row_code")
    }
  })

  observeEvent(input$btn_export, {
    stopApp(genDim())
  })
})
