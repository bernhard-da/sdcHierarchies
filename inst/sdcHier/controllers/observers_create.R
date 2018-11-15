# is the input by length or by endpos?
# this is an input for sdcHier_compute()
observeEvent(input$method, {
  if (!ok_res()) {
    if (input$method=="len") {
      txt <- paste("The sum of the slider inputs must be at least the maximum number (",tags$code(max_nchar()),") of characters in your input vector")
    }
    if (input$method=="endpos") {
      txt <- paste("The slider values must be in ascending order and the maximum value must be larger or equal of the
          maximum number (",tags$code(max_nchar()),") of characters in the input")
    }
    shinyjs::html(id="helptxt", html=as.character(txt))
  }
})

# is the total included in the string
observeEvent(input$tot_is_included, {
  if (input$tot_is_included=="yes") {
    shinyjs::hide("row_tot_level")
  } else {
    shinyjs::show("row_tot_level")
  }
})

# possible number of levels
observe({
  if (!ok_res()) {
    updateSliderInput(session, inputId="nr_levels", max=max_nchar(), val=max_nchar())
  }
})

# should createHierarchy-button be shown?
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
    nn <- sdcHier_compute(dim=curDim(), dim_spec=specs(), tot_lev=tot_lev, method=input$method, as_df=FALSE)
    newJson <- sdcHier_convert(nn, format="json")
    cat("newJson:", newJson,"\n")
    curJson(newJson)
    ok_res(TRUE)
    shinyjs::hide("error_gen")
    shinyjs::show("col_generated")
  } else {
    ok_res(FALSE)
    shinyjs::show("error_gen")
    shinyjs::hide("col_generated")
  }
})
