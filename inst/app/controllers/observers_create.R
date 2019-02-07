# is the input by length or by endpos?
# this is an input for hier_compute()
shiny::observeEvent(input$method, {
  if (!modify_mode()) {
    if (input$method == "len") {
      max_nr <- tags$code(max_nchar())
      txt <- "The sum of the slider inputs must be at at least"
      txt <- paste0(txt, " the maximum number (", max_nr, ")")
      txt <- paste(txt, "of characters in your input vector")
    }
    if (input$method == "endpos") {
      nc <- tags$code(max_nchar())
      txt <- "The slider values must be in ascending order and the maximum"
      txt <- paste0(txt, "value must be >= the maximum number (", nc, ") of")
      txt <- paste(txt, "characters in the input")
    }
    shinyjs::html(id = "helptxt", html = as.character(txt))
  }
})

# is the total included in the string
shiny::observeEvent(input$tot_is_included, {
  if (input$tot_is_included == "yes") {
    shinyjs::hide("row_tot_level")
  } else {
    shinyjs::show("row_tot_level")
  }
})

# possible number of levels
shiny::observe({
  if (!modify_mode()) {
    shiny::updateSliderInput(
      session,
      inputId = "nr_levels",
      max = max_nchar(),
      val = max_nchar()
    )
  }
})

# should createHierarchy-button be shown?
shiny::observe({
  if (!is.null(specs()) && !is.null(max_nchar())) {
    if (input$method == "len") {
      if (sum(specs()) < max_nchar()) {
        shinyjs::hide("create_hier")
      } else {
        shinyjs::show("create_hier")
      }
    }
    if (input$method == "endpos") {
      if (tail(specs(), 1) < max_nchar()) {
        shinyjs::hide("create_hier")
      } else {
        shinyjs::show("create_hier")
      }
    }
  }
})

shiny::observeEvent(input$create_hier, {
  if (input$tot_is_included == "yes") {
    tot_lev <- NULL
  } else {
    tot_lev <- input$tot_level
  }

  as_df <- FALSE
  res <- try(
    hier_compute(
      dim = data(),
      dim_spec = specs(),
      tot_lev = tot_lev,
      method = input$method,
      as_df = as_df
    )
  )
  if (!"try-error" %in% class(res)) {
    nn <- hier_compute(
      dim = data(),
      dim_spec = specs(),
      tot_lev = tot_lev,
      method = input$method,
      as_df = FALSE
    )

    code <- c("library(sdcHierarchies)")
    code <- c(code, "## impute hierarchy from vector")

    dims <- paste(shQuote(dim), collapse = ", ")
    code <- c(code, paste0("dim <- c(", dims, ")"))

    spec <- paste(specs(), collapse = ", ")
    cc <- paste0("d <- hier_compute(dim = dim, dim_spec = c(", spec, ")")
    cc <- paste0(cc, ", tot_lev = ")
    if (is.null(tot_lev)) {
      cc <- paste0(cc, "NULL")
    } else {
      cc <- paste0(cc, shQuote(tot_lev))
    }
    cc <- paste0(cc, ", method = ", shQuote(input$method))
    cc <- paste0(cc, ", as_df = FALSE)")
    code <- c(code, cc)

    code_import(code)
    json(hier_convert(nn, format = "json"))
    shinyjs::hide("txt_error_created")
    shinyjs::show("col_hierarchy")
    shinyjs::show("btn_switch")
  } else {
    shinyjs::show("txt_error_created")
    shinyjs::hide("col_hierarchy")
    shinyjs::hide("btn_switch")
  }
})

shiny::observeEvent(input$btn_switch, {
  if (input$tot_is_included == "yes") {
    tot_lev <- NULL
  } else {
    tot_lev <- input$tot_level
  }
  modify_mode(TRUE)
})
