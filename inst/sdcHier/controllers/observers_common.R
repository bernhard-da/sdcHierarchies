observe({
  if (!is.null(json())) {
    shinyjs::show("div_code"); shinyjs::hide("div_code_hidden")
    shinyjs::show("div_export"); shinyjs::hide("div_export_hidden")

    if (start_with_hier == FALSE) {
      shinyjs::show("btn_reset")
    } else {
      shinyjs::hide("btn_reset")
    }
  } else {
    shinyjs::hide("div_code"); shinyjs::show("div_code_hidden")
    shinyjs::hide("div_export"); shinyjs::show("div_export_hidden")
  }
})

observe({
  if (modify_mode() == TRUE) {
    shinyjs::show("sidebar_modify"); shinyjs::show("div_modify")
    shinyjs::hide("sidebar_create"); shinyjs::hide("div_create")
  } else {
    shinyjs::show("sidebar_create"); shinyjs::show("div_create")
    shinyjs::hide("sidebar_modify"); shinyjs::hide("div_modify")
  }
})

# write code to file
output$btn_dl_code <- downloadHandler(
  filename = function() {
    paste0("code_generate_hier_", format(Sys.Date(), "%d%m%Y"), ".R")
  },
  content = function(con) {
    cat(code_complete(), sep = "\n", file = con)
  }
)

output$btn_export_dl <- downloadHandler(
  filename = function() {
    ff <- input$exportFormat
    if (ff == "data.frame") {
      ext <- ".csv"
    } else if (ff == "argus") {
      ext <- ".hrc"
    } else if (ff == "code") {
      ext <- ".R"
    } else if (ff == "json") {
      ext <- ".json"
    } else {
      ext <- ".txt"
    }
    paste0("sdcHier_", input$exportFormat, "_", format(Sys.Date(), "%d%m%Y"), ext)
  },
  content = function(con) {
    dd <- sdcHier_import(inp = js, tot_lab = overall_level_name())
    if (input$exportFormat == "data.frame") {
      dd <- sdcHier_export(dd, format = "data.frame", path = con)
    }
    if (input$exportFormat == "code") {
      dd <- sdcHier_export(dd, format = "code", path = con)
    }
    if (input$exportFormat == "argus") {
      dd <- sdcHier_export(dd, format = "argus", path = con)
    }
    if (input$exportFormat == "json") {
      dd <- sdcHier_export(dd, format = "json", path = con)
    }
  }
)
