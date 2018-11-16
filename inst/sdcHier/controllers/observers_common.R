observe({
  if (ok_res()==TRUE) {
    shinyjs::show("sidebar_modify"); shinyjs::hide("sidebar_create")
    shinyjs::show("div_code"); shinyjs::hide("div_code_hidden")
    shinyjs::show("div_modify"); shinyjs::hide("div_create")
    shinyjs::show("div_export"); shinyjs::hide("div_export_hidden")

    if (start_with_hier==FALSE) {
      shinyjs::show("row_reset_btn")
    } else {
      shinyjs::hide("row_reset_btn")
    }
  } else {
    shinyjs::hide("sidebar_modify"); shinyjs::show("sidebar_create")
    shinyjs::hide("div_code"); shinyjs::show("div_code_hidden")
    shinyjs::hide("div_modify"); shinyjs::show("div_create")
    shinyjs::hide("div_export"); shinyjs::show("div_export_hidden")
  }
})
