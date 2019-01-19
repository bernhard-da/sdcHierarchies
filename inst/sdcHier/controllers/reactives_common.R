code_complete <- reactive({
  code <- code_import()
  if (modify_mode() == TRUE) {
    code2 <- code_modify()
    if (!is.null(code2)) {
      if (is.null(code)) {
        code <- c(code, "library(sdcHierarchies)", "## code to create hierarchy (after modification)", code2[-1])
      } else {
        code <- c(code, "", "## code to create hierarchy (after modification)", code2[-1])
      }
    }
  }
  code
})
