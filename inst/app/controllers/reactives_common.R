code_complete <- reactive({
  code <- code_import()
  if (modify_mode() == TRUE) {
    code2 <- code_modify()
    if (!is.null(code2)) {
      if (is.null(code)) {
        code <- c(code, "library(sdcHierarchies)")
        code <- c(code, "## code to create hierarchy (after modification)")
        code <- c(code, code2[-1])
      } else {
        ## notlint
        code <- c(code, "", "## code to create hierarchy (after modification)")
        code <- c(code, code2[-1])
      }
    }
  }
  code
})
