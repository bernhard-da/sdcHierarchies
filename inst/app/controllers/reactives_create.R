# maximum number of dimensions for a given hierarchy
max_nchar <- reactive({
  max(nchar(data()))
})

# read the len/endpos for each level
# this is an input for hier_compute()
specs <- reactive({
  req(input$nr_levels)
  if (!modify_mode()) {
    out <- sapply(1:input$nr_levels, function(i) {
      input[[paste0("pos", i)]]
    })

    xx <- sapply(out, function(x) {
      is.null(x)
    })
    if (sum(xx) > 0) {
      return(NULL)
    }
    out
  }
})
