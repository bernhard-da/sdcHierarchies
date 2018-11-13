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
