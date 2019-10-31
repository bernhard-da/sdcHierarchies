#' Show the package vignette
#'
#' This function opens the introductionary package vignette
#' and opens it in a new browser tab/window.
#'
#' @return a browser windows/tab with showing the vignette
#' @export
#' @md
#' @examples
#' \dontrun{
#' hier_vignette()
#' }
hier_vignette <- function() {
  RShowDoc("usage", package = "sdcHierarchies")
}
