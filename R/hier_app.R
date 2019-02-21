#' Create/Modify hierarchies interactively
#'
#' This function starts the interactive \emph{shiny}-app
#' to (optionally) create and/or modify a nested hierarchy.
#' It is possible to supply a character vector from which the
#' hierarchy can be interactively built. Once this has been done,
#' it is possible to modify and export the resulting hierarchy.
#'
#' Another option is to supply an already existing hierarchy object.
#' In this case, it is possible to modify the existing object in
#' the app.
#'
#' @return The app can return a hierarchy object (either
#' a \code{data.frame} or a tree-based object)
#' @param x a character vector containing nested levels or
#' an object generated with \code{hier_create()}
#' @param ... arguments (e.g \code{host}) that are passed
#' through \code{\link[shiny]{runApp}} when
#' starting the shiny application
#' @export
#' @examples
#' \dontrun{
#' # start with an empty hierarchy
#' res <- hier_app()
#'
#' # start with a character vector that is used to
#' build the hierarchy
#' codes <- c("11", "12", "21", "22", "23", "31", "32")
#' res <- hier_app(codes); print(res)
#' }
hier_app <- function(x=hier_create(), ...) {
  app_dir <- system.file("app", package = "sdcHierarchies")
  if (app_dir == "") {
    err <- "Could not find example directory."
    err < paste(err, "Try re-installing `sdcHierarchies`.")
    stop(err, call. = FALSE)
  }

  shinyOptions(.startdir = getwd())
  shinyOptions(.appDir = app_dir)

  res <- try(.is_valid(x), silent = TRUE)
  if ("error" %in% class(res) && !is.character(x)) {
    e <- c(
      "Argument", shQuote("x"),
      "needs to be either a character vector or a hierarchy object."
    )
    stop(paste(e, collapse = " "), call. = FALSE)
  }
  shinyOptions(.data = x)
  runApp(app_dir, launch.browser = TRUE, ...)
}
