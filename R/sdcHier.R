#' sdcHier
#'
#' starts the \emph{shiny}-app to create a nested hierarchy starting from codes by defining positions. Once
#' a hierarchy has been defined, it is possible to modify and export it.
#'
#' @return The app can return a hierarchy object (either a \code{data.frame} or a tree-based object)
#' @param x a character vector containing nested levels or an object generated with \code{sdcHier_create()}
#' @param ... arguments (e.g \code{host}) that are passed through \code{\link[shiny]{runApp}} when
#' starting the shiny application
#' @export
#' @examples
#' \dontrun{
#' codes <- c("11","12","21","22","23","31","32")
#' res <- sdcHier(codes); print(res)
#' }
sdcHier <- function(x, ...) {
  appDir <- system.file("sdcHier", package = "sdcHierarchies")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `sdcHierarchies`.", call. = FALSE)
  }

  shinyOptions(.startdir = getwd())
  shinyOptions(.appDir = appDir)

  res <- try(h_is_valid(x), silent = TRUE)
  if ("error" %in% class(res) & !is.character(x)) {
    stop("argument 'x' needs to be a character vector!\n")
  }
  shinyOptions(.data = x)
  runApp(appDir, launch.browser = TRUE)
}
