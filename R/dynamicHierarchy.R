#' dynamicHierarchy
#'
#' starts the \emph{shiny}-app to update/modify create a dynamic hierarchy that can be used for package \code{sdcTable}
#' and \code{cellKey}.
#'
#' @return The app can return a hierarchy object (either a \code{data.frame} or a tree-based object)
#' @param ... arguments (e.g \code{host}) that are passed through \code{\link[shiny]{runApp}} when
#' starting the shiny application
#' @export
#' @examples
#' \dontrun{
#' x <- dynamicHierarchy(); print(x)
#' }
dynamicHierarchy <- function(...) {
  appDir <- system.file("shiny_sdcHier", package="sdcHierarchies")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `sdcHierarchies`.", call.=FALSE)
  }
  runApp(appDir, launch.browser=TRUE)
}
