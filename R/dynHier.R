#' dynHier
#'
#' starts the \emph{shiny}-app to update/modify create a dynamic hierarchy that can be used for package \code{sdcTable}
#' and \code{cellKey}.
#'
#' @return The app can return a hierarchy object (either a \code{data.frame} or a tree-based object)
#' @param x if not NULL, one can specify a node created by {sdcTable::create_node()} or \code{cellKey::ck_create_node()}
#' @param ... arguments (e.g \code{host}) that are passed through \code{\link[shiny]{runApp}} when
#' starting the shiny application
#' @export
#' @examples
#' \dontrun{
#' x <- dynHier(); print(x)
#'
#' ## start with a hierarchy-input
#' aDim <- sdcHier_create("Total", node_labs=letters[1:5])
#' aDim_mod <- dynHier(aDim); print(aDim_mod)
#' }
dynHier <- function(x=NULL, ...) {
  appDir <- system.file("shiny_sdcHier", package="sdcHierarchies")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `sdcHierarchies`.", call.=FALSE)
  }

  shinyOptions(.startdir = getwd())
  shinyOptions(.appDir = appDir)
  shinyOptions(.data = x)

  runApp(appDir, launch.browser=TRUE)
}
