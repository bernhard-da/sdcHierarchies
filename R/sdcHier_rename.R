#' sdcHier_rename
#'
#' rename nodes/levels in a nested hierarchy
#' @inherit sdcHier_add
#' @param node_labs_new (character) new names of nodes/levels that should be changed
#' @export
#' @examples
#' ## for examples, see ?sdcHier_create
sdcHier_rename <- function(h, node_labs, node_labs_new) {
  h_is_valid(h)
  stopifnot(is.character(node_labs))
  stopifnot(is.character(node_labs_new))
  stopifnot(length(node_labs)==length(node_labs_new))

  all_nodes <- sdcHier_nodenames(h)
  if (!all(node_labs %in% all_nodes)) {
    stop(paste("some nodes specified in argument",shQuote("node_labs"), "don't exist!"), call.=FALSE)
  }
  if (any(node_labs_new %in% all_nodes)) {
    stop(paste("some nodes specified in argument",shQuote("node_labs_new"), "already exist!"), call.=FALSE)
  }

  for (i in 1:length(node_labs)) {
    aa <- FindNode(h, node_labs[i])
    aa$name <- node_labs_new[i]
  }
  return(h)
}
