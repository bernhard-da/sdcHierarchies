#' sdcHier_info
#'
#' get information about all or specific nodes in a nested hierarchy
#' @inherit sdcHier_add
#' @param node_labs (character) name(s) of nodes for which information should be extracted.
#' @export
#' @return a \code{list} with information about the required nodes. If \code{node_labs} is
#' \code{NULL} (the default), the information is computed for all available nodes of
#' the hierarchy.
#' @examples
#' ## for examples, see sdcHier_vignette()
sdcHier_info <- function(h, node_labs=NULL) {
  h_is_valid(h)
  all_nodes <- sdcHier_nodenames(h)
  if (is.null(node_labs)) {
    node_labs <- all_nodes
  } else {
    stopifnot(is.character(node_labs))
  }

  if (length(node_labs) == 1) {
    return(h_nodeinfo(h, node_lab = node_labs))
  }
  out <- lapply(node_labs, function(x) {
    h_nodeinfo(h, node_lab = x)
  })
  names(out) <- node_labs
  out
}
