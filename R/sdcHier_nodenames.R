#' sdcHier_nodenames
#'
#' get the names of the nodes and all (sub)-nodes and leaves of a given nested hierarchy
#' @inherit sdcHier_add
#' @param node_lab (character) name of start node from which all lower level-names should be returned
#' @export
#' @examples
#' ## for examples, see sdcHier_vignette()
sdcHier_nodenames <- function(h, node_lab=NULL) {
  h_is_valid(h)

  if (is.null(node_lab)) {
    return(as.character(h$Get("name")))
  }

  stopifnot(is_scalar_character(node_lab))
  if (!h_node_exists(h, node_lab)) {
    stop(paste(shQuote(node_lab),"is not a node in the given hierachy"), call.=FALSE)
  }
  nn <- FindNode(h, node_lab)
  return(as.character(nn$Get("name")))
}
