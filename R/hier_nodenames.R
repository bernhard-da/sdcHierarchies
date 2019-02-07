#' Extract name of nodes (levels)
#'
#' This function allows to extract the all the names of the
#' nodes including all (sub)-nodes and leaves in the given
#' hierarchy.
#' @inherit hier_add
#' @param node_lab (character) name of start node from which all lower level-names should be returned
#' @export
#' @examples
#' ## for examples, see hier_vignette()
hier_nodenames <- function(h, node_lab=NULL) {
  h_is_valid(h)

  if (is.null(node_lab)) {
    return(as.character(h$Get("name")))
  }

  stopifnot(is_scalar_character(node_lab))
  if (!h_node_exists(h, node_lab)) {
    ll <- shQuote(node_lab)
    stop(paste(ll, "is not a node in the given hierachy"), call. = FALSE)
  }
  nn <- FindNode(h, node_lab)
  return(as.character(nn$Get("name")))
}
