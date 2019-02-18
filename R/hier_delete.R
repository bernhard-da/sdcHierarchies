#' Delete nodes from an existing hierarchy
#'
#' This function allows to delete nodes (levels)
#' from an existing nested hierarchy.
#'
#' @inherit hier_add
#' @param nodes character vector of nodes that should be deleted
#' @export
#' @examples
#' ## for examples, see hier_vignette()
hier_delete <- function(tree, nodes) {
  .is_valid(tree)
  stopifnot(is.character(nodes))
  if (.rootnode(tree) %in% nodes) {
    stop("The rootnode cannot be deleted!", call. = FALSE)
  }

  ii <- nodes %in% .all_nodes(tree)
  if (!all(ii)) {
    warning("Some specified nodes were not found in the tree!\n")
    nodes <- nodes[ii]
    if (length(nodes) == 0) {
      return(tree)
    }
  }

  for (node in nodes) {
    tree <- .prune(tree, leaf = node)
  }
  tree <- .add_class(tree)
  .is_valid(tree)
  tree
}
