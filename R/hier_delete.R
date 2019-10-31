#' Delete nodes from an existing hierarchy
#'
#' This function allows to delete nodes (levels)
#' from an existing nested hierarchy.
#'
#' @inherit hier_add
#' @param nodes character vector of nodes that should be deleted
#' @export
#' @md
#' @examples
#' h <- hier_create(root = "Total", nodes = LETTERS[1:2])
#' h <- hier_add(h, root = "A", nodes = c("a1", "a2"))
#' h <- hier_add(h, root = "B", nodes = c("b1", "b2"))
#' h <- hier_add(h, root = "b1", nodes = "b1a")
#' hier_display(h)
#'
#' h <- hier_delete(h, nodes = c("a1", "b1a"))
#' hier_display(h)
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
