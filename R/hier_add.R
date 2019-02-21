#' Add nodes to an existing hierarchy
#'
#' This function allows to add nodes (levels)
#' to an existing nested hierarchy.
#'
#' @param tree a (nested) hierarchy created using \code{\link{hier_create}}
#' or modified using \code{\link{hier_add}}, \code{\link{hier_delete}}
#' or \code{\link{hier_rename}}.
#' @param root (character) a name of an existing node in the hierarchy
#' @param nodes (character) names of new nodes that should be added below
#' \code{"root"}
#' @export
#' @examples
#' h <- hier_create(root = "Total",  nodes = LETTERS[1:3])
#' h <- hier_add(h, root = "A", nodes = c("a1", "a5"))
#' hier_display(h)
hier_add <- function(tree, root, nodes) {
  .is_valid(tree)
  stopifnot(is_scalar_character(root))

  # rootnode needs to exist in the tree
  ex_nodes <- .all_nodes(tree)

  if (!root %in% ex_nodes) {
    stop("The root node does not exist!")
  }
  stopifnot(is.character(nodes))

  ii <- which(nodes %in% ex_nodes)
  if (sum(ii) > 0) {
    warning("Some of the provided nodes already exist and are not added.")
    nodes <- nodes[!ii]
    if (length(nodes) == 0) {
      return(tree)
    }
  }
  tree <- .add_nodes(
    tree = tree,
    new = data.table(
      root = root,
      leaf = nodes
    )
  )
  tree <- .add_class(tree)
  .is_valid(tree)
  tree
}
