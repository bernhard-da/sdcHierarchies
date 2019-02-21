#' Add nodes to an existing hierarchy
#'
#' This function allows to add nodes (levels)
#' to an existing nested hierarchy.
#'
#' @inherit hier_create
#' @param tree a (nested) hierarchy created using \code{\link{hier_create}}
#' or modified using \code{\link{hier_add}}, \code{\link{hier_delete}}
#' or \code{\link{hier_rename}}.
#' @param node (character) an existing node in the input \code{h}
#' @param leaves names of the new nodes/levels that should be added
#' @export
#' @examples
#' h <- hier_create(rootnode = "Total",  leaves = LETTERS[1:3])
#' h <- hier_add(h, node = "A", leaves = c("a1", "a5"))
#' hier_display(h)
hier_add <- function(tree, node, leaves) {
  .is_valid(tree)
  stopifnot(is_scalar_character(node))

  # rootnode needs to exist in the tree
  ex_nodes <- .all_nodes(tree)

  if (!node %in% ex_nodes) {
    stop("The reference node does not exist!")
  }
  stopifnot(is.character(leaves))

  ii <- which(leaves %in% ex_nodes)
  if (sum(ii) > 0) {
    warning("Some of the provided leaves already exist and are not added.")
    leaves <- leaves[!ii]
    if (length(leaves) == 0) {
      return(tree)
    }
  }
  tree <- .add_nodes(
    tree = tree,
    new = data.table(
      root = node,
      leaf = leaves
    )
  )
  tree <- .add_class(tree)
  .is_valid(tree)
  tree
}
