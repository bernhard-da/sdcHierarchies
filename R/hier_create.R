#' creates a nested hierarchy object
#'
#' This functions allows to generate a hierarchical
#' data structure that can be used in other packages such
#' as \href{https://github.com/sdcTools/cellKey}{cellKey}
#' or \href{https://github.com/sdcTools/sdcTable}{sdcTable}.
#'
#' @param rootnode (character) name of the overall total
#' @param leaves (character) name of leaves (nodes) in the hierarchy
#' @return a (nested) sdc hierarchy tree
#' @seealso hier_add hier_delete hier_rename hier_export
#' hier_convert hier_app hier_info
#' @export
#' @examples
#' # without leaves
#' h <- hier_create(rootnode = "tot")
#' hier_display(h)
#'
#' # with leaves
#' h <- hier_create(rootnode = "tot", leaves = LETTERS[1:5])
#' hier_display(h)
hier_create <- function(rootnode = "Total", leaves = NULL) {
  tree <- .init(rootnode = rootnode)
  if (!is.null(leaves)) {
    tree <- .add_nodes(
      tree = tree,
      new = data.table(
        root = rootnode,
        leaf  = leaves
      )
    )
  }
  tree <- .add_class(tree)
  .is_valid(tree)
  tree
}
