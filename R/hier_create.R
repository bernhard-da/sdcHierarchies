#' Create a hierarchy
#'
#' This functions allows to generate a hierarchical
#' data structure that can be used in other packages such
#' as [`cellKey`](https://github.com/sdcTools/cellKey)
#' or [`sdcTable`](https://github.com/sdcTools/sdcTable).
#'
#' @param root (character) name of the overall total
#' @param nodes (character) name of leaves (nodes) in the hierarchy
#' @return a (nested) sdc hierarchy tree
#' @seealso hier_add hier_delete hier_rename hier_export
#' hier_convert hier_app hier_info
#' @export
#' @md
#' @examples
#' # without nodes
#' h <- hier_create(root = "tot")
#' hier_display(h)
#'
#' # with nodes
#' h <- hier_create(root = "tot", nodes = LETTERS[1:5])
#' hier_display(h)
hier_create <- function(root = "Total", nodes = NULL) {
  tree <- .init(rootnode = root)
  if (!is.null(nodes)) {
    tree <- .add_nodes(
      tree = tree,
      new = data.table(
        root = root,
        leaf  = nodes,
        level = 2
      )
    )
  }
  tree <- .add_class(tree)
  .is_valid(tree)
  tree
}
