#' Information about hierarchy-codes
#'
#' [hier_info()] computes various information about hierarchy codes
#' or the (nested) hierarchy.
#'
#' @inherit hier_add
#' @export
#' @return a `list` with information about the required nodes.
#' If `nodes` is `NULL` (the default), the information is
#' computed for all available nodes of the hierarchy. The following
#' properties are computed:
#' - `exists`: (logical) does the node exist
#' - `name`: (character) node name
#' - `is_rootnode`: (logical) is the node the overall root of the tree?
#' - `level`: (numeric) what is the level of the node
#' - `is_leaf`: (logical) is the node a leaf?
#' - `siblings`: (character) what are siblings of this node?
#' - `contributing_codes`: (character) which codes are contributing
#' to this node? If none (it is a leaf), `NA` is returned
#' - `children`: (character) the names of the children of the node.
#' If it has none (it is a leaf), `NA` is returned
#' - `is_bogus`: (logical) is it a bogus code (i. e the only
#' children of a leaf?)
#' @md
#' @examples
#' h <- hier_create(root = "Total",  nodes = LETTERS[1:3])
#' h <- hier_add(h, root = "A", nodes = c("a1", "a5"))
#' hier_display(h)
#'
#' # about a specific node
#' hier_info(h, nodes = "a1")
#'
#' # about all nodes
#' hier_info(h)
hier_info <- function(tree, nodes=NULL) {
  nn <- .all_nodes(tree)
  if (is.null(nodes)) {
    nodes <- nn
  } else {
    stopifnot(is.character(nodes))
    if (!all(nodes %in% nn)) {
      e <- "not all codes provided in `nodes` are available in the hierarchy."
      stop(e, call. = FALSE)
    }
    stopifnot(sum(duplicated(nodes)) == 0)
  }

  if (length(nodes) == 1) {
    res <- .info(tree = tree, leaf = nodes)
  } else {
    res <- lapply(nodes, .info, tree = tree)
    names(res) <- nodes
  }
  res
}
