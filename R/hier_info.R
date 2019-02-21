#' hier_info
#'
#' get information about all or specific nodes in a nested hierarchy
#' @inherit hier_add
#' @export
#' @return a \code{list} with information about the required nodes.
#' If \code{node_labs} is \code{NULL} (the default), the information is
#' computed for all available nodes of the hierarchy. The following
#' properties are computed:
#' \itemize{
#' \item{exists: }{(logical) does the node exist}
#' \item{name: }{(character) node name}
#' \item{is_rootnode: }{(logical) is the node the overall root of the tree?}
#' \item{level: }{(numeric) what is the level of the nod}
#' \item{is_leaf: }{(logical) is the node a leaf?}
#' \item{siblings: }{(character) what are siblings of this node?}
#' \item{contributing_codes: }{(character) which codes are contributing
#' to this node? If none (it is a leaf), \code{NA} is returned}
#' \item{children: }{(character) the names of the children of the node.
#' If it has none (it is a leaf), \code{NA} is returned}
#' \item{is_bogus: }{(logical) is it a bogus code (i. e the only
#' children of a leaf?)}
#' }
#' @examples
#' h <- hier_create(rootnode = "Total",  leaves = LETTERS[1:3])
#' h <- hier_add(h, node = "A", leaves = c("a1", "a5"))
#' hier_display(h)
#'
#' # about a specific node
#' hier_info(h, "a1")
#'
#' # about all nodes
#' hier_info(h)
hier_info <- function(tree, leaves=NULL) {
  nn <- .all_nodes(tree)
  if (is.null(leaves)) {
    leaves <- nn
  } else {
    stopifnot(is.character(leaves))
    if (!all(leaves %in% nn)) {
      e <- "not all codes provided in `leaves` are available in the hierarchy."
      stop(e, call. = FALSE)
    }
    stopifnot(sum(duplicated(leaves)) == 0)
  }

  if (length(leaves) == 1) {
    res <- .info(tree = tree, leaf = leaves)
  } else {
    res <- lapply(leaves, .info, tree = tree)
    names(res) <- leaves
  }
  res
}
