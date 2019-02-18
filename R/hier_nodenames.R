#' Extract name of nodes (levels)
#'
#' This function allows to extract the all the names of the
#' nodes including all (sub)-nodes and leaves in the given
#' hierarchy.
#' @inherit hier_add
#' @param node_lab (character) name of start node from which all lower
#' level-names should be returned
#' @export
#' @examples
#' ## for examples, see hier_vignette()
hier_nodenames <- function(tree, node_lab=NULL) {
  .is_valid(tree)

  if (!is.null(node_lab)) {
    stopifnot(is_scalar_character(node_lab))
    .exists(tree = tree, leaf = node_lab)
  }

  if (is.null(node_lab)) {
    return(.all_nodes(tree))
  }

  stopifnot(is_scalar_character(node_lab))
  if (!.exists(tree, node_lab)) {
    ll <- shQuote(node_lab)
    stop(paste(ll, "is not a node in the given hierachy"), call. = FALSE)
  }

  todo <- node_lab
  res <- c()
  while (length(todo) > 0) {
    cc <- todo[1]
    todo <- setdiff(c(todo, .children(tree, cc)), NA)
    res <- c(res, todo[1])
    todo <- todo[-1]
  }
  res
}
