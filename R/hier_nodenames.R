#' Extract name of nodes (levels)
#'
#' This function allows to extract the all the names of the
#' nodes including all (sub)-nodes and leaves in the given
#' hierarchy.
#'
#' @inherit hier_add
#' @param root (character) name of start node from which all lower
#' level-names should be returned
#' @export
#' @md
#' @examples
#' h <- hier_create(root = "Total",  nodes = LETTERS[1:3])
#' h <- hier_add(h, root = "A", nodes = c("a1", "a5"))
#' hier_nodenames(h)
hier_nodenames <- function(tree, root=NULL) {
  .is_valid(tree)
  if (is.null(root)) {
    return(.all_nodes(tree))
  }

  stopifnot(is_scalar_character(root))
  if (!.exists(tree, root)) {
    e <- paste(
      "The given root", shQuote(root),
      "is not a node in the hierarchy!"
    )
    stop(paste(e, collapse = " "), call. = FALSE)
  }

  todo <- root
  res <- c()
  while (length(todo) > 0) {
    cc <- todo[1]
    todo <- setdiff(c(todo, .children(tree, cc)), NA)
    res <- c(res, todo[1])
    todo <- todo[-1]
  }
  res
}
