#' Rename nodes in an existing hierarchy
#'
#' This function allows to rename one or more node(s) (levels)
#' in an existing nested hierarchy.
#'
#' @inherit hier_add
#' @param leaves (character) new names of nodes/levels that should be changed as
#' a named vector: names refer to new names, values to existing names
#' @export
#' @examples
#' ## for examples, see hier_vignette()
hier_rename <- function(tree, leaves) {
  root <- NULL
  .is_valid(tree)
  stopifnot(is.character(leaves))
  stopifnot(is_named(leaves))

  old <- names(leaves)
  new <- as.character(leaves)

  if (any(duplicated(new))) {
    e <- c(
      "duplicated values for new leaf names",
      "are not allowed!"
    )
    stop(paste(e, collapse = " "), call. = FALSE)
  }

  if (sum(new %in% .all_nodes(tree)) > 0) {
    e <- c(
      "this tree already contains leaves with names specified in",
      "argument", shQuote(labels)
    )
    stop(paste(e, collapse = " "), call. = FALSE)
  }

  ex <- sapply(old, function(x) {
    .exists(tree = tree, leaf = x)
  })
  if (!all(ex)) {
    stop("Some leaves you want to rename do not exist!\n", call. = FALSE)
  }

  for (i in seq_along(old)) {
    tree$root[tree$root == old[i]] <- new[i]
    tree$leaf[tree$leaf == old[i]] <- new[i]
  }
  tree <- .add_class(tree)
  .is_valid(tree)
  tree
}
