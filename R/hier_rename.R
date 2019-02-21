#' Rename nodes in an existing hierarchy
#'
#' This function allows to rename one or more node(s) (levels)
#' in an existing nested hierarchy.
#'
#' @inherit hier_add
#' @param leaves (character) new names of nodes/levels that should be changed as
#' a named vector: names refer to old, existing names, the values to the
#' new labels
#' @export
#' @examples
#' h <- hier_create(rootnode = "Total",  leaves = LETTERS[1:3])
#' h <- hier_add(h, node = "A", leaves = c("a1", "a5"))
#' hier_display(h)
#'
#' h <- hier_rename(h, leaves = c("a1" = "x1", "A" = "X"))
#' hier_display(h)
hier_rename <- function(tree, leaves) {
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
      "It is not possible to rename nodes to names that already",
      "exist in the tree."
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
