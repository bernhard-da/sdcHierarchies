#' Rename nodes in an existing hierarchy
#'
#' This function allows to rename one or more node(s) (levels)
#' in an existing nested hierarchy.
#'
#' @inheritParams hier_add
#' @param nodes (character) new names of nodes/levels that should be changed as
#' a named vector: names refer to old, existing names, the values to the
#' new labels
#' @export
#' @md
#' @examples
#' h <- hier_create(root = "Total",  nodes = LETTERS[1:3])
#' h <- hier_add(h, root = "A", nodes = c("a1", "a5"))
#' hier_display(h)
#'
#' h <- hier_rename(h, nodes = c("a1" = "x1", "A" = "X"))
#' hier_display(h)
hier_rename <- function(tree, nodes) {
  .is_valid(tree)
  stopifnot(is.character(nodes))
  stopifnot(is_named(nodes))

  old <- names(nodes)
  new <- as.character(nodes)

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
    stop("Some nodes you want to rename do not exist!\n", call. = FALSE)
  }

  for (i in seq_along(old)) {
    tree$root[tree$root == old[i]] <- new[i]
    tree$leaf[tree$leaf == old[i]] <- new[i]
  }
  tree <- .add_class(tree)
  .is_valid(tree)
  tree
}
