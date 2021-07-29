#' Displays the hierarchy
#'
#' This function shows the entire hierarchy in a nice way.
#'
#' @param x a hierarchy object, either directly generated and modified using
#' [hier_create()], [hier_add()], [hier_delete()] and/or [hier_rename()] or
#' objects converted using [hier_convert()]
#' @param root `NULL` if the entire tree should be printed or a name of
#' a node which is used as temporary root-node for printing
#' @return `NULL`; the tree is printed to the prompt
#' @export
#' @md
#' @examples
#' h <- hier_create(root = "Total", nodes = LETTERS[1:2])
#' h <- hier_add(h, root = "A", nodes = c("a1", "a2"))
#'
#' # display the entire tree
#' hier_display(h)
#'
#' # display only a subtree
#' hier_display(h, root = "A")
hier_display <- function(x, root = NULL) {
  if (isTRUE(attributes(x)$hier_convert)) {
    x <- hier_import(inp = x, from = attributes(x)$hier_format)
  }

  .is_valid(x)
  #if (!.is_sorted(x)) {
  #  x <- .sort(x)
  #}

  if (is.null(root)) {
    from <- .rootnode(x)
  } else {
    stopifnot(is_scalar_character(root))
    stopifnot(.exists(tree = x, leaf = root))
    from <- root
  }

  df <- data.frame(
    nodes = .all_nodes(x),
    stringsAsFactors = FALSE
  )
  df$children <- lapply(.all_nodes(x), function(y) {
    setdiff(x$leaf[x$root == y], y)
  })
  cli::tree(df, root = from)
}
