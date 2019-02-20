#' Displays the hierarchy
#'
#' @param x a hierarchy object
#' @param root \code{NULL} if the entire tree should be printed or a name of
#' a node which is used as temporary root-node for printing
#' @return \code{NULL}; the tree is printed to the prompt
#' @export
#' @examples
#' ## for examples, see hier_vignette()
hier_display <- function(x, root = NULL) {
  .is_valid(x)
  if (!.is_sorted(x)) {
    x <- .sort(x)
  }

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
