#' Rename nodes in an existing hierarchy
#'
#' This function allows to rename one or more node(s) (levels)
#' in an existing nested hierarchy.
#'
#' @inherit hier_add
#' @param node_labs_new (character) new names of nodes/levels that should be changed
#' @export
#' @examples
#' ## for examples, see hier_vignette()
hier_rename <- function(h, node_labs, node_labs_new) {
  h_is_valid(h)
  stopifnot(is.character(node_labs))
  stopifnot(is.character(node_labs_new))
  stopifnot(length(node_labs) == length(node_labs_new))

  all_nodes <- hier_nodenames(h)
  if (!all(node_labs %in% all_nodes)) {
    ll <- shQuote("node_labs")
    err <- paste("some nodes specified in argument", ll, "don't exist!")
    stop(err, call. = TRUE)
  }
  if (any(node_labs_new %in% all_nodes)) {
    ll <- shQuote("node_labs_new")
    err <- paste("some nodes specified in argument", ll, "already exist!")
    stop(err, call. = TRUE)
  }

  for (i in 1:length(node_labs)) {
    aa <- FindNode(h, node_labs[i])
    aa$name <- node_labs_new[i]
  }
  return(invisible(h))
}