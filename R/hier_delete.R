#' Delete nodes from an existing hierarchy
#'
#' This function allows to delete nodes (levels)
#' from an existing nested hierarchy.
#'
#' @inherit hier_add
#' @export
#' @examples
#' ## for examples, see hier_vignette()
hier_delete <- function(h, node_labs) {
  h_is_valid(h)
  stopifnot(is.character(node_labs))

  res <- lapply(node_labs, function(x) {
    hier_info(h, x)
  })
  for (i in 1:length(node_labs)) {
    if (res[[1]]$exists == FALSE) {
      ll <- shQuote(node_labs[i])
      w <- paste("node", ll, "does not exist and can't be deleted!")
      warning(w, call. = FALSE)
    } else {
      FindNode(h, res[[i]]$parent)$RemoveChild(node_labs[i])
    }
  }
  return(invisible(h))
}
