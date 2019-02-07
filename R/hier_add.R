#' Add nodes to an existing hierarchy
#'
#' This function allows to add nodes (levels)
#' to an existing nested hierarchy.
#'
#' @inherit hier_create
#' @param h a (nested) hierarchy created using \code{\link{hier_create}} or modified
#' using \code{\link{hier_add}}, \code{\link{hier_delete}} or \code{\link{hier_rename}}.
#' @param refnode (character) an existing node in the input \code{h}
#' @param node_labs names of the new nodes/levels that should be added
#' @export
#' @examples
#' ## for examples, see hier_vignette()
hier_add <- function(h, refnode, node_labs) {
  h_is_valid(h)

  stopifnot(is_scalar_character(refnode))
  stopifnot(is.character(node_labs))

  if (is.null(FindNode(h, refnode))) {
    stop("The reference node does not exist!\n", call. = FALSE)
  }
  if (refnode %in% node_labs) {
    err <- paste("at least one leaf-name equals", shQuote(refnode))
    stop(err, call. = FALSE)
  }

  res <- sapply(node_labs, function(x) {
    hier_info(h, node_labs = x)$exists
  })

  nn <- node_labs
  for (i in 1:length(nn)) {
    if (res[i] == TRUE) {
      n <- shQuote(nn[i])
      w <- paste("Node", n, "already exists and won't be added --> skipping")
      warning(w, call. = FALSE)
    } else {
      data.tree::FindNode(h, refnode)$AddChild(node_labs[i])
    }
  }
  return(invisible(h))
}
