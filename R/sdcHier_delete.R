#' sdcHier_delete
#'
#' add nodes/levels from a nested hierarchy
#' @inherit sdcHier_add
#' @export
#' @examples
#' ## for examples, see sdcHier_vignette()
sdcHier_delete <- function(h, node_labs) {
  h_is_valid(h)
  stopifnot(is.character(node_labs))

  res <- lapply(node_labs, function(x) {
    sdcHier_info(h, x)
  })
  for (i in 1:length(node_labs)) {
    if (res[[1]]$exists == FALSE) {
      warning(paste("node", shQuote(node_labs[i]), "does not exist and can't be deleted!"), call. = FALSE)
    } else {
      FindNode(h, res[[i]]$parent)$RemoveChild(node_labs[i])
    }
  }
  return(invisible(h))
}
