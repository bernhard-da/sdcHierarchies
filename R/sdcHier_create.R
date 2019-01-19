#' sdcHier_create
#'
#' generate and modify hierarchical data structure to be used in other packages such
#' as \href{https://github.com/sdcTools/cellKey}{cellKey}
#' or \href{https://github.com/sdcTools/sdcTable}{sdcTable}.
#'
#' @param tot_lab (character) name of the overall total
#' @param node_labs (character) name of levels (nodes) in the hierarchy
#' @return a (nested) sdc hierarchy tree
#' @export
#' @examples
#' ## for examples, see sdcHier_vignette()
sdcHier_create <- function(tot_lab, node_labs=NULL) {
  stopifnot(is_scalar_character(tot_lab))
  if (!is.null(node_labs)) {
    stopifnot(is.character(node_labs))
    if (tot_lab %in% node_labs) {
      stop("at least one leaf-names matches the overall total", call. = FALSE)
    }
  }

  h <- Node$new(tot_lab)
  class(h) <- c(class(h), "sdcHier")
  if (!is.null(node_labs)) {
    for (i in seq_along(node_labs)) {
      h$AddChild(node_labs[i])
    }
  }
  h
}
