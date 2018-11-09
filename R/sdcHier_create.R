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
#' ## create a hierarchy
#' h <- sdcHier_create(tot="tot", node_labs=letters[1:3])
#'
#' ## add nodes
#' sdcHier_add(h, refnode="a", node_labs=c("a1"))
#' sdcHier_add(h, refnode="a", node_labs=c("a2","a3"))
#' sdcHier_add(h, refnode="a", node_labs=c("a2","a3"))
#'
#' ## delete nodes
#' sdcHier_delete(h, node_labs=c("a2","a3"))
#' sdcHier_delete(h, node_labs=c("a2","a3"))
#' sdcHier_delete(h, node_labs=c("c"))
#'
#' ## rename levels
#' sdcHier_rename(h, node_labs=c("a1","b"), node_labs_new=c("x1","B"))
#'
#' # info about a specific node
#' sdcHier_info(h, node_lab="a")
#'
#' # nodeinfo about all nodes
#' sdcHier_info(h, node_labs=NULL)
#' sdcHier_info(h, node_labs="tot")
#' sdcHier_info(h, node_labs="B")
#'
#' ## nodenames
#' sdcHier_nodenames(h)
#' sdcHier_nodenames(h, node_lab="a")
#'
#' ## convert to data.frame
#' sdcHier_convert(h, format="data.frame")
sdcHier_create <- function(tot_lab, node_labs=NULL) {
  stopifnot(is_scalar_character(tot_lab))
  if (!is.null(node_labs)) {
    stopifnot(is.character(node_labs))
    if (tot_lab %in% node_labs) {
      stop("at least one leaf-names matches the overall total", call.=FALSE)
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

