#' hier_info
#'
#' get information about all or specific nodes in a nested hierarchy
#' @inherit hier_add
#' @param node_labs (character) name(s) of nodes for which information should be extracted.
#' @export
#' @return a \code{list} with information about the required nodes. If \code{node_labs} is
#' \code{NULL} (the default), the information is computed for all available nodes of
#' the hierarchy. The following properties are computed:
#' \itemize{
#' \item{exists: }{(logical) does the node exist}
#' \item{name: }{(character) node name}
#' \item{is_rootnode: }{(logical) is the node the overall root of the tree?}
#' \item{level: }{(numeric) what is the level of the nod}
#' \item{is_leaf: }{(logical) is the node a leaf?}
#' \item{siblings: }{(character) what are siblings of this node?}
#' \item{contributing_codes: }{(character) which codes are contributing to this node? If none (it is a leaf), \code{NA} is returned}
#' \item{children: }{(character) the names of the children of the node. If it has none (it is a leaf), \code{NA} is returned}
#' \item{is_bogus: }{(logical) is it a bogus code (i. e the only children of a leaf?)}
#' }
#' @examples
#' ## for examples, see hier_vignette()
hier_info <- function(h, node_labs=NULL) {
  h_is_valid(h)
  all_nodes <- hier_nodenames(h)
  if (is.null(node_labs)) {
    node_labs <- all_nodes
  } else {
    stopifnot(is.character(node_labs))
  }

  if (length(node_labs) == 1) {
    return(h_nodeinfo(h, node_lab = node_labs))
  }

  out <- lapply(node_labs, function(x) {
    h_nodeinfo(h, node_lab = x)
  })
  names(out) <- node_labs
  out
}
