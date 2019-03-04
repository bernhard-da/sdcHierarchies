#' Convert a nested hierarchy into the default format
#'
#' This function returns a tree in default format (as for
#' example created using [hier_create()]) for objects created using
#' [hier_convert()].
#'
#' @param inp a nested tree object created using [hier_create()]
#' or an object converted with [hier_convert()]
#'
#' @return a nested hierarchy with default format
#' @export
#' @md
#' @examples
#' h <- hier_create(root = "Total",  nodes = LETTERS[1:3])
#' h <- hier_add(h, root = "A", nodes = c("a1", "a5"))
#' sdc <- hier_convert(h, as = "sdc")
#' hier_display(h)
#' hier_display(hier_to_tree(h))
#' hier_display(hier_to_tree(sdc))
hier_to_tree <- function(inp) {
  converted_format <- attributes(inp)$hier_format
  if (!is.null(converted_format)) {
    inp <- hier_import(inp, from = converted_format)
  }
  .is_valid(tree = inp)
  inp
}
