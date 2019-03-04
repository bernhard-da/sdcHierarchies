#' Match default and original node labels
#'
#' This function returns a `data.table` that maps original
#' and default codes.
#'
#' @param tree an input derived from [hier_create()] or
#' [hier_convert()]
#' @param nodes `NULL` or a character vector specifying either
#' original node names or standardized default codes. If
#' `NULL`, the information is returned for all nodes.
#' @param inputs (character) specifies what kind of node names are
#' provided in argument `nodes`. Allowed choices are:
#' - `"orig"`: argument `nodes` refers to original node names
#' - `"default"`: argument `nodes` refers to standardized
#' default codes
#' @return a `data.table` with the following columns:
#' - `"orig"`: the original node names
#' - `"default": the standardized names
#' - `"is_bogus"`: `TRUE` if the code is a `"bogus"` (duplicated)
#' node.
#' @export
#' @md
#' @examples
#' h <- hier_create(root = "Tot", nodes = letters[1:5])
#' h <- hier_add(h, root = "a", nodes = "a0")
#' h2 <- hier_convert(tree = h, as = "dt")
#' hier_match(tree = h, nodes = c("a", "b"), inputs = "orig")
#' hier_match(tree = h2, nodes = c("01", "02"), inputs = "default")
hier_match <- function(tree, nodes = NULL, inputs = "orig") {
  if (!is.null(nodes)) {
    if (!is.character(nodes)) {
      stop("Argument `nodes` must be a character vector.", call. = FALSE)
    }
  }
  if (!is_scalar_character(inputs)) {
    stop("Argument `inputs` must have a single element", call. = FALSE)
  }
  if (!inputs %in% c("orig", "default")) {
    stop("Argument `inputs` must be `orig` or `default`", call. = FALSE)
  }

  tree <- hier_to_tree(tree)

  tree_list <- hier_convert(tree, as = "sdc")
  dt <- data.table(
    orig = tree_list$codes$orig,
    default = tree_list$codes$default,
    is_bogus = FALSE
  )

  bogus_codes <- tree_list$bogus$bogus_codes
  if (!is.null(bogus_codes)) {
    dt <- rbind(
      dt,
      data.table(
        orig = bogus_codes,
        default = NA_character_,
        is_bogus = TRUE
      )
    )
  }

  if (is.null(nodes)) {
    return(dt)
  }

  if (inputs == "orig") {
    orig <- NULL
    dt <- dt[orig %in% nodes]
  }
  if (inputs == "default") {
    default <- NULL
    dt <- dt[default %in% nodes]

  }
  if (nrow(dt) != length(nodes)) {
    stop("Not all nodes specified in `nodes` were found.", call. = FALSE)
  }
  dt
}
