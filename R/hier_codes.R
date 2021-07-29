#' Default-Codes
#'
#' [hier_codes()] returns the standardized codes for the nodes of a tree.
#'
#' @inheritParams hier_add
#'
#' @return a named character vector with names being the node-names and the
#' values the standardized codes
#' @export
#' @md
#' @examples
#' h <- hier_create(root = "Total",  nodes = LETTERS[1:3])
#' h <- hier_add(h, root = "A", nodes = c("a1", "a5"))
#' hier_codes(h)
hier_codes <- function(tree) {
  .is_valid(tree)
  req_digits <- .required_digits(tree)

  dt <- hier_convert(tree, "df")
  dt$levs <- nchar(dt$level)
  cc <- paste(rep("0", sum(req_digits)), collapse = "")
  codes_default <- rep(cc, nrow(dt))
  names(codes_default) <- dt$name

  if (nrow(dt) == 1) {
    return(codes_default)
  }

  cs <- cumsum(req_digits)

  finished <- FALSE
  pool <- dt$name[-1]
  cur_node <- pool[1]
  while (!finished) {
    ii <- hier_info(tree, cur_node)
    pc <- codes_default[ii$parent]
    ll <- ii$level

    ss <- c(cur_node, ii$siblings)

    new_val <- sprintf(
      paste0("%0", req_digits[ll], "d"),
      seq_len(length(ss))
    )
    first <- cs[ll - 1] + 1
    last <- cs[ll]

    inp <- rep(pc, length(ss))
    names(inp) <- ss
    substr(inp, start = first, stop = last) <- new_val
    codes_default[ss] <- inp

    pool <- setdiff(pool, ss)
    if (length(pool) == 0) {
      finished <- TRUE
    } else {
      cur_node <- pool[1]
    }
  }
  codes_default
}


# tree <- hier_create(root = "total", nodes = LETTERS[1:10])
# tree <- hier_add(tree, root = "B", nodes = c("b2", "b3", "b1"))
# tree <- hier_add(tree, root = "C", nodes = c("c3", "c2", "c1"))
# tree <- hier_add(tree, root = "A", nodes = c("a1"))
# tree <- hier_add(tree, root = "a1", nodes = c("a1a"))
# tree <- hier_add(tree, root = "J", nodes = c("j1"))
# tree <- hier_add(tree, root = "j1", nodes = c("j1a"))
# tree <- hier_add(tree, root = "j1a", nodes = c("j1a-x"))
#
# hier_default_codes(tree)
