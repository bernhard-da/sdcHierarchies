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
#' h <- hier_create(root = "Total", nodes = LETTERS[1:3])
#' h <- hier_add(h, root = "A", nodes = c("a1", "a5"))
#' hier_codes(h)
hier_codes <- function(tree) {
  # simple re-implementation of stringr::str_pad(.., side = "left", pad = "0")
  str_pad_left_zero <- function(string, width) {
    string <- as.character(string)
    n <- nchar(string)
    pad_len <- pmax(width - n, 0)
    padding <- mapply(function(p) paste(rep("0", p), collapse = ""), pad_len)
    paste0(padding, string)
  }

  update_level <- function(x, lev = 2, req_digits) {
    level <- leaf <- codes_default <- NULL
    stopifnot(lev <= length(req_digits))
    stopifnot(lev >= 1)
    get_pos_start <- function(lev, req_digits) {
      if (lev == 1) {
        return(1)
      }
      cumsum(req_digits)[(lev - 1)] + 1
    }
    get_pos_end <- function(lev, req_digits) {
      cumsum(req_digits)[lev]
    }

    cs <- cumsum(req_digits)
    dd <- req_digits[lev]

    pos_start <- get_pos_start(lev = lev, req_digits = req_digits)
    pos_end <- get_pos_end(lev = lev, req_digits = req_digits)

    tmp <- x[level == lev]
    spl <- split(tmp, tmp$root)
    for (split_id in seq_len(length(spl))) {
      # 1: Use parent code (using x$root)
      tmp <- spl[[split_id]]
      code_parent <- x[leaf == tmp$root[1], codes_default][1]
      tmp[, codes_default := code_parent]

      # 2: Update of `code_default`
      substr(tmp$codes_default, pos_start, pos_end) <- str_pad_left_zero(seq_len(nrow(tmp)), width = dd)
      spl[[split_id]] <- tmp
    }
    tmp <- rbindlist(spl)
    x[tmp$id, codes_default := tmp$codes_default]
    x
  }

  .is_valid(tree)
  req_digits <- .required_digits(tree)

  x <- copy(tree)
  x$id <- seq_len(nrow(x))

  cc <- paste(rep("0", sum(req_digits)), collapse = "")
  x$codes_default <- rep(cc, nrow(x))

  if (nrow(x) > 1) {
    for (lev in 2:max(x$level)) {
      x <- update_level(x = x, lev = lev, req_digits = req_digits)
    }
  }

  # Sort
  data.table::setorderv(x, "codes_default")
  out <- x$codes_default
  names(out) <- x$leaf
  return(out)
}

# tree <- hier_create(root = "total", nodes = LETTERS[1:10])
# tree <- hier_add(tree, root = "B", nodes = c("b2", "b3", "b1"))
# tree <- hier_add(tree, root = "C", nodes = c("c3", "c2", "c1"))
# tree <- hier_add(tree, root = "A", nodes = c("a1"))
# tree <- hier_add(tree, root = "a1", nodes = c("a1a"))
# tree <- hier_add(tree, root = "J", nodes = c("j1"))
# tree <- hier_add(tree, root = "j1", nodes = c("j1a"))
# tree <- hier_add(tree, root = "j1a", nodes = c("j1a-x"))
# hier_codes(tree)
