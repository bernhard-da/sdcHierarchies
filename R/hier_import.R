#' Imports a nested data structure
#'
#' This function creates a nested sdc hierarchy from various
#' input structures.
#'
#' @param inp an import from which should be converted. This could either
#' be a character-string (json) or a \code{data.frame}.
#' @param from (character) from which format should be imported.
#' Possible choices are:
#' \itemize{
#' \item \strong{"json"}: json-encoded string should be converted
#' \item \strong{"df"}: a \code{data.frame} in \code{@;level}-format will
#' be converted
#' \item \strong{"dt"}: a \code{data.table} in \code{@;level}-format will
#' be converted
#' \item \strong{"argus"}: an object exported using \code{\link{hier_convert}}
#' using \code{as = "argus"}
#' \item \strong{"code"}: an object exported using \code{\link{hier_convert}}
#' using \code{as = "code"}
#' \item \strong{"hrc"}: text-files in tau-argus hrc-format
#' \item \strong{"sdc"}: an object exported using \code{\link{hier_convert}}
#' using \code{as = "sdc"}
#' }
#' @param root optional name of overall total
#' @return a (nested) hierarchy
#' @export
#' @examples
#' h <- hier_create(root = "Total", nodes = LETTERS[1:2])
#' h <- hier_add(h, root = "A", nodes = c("a1", "a2"))
#' h <- hier_add(h, root = "B", nodes = c("b1", "b2"))
#' h <- hier_add(h, root = "b1", nodes = "b1a")
#' hier_display(h)
#'
#' df <- hier_convert(h, as = "df")
#' hier_display(df)
#'
#' h2 <- hier_import(df, from = "df")
#' hier_display(h2)
hier_import <- function(inp, from="json", root=NULL) {
  .from_json <- function(json, root=NULL) {
    .lab_from_attr <- function(json, root) {
      if (!is.null(root)) {
        return(root)
      }
      root <- attributes(json)$totlev
      if (is.null(root) || root == "") {
        root <- "rootnode"
      }
      root
    }
    tab <- fromJSON(json)
    if (length(tab) == 0) {
      return(hier_create(root = .lab_from_attr(json, root)))
    }
    tab <- tab[, c(2, 1)]
    colnames(tab) <- c("from", "to")
    if (!is.null(root)) {
      tab$from[tab$from == "#"] <- root
    } else {
      tab$from[tab$from == "#"] <- .lab_from_attr(json, root)
    }

    tree <- hier_create(root = .lab_from_attr(json, root))
    new <- data.table(
      root = tab$from,
      leaf = tab$to
    )
    tree <- .add_nodes(tree = tree, new = new)
    tree <- .sort(tree)
    tree <- .add_class(tree)
    tree
  }
  .from_dt <- function(dt, root=NULL) {
    index <- level <- NULL
    stopifnot(is.data.table(dt))
    stopifnot(ncol(dt) == 2)
    setnames(dt, c("levels", "labs"))
    rr <- unique(unlist(strsplit(dt$levels, "")))
    stopifnot(length(rr) == 1, rr == "@")
    stopifnot(dt$levels[1] == "@")
    stopifnot(sum(dt$levels[1] == "@") == 1)
    dt$labs <- as.character(dt$labs)

    tree <- hier_create(root = dt$labs[1])
    if (nrow(dt) == 1) {
      tree <- .add_class(tree)
      return(tree)
    }
    dt$level <- nchar(dt$levels)
    dt$index <- 1:nrow(dt)

    dt$todo <- TRUE
    dt$todo[1] <- FALSE

    while (sum(dt$todo) > 0) {
      row <- which(dt$todo)[1]
      code <- dt$labs[row]
      tmp <- dt[1:(row - 1)]
      index_parent <- tmp[level == dt$level[row] - 1, max(index)]
      parent <- dt$labs[index_parent]
      tree <- .add_nodes(
        tree = tree,
        new = data.table(root = parent, leaf = code)
      )
      dt$todo[row] <- FALSE
    }
    tree <- .sort(tree)
    tree <- .add_class(tree)
    tree
  }
  .from_argus <- function(df, root=NULL) {
    stopifnot(is.data.frame(df))
    stopifnot(attributes(df)$hier_format == "argus")
    return(.from_dt(data.table(df), root = root))
  }
  .from_code <- function(code, root=NULL) {
    tree <- NULL
    stopifnot(is.character(code))
    stopifnot(attributes(code)$hier_convert == TRUE)
    stopifnot(attributes(code)$hier_format == "code")
    code <- paste(code[-c(1, length(code))], collapse = ";")
    eval(parse(text = code))
    tree <- .sort(tree)
    tree <- .add_class(tree)
    return(tree)
  }
  .from_hrc <- function(hrc, root=NULL) {
    stopifnot(file.exists(hrc))
    dt <- data.table(inp = readLines(hrc))
    dt$inp <- paste0("@", dt$inp)

    # compute levels and names
    rr <- strsplit(dt$inp, "@")
    dt$level <- sapply(rr, function(x) {
      paste(rep("@", times = length(x)), collapse = "")
    })
    dt$names <- sapply(rr, function(x) {
      trimws(x = tail(x, 1), which = "both")
    })

    dt$inp <- NULL

    if (is.null(root)) {
      root <- "Total"
    }
    dt <- rbind(
      data.table(
        level = "@",
        names = root
      ),
      dt
    )
    return(.from_dt(dt, root = NULL))
  }
  .from_sdc <- function(inp) {
    stopifnot(is.list(inp))
    stopifnot(attributes(inp)$hier_format == "sdc")

    dt <- data.table(
      levels = inp$codes$level,
      labs = inp$codes$orig
    )
    dt$levels <- sapply(1:nrow(dt), function(x) {
      paste(rep("@", dt$levels[x]), collapse = "")
    })

    tree <- .from_dt(dt)

    bogus <- inp$bogus
    if (!is.null(bogus$bogus_codes)) {
      tree <- .add_nodes(
        tree = tree,
        new = data.table(
          root = bogus$bogus_parents,
          leaf = bogus$bogus_codes
        )
      )
    }
    tree <- .sort(tree)
    tree <- .add_class(tree)
    return(tree)
  }

  stopifnot(is_scalar_character(from))
  stopifnot(from %in% c("json", "df", "dt", "argus", "hrc", "code", "sdc"))
  if (!is.null(root)) {
    stopifnot(is_scalar_character(root))
  }

  if (from == "json") {
    return(.from_json(json = inp, root = root))
  }
  if (from %in% c("df", "dt")) {
    if (from == "df") {
      inp <- as.data.table(inp)
    }
    return(.from_dt(dt = inp))
  }
  if (from == "argus") {
    return(.from_argus(df = inp))
  }
  if (from == "code") {
    return(.from_code(code = inp))
  }
  if (from == "hrc") {
    return(.from_hrc(hrc = inp, root = root))
  }
  if (from == "sdc") {
    return(.from_sdc(inp = inp))
  }
}
