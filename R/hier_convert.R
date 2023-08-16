#' Converts hierarchies into different formats
#'
#' This functions allows to convert nested hierarchies into
#' other data structures.
#'
#' @inherit hier_add
#' @param as (character) specifying the export format. Possible choices are:
#' - `"df"`: a `data.frame` with two columns. The first
#' columns contains a string containing as many `@` as the level of the
#' node in the string (e.g `@` corresponds to the overall
#' total while `@@` would be all codes contributing to the total.
#' The second column contains the names of the levels.
#' - `"dt"`: like the `df`-version but this result is
#' converted to a `data.table`
#' - `"argus"`: used to create hrc-files suitable for tau-argus
#' - `"json"`: json format suitable e.g. as input for
#' the shinyTree package.
#' - `"code"`: code required to generate the hierarchy
#' - `"sdc"`: a `list` which is a suitable input for `sdcTable`
#' @md
#' @export
#' @examples
#' h <- hier_create(root = "Total", nodes = LETTERS[1:2])
#' h <- hier_add(h, root = "A", nodes = c("a1", "a2"))
#' h <- hier_add(h, root = "B", nodes = c("b1", "b2"))
#' h <- hier_add(h, root = "b1", nodes = "b1a")
#' hier_display(h)
#'
#' # required code to build the hierarchy
#' hier_convert(h, as = "code")
#'
#' # data.frame
#' hier_convert(h, as = "df")
hier_convert <- function(tree, as="df") {
  # returns a quoted vector of input codes
  .qvec <- function(codes) {
    q <- shQuote(codes)
    if (length(codes) == 1) {
      return(q)
    }
    x <- paste(q, collapse = ", ")
    paste0("c(", x, ")")
  }

  # to data.frame
  .to_df <- function(tree, dt = TRUE) {
    leaf <- root <- NULL
    stopifnot(inherits(tree, "sdc_hierarchy"))

    convert_to_dt <- dt

    curnode <- .rootnode(tree)
    dt <- data.table(level = 1, name = curnode)
    pool <- setdiff(tree$leaf, curnode)
    finished <- length(pool) == 0
    while (!finished) {
      idx <- which(dt$name == curnode)
      tmpdt <- tree[root == curnode & leaf != curnode]
      if (nrow(tmpdt) > 0) {
        cc <- tmpdt$leaf
      } else {
        cc <- curnode
      }
      add_after <- idx < nrow(dt)
      if (add_after) {
        dt_after <- dt[(idx + 1):nrow(dt)]
      }
      tmp <- data.table(
        level = tmpdt$level[1],
        name = cc
      )
      dt <- rbind(dt[1:idx], tmp)
      if (add_after) {
        dt <- rbind(dt, dt_after)
      }

      pool <- setdiff(pool, cc)
      if (length(pool) == 0) {
        finished <- TRUE
      } else {
        curnode <- tree[leaf == pool[1], root]
      }
    }
    dt$level <- sapply(1:nrow(dt), function(x) paste(rep("@", dt$level[x]), collapse = ""))
    if (isFALSE(convert_to_dt)) {
      dt <- as.data.frame(dt)
    }
    dt
  }

  # node to json
  .to_json <- function(tree) {
    .write_js_row <- function(id,
                              parent,
                              text,
                              opened = TRUE,
                              disabled = FALSE,
                              selected = FALSE) {
      stopifnot(is_scalar_character(id))
      stopifnot(is_scalar_character(parent))
      stopifnot(is_scalar_character(text))
      op <- ifelse(opened == TRUE, "true", "false")
      dis <- ifelse(disabled == TRUE, "true", "false")
      sel <- ifelse(selected == TRUE, "true", "false")
      js <- paste0(
        "{",
        dQuote("id"), ":", dQuote(id), ",",
        dQuote("parent"), ":", dQuote(parent), ",",
        dQuote("text"), ":", dQuote(text), ",",
        dQuote("state"), ":{",
        dQuote("opened"), ":", op, ",",
        dQuote("disabled"), ":", dis, ",",
        dQuote("selected"), ":", sel, "}}"
      )
      js
    }


    # new
    rootnode <- .rootnode(tree)
    if (nrow(tree) == 1) {
      js <- paste0("[]")
      attr(js, "totlev") <- rootnode
      return(js)
    }
    tree <- as.data.frame(tree)
    tree <- tree[-1, ]

    ind <- tree$root == rootnode
    tree$root[ind] <- "#"

    js <- "["
    while (nrow(tree) > 0) {
      parent <- tree$root[1]
      ind <- which(tree$root == parent)
      codes <- tree$leaf[ind]

      for (code in codes) {
        js <- paste0(
          js,
          .write_js_row(
            id = code,
            parent = parent,
            text = code
          ),
          ","
        )
      }
      tree <- tree[-c(ind), ]
    }
    js <- paste0(js, "]")
    js <- sub(",\\]", "\\]", js)
    attr(js, "totlev") <- rootnode
    return(js)
  }

  # node to code
  .to_code <- function(tree) {
    all_names <- .all_nodes(tree)
    code <- "library(sdcHierarchies)"

    root <- .rootnode(tree)
    t <- shQuote(root)
    code_tot <- paste0("tree <- hier_create(root = ", t)

    childs <- .children(tree, root)
    if (length(childs) > 0) {
      code_tot <- paste0(code_tot, ", nodes = ", .qvec(childs), ")")
      all_names <- setdiff(all_names, c(root, childs))
    } else {
      code_tot <- paste0(code_tot, ")")
      all_names <- setdiff(all_names, root)
    }
    code <- c(code, code_tot)

    if (length(all_names) > 0) {
      while (length(all_names) > 0) {
        lev <- all_names[1]
        info <- hier_info(tree = tree, nodes = lev)
        nn <- setdiff(c(lev, info$siblings), NA)
        all_names <- setdiff(all_names, nn)

        s1 <- .qvec(info$parent)
        s2 <- .qvec(nn)
        s3 <- paste0(
          "tree <- hier_add(tree = tree, root = ",
          s1, ", nodes = ", s2, ")"
        )
        code <- c(code, s3)
      }
    }
    code <- c(code, "print(tree)")
    return(code)
  }

  # node to argus
  .to_argus <- function(tree) {
    dforig <- df <- hier_convert(tree, as = "df")
    df <- df[-1, ]
    df$level <- substr(df$level, 3, nchar(df$level))
    sout <-  df$name
    ind_levs <-  df$level != ""

    if (sum(ind_levs) > 0) {
      m1 <- max(nchar(df$level[ind_levs]))
      slev <- sprintf(paste0("%-", m1, "s"), df$level[ind_levs])

      m2 <- max(nchar(df$name[ind_levs]))
      sname <- sprintf(paste0("%", m2, "s"), df$name[ind_levs])

      sout[ind_levs] <- paste(slev, sname)
    }

    attr(dforig, "sout") <- sout
    return(dforig)
  }

  # to list-format suitable for sdcTable(2)
  .to_sdc <- function(tree) {
    all_info <- hier_info(tree, nodes = NULL)

    ## compute and remove bogus-codes
    bogus_codes <- .bogus_codes(tree)

    if (length(bogus_codes$bogus) > 0) {
      bogus <- list(
        bogus_codes = bogus_codes$bogus,
        bogus_parents = as.character(sapply(bogus_codes$bogus, function(x) all_info[[x]]$parent))
      )

      # remove these codes from the hierarchy
      for (i in length(bogus$bogus_codes):1) {
        b_up <- bogus$bogus_parents[i]
        b_code <- bogus$bogus_codes[i]
        ind <- !(tree$root == b_up & tree$leaf == b_code)
        tree <- tree[ind]
      }
      # compute information about nodes again after dups have been removed
      all_info <- hier_info(tree, nodes = NULL)
    } else {
      bogus <- list(
        bogus_codes = NULL,
        bogus_parents = NULL
      )
    }

    # required digits for each level
    req_digits <- .required_digits(tree)

    # compute codes_default
    codes_default <- hier_codes(tree = tree)

    ## which nodes are minimal (eg. no subtotals)
    ## these are those that are leaves in the tree
    codes_minimal <- .is_minimal_code(tree = tree)
    codes_minimal <- codes_minimal[match(names(codes_default), names(codes_minimal))]


    # levels
    levels <- .levels(tree)
    levels <- levels[match(names(codes_default), names(levels))]

    ## in sdcHierarchies, we do not add artificial categories
    ## only those specified will/can be used;
    ## this is a difference to sdcTable (old version)

    ## compute all dimensions (additivity!)
    sub_totals <- .subtotals(tree)
    dims <- list()

    # only the case if we do not have a root-only tree
    if (length(sub_totals) > 0) {
      for (i in 1:length(sub_totals)) {
        ch <- all_info[[sub_totals[i]]]$children
        v_tot <- codes_default[sub_totals[i]]
        v_contr <- codes_default[ch]
        dims <- append(dims, list(c(v_tot, v_contr)))
      }
    }

    out <- list(
      codes = list(
        orig = names(codes_default),
        default = as.character(codes_default),
        minimal = as.logical(codes_minimal),
        level = as.numeric(levels)
      ),
      structure = req_digits,
      dims = dims,
      bogus = bogus
    )
    out
  }

  stopifnot(is_scalar_character(as))
  stopifnot(as %in% c("df", "dt", "json", "argus", "code", "sdc"))
  .is_valid(tree)

  if (as %in% c("df", "dt")) {
    res <- .to_df(
      tree = tree,
      dt = ifelse(as == "dt", TRUE, FALSE)
    )
  } else {
    #if (!.is_sorted(tree)) {
    #  tree <- .sort(tree)
    #}
    if (as == "json") {
      res <- .to_json(tree)
    }
    if (as == "code") {
      res <- .to_code(tree)
    }
    if (as == "argus") {
      res <- .to_argus(tree)
    }
    if (as == "sdc") {
      res <- .to_sdc(tree)
    }
  }
  attr(res, "hier_convert") <- TRUE
  attr(res, "hier_format") <- as
  return(res)
}
