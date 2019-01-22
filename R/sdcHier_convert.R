#' sdcHier_convert
#'
#' convert nested hierarchies into other data structures
#'
#' @inherit sdcHier_add
#' @param format (character) specifying the export format. possible choices are:
#' \itemize{
#' \item \strong{"df"}: a \code{data.frame} with two columns. The first columns contains a string
#' containing as many \code{@} as the level of the node in the string (e.g \code{@} corresponds to the overall
#' total while \code{@@} would be all codes contributing to the total. The second column contains the names
#' of the levels.
#' \item \strong{"dt"}: like the \code{df}-version but this result is converted to a \code{data.table}
#' \item \strong{"argus"}: used to create hrc-files suitable for tau-argus
#' \item \strong{"json"}: json format suitable as input for shiny Tree
#' \item \strong{"code"}: code required to generate the hierarchy
#' \item \strong{"sdc"}: a \code{list} which is a suitable input for \code{sdcTable}
#' }
#' @param verbose (logical) if true, the result of the conversion will not only
#' be (invisibly) returned but also printed in the prompt
#' @export
#' @examples
#' ## for examples, see sdcHier_vignette()
sdcHier_convert <- function(h, format="df", verbose=FALSE) {
  # to data.frame
  h_to_df <- function(h, verbose) {
    res <- sdcHier_info(h)

    if (length(res) == 6 && names(res)[1] == "exists") {
      return(data.frame(level = "@", name = res$name, stringsAsFactors = FALSE))
    }
    df <- do.call("rbind", lapply(res, function(x) {
      data.frame(level = paste(rep("@", x$level), collapse = ""), name = x$name, stringsAsFactors = FALSE)
    }))
    rownames(df) <- NULL
    if (verbose) {
      print(df)
    }
    return(df)
  }

  # node to json
  h_to_json <- function(h, verbose) {
    write_json_row <- function(id, parent, text, opened=TRUE, disabled=FALSE, selected=FALSE) {
      stopifnot(is_scalar_character(id))
      stopifnot(is_scalar_character(parent))
      stopifnot(is_scalar_character(text))
      op <- ifelse(opened == TRUE, "true", "false")
      dis <- ifelse(disabled == TRUE, "true", "false")
      sel <- ifelse(selected == TRUE, "true", "false")
      js <- paste0("{",
        dQuote("id"), ":", dQuote(id), ",",
        dQuote("parent"), ":", dQuote(parent), ",",
        dQuote("text"), ":", dQuote(text), ",",
        dQuote("state"), ":{",
        dQuote("opened"), ":", op, ",",
        dQuote("disabled"), ":", dis, ",",
        dQuote("selected"), ":", sel, "}}")
      js
    }

    df <- ToDataFrameTypeCol(h)

    if (!is.data.frame(df)) {
      js <- paste0("[]")
      attr(js, "totlev") <- df
      return(js)
    }

    totlab <- df[[1]][1]
    df[[1]] <- "#"
    js <- "["
    for (i in 2:ncol(df)) {
      sub <- unique(df[, c(i - 1, i)])
      sub <- sub[!is.na(sub[[2]]), ]
      for (j in 1:nrow(sub)) {
        js <- paste0(js, write_json_row(id = sub[[2]][j], parent = sub[[1]][j], text = sub[[2]][j]), ",")
      }
    }
    js <- paste0(js, "]")
    js <- sub(",\\]", "\\]", js)
    if (verbose) {
      cat(js, sep = "\n")
    }
    attr(js, "totlev") <- as.character(totlab)
    return(js)
  }

  # node to code
  h_to_code <- function(h, verbose) {
    all_names <- sdcHier_nodenames(h)
    code <- "library(sdcHierarchies)"
    code <- c(code, paste0("d <- sdcHier_create(tot_lab = ", shQuote(all_names[1]), ")"))
    all_names <- all_names[-c(1)]

    if (length(all_names) > 0) {
      info <- sdcHier_info(h, node_labs = all_names)
      while (length(all_names) > 0) {
        lev <- all_names[1]
        cur_info <- info[[lev]]
        nn <- c(lev, cur_info$siblings)
        all_names <- setdiff(all_names, nn)

        s1 <- shQuote(cur_info$parent)
        s2 <- paste0("c(", paste0(shQuote(nn), collapse = ", "), ")")
        code <- c(code, paste0("sdcHier_add(d, refnode = ", s1, ", node_labs = ", s2, ")"))
      }
    }
    code <- c(code, "print(d)")
    if (verbose) {
      cat(code, sep = "\n")
    }
    return(code)
  }

  # node to argus
  h_to_argus <- function(h, verbose) {
    dforig <- df <- sdcHier_convert(h, "data.frame")
    df <- df[-1, ]
    df$level <- substr(df$level, 3, nchar(df$level))
    sout <-  df$name
    ind_levs <-  df$level != ""
    m1 <- max(nchar(df$level[ind_levs]))
    slev <- sprintf(paste0("%-", m1, "s"), df$level[ind_levs])

    m2 <- max(nchar(df$name[ind_levs]))
    sname <- sprintf(paste0("%", m2, "s"), df$name[ind_levs])

    sout[ind_levs] <- paste(slev, sname)
    if (verbose) {
      print(dforig)
    }
    attr(dforig, "sout") <- sout
    return(dforig)
  }

  # to list-format suitable for sdcTable(2)
  h_to_sdc <- function(h, verbose) {
    N <- code_default <- codes_minimal <- NULL
    id <- level <- levs <- name <- NULL
    all_info <- sdcHier_info(h, node_labs = NULL)

    ## compute and remove bogus-codes
    bogus_codes <- sapply(all_info, function(x) {
      x$is_bogus
    })

    if (any(bogus_codes)) {
      # remove these codes; they do not contribute to the problem
      bogus_codes <- names(bogus_codes[bogus_codes])
      bogus_parents <- sapply(bogus_codes, function(x) {
        all_info[[x]]$parent
      })
      bogus_parents <- as.character(bogus_parents)
      bogus <- list(
        bogus_codes = bogus_codes,
        bogus_parents = bogus_parents
      )
      # remove these codes from the hierarchy
      for (i in length(bogus$bogus_codes):1) {
        b_up <- bogus$bogus_parents[i]
        b_code <- bogus$bogus_codes[i]
        FindNode(h, name=b_up)$RemoveChild(b_code)
      }
      # compute information about nodes again after dups have been removed
      all_info <- sdcHier_info(h, node_labs = NULL)
    } else {
      bogus <- list(
        bogus_codes = NULL,
        bogus_parents = NULL
      )
    }

    dt <- sdcHier_convert(h, format = "dt")
    dt[, levs := nchar(level)]
    nr_levels <- length(unique(dt[, levs]))

    # compute required number of digits per level
    hdt <- data.table(ToDataFrameTypeCol(h))
    cn <- names(hdt)
    req_digits <- 1

    for (i in 2:ncol(hdt)) {
      tmp <- unique(hdt[, cn[c(i - 1, i)], with = FALSE])
      tmp <- tmp[!is.na(get(cn[i])), ]
      setkeyv(tmp, cn[i - 1])
      tmp <- tmp[, .N, by=key(tmp)]
      new_digits <- nchar(as.character(max(tmp[, N])))
      req_digits <- c(req_digits, new_digits)
    }

    # compute default codes
    nr_levels <- dt[, max(levs)]
    dt[, code_default := paste(rep("0", sum(req_digits)), collapse = "")]
    dt[, id := .I]

    cs <- cumsum(req_digits)
    for (i in 2:nrow(dt)) {
      c_lev <- dt[i, levs]
      dig_c_lev <- req_digits[c_lev]

      first <- cs[c_lev - 1] + 1
      last <- cs[c_lev]

      if (dt[i, levs] >= dt[i - 1, levs]) {
        val <- dt[i - 1, code_default]
      } else {
        # go back as far as necessary
        index <- max(dt[levs == c_lev & id < dt[i, id], id])
        val <- dt[index, code_default]
      }
      old_val <- as.integer(substring(text = val, first = first, last = last))
      new_val <- sprintf(paste("%0", dig_c_lev, "d", sep = ""), old_val + 1)
      substring(val, first = first, last = last) <- new_val
      dt[i, code_default := val]
    }

    ## which nodes are minimal (eg. no subtotals)
    ## these are those that are leaves in the tree
    dt[, codes_minimal := as.logical(sapply(all_info, function(x) x$is_leaf))]

    ## in sdcHierarchies, we do not add artificial categories; only those specified will/can be used
    ## this is a difference to sdcTable (old version)

    ## compute all dimensions (additivity!)
    sub_totals <- dt[codes_minimal == FALSE, name]
    dims <- list()
    for (i in 1:length(sub_totals)) {
      ch <- all_info[[sub_totals[i]]]$children

      v_tot <- dt[name == sub_totals[i], code_default]
      v_contr <- dt[name %in% ch, code_default]
      dims <- append(dims, list(c(v_tot, v_contr)))
    }

    out <- list(
      codes = list(
        orig = dt[, name],
        default = dt[, code_default],
        minimal = dt[, codes_minimal],
        level = dt[, levs]
      ),
      structure = req_digits,
      dims = dims,
      bogus = bogus
    )
    out
  }

  stopifnot(is_scalar_character(format))
  stopifnot(format %in% c("df", "dt", "json", "argus", "code", "sdc"))
  stopifnot(is_scalar_logical(verbose))
  h_is_valid(h)

  if (format == "df") {
    res <- h_to_df(h, verbose = verbose)
  }
  if (format == "dt") {
    res <- data.table(h_to_df(h, verbose = verbose))
  }
  if (format == "json") {
    res <- h_to_json(h, verbose = verbose)
  }
  if (format == "code") {
    res <- h_to_code(h, verbose = verbose)
  }
  if (format == "argus") {
    res <- h_to_argus(h, verbose = verbose)
  }
  if (format == "sdc") {
    res <- h_to_sdc(h, verbose = verbose)
  }
  attr(res, "sdcHier_convert") <- TRUE
  attr(res, "sdcHier_format") <- format
  return(res)
}
