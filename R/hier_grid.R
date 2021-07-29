#' Compute a grid given different hierarchies
#'
#' This function returns a `data.table` containing all possible combinations of
#' codes from at least one hierarchy object. This is useful to compute a *"complete"*
#' table from several hierarchies.
#'
#' @param ... one or more hierarchy objects created with [hier_create()] or
#' [hier_compute()]
#' @param add_dups scalar logical defining if bogus codes (codes that are the only
#' leaf contributing to a parent that also has no siblings) should be included.
#' @param add_levs scalar logical defining if numerical levels for each codes should
#' be appended to the output `data.table`.
#' @param add_default_codes scalar logical definining if standardized level codes
#' should be additionally returned
#' @return a `data.table` featuring a column for each hierarchy object specified in
#' argument `...`. These columns are labeled `v{n}`. If `add_levs` is `TRUE`,
#' for each hierarchy provided, an additional column labeled `levs_v{n}` is appended
#' to the output. Its values define the hierarchy level of the corresponding code
#' given in `v{n}` in the same row.
#' If `add_default_codes` is `TRUE`, for each hierarchy provided an additional
#' column `default_v{n}` is provided
#' @md
#' @export
#' @examples
#' # define some hierarchies with some "duplicates" or "bogus" codes
#' h1 <- hier_create("Total", nodes = LETTERS[1:3])
#' h1 <- hier_add(h1, root = "A", node = "a1")
#' h1 <- hier_add(h1, root = "a1", node = "aa1")
#'
#' h2 <- hier_create("Total", letters[1:5])
#' h2 <- hier_add(h2, root = "b", node = "b1")
#' h2 <- hier_add(h2, root = "d", node = "d1")
#'
#' # with all codes, also "bogus" codes
#' hier_grid(h1, h2)
#'
#' # only the required codes to build the complete hierarchy (no bogus codes)
#' hier_grid(h1, h2, add_dups = FALSE)
#'
#' # also contain columns specifying the hierarchy level
#' hier_grid(h1, h2, add_dups = FALSE, add_levs = TRUE)
hier_grid <- function(..., add_dups = TRUE, add_levs = FALSE, add_default_codes = FALSE) {
  args <- list(...)
  if (length(args) == 0) {
    stop("No arguments were provided", call. = FALSE)
  }

  if (!is_scalar_logical(add_dups)) {
    stop("Argument `add_dups` needs to be a scalar logical.", call. = FALSE)
  }
  if (!is_scalar_logical(add_levs)) {
    stop("Argument `add_levs` needs to be a scalar logical.", call. = FALSE)
  }
  if (!is_scalar_logical(add_default_codes)) {
    stop("Argument `add_default_codes` needs to be a scalar logical.", call. = FALSE)
  }
  out <- lapply(args, function(x) {
    if (!inherits(x, "sdc_hierarchy")) {
      stop("Invalid input detected.", call. = FALSE)
    }

    info <- hier_info(x)
    dt <- rbindlist(lapply(info, function(x) {
      data.table(
        name = x$name,
        level = x$level,
        bogus = x$is_bogus
      )
    }))
    bogus <- NULL
    if (!add_dups) {
      dt <- dt[bogus == FALSE]
    }
    dt
  })

  codes <- lapply(out, function(x) x$name)
  codes <- as.data.table(expand.grid(codes, stringsAsFactors = FALSE))
  setnames(codes, paste0("v", 1:length(codes)))

  if (add_levs) {
    levs <- lapply(out, function(x) x$level)
    levs <- as.data.table(expand.grid(levs))
    setnames(levs, paste0("levs_v", 1:length(codes)))
    codes <- cbind(codes, levs)
  }

  if (add_default_codes) {
    def_codes <- lapply(seq_len(length(args)), function(i) {
      curtree <- args[[i]]
      hier_codes(tree = curtree)
    })
    for (i in seq_len(length(def_codes))) {
      mm <- match(codes[[paste0("v", i)]], names(def_codes[[i]]))
      codes[[paste0("default_v", i)]] <- def_codes[[i]][mm]
    }
  }
  codes
}
