#' Compute a grid given different hierarchies
#'
#' This function returns a `data.table` containing all possible combinations of
#' codes from one or more hierarchy objects. This is useful to compute *"complete"*
#' tables for SDC purposes.
#'
#' @param ... one or more hierarchy objects created with [hier_create()] or
#' [hier_compute()]
#' @param add_dups scalar logical defining if bogus codes (parents with only one
#' child and no siblings) should be included. If `FALSE`, these parents are
#' removed and replaced by their most granular leaf.
#' @param add_levs scalar logical defining if numerical levels for each code should
#' be appended to the output.
#' @param add_default_codes scalar logical defining if standardized level codes
#' should be additionally returned.
#' @param add_contributing_cells logical: if `TRUE`, two columns are added:
#' `leaf_id` (a unique integer for terminal leaf combinations) and
#' `contributing_leaf_ids` (a list-column of integers representing all
#' terminal leaves contributing to that cell).
#'
#' @return a `data.table` featuring:
#' * `v{n}`: columns for each hierarchy object.
#' * `cell_id`: a unique string identifier created by concatenating default codes.
#' * `levs_v{n}`: (optional) numerical hierarchy levels.
#' * `default_v{n}`: (optional) standardized level codes.
#' * `leaf_id`: (optional) unique integer for base/terminal cells.
#' * `contributing_leaf_ids`: (optional) integer vectors for aggregation.
#'
#' @md
#' @export
#' @examples
#' # Define hierarchies with some "bogus" codes
#' h1 <- hier_create("Total", nodes = LETTERS[1:3])
#' h1 <- hier_add(h1, root = "A", node = "a1")
#' h1 <- hier_add(h1, root = "a1", node = "aa1")
#'
#' h2 <- hier_create("Total", letters[1:5])
#' h2 <- hier_add(h2, root = "b", node = "b1")
#' h2 <- hier_add(h2, root = "d", node = "d1")
#'
#' # Standard grid with all codes
#' hier_grid(h1, h2)
#'
#' # Grid without bogus codes
#' # h1: `A` and `a1` are replaced by `aa1`
#' # h2: `b` and `d` are replaced by `b1` and `d1`
#' # This ensures the grid is consistent with the most granular data.
#' hier_grid(h1, h2, add_dups = FALSE)
#'
#' # Advanced grid with contributing indices
#' # The 'cell_id' will be a concatenated string like '0000000'
#' # The 'contributing_leaf_ids' column allows for high-speed aggregation
#' # and/or matching with micro-data (for an example see `?hier_create_ids()`)
#' hier_grid(h1, h2,
#'   add_dups = FALSE,
#'   add_contributing_cells = TRUE
#' )
hier_grid <- function(..., add_dups = TRUE, add_levs = FALSE,
                      add_default_codes = FALSE, add_contributing_cells = FALSE) {

  name <- NULL
  args <- list(...)

  # validate inputs
  if (length(args) == 0) stop("No arguments were provided.", call. = FALSE)

  logicals <- list(
    add_dups = add_dups,
    add_levs = add_levs,
    add_default_codes = add_default_codes,
    add_contributing_cells = add_contributing_cells
  )
  for (n in names(logicals)) {
    val <- logicals[[n]]
    if (!is.logical(val) || length(val) != 1 || is.na(val)) {
      stop(sprintf("Argument `%s` must be a scalar logical.", n), call. = FALSE)
    }
  }

  # extract data and prepare req. objects
  leaf_maps <- list()
  code_maps <- list()
  dim_sizes <- integer(length(args))
  terminal_sets <- list()

  out <- lapply(seq_along(args), function(i) {
    x <- args[[i]]
    if (!inherits(x, "sdc_hierarchy")) {
      stop(sprintf("Arg %d is not a hierarchy.", i), call. = FALSE)
    }
    idx <- rcpp_get_sort_order(x)
    code_maps[[i]] <<- hier_codes(x)
    raw_leaves <- rcpp_get_leaves_list(x)

    terminals <- sort(unique(unlist(raw_leaves)), method = "radix")
    terminal_sets[[i]] <<- terminals
    dim_sizes[i] <<- length(terminals)
    leaf_maps[[i]] <<- lapply(raw_leaves, function(l) match(l, terminals))

    dt <- data.table(
      name = x$leaf[idx],
      level = as.integer(x$level[idx])
    )

    if (!add_dups) {
      b_info <- rcpp_bogus_codes(x)
      bogus_parents <- unique(as.character(b_info$bogus_parent))
      dt <- dt[!(name %in% bogus_parents)]
    }
    dt
  })

  # create grid
  grid_list <- lapply(out, function(x) x$name)
  codes <- as.data.table(expand.grid(grid_list, stringsAsFactors = FALSE))
  setnames(codes, paste0("v", seq_along(args)))

  # pre-calculate cell_id (~strID for sdcTable)
  def_cols_list <- lapply(seq_along(args), function(i) {
    code_maps[[i]][codes[[paste0("v", i)]]]
  })

  data.table::set(
    x = codes,
    j = "cell_id",
    value = do.call(paste, c(def_cols_list, sep = ""))
  )

  # add levels
  if (add_levs) {
    for (i in seq_along(out)) {
      # Use match to ensure levels correlate with potentially filtered names
      lev_vec <- out[[i]]$level[match(codes[[paste0("v", i)]], out[[i]]$name)]
      data.table::set(
        x = codes,
        j = paste0("levs_v", i),
        value = lev_vec
      )
    }
  }

  # add default_codes
  if (add_default_codes) {
    for (i in seq_along(args)) {
      data.table::set(
        x = codes,
        j = paste0("default_v", i),
        value = def_cols_list[[i]]
      )
    }
  }

  # assign leaf_id
  multipliers <- c(1, cumprod(dim_sizes[-length(dim_sizes)]))
  local_indices <- matrix(0L, nrow = nrow(codes), ncol = length(args))
  is_terminal_row <- matrix(FALSE, nrow = nrow(codes), ncol = length(args))

  for (i in seq_along(args)) {
    v_name <- codes[[paste0("v", i)]]
    local_indices[, i] <- match(v_name, terminal_sets[[i]])
    is_terminal_row[, i] <- !is.na(local_indices[, i])
  }

  all_terminal <- rowSums(is_terminal_row) == length(args)
  leaf_ids <- rep(NA_integer_, nrow(codes))
  if (any(all_terminal)) {
    leaf_ids[all_terminal] <- as.integer((local_indices[all_terminal, , drop=FALSE] - 1) %*% multipliers + 1)
  }
  data.table::set(
    x = codes,
    j = "leaf_id",
    value = leaf_ids
  )

  # add contributing leaf ids
  if (add_contributing_cells) {
    contrib_ids <- lapply(seq_len(nrow(codes)), function(row_idx) {
      dim_indices <- lapply(seq_along(args), function(dim_idx) {
        node_name <- codes[[paste0("v", dim_idx)]][row_idx]
        leaf_maps[[dim_idx]][[node_name]]
      })
      idx_grid <- as.matrix(expand.grid(dim_indices))
      return(as.integer((idx_grid - 1) %*% multipliers + 1))
    })
    data.table::set(
      x = codes,
      j = "contributing_leaf_ids",
      value = contrib_ids
    )
  }
  return(codes)
}
