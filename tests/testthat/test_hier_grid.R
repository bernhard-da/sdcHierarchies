context("hier_grid")

h1 <- hier_create("Total", nodes = LETTERS[1:3])
h1 <- hier_add(h1, root = "A", node = "a1")
h1 <- hier_add(h1, root = "a1", node = "aa1")

h2 <- hier_create("Total", letters[1:5])
h2 <- hier_add(h2, root = "b", node = "b1")
h2 <- hier_add(h2, root = "d", node = "d1")

# check inputs
expect_error(hier_grid())
expect_error(hier_grid(h1, h2, add_dups = 5))
expect_error(hier_grid(h1, h2, add_levs = 5))
expect_error(hier_grid(h1, 5, add_levs = TRUE))


# with all codes, also "bogus" codes
r1 <- hier_grid(h1, h2)
expect_identical(nrow(r1), 48L)
expect_identical(ncol(r1), 4L)
expect_identical(r1[.N, v1], "C")

# only the required codes to build the complete hierarchy (no bogus codes)
r2 <- hier_grid(h1, h2, add_dups = FALSE)
expect_identical(nrow(r2), 24L)
expect_identical(ncol(r2), 4L)
expect_identical(r2[.N, v1], "C")

# also contain columns specifying the hierarchy level
r3 <- hier_grid(h1, h2, add_dups = FALSE, add_levs = TRUE)
expect_identical(nrow(r3), 24L)
expect_identical(ncol(r3), 6L)
expect_identical(r3[.N, v2], "e")
expect_identical(r3[.N, levs_v1], 2L)
expect_identical(r3[.N, levs_v2], 2L)

# check default codes
r4 <- hier_grid(h1, h2, add_dups = FALSE, add_levs = TRUE, add_default_codes = TRUE)
expect_identical(nrow(r4), 24L)
expect_identical(ncol(r4), 8L)
expect_identical(r4[1, v1], "Total")
expect_identical(r4[1, v2], "Total")
expect_identical(r4[1, levs_v1], 1L)
expect_identical(r4[1, levs_v2], 1L)
expect_identical(r4[1, default_v1], "0000")
expect_identical(r4[1, default_v2], "000")
expect_identical(r4[.N, v1], "C")
expect_identical(r4[.N, v2], "e")
expect_identical(r4[.N, levs_v1], 2L)
expect_identical(r4[.N, levs_v2], 2L)
expect_identical(r4[.N, default_v1], "0300")
expect_identical(r4[.N, default_v2], "050")

test_that("hier_grid contributing cells and leaf_ids work correctly", {
  # r5: Full grid with contributing cells
  r5 <- hier_grid(h1, h2, add_contributing_cells = TRUE)

  # Check structure
  expect_true("leaf_id" %in% names(r5))
  expect_true("contributing_leaf_ids" %in% names(r5))

  # Check "Total_Total" cell
  total_cell <- r5[v1 == "Total" & v2 == "Total"]
  expect_true(is.na(total_cell$leaf_id))
  expect_identical(length(total_cell$contributing_leaf_ids[[1]]), 15L)
  expect_identical(sort(total_cell$contributing_leaf_ids[[1]]), 1:15)

  # Check a specific terminal leaf cell (aa1 is a leaf, a is a leaf)
  leaf_cell <- r5[v1 == "aa1" & v2 == "a"]
  expect_false(is.na(leaf_cell$leaf_id))
  expect_identical(leaf_cell$leaf_id, leaf_cell$contributing_leaf_ids[[1]])

  # Check bogus filtering impact on leaf_id
  # In r2 (add_dups = FALSE), A and a1 are gone, aa1 remains.
  r2_ext <- hier_grid(h1, h2, add_dups = FALSE, add_contributing_cells = TRUE)

  # aa1_a should have a valid leaf_id
  expect_identical(r2_ext[v1 == "aa1" & v2 == "a", leaf_id], 3L)

  # Total_Total should still have 15 contributing leaves
  # even if bogus codes are removed from grid
  expect_identical(
    length(r2_ext[v1 == "Total" & v2 == "Total"]$contributing_leaf_ids[[1]]), 15L
  )
})

test_that("hier_create_ids matches grid logic", {
  # Microdata
  micro <- data.table(
    V1 = c("aa1", "B", "C"),
    V2 = c("a", "b1", "c")
  )

  ids <- hier_create_ids(micro, list("V1" = h1, "V2" = h2))
  expect_identical(ids, c(3L, 4L, 8L))

  # Verify these IDs exist in the grid's contributing list for Total
  r_final <- hier_grid(h1, h2, add_contributing_cells = TRUE)
  total_contribs <- r_final[v1 == "Total" & v2 == "Total", contributing_leaf_ids[[1]]]
  expect_true(all(ids %in% total_contribs))
})
