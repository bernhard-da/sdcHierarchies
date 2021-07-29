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
expect_identical(ncol(r1), 2L)
expect_identical(r1[.N, v1], "C")

# only the required codes to build the complete hierarchy (no bogus codes)
r2 <- hier_grid(h1, h2, add_dups = FALSE)
expect_identical(nrow(r2), 24L)
expect_identical(ncol(r2), 2L)
expect_identical(r2[.N, v1], "C")

# also contain columns specifying the hierarchy level
r3 <- hier_grid(h1, h2, add_dups = FALSE, add_levs = TRUE)
expect_identical(nrow(r3), 24L)
expect_identical(ncol(r3), 4L)
expect_identical(r3[.N, v2], "e")
expect_identical(r3[.N, levs_v1], 2L)
expect_identical(r3[.N, levs_v2], 2L)

# check default codes
r4 <- hier_grid(h1, h2, add_dups = FALSE, add_levs = TRUE, add_default_codes = TRUE)
expect_identical(nrow(r4), 24L)
expect_identical(ncol(r4), 6L)
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

