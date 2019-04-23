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
expect_identical(r1[.N, v1], "aa1")

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
