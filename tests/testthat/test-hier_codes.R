context("Test default-codes")

# check valid input
expect_error(hier_codes(list()))

h <- hier_create(root = "tot", nodes = c("B", "A", "C"))
h <- hier_add(h, root = "A", nodes = paste0("a", 1:10))
h <- hier_add(h, root = "a5", nodes = paste0("a5-", 1:10))

cc <- hier_codes(h)
expect_identical(names(cc), hier_nodenames(h))
expect_equal(as.character(cc[1]), "000000")
expect_equal(as.character(cc[length(cc)]), "030000")

