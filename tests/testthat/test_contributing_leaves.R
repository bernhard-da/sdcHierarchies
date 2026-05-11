h <- hier_create(
  root = "total", nodes <- c("A", "B")
)
h <- hier_add(h, "A", "A1")
h <- hier_add(h, "B", c("B1", "B2"))

ii <- hier_info(h)

expect_error(rcpp_contributing_leaves(h, "non-ex"), "invalid leaf detected")
expect_equal(ii$total$contributing_codes, c("A", "B1", "B2"))
expect_equal(ii$A$contributing_codes, "A")
expect_equal(ii$B$contributing_codes, c("B1", "B2"))
