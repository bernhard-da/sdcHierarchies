context("test only dups")

df <- data.frame(
  level = c("@", "@@", "@@@", "@@@@"),
  name = c("01", "012", "0121", "01210")
)
tree <- hier_import(df, from = "df")
expect_true(inherits(tree, "sdc_hierarchy"))

sdc <- hier_convert(tree, as = "sdc")
expect_true(attributes(sdc)$hier_convert == TRUE)
expect_true(attributes(sdc)$hier_format == "sdc")

expect_identical(sdc$bogus$bogus_codes, c("012", "0121", "01210"))
expect_identical(sdc$bogus$bogus_parents, c("01", "012", "0121"))

tt <- hier_create(root = "01", node = "012")
tt <- hier_add(tt, root = "012", node = "0121")
tt <- hier_add(tt, root = "0121", node = "01210")
expect_true(inherits(tt, "sdc_hierarchy"))
