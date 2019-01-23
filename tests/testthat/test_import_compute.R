context("Importing and ")

dim1 <- sdcHier_create(
  tot_lab = "Total",
  node_labs = NULL
)

dim2 <- sdcHier_create(
  tot_lab = "top",
  node_labs = letters[1:4]
)

expect_is(dim1, "sdcHier")
expect_is(dim2, "sdcHier")

expect_identical(dim1$levelName, "Total")
expect_identical(dim2$levelName, "top")

expect_error(
  sdcHier_rename(
    dim1,
    node_labs = "Totalx",
    node_labs_new = "newCode"
  ),
  "some nodes specified in argument 'node_labs' don't exist!"
)

sdcHier_rename(
  dim1,
  node_labs = "Total",
  node_labs_new = "tot"
)

expect_identical(dim1$levelName, "tot")

expect_error(
  sdcHier_rename(
    dim2,
    node_labs = c("a", "C"),
    node_labs_new = c("A", "C")
  ),
  "some nodes specified in argument 'node_labs' don't exist!"
)
sdcHier_rename(
  dim2,
  node_labs = c("a", "c"),
  node_labs_new = c("A", "C")
)

expect_error(
  sdcHier_add(
    dim2,
    refnode = "X",
    node_labs = c("a1", "a2")
  ),
  "The reference node does not exist!"
)


sdcHier_add(dim2, refnode = "A", node_labs = c("a1", "a2"))

df <- sdcHier_convert(dim2, format = "df")
expect_is(df, "data.frame")

exp_names <- c("top", "A", "a1", "a2", "b", "C", "d")
exp_levs <- c("@", "@@", rep("@@@", 2), rep("@@", 3))
expect_identical(df$name, exp_names)
expect_identical(df$level, exp_levs)

dt <- sdcHier_convert(dim2, format = "dt")
expect_is(dt, "data.table")

sdc <- sdcHier_convert(dim2, format = "sdc")
expect_is(sdc, "list")
expect_is(sdc$codes, "list")
expect_identical(sdc$codes$orig, exp_names)
expect_identical(sdc$codes$level, nchar(exp_levs))

info <- sdcHier_info(dim2)
expect_is(info, "list")
expect_equal(
  as.character(sapply(info, function(x) {
    x$name
  })),
  exp_names
)

expect_warning(sdcHier_delete(dim2, node_labs = "X"))

sdcHier_delete(dim2, node_labs = c("a1", "d"))
df <- sdcHier_convert(dim2, format = "df")

exp_names <- c("top", "A", "a2", "b", "C")
exp_levs <- c("@", "@@", "@@@", rep("@@", 2))
expect_identical(df$name, exp_names)
expect_identical(df$level, exp_levs)

info <- sdcHier_info(dim2)

expect_identical(info[["a2"]]$is_bogus, TRUE)

expect_error(sdcHier_nodenames(dim2, "X"))
expect_equal(sdcHier_nodenames(dim2, "A"), c("A", "a2"))
