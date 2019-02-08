context("Importing and modifying hierarchies")

expect_error(
  hier_create(
    tot_lab = "top",
    node_labs = c("top")
  ),
  "at least one leaf-names matches the overall total"
)

dim1 <- hier_create(
  tot_lab = "Total",
  node_labs = NULL
)

dim2 <- hier_create(
  tot_lab = "top",
  node_labs = letters[1:4]
)

expect_is(dim1, "sdc_hierarchy")
expect_is(dim2, "sdc_hierarchy")

expect_identical(dim1$levelName, "Total")
expect_identical(dim2$levelName, "top")

expect_error(
  hier_rename(
    dim1,
    node_labs = "Totalx",
    node_labs_new = "newCode"
  ),
  "some nodes specified in argument 'node_labs' don't exist!"
)

hier_rename(
  dim1,
  node_labs = "Total",
  node_labs_new = "tot"
)

expect_identical(dim1$levelName, "tot")

expect_error(
  hier_rename(
    dim2,
    node_labs = c("a", "C"),
    node_labs_new = c("A", "C")
  ),
  "some nodes specified in argument 'node_labs' don't exist!"
)
hier_rename(
  dim2,
  node_labs = c("a", "c"),
  node_labs_new = c("A", "C")
)

expect_error(
  hier_add(
    dim2,
    refnode = "X",
    node_labs = c("a1", "a2")
  ),
  "The reference node does not exist!"
)

expect_error(
  hier_add(
    dim2,
    refnode = "A",
    node_labs = "A"
  )
)

expect_warning(
  hier_add(
    dim2,
    refnode = "top",
    node_labs = "A"
  )
)

hier_add(
  dim2,
  refnode = "A",
  node_labs = c("a1", "a2")
)

expect_error(
  hier_rename(
    dim2,
    node_labs = "A",
    node_labs_new = "C"
  ),
  "some nodes specified in argument 'node_labs_new' already exist!"
)

df <- hier_convert(dim2, format = "df")
expect_is(df, "data.frame")

exp_names <- c("top", "A", "a1", "a2", "b", "C", "d")
exp_levs <- c("@", "@@", rep("@@@", 2), rep("@@", 3))
expect_identical(df$name, exp_names)
expect_identical(df$level, exp_levs)

dt <- hier_convert(dim2, format = "dt")
expect_is(dt, "data.table")

sdc <- hier_convert(dim2, format = "sdc")
expect_is(sdc, "list")
expect_is(sdc$codes, "list")
expect_identical(sdc$codes$orig, exp_names)
expect_identical(sdc$codes$level, nchar(exp_levs))

info <- hier_info(dim2)
expect_is(info, "list")
expect_equal(
  as.character(sapply(info, function(x) {
    x$name
  })),
  exp_names
)

expect_warning(hier_delete(dim2, node_labs = "X"))

hier_delete(dim2, node_labs = c("a1", "d"))
df <- hier_convert(dim2, format = "df")

exp_names <- c("top", "A", "a2", "b", "C")
exp_levs <- c("@", "@@", "@@@", rep("@@", 2))
expect_identical(df$name, exp_names)
expect_identical(df$level, exp_levs)

info <- hier_info(dim2)

expect_identical(info[["a2"]]$is_bogus, TRUE)

expect_error(hier_nodenames(dim2, "X"))
expect_equal(hier_nodenames(dim2, "A"), c("A", "a2"))

context("Importing from list-input")
ll <- list()
ll[[1]] <- c("1.", "2.", "3.")
ll[["1."]] <- paste0("1.", 1:9, ".")
ll[["1.1."]] <- paste0("1.1.", 1:2, ".")
ll[["1.2."]] <- paste0("1.2.", 1:5, ".")
ll[["1.3."]] <- paste0("1.3.", 1:5, ".")
ll[["1.4."]] <- paste0("1.4.", 1:6, ".")

expect_error(
  d <- hier_compute(
    inp = ll,
    tot_lev = "Total",
    method = "list",
    as_df = FALSE
  ),
  "Some elements of argument 'inp' are not named."
)

names(ll)[1] <- "Total"

d <- hier_compute(
  inp = ll,
  tot_lev = "Total",
  method = "list",
  as_df = FALSE
)
expect_is(d, "sdc_hierarchy")

expect_identical(d$levelName, "Total")

# test imports
out_json <- hier_convert(d, format = "json")
out_argus <- hier_convert(d, format = "argus")
out_code <- hier_convert(d, format = "code")
out_sdc <- hier_convert(d, format = "sdc")

d_from_json1 <- hier_import(inp = out_json, from = "json", tot_lab = "Total")
d_from_json2 <- hier_import(inp = out_json, from = "json")

d_from_code <- hier_import(inp = out_code, from = "code")
d_from_sdc <- hier_import(inp = out_sdc, from = "sdc")
d_from_argus <- hier_import(inp = out_argus, from = "argus")

expect_equal(d_from_json1, d_from_json2)
expect_equal(d_from_json2, d_from_code)
expect_equal(d_from_code, d_from_sdc)
expect_equal(d_from_sdc, d_from_sdc)


out_df <- hier_export(h = d, format = "df", path = tempfile())
d_df <- hier_import(out_df, from = "df")
expect_is(out_df, "data.frame")

out_json <- hier_export(h = d, format = "json", path = tempfile())
d_json <- hier_import(out_json, from = "json")
expect_equal(attr(out_json, "hier_format"), "json")
expect_equal(attr(out_json, "hier_convert"), TRUE)

out_argus <- hier_export(h = d, format = "argus", path = tempfile())
d_argus <- hier_import(out_argus, from = "argus")
expect_equal(attr(out_argus, "hier_format"), "argus")
expect_equal(attr(out_argus, "hier_convert"), TRUE)

out_code <- hier_export(h = d, format = "code", path = tempfile())
d_code <- hier_import(out_code, from = "code")
expect_equal(attr(out_code, "hier_format"), "code")
expect_equal(attr(out_code, "hier_convert"), TRUE)

out_sdc <- hier_export(h = d, format = "sdc", path = tempfile())
d_sdc <- hier_import(out_sdc, from = "sdc")
expect_equal(attr(out_sdc, "hier_format"), "sdc")
expect_equal(attr(out_sdc, "hier_convert"), TRUE)


expect_equal(d_df, d_json)
expect_equal(d_json, d_argus)
expect_equal(d_argus, d_code)
expect_equal(d_code, d_sdc)

