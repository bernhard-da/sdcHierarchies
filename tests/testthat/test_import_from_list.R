context("Importing from list-input")

ll <- list()
ll[[1]] <- c("1.", "2.", "3.")
ll[["1."]] <- paste0("1.", 1:9, ".")
ll[["1.1."]] <- paste0("1.1.", 1:2, ".")
ll[["1.2."]] <- paste0("1.2.", 1:5, ".")
ll[["1.3."]] <- paste0("1.3.", 1:5, ".")
ll[["1.4."]] <- paste0("1.4.", 1:6, ".")

expect_error(
  d <- hier_compute_from_list(
    dim = ll,
    tot_lev = "Total",
    as_df = FALSE
  ),
  "Some elements of argument 'll' are not named."
)

names(ll)[1] <- "Total"

d <- hier_compute_from_list(
  dim = ll,
  tot_lev = "Total",
  as_df = FALSE
)
expect_is(d, "sdc_hierarchy")

expect_identical(d$levelName, "Total")

# test imports
out_json <- hier_convert(d, format = "json")
out_argus <- hier_convert(d, format = "argus")
out_code <- hier_convert(d, format = "code")
out_sdc <- hier_convert(d, format = "sdc")

d_from_json <- hier_import(inp = out_json, from = "json")
d_from_code <- hier_import(inp = out_code, from = "code")
d_from_sdc <- hier_import(inp = out_sdc, from = "sdc")
d_from_argus <- hier_import(inp = out_argus, from = "argus")

expect_equal(d_from_json, d_from_code)
expect_equal(d_from_code, d_from_sdc)
expect_equal(d_from_sdc, d_from_sdc)
