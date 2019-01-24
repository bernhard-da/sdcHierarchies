context("Importing from list-input")

ll <- list()
ll[[1]] <- c("1.", "2.", "3.")
ll[["1."]] <- paste0("1.", 1:9, ".")
ll[["1.1."]] <- paste0("1.1.", 1:2, ".")
ll[["1.2."]] <- paste0("1.2.", 1:5, ".")
ll[["1.3."]] <- paste0("1.3.", 1:5, ".")
ll[["1.4."]] <- paste0("1.4.", 1:6, ".")

expect_error(
  d <- sdcHier_compute_fromList(
    dim = ll,
    tot_lev = "Total",
    as_df = FALSE
  ),
  "Some elements of argument 'll' are not named."
)

names(ll)[1] <- "Total"

d <- sdcHier_compute_fromList(
  dim = ll,
  tot_lev = "Total",
  as_df = FALSE
)
expect_is(d, "sdcHier")

expect_identical(d$levelName, "Total")

# test imports
out_json <- sdcHier_convert(d, format = "json")
out_argus <- sdcHier_convert(d, format = "argus")
out_code <- sdcHier_convert(d, format = "code")
out_sdc <- sdcHier_convert(d, format = "sdc")

d_from_code <- sdcHier_import(
  inp = out_code, from = "code"
)
d_from_sdc <- sdcHier_import(
  inp = out_sdc, from = "sdc"
)
expect_equal(d_from_code, d_from_sdc)
