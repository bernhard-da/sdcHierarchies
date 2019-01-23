context("Compute hierarchies")

## nuts: digits 1-2 (nuts1), digit 3 (nut2), digits 4-5 (nuts3)
## all strings have equal length but total is not encoded in these values
geo_m <- c(
  "01051", "01053", "01054", "01055", "01056", "01057", "01058", "01059",
  "01060", "01061", "01062",
  "02000",
  "03151", "03152", "03153", "03154", "03155", "03156", "03157", "03158",
  "03251", "03252", "03254", "03255",
  "03256", "03257", "03351", "03352", "03353", "03354", "03355", "03356",
  "03357", "03358", "03359", "03360",
  "03361", "03451", "03452", "03453", "03454", "03455", "03456",
  "10155")

dim_endpos <- sdcHier_compute(
  dim = geo_m,
  dim_spec = c(2, 3, 5),
  tot_lev = "Tot",
  method = "endpos")

dim_len <- sdcHier_compute(
  dim = geo_m,
  dim_spec = c(2, 1, 2),
  tot_lev = "Tot",
  method = "len")

expect_identical(
  sdcHier_convert(dim_endpos, format = "df"),
  sdcHier_convert(dim_len, format = "df")
)

# Total not contained
yae_h <- c(
  "1.1.1.", "1.1.2.",
  "1.2.1.", "1.2.2.", "1.2.3.", "1.2.4.", "1.2.5.", "1.3.1.",
  "1.3.2.", "1.3.3.", "1.3.4.", "1.3.5.",
  "1.4.1.", "1.4.2.", "1.4.3.", "1.4.4.", "1.4.5.",
  "1.5.", "1.6.", "1.7.", "1.8.", "1.9.", "2.", "3.")

dim_endpos <- sdcHier_compute(
  dim = yae_h,
  dim_spec = c(2, 4, 6),
  tot_lev = "Tot",
  method = "endpos")

dim_len <- sdcHier_compute(
  dim = yae_h,
  dim_spec = c(2, 2, 2),
  tot_lev = "Tot",
  method = "len")

expect_identical(
  sdcHier_convert(dim_endpos, format = "df"),
  sdcHier_convert(dim_len, format = "df")
)
expect_identical(
  sdcHier_convert(dim_endpos, format = "json"),
  sdcHier_convert(dim_len, format = "json")
)
expect_identical(
  sdcHier_convert(dim_endpos, format = "dt"),
  sdcHier_convert(dim_len, format = "dt")
)
expect_identical(
  sdcHier_convert(dim_endpos, format = "argus"),
  sdcHier_convert(dim_len, format = "argus")
)
expect_identical(
  sdcHier_convert(dim_endpos, format = "code"),
  sdcHier_convert(dim_len, format = "code")
)
expect_identical(
  sdcHier_convert(dim_endpos, format = "sdc"),
  sdcHier_convert(dim_len, format = "sdc")
)
