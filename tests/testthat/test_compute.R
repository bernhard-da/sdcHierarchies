context("Compute hierarchies")

## nuts: digits 1-2 (nuts1), digit 3 (nut2), digits 4-5 (nuts3)
## all strings have equal length but total is not encoded in these values
geo_m <- c(
  "01051", "01053", "01054", "01055", "01056", "01057", "01058", "01059", "01060", "01061", "01062",
  "02000",
  "03151", "03152", "03153", "03154", "03155", "03156", "03157", "03158", "03251", "03252", "03254", "03255",
  "03256", "03257", "03351", "03352", "03353", "03354", "03355", "03356", "03357", "03358", "03359", "03360",
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
