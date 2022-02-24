

Sys.getenv("CENSUS_API_KEY")


sep21 <- cpsR::get_basic(
  year = 2021,
  month = 6,
  vars = c("pemlr")
)


