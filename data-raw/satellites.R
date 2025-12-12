## code to prepare `satellites` dataset goes here

satellites <- data.table::fread("data-raw/satellites.csv")

usethis::use_data(
  satellites,
  overwrite = TRUE,
  internal = TRUE
)


