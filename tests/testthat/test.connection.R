
context("Test connection to databricks")

library(dplyr)

con  <- connect_cluster()

test_that("connection to db works", {
  clinvar <- dplyr::tbl(con, dbplyr::in_schema("tobias", "original_table"))
  expect_gt((clinvar %>% count() %>% collect())[1,1, drop=TRUE], 3)
})
