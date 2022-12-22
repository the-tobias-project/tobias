# This file is part of the standard setup for testthat.  It is recommended that
# you do not modify it.  Where should you do additional test configuration?
# Learn more about the roles of various files in: *
# https://r-pkgs.org/tests.html *
# https://testthat.r-lib.org/reference/test_package.html#special-files

library(testthat)
library(tobias)
library(dplyr)

test_check("tobias")

con  <- connect_cluster()

test_that("connection to db works", {
    clinvar <- dplyr::tbl(con, dbplyr::in_schema("tobias", "original_table"))
    expect_gt((clinvar %>% count() %>% collect())[1,1, drop=TRUE], 3)
})
