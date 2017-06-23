# test_myfunction.R
# Author: Paul Taconet <paul.taconet@ird.fr>
#
# Description: Unit tests for myfunction.R
#=======================
require(rtunaatlas, quietly = TRUE)
require(testthat)


context("myfunction")

test_that("the function correctly works",{
  
  out <- myfunction(1,2,3)
  expect_equal(out, 6L)
  
})