
## ------------------------------------------------------------------------------------------------# create temporary directory in file system
test_dir <- tempdir()
dir.create(test_dir, showWarnings = FALSE)
setwd(test_dir)

## ------------------------------------------------------------------------------------------------
context("Make a simple set of spokes")
spk <- calc_spokes(5)
test_that("calc_spokes() gives correctly formatted data frame", {
  expect_true(nrow(spk) == 5)
  expect_true(ncol(spk) == 4)
  expect_equal(names(spk)[1], "x")
  expect_equal(names(spk)[2], "y")
  expect_equal(names(spk)[3], "xend")
  expect_equal(names(spk)[4], "yend")
})
test_that("erroneous values of argument 'num' cause an error", {
  expect_error(calc_spokes())
  expect_error(calc_spokes(NA))
  expect_error(calc_spokes(NULL))
  expect_error(calc_spokes(-1))
  expect_error(calc_spokes(c(1, 2)))
  expect_error(calc_spokes("a"))
  expect_error(calc_spokes(matrix(1, nrow=1)))
})

## ------------------------------------------------------------------------------------------------
context("Make a simple web (data object)")
df <- data.frame(nm = c("A", "B", "C",
                        "A", "B", "C",
                        "A", "B", "C",
                        "A", "B", "C",
                        "A", "B", "C"),
                 spk = c("P1", "P1", "P1",
                         "P2", "P2", "P2",
                         "P3", "P3", "P3",
                         "P4", "P4", "P4",
                         "P5", "P5", "P5"),
                 value = c(.1, .2, .3,
                           .4, .5, .6,
                           .7, .8, .9,
                           .10, .11, .12,
                           .13, .14, .15))
ds <- tidyr::spread(df, key = "spk", value = "value")
web <- calc_web(ds)
test_that("calc_web() gives correctly formatted data frame", {
  expect_true(nrow(web) == nrow(ds) * ncol(ds))
  expect_true(ncol(web) == 3)
  expect_true(names(web)[1] == "group")
  expect_true(names(web)[2] == "x")
  expect_true(names(web)[3] == "y")
})
test_that("erroneous values of argument 'mydf' cause an error", {
  expect_error(calc_web())
  expect_error(calc_web(NA))
  expect_error(calc_web(NULL))
  expect_error(calc_web(1))
  expect_error(calc_web(c(1, 2)))
  expect_error(calc_web("a"))
  expect_error(calc_web(matrix(1, nrow=1)))
  expect_error(calc_web(data.frame()))
  expect_error(calc_web(data.frame(nm = "a")))
  expect_error(calc_web(data.frame(nm = c("A", "B"))))
})


## ------------------------------------------------------------------------------------------------
context("Plot the spider web")
err_df1 <- data.frame(nm = c("A", "B", "C",
                             "A", "B", "C",
                             "A", "B", "C",
                             "A", "B", "C",
                             "A", "B", "C"),
                      spk = c("P1", "P1", "P1",
                              "P2", "P2", "P2",
                              "P3", "P3", "P3",
                              "P4", "P4", "P4",
                              "P5", "P5", "P5"))
err_df2 <- data.frame()

test_that("erroneous values of arguments cause an error", {
  expect_error(spider_web(err_df1))
  expect_error(spider_web(err_df2))
  expect_error(spider_web(df, ref_lines_val = c(0, 0.1, 0.3, 0.4)))
  expect_error(spider_web(df, ref_lines_color = c("blue", "blue", "blue", "blue")))
  expect_error(spider_web(df, ref_lines_type = c(1, 1)))
  expect_error(spider_web(df, ref_label_color = c("red", "red")))
  expect_error(spider_web(df, ref_lines_label_spoke = c(4,3,1,2)))
  expect_error(spider_web(df, ref_lines_label_spoke = c(10,3,1)))
})
