# BUGS #34: f_factors() and ordered factors

d <- data.frame(
  damage = c("low", "high", "medium", "low", "high", "medium"),
  cyl    = c(4, 6, 8, 4, 6, 8),
  trt    = c("a", "b", "a", "b", "a", "b"),
  stringsAsFactors = FALSE
)

test_that("properties = TRUE works when a column is ordered", {
  m <- d
  m$cyl <- factor(m$cyl, ordered = TRUE)
  out <- capture.output(r <- f_factors(m, select = c("cyl", "trt"), properties = TRUE))
  expect_true(any(grepl("ordered", out)))
  expect_true(is.ordered(r$cyl))
})

test_that("ordered = column names orders only those columns", {
  r <- f_factors(d, select = c("cyl", "trt"), ordered = "cyl")
  expect_true(is.ordered(r$cyl))
  expect_false(is.ordered(r$trt))
  expect_true(is.factor(r$trt))
  r2 <- f_factors(d, select = c("damage", "cyl", "trt"), ordered = c("cyl", "damage"))
  expect_true(is.ordered(r2$cyl) && is.ordered(r2$damage))
  expect_false(is.ordered(r2$trt))
})

test_that("ordered = TRUE orders all converted columns, FALSE none", {
  r <- f_factors(d, select = c("cyl", "trt"), ordered = TRUE)
  expect_true(is.ordered(r$cyl) && is.ordered(r$trt))
  m <- d
  m$cyl <- factor(m$cyl, ordered = TRUE)
  r0 <- f_factors(m, select = "cyl", ordered = FALSE)
  expect_false(is.ordered(r0$cyl))
  # default keeps an existing ordered factor ordered
  expect_true(is.ordered(f_factors(m, select = "cyl")$cyl))
})

test_that("ordered: unknown or unconverted names and bad values stop clearly", {
  expect_error(f_factors(d, select = "cyl", ordered = "cly"), "not found.*cly")
  expect_error(f_factors(d, select = "cyl", ordered = "trt"), "not converted")
  expect_error(f_factors(d, select = "cyl", ordered = c(TRUE, FALSE)), "ordered")
})

test_that("levels sets the level order per column", {
  r <- f_factors(d, select = c("damage", "trt"), ordered = "damage",
                 levels = list(damage = c("low", "medium", "high")))
  expect_equal(levels(r$damage), c("low", "medium", "high"))
  expect_true(r$damage[1] < r$damage[2])
  expect_equal(levels(r$trt), c("a", "b"))
  expect_false(is.ordered(r$trt))
})

test_that("levels: unknown column, unconverted column or uncovered values stop", {
  expect_error(f_factors(d, select = "damage", levels = list(damge = c("low", "medium", "high"))),
               "not found.*damge")
  expect_error(f_factors(d, select = "damage", levels = list(trt = c("a", "b"))),
               "not converted")
  expect_error(f_factors(d, select = "damage", levels = list(damage = c("low", "high"))),
               "medium")
  expect_error(f_factors(d, select = "damage", levels = c("low", "medium", "high")),
               "named list")
})

test_that("levels may contain a level that has no observations", {
  r <- f_factors(d, select = "damage",
                 levels = list(damage = c("none", "low", "medium", "high")))
  expect_equal(nlevels(r$damage), 4)
})

test_that("missing values are not counted as uncovered values", {
  m <- d
  m$damage[2] <- NA
  r <- f_factors(m, select = "damage", levels = list(damage = c("low", "medium", "high")))
  expect_true(is.na(r$damage[2]))
})

test_that("ref on an ordered column gives a clear rfriend error", {
  expect_error(f_factors(d, select = "cyl", ordered = "cyl", ref = c(cyl = "8")),
               "ordered")
  m <- d
  m$cyl <- factor(m$cyl, ordered = TRUE)
  expect_error(f_factors(m, select = "cyl", ref = "8"), "ordered")
  # ref still works on an unordered column next to an ordered one
  r <- f_factors(d, select = c("cyl", "trt"), ordered = "cyl", ref = c(trt = "b"))
  expect_equal(levels(r$trt)[1], "b")
})
