# Tests for f_clear(): without a restart it names the packages that are
# still attached, because removing objects does not detach them.

test_that("attached_extra_packages() leaves out start-up packages and rfriend", {
  sp <- c(".GlobalEnv", "package:ggplot2", "package:rfriend", "package:stats",
          "package:graphics", "package:grDevices", "package:utils",
          "package:datasets", "package:methods", "tools:rstudio",
          "Autoloads", "package:base", "package:dplyr")
  expect_identical(rfriend:::attached_extra_packages(sp), c("ggplot2", "dplyr"))
  expect_identical(
    rfriend:::attached_extra_packages(c(".GlobalEnv", "package:stats", "package:base")),
    character(0)
  )
})

test_that("f_clear() names the packages that are still attached", {
  # 'tools' ships with R but is not attached at start-up
  if (!"package:tools" %in% search()) {
    suppressPackageStartupMessages(library(tools))
    on.exit(detach("package:tools"), add = TRUE)
  }
  expect_message(
    utils::capture.output(
      f_clear(env = FALSE, gc = FALSE, console = FALSE, graph = FALSE)
    ),
    "Still attached: .*tools.*f_clear\\(restart = TRUE\\)"
  )
})
