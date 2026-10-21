# BUGS #27: the rank tests accept an ordinal response stored as an ordered
# factor (tested on the rank codes of its levels, with a message), and stop
# with a clear message for other non-numeric responses.

rank_data <- function() {
  set.seed(2)
  d <- data.frame(treatment = factor(rep(c("A", "B", "C"), each = 8)),
                  id        = factor(rep(1:8, 3)),
                  damage    = c(sample(1:3, 8, TRUE), sample(2:4, 8, TRUE),
                                sample(3:5, 8, TRUE)))
  d$damage_o <- factor(d$damage, ordered = TRUE)
  d$damage_f <- factor(d$damage)
  d$damage_c <- as.character(d$damage)
  d
}

test_that("f_kruskal_test: ordered factor is tested on its rank codes", {
  d <- rank_data()
  expect_message(
    k_o <- suppressWarnings(f_kruskal_test(damage_o ~ treatment, data = d,
                                           diagnostic_plots = FALSE, effect_plots = FALSE)),
    "ordered factor.*rank codes")
  k_n <- suppressMessages(suppressWarnings(
    f_kruskal_test(damage ~ treatment, data = d, diagnostic_plots = FALSE, effect_plots = FALSE)))
  expect_equal(unname(k_o$damage_o_treatment$kruskal.test$statistic),
               unname(k_n$damage_treatment$kruskal.test$statistic))
  expect_equal(k_o$damage_o_treatment$kruskal.test$p.value,
               k_n$damage_treatment$kruskal.test$p.value)
})

test_that("f_friedman: ordered factor is tested on its rank codes", {
  d <- rank_data()
  expect_message(
    f_o <- suppressWarnings(f_friedman(damage_o ~ treatment | id, data = d,
                                       diagnostic_plots = FALSE, effect_plots = FALSE)),
    "ordered factor.*rank codes")
  f_n <- suppressMessages(suppressWarnings(
    f_friedman(damage ~ treatment | id, data = d, diagnostic_plots = FALSE, effect_plots = FALSE)))
  get_test <- function(res) {
    obj <- res[[1]]
    tst <- Filter(function(z) inherits(z, "htest"), obj)
    tst[[1]]
  }
  expect_equal(unname(get_test(f_o)$statistic), unname(get_test(f_n)$statistic))
  expect_equal(get_test(f_o)$p.value, get_test(f_n)$p.value)
})

test_that("f_wilcox_test: ordered factor is tested on its rank codes", {
  d <- droplevels(subset(rank_data(), treatment != "C"))
  expect_message(
    w_o <- suppressWarnings(f_wilcox_test(damage_o ~ treatment, data = d)),
    "ordered factor.*rank codes")
  w_n <- suppressMessages(suppressWarnings(f_wilcox_test(damage ~ treatment, data = d)))
  expect_equal(w_o$damage_o$wilcox_test$p.value, w_n$damage$wilcox_test$p.value)
})

test_that("unordered factor or character response stops with a clear message", {
  d <- rank_data()
  d2 <- droplevels(subset(d, treatment != "C"))
  expect_error(f_kruskal_test(damage_c ~ treatment, data = d),
               "must be numeric or an ordered factor")
  expect_error(f_friedman(damage_f ~ treatment | id, data = d),
               "must be numeric or an ordered factor")
  expect_error(f_wilcox_test(damage_f ~ treatment, data = d2),
               "f_chisq_test()", fixed = TRUE)
})

test_that("the internal quantile() error no longer appears", {
  d <- rank_data()
  res <- tryCatch(suppressMessages(suppressWarnings(
    f_kruskal_test(damage_o ~ treatment, data = d, diagnostic_plots = FALSE,
                   effect_plots = FALSE))), error = function(e) conditionMessage(e))
  expect_false(is.character(res) && grepl("'type' must be 1 or 3", res, fixed = TRUE))
})
