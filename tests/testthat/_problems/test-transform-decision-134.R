# Extracted from test-transform-decision.R:134

# prequel ----------------------------------------------------------------------
quiet_out <- function(expr) {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  invisible(utils::capture.output(res <- suppressWarnings(suppressMessages(expr))))
  res
}
printed <- function(x) {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  paste(utils::capture.output(suppressWarnings(suppressMessages(print(x)))),
        collapse = "\n")
}
wk6_td <- subset(plant_trial, time_weeks == 6)
two_td <- subset(plant_trial, treatment %in% c("control", "high_N"))
rejected_td <- local({  # Box-Cox lowers Shapiro p slightly: 0.00802 -> 0.00801
  set.seed(12)
  y <- c(rnorm(15, 50, 5), rnorm(15, 55, 5))
  y[c(3, 20)] <- y[c(3, 20)] + c(25, -20)
  data.frame(y = y, g = rep(c("a", "b"), each = 15))
})

# test -------------------------------------------------------------------------
out <- quiet_out(f_lm(biomass_g ~ treatment + chloroplast_count, data = plant_trial))
s <- out$biomass_g
expect_true(s$Response_Transformed)
expect_false(is.null(s$transformed_anova))
ref_u <- car::Anova(lm(biomass_g ~ treatment + chloroplast_count, data = plant_trial), type = 2)
expect_equal(s$anova["chloroplast_count", "F value"], ref_u["chloroplast_count", "F value"])
expect_equal(s$transformed_anova["chloroplast_count", "F value"],
               car::Anova(s$transformed_lm_fit, type = 2)["chloroplast_count", "F value"])
expect_equal(s$summary$coefficients, summary(s$lm_fit)$coefficients)
expect_equal(as.matrix(s$transformed_coefficients),
               summary(s$transformed_lm_fit)$coefficients)
txt <- printed(out)
expect_match(txt, "Type II Analysis of Variance on Box-Cox transformed biomass_g (lambda = -0.131)",
               fixed = TRUE)
expect_match(txt, format(round(s$transformed_anova["chloroplast_count", "F value"], 4)),
               fixed = TRUE)
