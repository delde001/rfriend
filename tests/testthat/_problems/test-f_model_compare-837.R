# Extracted from test-f_model_compare.R:837

# test -------------------------------------------------------------------------
skip_if_not_installed("lme4")
pt <- plant_trial
pt$time_f <- factor(pt$time_weeks)
ml1 <- lme4::lmer(height_cm ~ treatment + time_f + (1 | block/plant_id),
                    data = pt, REML = FALSE)
ml2 <- lme4::lmer(height_cm ~ treatment * time_f + (1 | block/plant_id),
                    data = pt, REML = FALSE)
ref <- suppressWarnings(f_model_compare(ml1, ml2))
reml1 <- lme4::lmer(height_cm ~ treatment + time_f + (1 | block/plant_id), data = pt)
reml2 <- lme4::lmer(height_cm ~ treatment * time_f + (1 | block/plant_id), data = pt)
expect_message(r1 <- suppressWarnings(f_model_compare(reml1, reml2)),
                 "refitted with ML")
expect_equal(r1$metrics_table$Model1[1:3], ref$metrics_table$Model1[1:3])
l1 <- suppressMessages(f_lmer(height_cm ~ treatment + time_f + (1 | block/plant_id), data = pt))
l2 <- suppressMessages(f_lmer(height_cm ~ treatment * time_f + (1 | block/plant_id), data = pt))
r2 <- suppressWarnings(suppressMessages(f_model_compare(l1, l2)))
expect_equal(r2$metrics_table$Model1[1:3], ref$metrics_table$Model1[1:3])
p_ref <- ref$metrics_table$Difference[ref$metrics_table$Metric == "ANOVA p-value"]
p_r2  <- r2$metrics_table$Difference[r2$metrics_table$Metric == "ANOVA p-value"]
expect_equal(p_r2, p_ref, tolerance = 1e-8)
