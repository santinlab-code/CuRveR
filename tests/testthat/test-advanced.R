piecewise <- function(t) {
  # ln(OD): slope 0.01 until 300 min, 0.002 until 700 min, then -0.005 (lysis).
  z <- log(.05) + .01 * t + (.002 - .01) * pmax(t - 300, 0) + (-.005 - .002) * pmax(t - 700, 0)
  exp(z)
}

test_that("segmented regression recovers known breakpoints, rates and doubling times", {
  t <- seq(0, 1200, 10); y <- piecewise(t) * exp(stats::rnorm(length(t), 0, .002))
  f <- CuRveR:::web_fit_segmented(t, y, 2L, scale = "log")
  expect_equal(f$breaks, c(300, 700), tolerance = .02)
  expect_equal(f$slopes, c(.01, .002, -.005), tolerance = .03)
  ph <- CuRveR:::web_segment_phases(f)
  expect_equal(ph$doubling_time[1:2], log(2) / c(.01, .002), tolerance = .03)
  expect_true(is.na(ph$doubling_time[3]))
  expect_equal(ph$halving_time[3], log(2) / .005, tolerance = .03)
  expect_equal(CuRveR:::web_predict_segmented(f, t), piecewise(t), tolerance = .01)
  p <- CuRveR:::web_segment_parameters(f)
  expect_setequal(p$parameter, c("phase1_slope", "phase2_slope", "phase3_slope", "phase2_start", "phase3_start",
    "phase1_doubling_time", "phase2_doubling_time", "phase3_halving_time"))
})

test_that("zero breakpoints equal ordinary regression and non-positive values are excluded on log scale", {
  t <- 1:20; y <- 2 + .5 * t + sin(t) / 50
  f <- CuRveR:::web_fit_segmented(t, y, 0L, scale = "linear")
  expect_equal(c(f$intercept, f$slopes), unname(stats::coef(stats::lm(y ~ t))))
  y2 <- exp(.1 * t + sin(t) / 50); y2[c(1, 2)] <- c(0, -1)
  expect_equal(CuRveR:::web_fit_segmented(t, y2, 0L, scale = "log")$dropped, 2)
  expect_error(CuRveR:::web_fit_segmented(1:6, exp(1:6), 2L), "distinct")
  expect_error(CuRveR:::web_fit_segmented(t, y, 1L, guesses = 50, scale = "linear"), "inside")
  expect_equal(CuRveR:::web_parse_breaks("480; 180"), c(180, 480))
  expect_error(CuRveR:::web_parse_breaks("a, 3"), "numbers")
})

test_that("automatic selection picks the true number of breakpoints for all wells", {
  t <- seq(0, 1200, 10)
  units <- lapply(1:3, function(i) data.frame(Time = t, value = piecewise(t) * exp(stats::rnorm(length(t), 0, .003))))
  expect_equal(CuRveR:::web_select_breaks(units, 3L)$k, 2L)
})

test_that("model comparison favours Richard on sigmoids and segments on diauxic curves", {
  set.seed(1); t <- seq(0, 1200, 10)
  sig <- .08 + .9 / (1 + exp(-.012 * (t - 400))) + stats::rnorm(length(t), 0, .004)
  dia <- .08 + .45 / (1 + exp(-.02 * (t - 250))) + .4 / (1 + exp(-.015 * (t - 700))) + stats::rnorm(length(t), 0, .004)
  res <- lapply(list(sig, dia), function(y) {
    r <- CuRveR:::web_fit_richard(t, y)
    s <- CuRveR:::web_fit_segmented(t, y, 2L, scale = "log")
    CuRveR:::web_compare_models(y, t, r$predict(t), s)
  })
  expect_equal(res[[1]]$verdict, "richard")
  expect_false(res[[1]]$richard_structured_residuals)
  expect_equal(res[[2]]$verdict, "segmented")
  expect_true(res[[2]]$richard_structured_residuals)
  expect_match(CuRveR:::web_verdict_text("segmented", TRUE, "fr"), "Segments")
})

test_that("runs test detects systematic residuals and AICc follows its definition", {
  set.seed(3)
  expect_gt(CuRveR:::web_runs_test(1:60, stats::rnorm(60)), .01)
  expect_lt(CuRveR:::web_runs_test(1:60, sin((1:60) / 10)), .001)
  expect_equal(CuRveR:::web_aicc(2, 20, 4), 20 * log(.1) + 10 + 60 / 14)
  expect_true(is.na(CuRveR:::web_aicc(1, 5, 4)))
})

test_that("replicate statistics match base R tests", {
  w <- data.frame(signal = "OD", method = "Richard", parameter = "r_max", well = paste0("W", 1:9),
    condition = rep(c("Ctrl", "T1", "T2"), each = 3), value = c(1, 1.2, 1.1, 2, 2.3, 2.1, 1.05, 1.15, 1.2))
  s <- CuRveR:::web_parameter_stats(w, "OD", "Richard", "r_max", reference = "Ctrl")
  a <- c(1, 1.2, 1.1); b <- c(2, 2.3, 2.1)
  expect_equal(s$summary$mean, c(1.1, 2.1333333, 1.1333333), tolerance = 1e-6)
  expect_equal(s$summary$ci_high[1], 1.1 + stats::qt(.975, 2) * stats::sd(a) / sqrt(3))
  tt <- stats::t.test(b, a)
  expect_equal(s$tests$p_value[1], tt$p.value)
  expect_equal(c(s$tests$ci_low[1], s$tests$ci_high[1]), as.numeric(tt$conf.int))
  expect_equal(s$tests$p_adjusted, stats::p.adjust(s$tests$p_value, "holm"))
  expect_equal(s$tests$hedges_g[1], (mean(b) - mean(a)) / sqrt((stats::var(a) + stats::var(b)) / 2) * (1 - 3 / 15))
  expect_equal(nrow(CuRveR:::web_parameter_stats(w, "OD", "Richard", "r_max", mode = "pairwise")$tests), 3)
  expect_equal(s$global$test, c("Welch ANOVA", "Kruskal-Wallis"))
  expect_equal(CuRveR:::web_stars(c(.0001, .005, .03, .2, NA)), c("***", "**", "*", "ns", ""))
  single <- CuRveR:::web_parameter_stats(w[c(1, 4:6), ], "OD", "Richard", "r_max", "Ctrl")
  expect_true(is.na(single$tests$p_value))
  expect_s3_class(CuRveR:::web_stats_plot(s), "ggplot")
})

test_that("advanced pipeline keeps Richard results unchanged and is reproducible", {
  t <- seq(0, 1200, 20)
  mk <- function(mid) .05 + .08 + .9 / (1 + exp(-.012 * (t - mid)))
  set.seed(5)
  raw <- data.frame(Time = t, A1 = mk(400), A2 = mk(410), A3 = mk(395), B1 = mk(600), B2 = mk(610), B3 = mk(590),
    C1 = .05, C2 = .05, C3 = .05)
  raw[-1] <- raw[-1] + stats::rnorm(length(t) * 9, 0, .003)
  wells <- names(raw)[-1]
  d <- CuRveR:::web_prepare(raw, "Time", wells, "OD")
  cond <- list(A = list(replicates = c("A1", "A2", "A3"), blanks = c("C1", "C2", "C3")),
               B = list(replicates = c("B1", "B2", "B3"), blanks = c("C1", "C2", "C3")))
  win <- data.frame(signal = "OD", well = wells, left = 0, right = 1200)
  set.seed(9); state <- .Random.seed
  base <- CuRveR:::web_analyze(d, cond, win, "LAD", 42L)
  adv <- CuRveR:::web_analyze_advanced(d, cond, win, "LAD", 42L,
    list(scale = "log", mode = "fixed", breaks = 1L), per_well_richard = TRUE)
  expect_identical(.Random.seed, state)
  expect_equal(adv$parameters, base$parameters)
  expect_equal(adv$data$fit, base$data$fit)
  expect_setequal(unique(adv$wells$method), c("Richard", "Segmented"))
  expect_equal(nrow(adv$comparison), 2)
  expect_true(all(is.finite(adv$data$segment_fit)))
  again <- CuRveR:::web_analyze_advanced(d, cond, win, "LAD", 42L,
    list(scale = "log", mode = "fixed", breaks = 1L), per_well_richard = TRUE)
  expect_equal(adv, again)
  s <- CuRveR:::web_parameter_stats(adv$wells, "OD", "Richard", "s", "A")
  expect_equal(s$tests$difference, 200, tolerance = .05)
  expect_lt(s$tests$p_value, .01)
  expect_gt(nrow(CuRveR:::web_stats_table(adv)), 0)
  expect_silent(ggplot2::ggplot_build(CuRveR:::web_segment_plot(adv, "OD", c("A", "B"))))
  guided <- CuRveR:::web_analyze_advanced(d, cond, win, "LAD", 42L,
    list(scale = "linear", mode = "guided", guesses = c(300, 700)), per_well_richard = FALSE)
  expect_equal(unique(guided$comparison$breakpoints), 2L)
  expect_equal(unique(guided$wells$method), "Segmented")
  expect_error(CuRveR:::web_analyze_advanced(d, cond, win, "LAD", 42L, list(mode = "guided")), "Guided")
})

test_that("results panel exposes the new views, options and exports", {
  ui <- as.character(CuRveR:::modern_ui())
  for (id in c('value="segments"', 'value="stats"', 'id="seg_scale"', 'id="seg_mode"', 'id="stat_param"',
               'id="comparison_export"', 'id="stats_export"', 'id="wells_export"', 'id="phases_export"'))
    expect_match(ui, id, fixed = TRUE)
})
