# CuRveR 2.1 — additional analysis modules.
# The Richard pipeline (web_analyze) is reused unchanged; everything below is
# computed on its blank-corrected, windowed output.
#   1. Breakpoint (segmented) regression, per well and per condition.
#   2. Model comparison: Richard vs segments (AICc, R², residual runs test).
#   3. Replicate statistics: per-well parameters, 95 % CI, Welch tests, Hedges' g.

# ---------------------------------------------------------------------------
# 1. Segmented regression
# ---------------------------------------------------------------------------

# Continuous piecewise-linear model on ln(signal) (default) or on the signal.
# k = number of breakpoints; guesses = optional starting breakpoints (time units).
web_fit_segmented <- function(x, y, k = 1L, guesses = NULL, scale = c("log", "linear"), seed = 42L) {
  scale <- match.arg(scale)
  k <- as.integer(k)
  keep <- is.finite(x) & is.finite(y) & (scale == "linear" | y > 0)
  x0 <- x[keep]
  z <- if (scale == "log") log(y[keep]) else y[keep]
  times <- sort(unique(x0))
  if (length(times) < 2L * k + 3L)
    stop("Not enough distinct time points for ", k, " breakpoint(s).")
  d <- data.frame(x = x0, z = z)
  base <- stats::lm(z ~ x, data = d)
  if (k == 0L) {
    breaks <- numeric(); breaks_se <- numeric()
    slopes <- unname(stats::coef(base)[2]); slopes_se <- unname(sqrt(diag(stats::vcov(base)))[2])
    intercept <- unname(stats::coef(base)[1])
    rss <- sum(stats::residuals(base)^2)
  } else {
    psi <- if (length(guesses)) sort(as.numeric(guesses)) else
      stats::quantile(times, probs = seq_len(k) / (k + 1), names = FALSE, type = 1)
    if (length(psi) != k) stop("The number of guided breakpoints must equal k.")
    if (any(psi <= min(times) | psi >= max(times))) stop("Guided breakpoints must lie inside the time window.")
    fit <- suppressWarnings(tryCatch(
      segmented::segmented(base, seg.Z = ~x, psi = list(x = psi),
        control = segmented::seg.control(n.boot = 20, seed = seed, display = FALSE)),
      error = function(e) NULL))
    if (is.null(fit) || !inherits(fit, "segmented") || NROW(fit$psi) != k)
      stop("Segmented regression did not converge with ", k, " breakpoint(s).")
    o <- order(fit$psi[, "Est."])
    breaks <- unname(fit$psi[o, "Est."]); breaks_se <- unname(fit$psi[o, "St.Err"])
    sl <- segmented::slope(fit)$x
    slopes <- unname(sl[, "Est."]); slopes_se <- unname(sl[, "St.Err."])
    intercept <- unname(stats::coef(fit)["(Intercept)"])
    rss <- sum(stats::residuals(fit)^2)
  }
  n <- length(z); p <- 2L + 2L * k
  structure(list(k = k, scale = scale, breaks = breaks, breaks_se = breaks_se,
    slopes = slopes, slopes_se = slopes_se, intercept = intercept,
    range = range(times), n = n, dropped = sum(!keep), rss = rss,
    bic = n * log(max(rss, .Machine$double.xmin) / n) + log(n) * (p + 1)),
    class = "curver_segments")
}

# Prediction in the original signal units (exp() back-transform on log scale).
web_predict_segmented <- function(fit, x) {
  z <- fit$intercept + fit$slopes[1] * x
  for (j in seq_along(fit$breaks))
    z <- z + (fit$slopes[j + 1] - fit$slopes[j]) * pmax(x - fit$breaks[j], 0)
  if (fit$scale == "log") exp(z) else z
}

# One row per phase. On log scale the slope is the specific rate (per time unit);
# doubling time is reported for positive slopes, halving time for negative ones.
web_segment_phases <- function(fit) {
  starts <- c(fit$range[1], fit$breaks); ends <- c(fit$breaks, fit$range[2])
  out <- data.frame(phase = seq_along(fit$slopes), start = starts, end = ends,
    duration = ends - starts, slope = fit$slopes, slope_se = fit$slopes_se)
  if (fit$scale == "log") {
    out$doubling_time <- ifelse(fit$slopes > 0, log(2) / fit$slopes, NA_real_)
    out$halving_time <- ifelse(fit$slopes < 0, log(2) / abs(fit$slopes), NA_real_)
  }
  out
}

# Long parameter table: phaseN_start (N >= 2), phaseN_slope, phaseN_doubling_time...
web_segment_parameters <- function(fit) {
  ph <- web_segment_phases(fit)
  values <- c(stats::setNames(ph$slope, paste0("phase", ph$phase, "_slope")),
    if (length(fit$breaks)) stats::setNames(fit$breaks, paste0("phase", seq_along(fit$breaks) + 1L, "_start")))
  if (fit$scale == "log") {
    values <- c(values, stats::setNames(ph$doubling_time, paste0("phase", ph$phase, "_doubling_time")),
      stats::setNames(ph$halving_time, paste0("phase", ph$phase, "_halving_time")))
  }
  values <- values[is.finite(values)]
  data.frame(parameter = names(values), value = unname(values))
}

# Choose one number of breakpoints per signal, so phases are comparable between
# wells and conditions: the k minimising the BIC summed over all wells.
web_select_breaks <- function(units, max_breaks = 2L, scale = "log", seed = 42L) {
  candidates <- 0:as.integer(max_breaks)
  bic <- vapply(candidates, function(k) {
    total <- 0
    for (u in units) {
      f <- tryCatch(web_fit_segmented(u$Time, u$value, k, NULL, scale, seed), error = function(e) NULL)
      if (is.null(f)) return(Inf)
      total <- total + f$bic
    }
    total
  }, numeric(1))
  if (!any(is.finite(bic))) stop("Segmented regression failed for every number of breakpoints.")
  list(k = candidates[which.min(bic)], table = data.frame(breakpoints = candidates, total_BIC = bic))
}

web_parse_breaks <- function(text) {
  if (is.null(text) || !nzchar(trimws(text))) return(numeric())
  values <- suppressWarnings(as.numeric(strsplit(gsub(";", ",", text), ",")[[1]]))
  if (any(is.na(values))) stop("Guided breakpoints: enter numbers separated by commas (e.g. 180, 480).")
  sort(values)
}

# ---------------------------------------------------------------------------
# 2. Model comparison
# ---------------------------------------------------------------------------

# Wald–Wolfowitz runs test on residual signs, one-sided (too few runs means
# systematic misfit). Residuals are averaged per time point first.
web_runs_test <- function(time, residual) {
  r <- tapply(residual, time, mean)
  s <- sign(r[r != 0])
  n1 <- sum(s > 0); n2 <- sum(s < 0)
  if (n1 < 2 || n2 < 2) return(NA_real_)
  runs <- 1 + sum(diff(s) != 0)
  mu <- 1 + 2 * n1 * n2 / (n1 + n2)
  v <- 2 * n1 * n2 * (2 * n1 * n2 - n1 - n2) / ((n1 + n2)^2 * (n1 + n2 - 1))
  stats::pnorm((runs - mu) / sqrt(v))
}

# Gaussian AICc from the residual sum of squares; K counts sigma.
web_aicc <- function(rss, n, p) {
  K <- p + 1
  if (n - K - 1 <= 0) return(NA_real_)
  n * log(max(rss, .Machine$double.xmin) / n) + 2 * K + 2 * K * (K + 1) / (n - K - 1)
}

web_compare_models <- function(value, time, richard_fit, segment_fit, threshold = 10, bias_threshold = 5) {
  # Both models are evaluated on the same points and in the same (linear) units.
  keep <- if (segment_fit$scale == "log") value > 0 else rep(TRUE, length(value))
  y <- value[keep]; t <- time[keep]
  fr <- richard_fit[keep]; fs <- web_predict_segmented(segment_fit, t)
  n <- length(y); tss <- sum((y - mean(y))^2)
  rss_r <- sum((y - fr)^2); rss_s <- sum((y - fs)^2)
  aicc_r <- web_aicc(rss_r, n, 4); aicc_s <- web_aicc(rss_s, n, 2 + 2 * segment_fit$k)
  delta <- aicc_r - aicc_s
  runs_r <- web_runs_test(t, y - fr); runs_s <- web_runs_test(t, y - fs)
  # Largest systematic deviation (residual averaged per time point), in % of the
  # signal amplitude: with very low noise the runs test alone flags negligible bias.
  amplitude <- diff(range(y))
  bias <- function(res) if (amplitude > 0) 100 * max(abs(tapply(res, t, mean))) / amplitude else NA_real_
  bias_r <- bias(y - fr); bias_s <- bias(y - fs)
  verdict <- if (!is.finite(delta)) "undetermined" else if (delta > threshold) "segmented" else
    if (delta < -threshold) "richard" else "equivalent"
  data.frame(n = n, breakpoints = segment_fit$k,
    AICc_richard = aicc_r, AICc_segmented = aicc_s, delta_AICc = delta,
    R2_richard = 1 - rss_r / tss, R2_segmented = 1 - rss_s / tss,
    runs_p_richard = runs_r, runs_p_segmented = runs_s,
    max_bias_percent_richard = bias_r, max_bias_percent_segmented = bias_s,
    richard_structured_residuals = is.finite(runs_r) & runs_r < .01 & is.finite(bias_r) & bias_r > bias_threshold,
    excluded_nonpositive = sum(!keep), verdict = verdict)
}

web_verdict_text <- function(verdict, structured, language = "en") {
  fr <- identical(language, "fr")
  base <- switch(verdict,
    segmented = if (fr) "Segments nettement meilleurs (ΔAICc > 10) : Richard décrit mal cette courbe." else "Segments clearly better (ΔAICc > 10): Richard describes this curve poorly.",
    richard = if (fr) "Richard nettement meilleur (ΔAICc < −10)." else "Richard clearly better (ΔAICc < −10).",
    equivalent = if (fr) "Modèles équivalents (|ΔAICc| ≤ 10)." else "Models equivalent (|ΔAICc| ≤ 10).",
    if (fr) "Comparaison impossible (trop peu de points)." else "Comparison not possible (too few points).")
  if (isTRUE(structured)) paste(base, if (fr) "Résidus de Richard structurés (test des runs p < 0,01 et écart systématique > 5 % de l’amplitude)." else "Richard residuals are structured (runs test p < 0.01 and systematic deviation > 5% of amplitude).") else base
}

# ---------------------------------------------------------------------------
# Pipeline
# ---------------------------------------------------------------------------

web_analyze_advanced <- function(data, conditions, windows, loss = "LAD", seed = 42L,
                                 segments = list(scale = "auto", mode = "auto", max_breaks = 2L, breaks = 2L, guesses = numeric()),
                                 per_well_richard = TRUE) {
  base <- web_analyze(data, conditions, windows, loss, seed)
  losses <- list(LAD = least_absolute_deviation, OLS = ordinary_least_squares,
                 Huber = huber, Tukey = tukey_bisquare, Andrew = andrew, Jaeckel = jaeckel)
  had_seed <- exists(".Random.seed", .GlobalEnv, inherits = FALSE)
  if (had_seed) old_seed <- get(".Random.seed", .GlobalEnv)
  on.exit(if (had_seed) assign(".Random.seed", old_seed, .GlobalEnv) else
    if (exists(".Random.seed", .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv))
  set.seed(seed)
  # "auto": log scale for the OD signal (growth, lysis), linear for the others
  # (fluorescence, luminescence), which may approach zero or be negative after correction.
  scale_setting <- segments$scale %||% "auto"; mode <- segments$mode %||% "auto"
  od_signal <- attr(data, "od_signal")
  scale_for <- function(signal) {
    if (scale_setting != "auto") return(scale_setting)
    is_od <- if (!is.null(od_signal)) identical(signal, od_signal) else grepl("OD|DO|600", signal, ignore.case = TRUE)
    if (is_od) "log" else "linear"
  }
  scales <- character()
  guesses <- as.numeric(segments$guesses %||% numeric())
  d <- base$data
  wells <- list(); phases <- list(); comparison <- list(); curves <- list(); selection <- list()
  d$segment_fit <- NA_real_
  for (signal in sort(unique(d$signal))) {
    ds <- d[d$signal == signal, ]
    scale <- scales[[signal]] <- scale_for(signal)
    units <- split(ds, list(ds$condition, ds$well), drop = TRUE)
    k <- switch(mode,
      guided = { if (!length(guesses)) stop("Guided mode needs at least one breakpoint time."); length(guesses) },
      fixed = as.integer(segments$breaks %||% 1L),
      { s <- web_select_breaks(units, segments$max_breaks %||% 2L, scale, seed)
        selection[[signal]] <- cbind(signal = signal, s$table); s$k })
    g <- if (mode == "guided") guesses else NULL
    for (u in units) {
      cond <- u$condition[1]; well <- u$well[1]
      f <- tryCatch(web_fit_segmented(u$Time, u$value, k, g, scale, seed), error = function(e) e)
      if (inherits(f, "error")) stop(paste0(signal, " / ", cond, " / ", well, ": ", conditionMessage(f)))
      idx <- which(d$signal == signal & d$condition == cond & d$well == well)
      d$segment_fit[idx] <- web_predict_segmented(f, d$Time[idx])
      ph <- web_segment_phases(f)
      phases[[length(phases) + 1L]] <- cbind(signal = signal, condition = cond, well = well, ph)
      sp <- web_segment_parameters(f)
      wells[[length(wells) + 1L]] <- data.frame(signal = signal, condition = cond, well = well, method = "Segmented", sp)
      if (per_well_richard) {
        r <- web_fit_richard(u$Time, u$value, losses[[loss]])
        wells[[length(wells) + 1L]] <- data.frame(signal = signal, condition = cond, well = well, method = "Richard",
          parameter = names(r$estimated), value = unname(r$estimated))
      }
    }
    for (cond in sort(unique(ds$condition))) {
      dc <- ds[ds$condition == cond, ]
      f <- tryCatch(web_fit_segmented(dc$Time, dc$value, k, g, scale, seed), error = function(e) e)
      if (inherits(f, "error")) stop(paste0(signal, " / ", cond, " (pooled): ", conditionMessage(f)))
      ph <- web_segment_phases(f)
      phases[[length(phases) + 1L]] <- cbind(signal = signal, condition = cond, well = "pooled", ph)
      grid <- seq(f$range[1], f$range[2], length.out = 200)
      curves[[length(curves) + 1L]] <- data.frame(signal = signal, condition = cond, Time = grid,
        segment_fit = web_predict_segmented(f, grid))
      comparison[[length(comparison) + 1L]] <- cbind(signal = signal, condition = cond,
        web_compare_models(dc$value, dc$Time, dc$fit, f))
    }
  }
  base$data <- d
  base$wells <- dplyr::bind_rows(wells)
  base$segments <- list(phases = dplyr::bind_rows(phases), curves = dplyr::bind_rows(curves),
    selection = dplyr::bind_rows(selection))
  base$comparison <- dplyr::bind_rows(comparison)
  base$settings$segments <- list(scale = scale_setting, scales = scales, mode = mode, max_breaks = segments$max_breaks,
    breaks = segments$breaks, guesses = guesses, package = paste("segmented", utils::packageVersion("segmented")))
  base$settings$per_well_richard <- per_well_richard
  base
}

# ---------------------------------------------------------------------------
# 3. Replicate statistics
# ---------------------------------------------------------------------------

web_stars <- function(p) ifelse(is.na(p), "", ifelse(p < .001, "***", ifelse(p < .01, "**", ifelse(p < .05, "*", "ns"))))

web_hedges_g <- function(a, b) {
  na <- length(a); nb <- length(b)
  sp <- sqrt(((na - 1) * stats::var(a) + (nb - 1) * stats::var(b)) / (na + nb - 2))
  if (!is.finite(sp) || sp == 0) return(NA_real_)
  (mean(b) - mean(a)) / sp * (1 - 3 / (4 * (na + nb) - 9))
}

# n = wells. These are technical replicates unless the user groups independent
# experiments: the tests then describe technical variability only.
web_parameter_stats <- function(wells, signal, method, parameter, reference = NULL,
                                mode = c("reference", "pairwise"), level = .95) {
  mode <- match.arg(mode)
  d <- wells[wells$signal == signal & wells$method == method & wells$parameter == parameter & is.finite(wells$value), ]
  if (!nrow(d)) return(list(summary = data.frame(), tests = data.frame(), global = data.frame()))
  conds <- unique(d$condition)
  conds <- conds[gtools::mixedorder(conds)]
  summary <- dplyr::bind_rows(lapply(conds, function(cnd) {
    v <- d$value[d$condition == cnd]; n <- length(v); m <- mean(v)
    s <- if (n > 1) stats::sd(v) else NA_real_; se <- s / sqrt(n)
    h <- if (n > 1) stats::qt(1 - (1 - level) / 2, n - 1) * se else NA_real_
    data.frame(condition = cnd, n = n, mean = m, sd = s, sem = se, ci_low = m - h, ci_high = m + h,
      cv_percent = 100 * s / abs(m))
  }))
  if (is.null(reference) || !reference %in% conds) reference <- conds[1]
  pairs <- if (mode == "reference") lapply(setdiff(conds, reference), function(x) c(reference, x)) else
    if (length(conds) > 1) utils::combn(conds, 2, simplify = FALSE) else list()
  tests <- dplyr::bind_rows(lapply(pairs, function(pr) {
    a <- d$value[d$condition == pr[1]]; b <- d$value[d$condition == pr[2]]
    tt <- if (length(a) > 1 && length(b) > 1) tryCatch(stats::t.test(b, a, var.equal = FALSE, conf.level = level), error = function(e) NULL) else NULL
    data.frame(group_1 = pr[1], group_2 = pr[2], n_1 = length(a), n_2 = length(b),
      difference = mean(b) - mean(a),
      ci_low = if (is.null(tt)) NA_real_ else tt$conf.int[1], ci_high = if (is.null(tt)) NA_real_ else tt$conf.int[2],
      ratio = if (mean(a) != 0) mean(b) / mean(a) else NA_real_,
      hedges_g = if (length(a) > 1 && length(b) > 1) web_hedges_g(a, b) else NA_real_,
      p_value = if (is.null(tt)) NA_real_ else tt$p.value)
  }))
  if (nrow(tests)) {
    tests$p_adjusted <- stats::p.adjust(tests$p_value, "holm")
    tests$significance <- web_stars(tests$p_adjusted)
  }
  ok <- summary$condition[summary$n > 1]
  dg <- d[d$condition %in% ok, ]
  global <- if (length(ok) >= 3) {
    w <- tryCatch(stats::oneway.test(value ~ condition, dg, var.equal = FALSE)$p.value, error = function(e) NA_real_)
    kw <- tryCatch(stats::kruskal.test(value ~ factor(condition), dg)$p.value, error = function(e) NA_real_)
    data.frame(test = c("Welch ANOVA", "Kruskal-Wallis"), p_value = c(w, kw))
  } else data.frame()
  list(summary = summary, tests = tests, global = global, reference = reference,
       signal = signal, method = method, parameter = parameter, level = level, data = d)
}

web_stats_table <- function(result, reference = NULL, mode = "reference") {
  w <- result$wells
  if (is.null(w) || !nrow(w)) return(data.frame())
  keys <- unique(w[c("signal", "method", "parameter")])
  dplyr::bind_rows(lapply(seq_len(nrow(keys)), function(i) {
    s <- web_parameter_stats(w, keys$signal[i], keys$method[i], keys$parameter[i], reference, mode)
    if (!nrow(s$tests)) return(NULL)
    cbind(keys[i, ], s$tests, row.names = NULL)
  }))
}

# ---------------------------------------------------------------------------
# Plots
# ---------------------------------------------------------------------------

web_segment_plot <- function(result, signal, conditions, language = "en", xlim = NULL) {
  fr <- identical(language, "fr")
  scale <- result$settings$segments$scales[[signal]] %||% result$settings$segments$scale %||% "log"
  d <- result$data[result$data$signal == signal & result$data$condition %in% conditions, ]
  cv <- result$segments$curves; cv <- cv[cv$signal == signal & cv$condition %in% conditions, ]
  br <- result$segments$phases
  br <- br[br$signal == signal & br$condition %in% conditions & br$well == "pooled" & br$phase > 1, ]
  if (scale == "log") d <- d[d$value > 0, ]
  p <- ggplot2::ggplot(d, ggplot2::aes(Time, value, color = condition)) +
    ggplot2::geom_point(alpha = .35, size = 1.2) +
    ggplot2::geom_line(data = cv, ggplot2::aes(y = segment_fit), linewidth = 1) +
    ggplot2::facet_wrap(~condition) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::labs(x = if (fr) "Temps (in min)" else "Time (in min)",
      y = if (scale == "log") (if (fr) "Signal corrigé (échelle log)" else "Corrected signal (log scale)") else (if (fr) "Signal corrigé" else "Corrected signal"),
      color = "Condition") +
    ggplot2::coord_cartesian(xlim = xlim)
  if (nrow(br)) p <- p + ggplot2::geom_vline(data = br, ggplot2::aes(xintercept = start, color = condition), linetype = 2)
  if (scale == "log") p <- p + ggplot2::scale_y_log10()
  p
}

web_stats_plot <- function(stats, language = "en") {
  fr <- identical(language, "fr")
  if (!nrow(stats$summary)) return(ggplot2::ggplot() + ggplot2::theme_void() +
    ggplot2::annotate("text", 0, 0, label = if (fr) "Aucune valeur pour ce paramètre." else "No values for this parameter."))
  lv <- stats$summary$condition
  d <- stats$data; d$condition <- factor(d$condition, lv)
  s <- stats$summary; s$condition <- factor(s$condition, lv)
  p <- ggplot2::ggplot(d, ggplot2::aes(condition, value, color = condition)) +
    ggplot2::geom_errorbar(data = s, ggplot2::aes(y = NULL, ymin = ci_low, ymax = ci_high), width = .25, linewidth = .8, na.rm = TRUE) +
    ggplot2::geom_point(data = s, ggplot2::aes(y = mean), shape = 95, size = 12) +
    ggplot2::geom_jitter(width = .08, height = 0, size = 2.5, alpha = .8) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(legend.position = "none", axis.text.x = ggplot2::element_text(angle = 30, hjust = 1)) +
    ggplot2::labs(x = NULL, y = paste(stats$method, "·", stats$parameter),
      caption = if (fr) paste0("Points : puits · trait : moyenne · barres : IC ", 100 * stats$level, " % · étoiles : Welch vs ", stats$reference, " (Holm)")
        else paste0("Dots: wells · bar: mean · error bars: ", 100 * stats$level, "% CI · stars: Welch vs ", stats$reference, " (Holm)"))
  t <- stats$tests
  if (nrow(t) && all(t$group_1 == stats$reference)) {
    top <- max(c(d$value, s$ci_high), na.rm = TRUE); span <- diff(range(c(d$value, s$ci_low, s$ci_high), na.rm = TRUE))
    if (!is.finite(span) || span == 0) span <- abs(top) + 1
    lab <- data.frame(condition = factor(t$group_2, lv), y = top + .08 * span, label = t$significance)
    p <- p + ggplot2::geom_text(data = lab, ggplot2::aes(y = y, label = label), color = "black", size = 5)
  }
  p
}
