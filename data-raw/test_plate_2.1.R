# Synthetic plate used to test the CuRveR 2.1 modules (segments, model comparison, statistics).
# Run from the repository root: Rscript data-raw/test_plate_2.1.R
# Time in minutes, every 10 min for 20 h. Each row = one condition, 3 technical replicates;
# row E = medium blank. Truth values are written to test_plate_2.1_truth.csv.
set.seed(2026)
time <- seq(0, 1200, by = 10)
logistic <- function(t, mid, rate) 1 / (1 + exp(-rate * (t - mid)))
blank_od <- 0.05; blank_gfp <- 50
truth <- data.frame(
  row = c("A", "B", "C", "D"),
  condition = c("Sigmoid", "Diauxie", "Lysis", "Lysis_delayed"),
  description = c("Single sigmoid growth: Richard should be adequate",
                  "Diauxic growth (two phases): segments should win",
                  "Prey growth then Bdellovibrio-like lysis at ~500 min",
                  "Same as Lysis, lysis delayed to ~650 min and slower"),
  lysis_mid = c(NA, NA, 500, 650), lysis_rate = c(NA, NA, 0.02, 0.012),
  gfp_onset = c(350, 600, 250, 250))
od_curve <- function(cond, t, j) {
  switch(cond,
    Sigmoid = 0.08 + 0.9 * j$amp * logistic(t, 400 + j$shift, 0.012),
    Diauxie = 0.08 + 0.45 * j$amp * logistic(t, 250 + j$shift, 0.02) + 0.40 * j$amp * logistic(t, 700 + j$shift, 0.015),
    Lysis = (0.15 + 0.35 * j$amp * logistic(t, 200 + j$shift, 0.015)) * (1 - 0.85 * logistic(t, 500 + j$shift, 0.02)),
    Lysis_delayed = (0.15 + 0.35 * j$amp * logistic(t, 200 + j$shift, 0.015)) * (1 - 0.85 * logistic(t, 650 + j$shift, 0.012)))
}
od <- data.frame(Time = time); gfp <- data.frame(Time = time)
for (i in seq_len(nrow(truth))) for (col in 1:3) {
  well <- paste0(truth$row[i], col)
  j <- list(amp = rnorm(1, 1, 0.03), shift = rnorm(1, 0, 15))
  signal <- od_curve(truth$condition[i], time, j)
  od[[well]] <- round(blank_od + signal + rnorm(length(time), 0, 0.004), 4)
  expression <- 3000 * signal * logistic(time, truth$gfp_onset[i] + j$shift, 0.03)
  gfp[[well]] <- round(blank_gfp + expression + rnorm(length(time), 0, 8), 1)
}
for (col in 1:3) {
  od[[paste0("E", col)]] <- round(blank_od + rnorm(length(time), 0, 0.003), 4)
  gfp[[paste0("E", col)]] <- round(blank_gfp + rnorm(length(time), 0, 8), 1)
}
dir.create("inst/extdata", showWarnings = FALSE, recursive = TRUE)
write.csv(od, "inst/extdata/test_plate_2.1_OD.csv", row.names = FALSE)
openxlsx::write.xlsx(list(OD = od, GFP = gfp), "inst/extdata/test_plate_2.1.xlsx", overwrite = TRUE)
write.csv(truth, "inst/extdata/test_plate_2.1_truth.csv", row.names = FALSE)
