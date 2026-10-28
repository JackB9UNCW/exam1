# ══════════════════════════════════════════════════════════════════
#  ECN 377 · Quiz 9 Solver  (color-coded)
#
#  STEP 1 · BEFORE THE QUIZ: click "Source" once (top right of editor).
#           Loads data, runs regressions, prints the cheat card.
#
#  STEP 2 · DURING THE QUIZ: scroll to the matching question below,
#           replace each NA marked  ◀◀◀ FILL IN  with the quiz's number,
#           highlight that block, press Ctrl+Enter (Mac: Cmd+Enter).
#
#  Colors in the Console:
#     BLUE    = logs (#1-3)
#     MAGENTA = variance / standard errors (#4, 6, 7, 9)
#     GREEN   = multiple regression (#12-15)
#     YELLOW  = a blank you still need to fill in
# ══════════════════════════════════════════════════════════════════

for (pkg in c("wooldridge", "cli")) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
}
library(wooldridge)
library(cli)

# ── engine (don't edit) ───────────────────────────────────────────

colr <- list(log = col_blue, var = col_magenta, reg = col_green)

ans <- function(x, label, group) {
  paint <- colr[[group]]
  if (length(x) == 0 || any(is.na(x))) {
    cat("\n", col_yellow(style_bold("  ⚠  BLANK NOT FILLED IN: replace every NA marked ◀◀◀ FILL IN")), "\n\n")
    return(invisible(NA))
  }
  cat("\n", paint(style_bold(sprintf("  ▶ %s:  %.2f", label, x))), "\n\n")
  invisible(x)
}
step <- function(group, ...) cat(colr[[group]](paste0("    ", sprintf(...))), "\n")

reg_bw   <- lm(log(bwght) ~ log(faminc), data = bwght)
reg_wage <- lm(wage ~ educ + exper + tenure, data = wage1)
bw <- coef(reg_bw)
bg <- coef(reg_wage)

q1  <- function(b)          ans(b / 100, "1% rise in x → y changes by about (units)", "log")
q2  <- function(b, dx)      ans(100 * b * dx, "% change in y", "log")
q3  <- function(pct)        ans(bw[[2]] * pct, "% change in birth weight", "log")
q3g <- function(b, pct)     ans(b * pct, "% change in y", "log")
q7  <- function(n, sigma, sx) {
  sst <- (n - 1) * sx^2
  if (!is.na(sst)) step("var", "SST_X = (%s − 1) × %s² = %.4f", n, sx, sst)
  ans(sigma / sqrt(sst), "se(β̂₁)", "var")
}
q7x <- function(x, SSR) {
  n <- length(x); sst <- sum((x - mean(x))^2); sig <- sqrt(SSR / (n - 2))
  if (!is.na(sig)) step("var", "σ̂ = %.4f,  SST_X = %.4f", sig, sst)
  ans(sig / sqrt(sst), "se(β̂₁)", "var")
}
q9  <- function(sd_old, k)  ans(sd_old / sqrt(k), "new sd(β̂₁)", "var")
q12 <- function(b0, b1, b2, x1, x2) ans(b0 + b1 * x1 + b2 * x2, "ŷ", "reg")
q13 <- function(b1, b2, dx1, dx2)   ans(b1 * dx1 + b2 * dx2, "change in ŷ", "reg")
q14 <- function(educ, exper, tenure)
  ans(bg[[1]] + bg[["educ"]] * educ + bg[["exper"]] * exper + bg[["tenure"]] * tenure,
      "predicted hourly wage ($)", "reg")
q15 <- function(d_tenure = 0, d_educ = 0, d_exper = 0)
  ans(bg[["tenure"]] * d_tenure + bg[["educ"]] * d_educ + bg[["exper"]] * d_exper,
      "change in predicted wage ($)", "reg")

# ── cheat card (prints when you Source) ───────────────────────────

cat("\n")
cli_rule(center = style_bold(" QUIZ 9 SOLVER LOADED "))
cat("\n", col_blue(style_bold("LOGS")), "\n")
cat(col_blue("  #1  y = a + b·log(x)   → pick '1% rise in x → about b/100 units'\n"))
cat(col_blue("  #2  log(y) = a + b·x   → 100 × b × Δx   (fall = negative)\n"))
cat(col_blue(sprintf("  #3  bwght elasticity   = %.4f   → × the %% change in income\n", bw[[2]])))
cat("\n", col_magenta(style_bold("VARIANCE / STANDARD ERRORS")), "\n")
cat(col_magenta("  #4  Unbiased → 'The AVERAGE of the 1,000 estimates is close to β₁'\n"))
cat(col_magenta("  #6  Var bigger (select all):\n"))
cat(col_magenta("        ✔ larger error variance σ²\n"))
cat(col_magenta("        ✔ X bunched more tightly (smaller SST_X)\n"))
cat(col_magenta("        ✔ smaller sample\n"))
cat(col_silver ("        ✘ larger sample     ✘ larger true slope\n"))
cat(col_magenta("  #7  se = σ̂ / √[(n−1)·sₓ²]      #9  new sd = old sd / √k\n"))
cat("\n", col_green(style_bold("MULTIPLE REGRESSION")), "\n")
cat(col_green("  #12 prediction: KEEP the intercept   #13 change: DROP the intercept\n"))
cat(col_green("  wage1 coefficients:\n"))
cat(col_green(sprintf("      intercept %.4f   educ %.4f   exper %.4f   tenure %.4f\n",
                      bg[[1]], bg[["educ"]], bg[["exper"]], bg[["tenure"]])))
cat("\n")
cli_rule(center = "scroll down · fill the NAs · highlight · Ctrl/Cmd+Enter")
cat("\n")

# Stop here when Sourcing. Everything below is run block by block.
if (FALSE) {

# ══════════════════════════════════════════════════════════════════
#  ███  LOGS  ███   (blue)
# ══════════════════════════════════════════════════════════════════

# ── #1 · level-log · multiple choice ──  y = a + b·log(x)
b <- NA            # ◀◀◀ FILL IN: the number in front of log(x)   (e.g. 8)
q1(b)
#   → pick the option that says "about [this number] points/units"


# ── #2 · log-level ──  log(y) = a + b·x
b  <- NA           # ◀◀◀ FILL IN: the slope on x                  (e.g. 0.07)
dx <- NA           # ◀◀◀ FILL IN: change in x, NEGATIVE if it falls (e.g. -3)
q2(b, dx)


# ── #3 · log-log · bwght data ──
pct <- NA          # ◀◀◀ FILL IN: % rise in family income          (e.g. 20)
q3(pct)

#    (other log-log with the slope GIVEN in the question)
b   <- NA          # ◀◀◀ FILL IN: the slope                        (e.g. 0.35)
pct <- NA          # ◀◀◀ FILL IN: % change in x                    (e.g. 10)
q3g(b, pct)


# ══════════════════════════════════════════════════════════════════
#  ███  VARIANCE / STANDARD ERRORS  ███   (magenta)
# ══════════════════════════════════════════════════════════════════

# ── #4 · unbiasedness ── no math:
#   "The AVERAGE of the 1,000 estimates is close to β₁"

# ── #6 · select all ── no math:
#   ✔ larger σ²   ✔ X bunched tighter   ✔ smaller sample

# ── #7 · se from n, σ̂ and the SD of X ──
n     <- NA        # ◀◀◀ FILL IN: number of observations           (e.g. 8)
sigma <- NA        # ◀◀◀ FILL IN: σ̂ (sigma-hat)                    (e.g. 6.8)
sx    <- NA        # ◀◀◀ FILL IN: standard deviation of X, sₓ       (e.g. 2.8)
q7(n, sigma, sx)

#    (variant: the question lists the x values and gives SSR)
x   <- c(NA, NA, NA, NA, NA)   # ◀◀◀ FILL IN: every x value     (e.g. 3, 5, 6, 8, 9)
SSR <- NA                      # ◀◀◀ FILL IN: SSR                (e.g. 37)
q7x(x, SSR)


# ── #9 · sample k times as large ──
sd_old <- NA       # ◀◀◀ FILL IN: current sd(β̂₁)                   (e.g. 1.84)
k      <- NA       # ◀◀◀ FILL IN: how many times larger             (e.g. 10)
q9(sd_old, k)


# ══════════════════════════════════════════════════════════════════
#  ███  MULTIPLE REGRESSION  ███   (green)
# ══════════════════════════════════════════════════════════════════

# ── #12 · predicted value ──  ŷ = b0 + b1·x1 + b2·x2
b0 <- NA           # ◀◀◀ FILL IN: intercept                        (e.g. 3)
b1 <- NA           # ◀◀◀ FILL IN: coefficient on x1 (keep its sign) (e.g. 0.6)
b2 <- NA           # ◀◀◀ FILL IN: coefficient on x2 (keep its sign) (e.g. 1.2)
x1 <- NA           # ◀◀◀ FILL IN: value of x1                      (e.g. 7)
x2 <- NA           # ◀◀◀ FILL IN: value of x2                      (e.g. 14)
q12(b0, b1, b2, x1, x2)


# ── #13 · predicted change ──  Δŷ = b1·Δx1 + b2·Δx2   (intercept ignored)
b1  <- NA          # ◀◀◀ FILL IN: coefficient on x1 (keep its sign) (e.g. 1.1)
b2  <- NA          # ◀◀◀ FILL IN: coefficient on x2 (keep its sign) (e.g. -2.1)
dx1 <- NA          # ◀◀◀ FILL IN: change in x1                     (e.g. 6)
dx2 <- NA          # ◀◀◀ FILL IN: change in x2                     (e.g. 1)
q13(b1, b2, dx1, dx2)


# ── #14 · wage1 predicted wage ──
educ   <- NA       # ◀◀◀ FILL IN: years of education               (e.g. 14)
exper  <- NA       # ◀◀◀ FILL IN: years of experience              (e.g. 10)
tenure <- NA       # ◀◀◀ FILL IN: years of tenure                  (e.g. 5)
q14(educ, exper, tenure)


# ── #15 · wage1 change ──  (put 0 for anything that doesn't change)
d_tenure <- NA     # ◀◀◀ FILL IN: change in tenure                 (e.g. 3)
d_educ   <- 0      #               change in education (usually 0)
d_exper  <- 0      #               change in experience (usually 0)
q15(d_tenure, d_educ, d_exper)

}
