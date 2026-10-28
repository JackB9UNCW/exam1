# ==================================================================
#  ECN 377 - Quiz 9 Solver
#
#  STEP 1 - BEFORE THE QUIZ: click "Source" once (top right of editor).
#           Loads data, runs regressions, prints the cheat card.
#
#  STEP 2 - DURING THE QUIZ: scroll to the matching question below.
#           Each question has:
#             EXAMPLE  - already filled in with the Problem Set 10 numbers
#             YOUR QUIZ - blanks (NA) where the quiz's numbers go
#           Replace each NA marked  <<< FILL IN, highlight the block,
#           press Cmd+Enter (Windows: Ctrl+Enter).
# ==================================================================

if (!requireNamespace("wooldridge", quietly = TRUE)) install.packages("wooldridge")
library(wooldridge)

# ---- engine (don't edit) -----------------------------------------

ans <- function(x, label) {
  if (length(x) == 0 || any(is.na(x))) {
    cat("\n  !! BLANK NOT FILLED IN: replace every NA marked <<< FILL IN\n\n")
    return(invisible(NA))
  }
  cat(sprintf("\n  >>> %s:  %.2f\n\n", label, x))
  invisible(x)
}

reg_bw   <- lm(log(bwght) ~ log(faminc), data = bwght)
reg_wage <- lm(wage ~ educ + exper + tenure, data = wage1)
bw <- coef(reg_bw)
bg <- coef(reg_wage)

q1  <- function(b)          ans(b / 100, "1% rise in x -> y changes by about (units)")
q2  <- function(b, dx)      ans(100 * b * dx, "% change in y")
q3  <- function(pct)        ans(bw[[2]] * pct, "% change in birth weight")
q3g <- function(b, pct)     ans(b * pct, "% change in y")
q7  <- function(n, sigma, sx) {
  sst <- (n - 1) * sx^2
  if (!is.na(sst)) cat(sprintf("    SST_X = (%s - 1) * %s^2 = %.4f\n", n, sx, sst))
  ans(sigma / sqrt(sst), "se(b1-hat)")
}
q7x <- function(x, SSR) {
  n <- length(x); sst <- sum((x - mean(x))^2); sig <- sqrt(SSR / (n - 2))
  if (!is.na(sig)) cat(sprintf("    sigma-hat = %.4f,  SST_X = %.4f\n", sig, sst))
  ans(sig / sqrt(sst), "se(b1-hat)")
}
q9  <- function(sd_old, k)  ans(sd_old / sqrt(k), "new sd(b1-hat)")
q12 <- function(b0, b1, b2, x1, x2) ans(b0 + b1 * x1 + b2 * x2, "y-hat")
q13 <- function(b1, b2, dx1, dx2)   ans(b1 * dx1 + b2 * dx2, "change in y-hat")
q14 <- function(educ, exper, tenure)
  ans(bg[[1]] + bg[["educ"]] * educ + bg[["exper"]] * exper + bg[["tenure"]] * tenure,
      "predicted hourly wage ($)")
q15 <- function(d_tenure = 0, d_educ = 0, d_exper = 0)
  ans(bg[["tenure"]] * d_tenure + bg[["educ"]] * d_educ + bg[["exper"]] * d_exper,
      "change in predicted wage ($)")

# ---- cheat card (prints when you Source) -------------------------

cat(sprintf("
==================== QUIZ 9 SOLVER LOADED ====================

LOGS
  #1  y = a + b*log(x)  -> pick '1%% rise in x -> about b/100 units'
  #2  log(y) = a + b*x  -> 100 * b * dx   (fall = negative)
  #3  bwght elasticity  = %.4f  -> times the %% change in income

VARIANCE / STANDARD ERRORS
  #4  Unbiased -> 'The AVERAGE of the 1,000 estimates is close to b1'
  #6  Var bigger (select all):
        [x] larger error variance sigma^2
        [x] X bunched more tightly (smaller SST_X)
        [x] smaller sample
        [ ] larger sample      [ ] larger true slope
  #7  se = sigma-hat / sqrt((n-1) * sx^2)
  #9  new sd = old sd / sqrt(k)

MULTIPLE REGRESSION
  #12 prediction: KEEP the intercept
  #13 change:     DROP the intercept
  wage1: intercept %.4f  educ %.4f  exper %.4f  tenure %.4f

========= scroll down - fill the NAs - highlight - Cmd+Enter =========

", bw[[2]], bg[[1]], bg[["educ"]], bg[["exper"]], bg[["tenure"]]))

# Stop here when Sourcing. Everything below is run block by block.
if (FALSE) {

# ==================================================================
#  LOGS
# ==================================================================

# ---- #1 - level-log - multiple choice ----  y = a + b*log(x)
#  SPOT IT:  the log is only on the RIGHT side, next to x. y has no log.
#  MEANING:  a 1% rise in x changes y by b/100 UNITS (points, dollars...).
#  TRAP:     options saying "b%" or "b points" forgot to divide by 100.
#            Options with "$1 rise" are wrong too: x is in logs, so think in %.

# EXAMPLE: test score = 12 + 8*log(income)
b <- 8
q1(b)                       # -> 0.08  -> pick "about 0.08 points"

# YOUR QUIZ:
b <- NA                     # <<< FILL IN: number in front of log(x)
q1(b)


# ---- #2 - log-level ----  log(y) = a + b*x
#  SPOT IT:  the log is only on the LEFT side, on y. x has no log.
#  FORMULA:  %change in y = 100 * b * (change in x)
#  TRAP:     "falls" / "decreases" -> dx is NEGATIVE. Answer is a percent:
#            type -21, not -0.21.

# EXAMPLE: log(y) = 0.9 + 0.07x, x falls by 3
b  <- 0.07
dx <- -3
q2(b, dx)                   # -> -21.00

# YOUR QUIZ:
b  <- NA                    # <<< FILL IN: slope on x
dx <- NA                    # <<< FILL IN: change in x (NEGATIVE if it falls)
q2(b, dx)


# ---- #3 - log-log - bwght data ----
#  SPOT IT:  log on BOTH sides: lm(log(bwght) ~ log(faminc), data = bwght)
#  MEANING:  the slope is an ELASTICITY: 1% rise in x -> slope % change in y.
#  FORMULA:  %change in y = slope * (%change in x). No 100 here.
#  NOTE:     q3() already ran the regression; slope = 0.0206.
#            Answer is a percent: type 0.41, not 0.0041.

# EXAMPLE: family income rises 20%
pct <- 20
q3(pct)                     # -> 0.41

# YOUR QUIZ:
pct <- NA                   # <<< FILL IN: % rise in family income
q3(pct)

# FULL WOOLDRIDGE R (same answer, done by hand - highlight and run):
library(wooldridge)
reg3 <- lm(log(bwght) ~ log(faminc), data = bwght)
summary(reg3)               # slope on log(faminc) = 0.0206
coef(reg3)[2] * 20          # <<< change 20 to the quiz's % -> 0.41

# (other log-log, slope GIVEN in the question)
# EXAMPLE: slope 0.35, x rises 10%
b   <- 0.35
pct <- 10
q3g(b, pct)                 # -> 3.50

# YOUR QUIZ:
b   <- NA                   # <<< FILL IN: the slope
pct <- NA                   # <<< FILL IN: % change in x
q3g(b, pct)


# ==================================================================
#  VARIANCE / STANDARD ERRORS
# ==================================================================

# ---- #4 - unbiasedness ---- no math:
#  MEANING:  unbiased = E(b1-hat) = b1 = right ON AVERAGE across samples.
#            Single estimates still differ from b1 and from each other.
#  WRONG:    "all positive", "no spread / Var = 0", "every one equals b1".
#   "The AVERAGE of the 1,000 estimates is close to b1"

# ---- #6 - select all ---- no math:
#  FORMULA:  Var(b1-hat) = sigma^2 / SST_X
#            Bigger numerator OR smaller denominator -> bigger variance.
#            SST_X shrinks when X is bunched tighter or the sample is smaller.
#            The true b1 is not in the formula, so it does nothing.
#   [x] larger sigma^2   [x] X bunched tighter   [x] smaller sample


# ---- #7 - se from n, sigma-hat, SD of X ----
#  SPOT IT:  gives n, sigma-hat, and the "sample standard deviation of X".
#  STEPS:    SST_X = (n - 1) * sx^2   ->   se = sigma-hat / sqrt(SST_X)
#  TRAP:     sx is a standard deviation: it must be SQUARED (q7 does this).
#            Use n - 1, not n.

# EXAMPLE: n = 8, sigma-hat = 6.8, sx = 2.8
n     <- 8
sigma <- 6.8
sx    <- 2.8
q7(n, sigma, sx)            # -> 0.92

# YOUR QUIZ:
n     <- NA                 # <<< FILL IN: number of observations
sigma <- NA                 # <<< FILL IN: sigma-hat
sx    <- NA                 # <<< FILL IN: standard deviation of X
q7(n, sigma, sx)

# (variant: x values listed + SSR given)
# EXAMPLE: x = (3, 5, 6, 8, 9), SSR = 37
x   <- c(3, 5, 6, 8, 9)
SSR <- 37
q7x(x, SSR)                 # -> 0.74

# YOUR QUIZ:
x   <- c(NA, NA, NA, NA, NA)  # <<< FILL IN: every x value
SSR <- NA                     # <<< FILL IN: SSR
q7x(x, SSR)


# ---- #9 - sample k times as large ----
#  SPOT IT:  "if the sample were 10 times as large" with X spread the same way.
#  MEANING:  SST_X grows k times, so sd = sigma / sqrt(SST_X) shrinks by sqrt(k).
#  TRAP:     divide by sqrt(k), NOT by k.  1.84/10 = 0.18 is WRONG.

# EXAMPLE: sd = 1.84, sample 10 times as large
sd_old <- 1.84
k      <- 10
q9(sd_old, k)               # -> 0.58

# YOUR QUIZ:
sd_old <- NA                # <<< FILL IN: current sd(b1-hat)
k      <- NA                # <<< FILL IN: how many times larger
q9(sd_old, k)


# ==================================================================
#  MULTIPLE REGRESSION
# ==================================================================

# ---- #12 - predicted value ----  y-hat = b0 + b1*x1 + b2*x2
#  SPOT IT:  "find the predicted value" at given x values (LEVELS).
#  RULE:     KEEP the intercept. Plug in every x.
#  TRAP:     if a coefficient has a minus sign in the equation, enter it
#            negative (e.g. y = 3 - 0.6x1  ->  b1 <- -0.6).

# EXAMPLE: y-hat = 3 + 0.6x1 + 1.2x2 at x1 = 7, x2 = 14
b0 <- 3
b1 <- 0.6
b2 <- 1.2
x1 <- 7
x2 <- 14
q12(b0, b1, b2, x1, x2)     # -> 24.00

# YOUR QUIZ:
b0 <- NA                    # <<< FILL IN: intercept
b1 <- NA                    # <<< FILL IN: coefficient on x1 (keep its sign)
b2 <- NA                    # <<< FILL IN: coefficient on x2 (keep its sign)
x1 <- NA                    # <<< FILL IN: value of x1
x2 <- NA                    # <<< FILL IN: value of x2
q12(b0, b1, b2, x1, x2)


# ---- #13 - predicted change ----  (intercept ignored)
#  SPOT IT:  "if x1 rises by ... and x2 rises by ..., by how much does y-hat CHANGE?"
#  RULE:     change = b1*dx1 + b2*dx2. The intercept does NOT matter.
#  TRAP:     y = 6 + 1.1x1 - 2.1x2 means b2 = -2.1 (keep the minus).
#            A variable that "falls" gets a negative change.

# EXAMPLE: y-hat = 6 + 1.1x1 - 2.1x2, x1 up 6, x2 up 1
b1  <- 1.1
b2  <- -2.1
dx1 <- 6
dx2 <- 1
q13(b1, b2, dx1, dx2)       # -> 4.50

# YOUR QUIZ:
b1  <- NA                   # <<< FILL IN: coefficient on x1 (keep its sign)
b2  <- NA                   # <<< FILL IN: coefficient on x2 (keep its sign)
dx1 <- NA                   # <<< FILL IN: change in x1
dx2 <- NA                   # <<< FILL IN: change in x2
q13(b1, b2, dx1, dx2)


# ---- #14 - wage1 predicted wage ----
#  SPOT IT:  lm(wage ~ educ + exper + tenure, data = wage1), "predicted hourly wage".
#  RULE:     intercept + 0.599*educ + 0.022*exper + 0.169*tenure (q14 uses
#            the unrounded coefficients for you).
#  TRAP:     put each number in the right slot: educ, THEN exper, THEN tenure.
#            Answer is in dollars per hour.

# EXAMPLE: educ 14, exper 10, tenure 5
educ   <- 14
exper  <- 10
tenure <- 5
q14(educ, exper, tenure)    # -> 6.58

# YOUR QUIZ:
educ   <- NA                # <<< FILL IN: years of education
exper  <- NA                # <<< FILL IN: years of experience
tenure <- NA                # <<< FILL IN: years of tenure
q14(educ, exper, tenure)

# FULL WOOLDRIDGE R (same answer, done by hand - highlight and run):
library(wooldridge)
reg <- lm(wage ~ educ + exper + tenure, data = wage1)
summary(reg)
b <- reg$coefficients
b[1] + b[2]*14 + b[3]*10 + b[4]*5     # <<< swap in educ, exper, tenure -> 6.58
predict(reg, data.frame(educ = 14, exper = 10, tenure = 5))   # same check


# ---- #15 - wage1 change ----  (0 for anything that doesn't change)
#  SPOT IT:  same wage1 regression, "by how much does predicted wage CHANGE when
#            tenure rises by 3, holding education and experience fixed".
#  RULE:     change = tenure coefficient (0.169) * change in tenure. No intercept.
#  NOTE:     if the quiz changes educ or exper instead, put the change there
#            and 0 for tenure.

# EXAMPLE: tenure rises 3, educ and exper fixed
d_tenure <- 3
d_educ   <- 0
d_exper  <- 0
q15(d_tenure, d_educ, d_exper)   # -> 0.51

# YOUR QUIZ:
d_tenure <- NA              # <<< FILL IN: change in tenure
d_educ   <- 0               #     change in education (usually 0)
d_exper  <- 0               #     change in experience (usually 0)
q15(d_tenure, d_educ, d_exper)

# FULL WOOLDRIDGE R (same answer, done by hand - highlight and run):
library(wooldridge)
reg <- lm(wage ~ educ + exper + tenure, data = wage1)
b <- reg$coefficients
b[4] * 3                    # <<< change 3 to the quiz's tenure change -> 0.51
#  (b[2] = educ, b[3] = exper, b[4] = tenure)


# ==================================================================
#  BONUS - other Wooldridge problems from PS10 (NOT on Quiz 9,
#  included in case the quiz swaps datasets)
# ==================================================================

# ---- PS10 #10 - se by hand, ceosal1 ----
reg10 <- lm(salary ~ roe, data = ceosal1)
n10   <- nobs(reg10)
sig10 <- sqrt(sum(resid(reg10)^2) / (n10 - 2))
sst10 <- sum((ceosal1$roe - mean(ceosal1$roe))^2)
sig10 / sqrt(sst10)         # se by hand
summary(reg10)              # check: "Std. Error" on roe

# ---- PS10 #16 - log-level piece, ceosal1 ----
reg16 <- lm(log(salary) ~ log(sales) + roe, data = ceosal1)
b16 <- reg16$coefficients
100 * b16[3] * 10           # % change in salary when roe rises 10

# ---- PS10 #17 - predicted salary, ceosal1 ----
exp(b16[1] + b16[2]*log(5000) + b16[3]*15)   # sales 5000, roe 15 (thousands $)

# ---- PS10 #21 - omitted variable, wage1 ----
reg21a <- lm(wage ~ educ + exper, data = wage1)
reg21b <- lm(exper ~ educ, data = wage1)
coef(reg21a)[3] * coef(reg21b)[2]

}
