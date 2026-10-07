## =====================================================================
## ECN 377 - Exam I script   (Jack Busbice)
## Covers Days 1-12 (through R^2).  Matches Section 0 of my reference sheet.
##
## HOW TO USE
##   1. Open this file in RStudio (File -> Open File).
##   2. Run Part 1 ONCE (select it, Ctrl+Enter / Cmd+Enter). It loads data
##      and defines helper functions.
##   3. For each exam question, scroll to the matching recipe, change the
##      numbers, and run those lines. Keep your work in THIS file.
##   4. Round final answers to the hundredths.
##   5. Save (Ctrl+S) and submit this script with the exam -- partial credit
##      requires the script.
## =====================================================================


## =====================================================================
## PART 1: SETUP  (run once at the start)
## =====================================================================
library(wooldridge)          # if error: install.packages("wooldridge")

# r2(x): round to hundredths for the final answer
r2 <- function(x) round(x, 2)

# Percent change from old to new
pct_change <- function(old, new) (new - old) / old * 100

# Everything from ONE-variable probability table: values x, probs p
ptable1 <- function(x, p) {
  if (abs(sum(p) - 1) > 1e-8) warning("probabilities do not sum to 1!")
  EX  <- sum(x * p)
  EX2 <- sum(x^2 * p)
  VarX <- EX2 - EX^2
  c(EX = EX, EX2 = EX2, VarX = VarX, sdX = sqrt(VarX))
}

# Everything from a JOINT probability table: x, y, p in the SAME column order
ptable2 <- function(x, y, p) {
  if (abs(sum(p) - 1) > 1e-8) warning("probabilities do not sum to 1!")
  EX <- sum(x * p);  EY <- sum(y * p)
  EX2 <- sum(x^2 * p); EY2 <- sum(y^2 * p)
  EXY <- sum(x * y * p)
  VarX <- EX2 - EX^2; VarY <- EY2 - EY^2
  CovXY <- EXY - EX * EY
  c(EX = EX, EY = EY, EX2 = EX2, EY2 = EY2, EXY = EXY,
    VarX = VarX, VarY = VarY, sdX = sqrt(VarX), sdY = sqrt(VarY),
    CovXY = CovXY, CorXY = CovXY / (sqrt(VarX) * sqrt(VarY)))
}

# Sample statistics for two lists of numbers (data, NOT a probability table)
sample_stats <- function(x, y) {
  c(n = length(x), xbar = mean(x), ybar = mean(y),
    varx = var(x), vary = var(y), sdx = sd(x), sdy = sd(y),
    cov = cov(x, y), cor = cor(x, y))
}

# OLS by hand from two lists of numbers
ols_hand <- function(x, y) {
  b1 <- cov(x, y) / var(x)
  b0 <- mean(y) - b1 * mean(x)
  c(b0 = b0, b1 = b1)
}

# Full regression report: everything an Exam I question can ask about
#   example: slr(wage ~ educ, wage1)
slr <- function(formula, data) {
  reg <- lm(formula, data = data)
  y   <- model.response(model.frame(reg))
  SST <- sum((y - mean(y))^2)
  SSR <- sum(reg$residuals^2)
  SSE <- SST - SSR
  cat("b0hat =", reg$coefficients[1], "\n")
  cat("b1hat =", reg$coefficients[2], "\n")
  cat("n     =", length(y), "\n")
  cat("SST   =", SST, "\nSSE   =", SSE, "\nSSR   =", SSR, "\n")
  cat("R^2   =", summary(reg)$r.squared, "\n")
  invisible(reg)
}


## =====================================================================
## A. PLAIN NUMBERS (a list of data)
## =====================================================================
x <- c(4, 9, 7, 3)            # <-- change
y <- c(2, 7, 5, 3)            # <-- change (same length & order as x)

sum(x)
mean(x)
var(x)                        # sample variance (n - 1)
sd(x)                         # sample sd
cov(x, y)                     # sample covariance
cor(x, y)                     # sample correlation
sample_stats(x, y)            # all of the above at once

# Variance by hand (if asked to show steps)
sum((x - mean(x))^2) / (length(x) - 1)

# Percent vs percentage-point change
pct_change(old = 80.8, new = 92)   # percent change      <-- change
5 - 3                              # percentage-POINT change (just new - old)
0.25 * 100                         # proportion -> percent

# Logs / exponentials
exp(2.62)
log(10)                            # natural log


## =====================================================================
## B. PROBABILITY TABLE   (NEVER use var/sd/cov/cor here!)
## =====================================================================
x <- c(1, 8, 2)               # values of X           <-- change
y <- c(9, 3, 6)               # values of Y           <-- change
p <- c(1/4, 1/2, 1/4)         # probabilities         <-- change
sum(p)                        # must be 1

ptable1(x, p)                 # E[X], E[X^2], Var(X), sd(X)
ptable2(x, y, p)              # adds E[Y], E[XY], Cov, Corr

# Any other expectation:  E[g(X,Y)] = sum( g(x,y) * p )
sum(x * y^2 * p)              # E[X Y^2]
sum(x^2 * y * p)              # E[X^2 Y]
sum(log(x + y) * p)           # E[log(X+Y)]
sum(sin(x) * exp(y) * p)      # E[sin(X) e^Y]

# Common tables
x <- 1:6; p <- rep(1/6, 6); ptable1(x, p)                      # fair die
x <- c(0,0,1,1); y <- c(0,1,0,1); p <- rep(1/4, 4); ptable2(x, y, p)  # 2 fair coins

# Conditional E[Y | X = x0] from a joint table
x <- c(0,0,1,1); y <- c(0,1,0,1); p <- c(0.4, 0.1, 0.1, 0.4)   # <-- change
x0  <- 1                                                     # <-- change
sel <- (x == x0)
pc  <- p[sel] / sum(p[sel])                # conditional probabilities
EYgx <- sum(y[sel] * pc)                   # E[Y | X = x0]
VYgx <- sum(y[sel]^2 * pc) - EYgx^2        # Var(Y | X = x0)
EYgx; VYgx


## =====================================================================
## C. GIVEN SUMMARY NUMBERS (no data) -- just a calculator
## =====================================================================
sqrt(5.3 - (-1.9)^2)          # sd(X) from E[X^2]=5.3, E[X]=-1.9
1 / (sqrt(6.9) * sqrt(5.4))   # Corr from Cov=1, Var(X)=6.9, Var(Y)=5.4
4*2 + 5*3 - 3                 # E[4X+5Y-3] with E[X]=2, E[Y]=3

VarX <- 2; VarY <- 3; CovXY <- 1; a <- 4; b <- -2      # <-- change
a^2 * VarX                                 # Var(aX + b)   (b drops out)
a^2*VarX + b^2*VarY + 2*a*b*CovXY          # Var(aX + bY)

# OLS from summary numbers
SXY <- 4; SX2 <- 8; Ybar <- 2; Xbar <- 6               # <-- change
b1hat <- SXY / SX2
b0hat <- Ybar - b1hat * Xbar
b1hat; b0hat
# from correlation:  b1hat = cor * sy / sx

# Prediction and predicted change
b0 <- 3; b1 <- 7                                       # <-- change
b0 + b1 * 3981                # prediction at x = 3981
b1 * 20                       # predicted change when x rises by 20

# Residual = actual - fitted   (positive -> model UNDER-predicted)
9.0 - (-0.90 + 0.54 * 12)

# Sums of squares / R^2
SST <- 200; SSR <- 150                                 # <-- change
SSE <- SST - SSR
R2  <- SSE / SST              # = 1 - SSR/SST
1 - R2                        # fraction UNexplained
# simple regression: R^2 = cor(x, y)^2
# ALWAYS: sum of residuals = 0; cov(x, residuals) = 0; line passes (xbar, ybar)


## =====================================================================
## D. WOOLDRIDGE DATA
## =====================================================================
?wage1                        # variable definitions + UNITS
nrow(wage1)                   # n
ls(wage1)                     # variable names (check spelling!)

mean(wage1$wage)
var(wage1$wage); sd(wage1$wage)
cor(wage1$wage, wage1$educ)
cov(bwght$motheduc, bwght$bwght, use = "p")    # NAs -> use = "p"
mean(bwght$motheduc, na.rm = TRUE)            # NAs -> na.rm = TRUE

# --- Regression: "regress Y on X"  ->  lm(Y ~ X) ---
reg <- slr(wage ~ educ, wage1)        # prints b0, b1, SST, SSE, SSR, R^2  <-- change
b0hat <- reg$coefficients[1]
b1hat <- reg$coefficients[2]

b0hat + b1hat * 16            # prediction at x = 16           <-- change
b1hat * 4                     # predicted change, x up by 4     <-- change
reg$fitted.values[200]        # fitted value, observation #200  <-- change
reg$residuals[200]            # residual, observation #200
summary(reg)$r.squared        # R^2
sum(reg$residuals^2)          # SSR

# Check OLS by hand matches lm()
ols_hand(wage1$educ, wage1$wage)

# OLS properties (always true)
sum(reg$residuals)                        # ~ 0
sum(wage1$educ * reg$residuals)           # ~ 0
b0hat + b1hat * mean(wage1$educ); mean(wage1$wage)   # equal

# --- Conditional means / subsets ---
mean(wage1$wage[wage1$educ == 12])        # E[wage | educ = 12]
var(wage1$wage[wage1$educ == 12])         # Var(wage | educ = 12)
mean(wage1$wage[wage1$female == 1])       # average wage of women

# --- Counting ---
sum(ceosal2$ceoten == 0)                          # how many
sum(ceosal2$ceoten == 0 & ceosal2$salary >= 600)  # BOTH (&)
sum(ceosal2$ceoten == 0 | ceosal2$salary >= 600)  # EITHER (|)
mean(wage1$female == 1)                           # proportion


## =====================================================================
## E. CLASS EXAMPLES (to check answers against known values)
## =====================================================================
slr(wage ~ educ, wage1)       # b0 = -0.90, b1 = 0.54, R^2 = 0.16
slr(salary ~ roe, ceosal1)    # b0 = 963.19, b1 = 18.50  (salary in $1000s)
slr(voteA ~ shareA, vote1)    # Example 2.5
slr(bwght ~ cigs, bwght)      # R^2 ~ 0.02
slr(colgpa ~ hsrank, gpa2)    # 2025 exam-review question
slr(price ~ sqrft, hprice1)
slr(wage ~ looks, beauty)


## =====================================================================
## EXAM ANSWERS -- write each question below as you go
## =====================================================================
# Q1


# Q2


# Q3

