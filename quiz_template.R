# =====================================================================
#  ECON QUIZ TEMPLATE — Problems 1–12
#  How to use: for each question, replace the NA values with the numbers
#  from the quiz, then highlight that block and run it (Ctrl/Cmd + Enter).
#  Every answer is rounded to the hundredths.
# =====================================================================
library(wooldridge)   # needed for Q11 (install.packages("wooldridge") once)


# ---------------------------------------------------------------------
# Q1 — Zero conditional mean (concept, no code)
# Question: y = b0 + b1*x + u, and some variable Z is inside u.
#           Z is correlated with x. Which statement is correct?
# HOW-TO:  If the thing left in u moves with x, then E(u|x) changes with x
#          -> the zero conditional mean assumption FAILS.
#          Traps: "u is always zero" (no), "b1 must be zero" (no),
#          "it holds because Z isn't in the model" (backwards: that's
#          exactly WHY it fails).
# ---------------------------------------------------------------------


# ---------------------------------------------------------------------
# Q2 — Which candidate line is OLS?
# Question: table of lines with sum(u), sum(|u|), sum(u^2). Pick OLS.
# HOW-TO:  (1) keep only lines where sum(u) = 0
#          (2) among those, pick the SMALLEST sum(u^2).  Ignore sum(|u|).
# ---------------------------------------------------------------------
lines  <- c("A", "B", "C", "D")
sum_u  <- c(NA, NA, NA, NA)     # first column
sum_u2 <- c(NA, NA, NA, NA)     # last column
ok <- sum_u == 0
lines[ok][which.min(sum_u2[ok])]


# ---------------------------------------------------------------------
# Q3 — Standard deviation of aX + b
# Question: given E[X] and E[X^2], find sd(aX + b).
# HOW-TO:  Var(X) = E[X^2] - (E[X])^2 ;  sd(X) = sqrt(Var(X))
#          sd(aX + b) = |a| * sd(X)      (b does nothing)
# ---------------------------------------------------------------------
EX  <- NA      # E[X]
EX2 <- NA      # E[X^2]
a   <- NA      # number multiplying X
round(abs(a) * sqrt(EX2 - EX^2), 2)


# ---------------------------------------------------------------------
# Q4 — Variance of aX + bY
# Question: given Var(X), Var(Y), Cov(X,Y), find Var(aX + bY).
# HOW-TO:  Var(aX + bY) = a^2 Var(X) + b^2 Var(Y) + 2ab Cov(X,Y)
#          If it says "aX - cY", then b = -c (b is NEGATIVE).
# ---------------------------------------------------------------------
vX  <- NA
vY  <- NA
cXY <- NA      # Cov(X,Y), keep its sign
a   <- NA
b   <- NA      # negative if it's a minus sign
round(a^2*vX + b^2*vY + 2*a*b*cXY, 2)


# ---------------------------------------------------------------------
# Q5 — Correlation from a joint distribution (may ask Cov instead)
# Question: table of x, y, P(X=x, Y=y). Find Corr(X,Y).
# HOW-TO:  weight EVERYTHING by p:
#          E[X]=sum(p*x), E[Y]=sum(p*y), E[XY]=sum(p*x*y)
#          Cov = E[XY] - E[X]E[Y]
#          Var(X) = sum(p*x^2) - E[X]^2   (same for Y)
#          Corr = Cov / sqrt(Var(X)*Var(Y))
#          Do NOT use cor(x, y) — it ignores the probabilities.
# ---------------------------------------------------------------------
x <- c(NA, NA, NA, NA)
y <- c(NA, NA, NA, NA)
p <- c(NA, NA, NA, NA)
EX  <- sum(p*x); EY <- sum(p*y)
cov <- sum(p*x*y) - EX*EY
vx  <- sum(p*x^2) - EX^2
vy  <- sum(p*y^2) - EY^2
round(cov, 2)                    # if they ask for Cov
round(cov / sqrt(vx*vy), 2)      # if they ask for Corr


# ---------------------------------------------------------------------
# Q6 — Residual of one observation
# Question: given x and y vectors, regress y on x, find residual of obs k.
# HOW-TO:  slope b1 = sum((x-xbar)(y-ybar)) / sum((x-xbar)^2)
#          intercept b0 = ybar - b1*xbar
#          residual = y_k - (b0 + b1*x_k)      (actual minus fitted)
# ---------------------------------------------------------------------
x <- c(NA, NA, NA, NA, NA)
y <- c(NA, NA, NA, NA, NA)
k <- NA                          # which observation (e.g. 3)
round(resid(lm(y ~ x))[k], 2)


# ---------------------------------------------------------------------
# Q7 — Missing residual
# Question: n residuals given except one; find the last.
# HOW-TO:  OLS residuals sum to 0, so missing = -(sum of the others).
# ---------------------------------------------------------------------
given <- c(NA, NA, NA, NA, NA)
round(-sum(given), 2)


# ---------------------------------------------------------------------
# Q8 — sum(x_i * uhat_i)   (or sum(uhat_i))
# HOW-TO:  ANSWER IS ALWAYS 0. Both are OLS first-order conditions.
#          Code below just confirms; e-15 style output means 0.
# ---------------------------------------------------------------------
x <- c(NA, NA, NA, NA, NA, NA)
y <- c(NA, NA, NA, NA, NA, NA)
round(sum(x * resid(lm(y ~ x))), 2)


# ---------------------------------------------------------------------
# Q9 — SSR from SSE and R^2
# HOW-TO:  R^2 = SSE/SST  ->  SST = SSE / R^2
#          SST = SSE + SSR ->  SSR = SST - SSE   (don't stop at SST)
#          (If they give SSR and R^2 instead: SST = SSR/(1-R^2).)
# ---------------------------------------------------------------------
SSE <- NA
R2  <- NA
SST <- SSE / R2
round(SST, 2)          # in case they ask for SST
round(SST - SSE, 2)    # SSR


# ---------------------------------------------------------------------
# Q10 — SST from sample variance
# HOW-TO:  s^2 = SST/(n-1)  ->  SST = (n-1) * s^2
#          (If given sd instead of variance, square it first.)
# ---------------------------------------------------------------------
n  <- NA
s2 <- NA
round((n - 1) * s2, 2)


# ---------------------------------------------------------------------
# Q11 — wage1 residual for a given row
# HOW-TO:  run the regression, pull residual for that row.
#          Check the variable they name (tenure, educ, exper...).
# ---------------------------------------------------------------------
row <- NA
reg <- lm(wage ~ tenure, data = wage1)   # change tenure if needed
round(reg$residuals[row], 2)
# check by hand: wage1$wage[row] - fitted(reg)[row]


# ---------------------------------------------------------------------
# Q12 — Mean of y from the OLS line
# HOW-TO:  (xbar, ybar) is always on the OLS line:  ybar = b0 + b1*xbar
#          (If they give ybar and ask xbar: xbar = (ybar - b0)/b1.)
# ---------------------------------------------------------------------
b0   <- NA
b1   <- NA
xbar <- NA
round(b0 + b1*xbar, 2)
