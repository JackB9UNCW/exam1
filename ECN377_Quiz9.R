# ==================================================================
#  ECN 377 - QUIZ 9 - everything in one script
#
#  HOW TO USE (no Source needed):
#    1. Find the question type below.
#    2. Change the numbers after  <-  to the quiz's numbers.
#    3. Highlight the whole block (from the first line to "ANSWER")
#       and press Cmd+Enter (Windows: Ctrl+Enter).
#    4. The answer prints, rounded to two decimals.
#
#  Every block already has the Problem Set 10 numbers in it, so you
#  can run it first to see the right answer, then swap in the quiz's.
#
#  Quiz 9 = PS10 #1, 2, 3, 4, 6, 7, 9, 12, 13, 14, 15.
#  15 minutes, 11 questions, round to the hundredths.
# ==================================================================


# ---------- RUN THIS ONCE AT THE START (loads the data) -----------
if (!requireNamespace("wooldridge", quietly = TRUE)) install.packages("wooldridge")
library(wooldridge)
# ------------------------------------------------------------------


# ##################################################################
#  QUICK CHEAT SHEET
# ##################################################################
#
#  WHERE IS THE LOG?
#    log on x only   y = a + b*log(x)   -> 1% rise in x = b/100 units of y
#    log on y only   log(y) = a + b*x   -> %change y = 100 * b * change in x
#    log on both     log(y) = a + b*log(x) -> %change y = b * %change x
#
#  #4  Unbiased -> "The AVERAGE of the 1,000 estimates is close to b1"
#  #6  Var(b1-hat) = sigma^2 / SST_X gets BIGGER with:
#        [x] larger error variance sigma^2
#        [x] X bunched more tightly (smaller SST_X)
#        [x] smaller sample
#        [ ] larger sample   [ ] larger true slope   <- do NOT check
#
#  PREDICTED VALUE  -> KEEP the intercept
#  PREDICTED CHANGE -> DROP the intercept
#
#  The INTERCEPT is the number standing alone (no x next to it).
#  A coefficient with a minus sign goes in NEGATIVE.
#  Anything that FALLS / DECREASES gets a NEGATIVE change.
# ##################################################################



# ==================================================================
#  #1 - LOG ON x ONLY - multiple choice
#  Looks like:  test score = 12 + 8*log(income). What does the 8 mean?
#  Rule: a 1% rise in x changes y by (number / 100) units.
#  Trap: "8%" and "8 points" are WRONG. Pick "about 0.08 points".
# ==================================================================
b <- 8                      # <<< number in front of log(x)
round(b / 100, 2)           # ANSWER -> 0.08 -> pick "about 0.08 points"



# ==================================================================
#  #2 - LOG ON y ONLY
#  Looks like:  log(y) = 0.9 + 0.07x. By what % does y change when x falls by 3?
#  Rule: %change in y = 100 * slope * change in x
#  Trap: the 0.9 is the intercept - NOT used.
#        "falls" -> change is NEGATIVE.
# ==================================================================
slope    <- 0.07            # <<< number next to x
change_x <- -3              # <<< change in x (NEGATIVE if it falls)
round(100 * slope * change_x, 2)          # ANSWER -> -21.00



# ==================================================================
#  #3 - LOG ON BOTH - bwght data (R question)
#  Looks like:  Run lm(log(bwght) ~ log(faminc), data = bwght).
#               By what % does birth weight change when income rises 20%?
#  Rule: %change in y = slope * %change in x   (slope = elasticity, no 100)
#  Trap: answer is a percent: 0.41, not 0.0041.
# ==================================================================
library(wooldridge)
reg3 <- lm(log(bwght) ~ log(faminc), data = bwght)
summary(reg3)               # slope on log(faminc) = 0.0206
pct_income <- 20            # <<< % rise in family income
round(coef(reg3)[[2]] * pct_income, 2)    # ANSWER -> 0.41



# ==================================================================
#  #4 - UNBIASEDNESS - multiple choice, no math
#  Pick: "The average of the 1,000 estimates is close to b1."
#  Unbiased = right ON AVERAGE. Single estimates still bounce around.
#  WRONG: "all positive", "no spread / Var = 0", "every one equals b1".
# ==================================================================



# ==================================================================
#  #6 - WHAT MAKES Var(b1-hat) BIGGER - select all, no math
#  Var(b1-hat) = sigma^2 / SST_X  -> bigger top or smaller bottom.
#  CHECK:   larger sigma^2  |  X bunched tighter  |  smaller sample
#  LEAVE:   larger sample   |  larger true b1
# ==================================================================



# ==================================================================
#  #7 - STANDARD ERROR from n, sigma-hat, and SD of X
#  Looks like:  n = 8, sigma-hat = 6.8, sample SD of X = 2.8. Find se(b1-hat).
#  Steps: SST_X = (n - 1) * sx^2  ->  se = sigma-hat / sqrt(SST_X)
#  Trap: SQUARE the SD. Use n - 1, not n.
# ==================================================================
n     <- 8                  # <<< number of observations
sigma <- 6.8                # <<< sigma-hat
sx    <- 2.8                # <<< standard deviation of X
SST_X <- (n - 1) * sx^2
round(sigma / sqrt(SST_X), 2)             # ANSWER -> 0.92


# ---- #7 variant: question lists the x values and gives SSR ----
x   <- c(3, 5, 6, 8, 9)     # <<< every x value
SSR <- 37                   # <<< SSR
sigma <- sqrt(SSR / (length(x) - 2))
SST_X <- sum((x - mean(x))^2)
round(sigma / sqrt(SST_X), 2)             # ANSWER -> 0.74



# ==================================================================
#  #9 - SAMPLE k TIMES AS LARGE
#  Looks like:  sd(b1-hat) = 1.84. If the sample were 10 times as large...
#  Rule: new sd = old sd / sqrt(k)
#  Trap: divide by sqrt(10), NOT by 10.  (0.18 is WRONG)
# ==================================================================
old_sd <- 1.84              # <<< current sd(b1-hat)
k      <- 10                # <<< how many times larger
round(old_sd / sqrt(k), 2)                # ANSWER -> 0.58



# ==================================================================
#  #12 - PREDICTED VALUE (multiple regression)
#  Looks like:  y-hat = 3 + 0.6x1 + 1.2x2. Find y-hat at x1 = 7, x2 = 14.
#  Rule: KEEP the intercept. Plug in both x's.
#  Trap: a minus sign in the equation -> enter that coefficient NEGATIVE.
# ==================================================================
intercept <- 3              # <<< number standing alone
b1 <- 0.6                   # <<< number next to x1 (keep sign)
b2 <- 1.2                   # <<< number next to x2 (keep sign)
x1 <- 7                     # <<< value of x1
x2 <- 14                    # <<< value of x2
round(intercept + b1*x1 + b2*x2, 2)       # ANSWER -> 24.00



# ==================================================================
#  #13 - PREDICTED CHANGE (multiple regression)
#  Looks like:  y-hat = 6 + 1.1x1 - 2.1x2. x1 rises by 6, x2 rises by 1.
#  Rule: change = b1*change_x1 + b2*change_x2. NO intercept.
#  Trap: "- 2.1x2" means b2 = -2.1.
# ==================================================================
b1 <- 1.1                   # <<< number next to x1 (keep sign)
b2 <- -2.1                  # <<< number next to x2 (keep sign)
change_x1 <- 6              # <<< change in x1 (negative if it falls)
change_x2 <- 1              # <<< change in x2 (negative if it falls)
round(b1*change_x1 + b2*change_x2, 2)     # ANSWER -> 4.50



# ==================================================================
#  #14 - PREDICTED WAGE - wage1 data (R question)
#  Looks like:  Run lm(wage ~ educ + exper + tenure, data = wage1).
#               Predicted wage for educ 14, exper 10, tenure 5?
#  Rule: intercept + b_educ*educ + b_exper*exper + b_tenure*tenure
#  Trap: keep the order: educ, exper, tenure. Answer in $ per hour.
# ==================================================================
library(wooldridge)
reg <- lm(wage ~ educ + exper + tenure, data = wage1)
summary(reg)
b <- reg$coefficients       # b[1] intercept, b[2] educ, b[3] exper, b[4] tenure
educ   <- 14                # <<< years of education
exper  <- 10                # <<< years of experience
tenure <- 5                 # <<< years of tenure
round(b[[1]] + b[[2]]*educ + b[[3]]*exper + b[[4]]*tenure, 2)   # ANSWER -> 6.58



# ==================================================================
#  #15 - CHANGE IN WAGE FROM TENURE - wage1 data (R question)
#  Looks like:  Same regression. Tenure rises by 3, educ and exper fixed.
#  Rule: change = tenure coefficient * change in tenure. NO intercept.
# ==================================================================
library(wooldridge)
reg <- lm(wage ~ educ + exper + tenure, data = wage1)
b <- reg$coefficients
change_tenure <- 3          # <<< change in tenure
round(b[[4]] * change_tenure, 2)          # ANSWER -> 0.51

# If the quiz changes educ or exper instead:
#   educ  -> round(b[[2]] * change, 2)
#   exper -> round(b[[3]] * change, 2)



# ##################################################################
#  BONUS - other PS10 Wooldridge problems (NOT on Quiz 9)
# ##################################################################

# PS10 #10 - se by hand, ceosal1
reg10 <- lm(salary ~ roe, data = ceosal1)
sigma10 <- sqrt(sum(resid(reg10)^2) / (nobs(reg10) - 2))
SST10   <- sum((ceosal1$roe - mean(ceosal1$roe))^2)
round(sigma10 / sqrt(SST10), 2)           # check against summary(reg10)

# PS10 #16 - % change in salary when roe rises 10, ceosal1
reg16 <- lm(log(salary) ~ log(sales) + roe, data = ceosal1)
b16 <- reg16$coefficients
round(100 * b16[[3]] * 10, 2)

# PS10 #17 - predicted salary, sales 5000, roe 15 (thousands of $)
round(exp(b16[[1]] + b16[[2]]*log(5000) + b16[[3]]*15), 2)
