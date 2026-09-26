# Extracted from regression.qmd, first R chunk.
# Original course code; not re-executed for this website refresh.

#| code-fold: true
#| code-summary: "Show the code"
#| eval: false

##################################################################
# This code generates the numerical results in chapter 4         #
##################################################################

# install.packages("WR")
library(WR)
library(tidyverse)
library(knitr) # for formatted table output

# load the data
data(non_ischemic)
# head(non_ischemic)
#> non_ischemic dataset containing columns for ID, time, status, trt_ab, and covariates

# -------------------------------------------------------------------------
# Descriptive Analysis
# -------------------------------------------------------------------------

# function to convert 1-0 to Yes-No strings
one_zero_to_yn <- function(x) {
  if_else(x == 1, "Yes", "No")
}

# clean up data
df <- non_ischemic |>
  filter(status != 2) |>  # remove rows where status=2 (duplicates or not needed)
  mutate(
    trt_ab = fct(if_else(trt_ab == 0, "Usual care", "Training")), 
    # Convert trt_ab=0 -> "Usual care", 1 -> "Training"
    
    sex = if_else(sex == 1, "Female", "Male"),
    # Convert numeric sex variable to "Female"/"Male"
    
    race = case_when(
      Black.vs.White == 1 ~ "Black",
      Other.vs.White == 1 ~ "Other",
      Black.vs.White == 0 & Other.vs.White == 0 ~ "White"
    ),
    # Recode race based on binary indicators
    
    race = fct(race), # Convert to factor
    
    across(hyperten:smokecurr, one_zero_to_yn)
    # Convert all these binary variables to "Yes"/"No" strings
  )

## A function to compute median (IQR) for a numeric vector 'x', 
## rounded to the r-th decimal place
med_iqr <- function(x, r = 1) {
  qt <- quantile(x, na.rm = TRUE)  # Calculate quartiles
  str_c(
    round(qt[3], r), " (",  # Median
    round(qt[2], r), ", ",  # Q1
    round(qt[4], r), ")"    # Q3
  )
}

# create summary table for quantitative variables
# We summarize across arms: 'age', 'bmi', and 'bipllvef' (Left Ventricular Ejection Fraction)
tab_quant <- df |>
  group_by(trt_ab) |>
  summarize(
    across(c(age, bmi, bipllvef), med_iqr)
  ) |>
  pivot_longer(
    !trt_ab,
    values_to = "value",
    names_to = "name"
  ) |>
  pivot_wider(
    values_from = value,
    names_from = trt_ab
  ) |>
  mutate(
    name = case_when(
      name == "age" ~ "Age (years)",
      name == "bmi" ~ "BMI",
      name == "bipllvef" ~ "LVEF (%)"
    )
  )

## A function that computes N (%) for each level of `var`,
## grouped by 'group' in df (percent rounded to r-th decimal).
freq_pct <- function(df, group, var, r = 1) {
  # Tally up the count n for each level of 'var' by 'group'
  var_counts <- df |>
    group_by({{ group }}, {{ var }}) |>
    summarize(
      n = n(),
      .groups = "drop"
    )
  
  # Join with the total count N in each group, then compute "n (xx%)"
  var_counts |>
    left_join(
      var_counts |>
        group_by({{ group }}) |>
        summarize(N = sum(n)),
      by = join_by({{ group }})
    ) |>
    mutate(
      value = str_c(n, " (", round(100 * n / N, r), "%)")
    ) |>
    select(-c(n, N)) |>
    pivot_wider(
      names_from = {{ group }},
      values_from = value
    ) |>
    rename(
      name = {{ var }}
    )
}

# Apply freq_pct() to sex, race, hyperten:smokecurr

# sex
sex <- df |>
  freq_pct(trt_ab, sex) |>
  mutate(
    name = str_c("Sex - ", name) 
    # e.g., "Sex - Female", "Sex - Male"
  )

# race
race <- df |>
  freq_pct(trt_ab, race) |>
  mutate(
    name = str_c("Race - ", name)
    # e.g., "Race - Black", "Race - White", etc.
  )

hyperten <- df |>
  freq_pct(trt_ab, hyperten) |>
  filter(name == "Yes") |>
  mutate(
    name = "Hypertension"
  )

# The following block is repeated as in the original code:
hyperten <- df |>
  freq_pct(trt_ab, hyperten) |>
  filter(name == "Yes") |>
  mutate(
    name = "Hypertension"
  )

COPD <- df |>
  freq_pct(trt_ab, COPD) |>
  filter(name == "Yes") |>
  mutate(
    name = "COPD"
  )

diabetes <- df |>
  freq_pct(trt_ab, diabetes) |>
  filter(name == "Yes") |>
  mutate(
    name = "Diabetes"
  )

acei <- df |>
  freq_pct(trt_ab, acei) |>
  filter(name == "Yes") |>
  mutate(
    name = "ACE Inhibitor"
  )

betab <- df |>
  freq_pct(trt_ab, betab) |>
  filter(name == "Yes") |>
  mutate(
    name = "Beta Blocker"
  )

smokecurr <- df |>
  freq_pct(trt_ab, betab) |>  # This line references betab for grouping
  filter(name == "smokecurr") |>
  mutate(
    name = "Smoker"
  )

# Combine all the partial tables (quantitative + categorical summaries)
tabone <- bind_rows(
  tab_quant[1, ],
  sex,
  race,
  tab_quant[2:3, ],
  hyperten,
  COPD,
  diabetes,
  acei,
  betab,
  smokecurr
)

## Add the group sample size (N=...) to column names
colnames(tabone) <- c(
  " ",
  str_c(
    colnames(tabone)[2:3],
    " (N=", table(df$trt_ab), ")"
  )
)

## Print out the final table in a formatted manner
kable(tabone, align = c("lcc"))


# PW regression analysis --------------------------------------------------
# The following code chunk is not executed by default (eval=FALSE) but 
# shows how to fit piecewise regression using 'pwreg()'.

# re-label the covariates with informative names.
colnames(non_ischemic)[4:16] = c(
  "Training vs Usual", "Age (year)", "Male vs Female", "Black vs White", 
  "Other vs White", "BMI", "LVEF", "Hypertension", "COPD", "Diabetes",
  "ACE Inhibitor", "Beta Blocker", "Smoker"
)

p <- ncol(non_ischemic) - 3

# extract ID, time, status, and covariates matrix Z
# note that ID, time, and status should be column vectors
ID <- non_ischemic[, "ID"]
time <- non_ischemic[, "time"] / 30.5 # convert days to months
status <- non_ischemic[, "status"]
Z <- as.matrix(non_ischemic[, 4:(3 + p)])

# pass parameters into the function
obj <- pwreg(ID, time, status, Z)
obj
#> Displays the main results from piecewise regression

# extract estimates of (beta_4, beta_5)
beta <- matrix(obj$beta[4:5])
# extract estimated covariance matrix for (beta_4, beta_5)
Sigma <- obj$Var[4:5, 4:5]

# compute chisq statistic in quadratic form to jointly test beta_4, beta_5
chistats <- t(beta) %*% solve(Sigma) %*% beta

# compare the Wald statistic with a chisq(2) reference distribution
1 - pchisq(chistats, df = 2)
#> example p-value for the joint test

# compute score processes
score_obj <- score.proc(obj)

# plot scores for all 13 covariates to check time-varying effects
par(mfrow = c(4, 4))
for (i in 1:13) {
  plot(score_obj, k = i, xlab = "Time (months)")
  # add reference lines for approx. +/-2 standard errors
  abline(a = 2, b = 0, lty = 3)
  abline(a = 0, b = 0, lty = 3)
  abline(a = -2, b = 0, lty = 3)
}

### Exercise: Stratify by sex ###
# Drop the sex variable from Z (assuming it's at column 3)
sex <- Z[, 3]
Zs <- Z[, -3]
obj_str <- pwreg(ID, time, status, Zs, strata = sex)
obj_str
#> Fits a PW regression model stratified by sex
