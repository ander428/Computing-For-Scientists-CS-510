library(tidyverse)
library(rstanarm)
library(yardstick)

options(scipen = 999)
set.seed(1818)

### define functions ###

# create linear regression algorithm using linear algebra
linear_regression <- function(f, df) {
  y <- model.frame(f, data=df)[,1]
  X <- model.matrix(f, data = df)
  
  # Calculate Coefficients
  # Betas = (X^T * X)^-1 * X^T * y
  Betas <- solve(t(X) %*% X) %*% t(X) %*% y
  
  Betas
}

# reformat regression coefs for easy view
lm_summary <- function(lm_mod) {
  summary_df <- data.frame(
    mean = sapply(lm_mod, mean),
    median = sapply(lm_mod, median),
    sd = sapply(lm_mod, sd),
    row.names = names(lm_mod))
  summary_df
}

# make linear regression predictions using coefs
# type 0 = sampling regression method
# type 1 = classical regression method
predict_lm <- function(lm_boot, newdata, type) {
  preds <- NULL
  X <- newdata %>% select(rownames(lm_boot[-1,])) # only grab relevant columns
  
  for(i in 1:nrow(X)) {
    preds$mean[i] <- lm_boot[1,1] # mean intercept
    preds$median[i] <- lm_boot[1,2] # median intercept
    for(j in 1:ncol(X)) {
      preds$mean[i] <- preds$mean[i] + (lm_boot[j+1, 1] * X[i,j])
      preds$median[i] <- preds$median[i] + (lm_boot[j+1, 2] * X[i,j])
    }
  }
  preds
}

### data cleaning ###

kickstarters <- read.csv("ks-projects-201801.csv")

# view data frame structure
kickstarters %>% glimpse()

kickstarters_clean <- kickstarters %>%
  # create length variable
  mutate(length = as.numeric(as.Date(deadline)-as.Date(launched))) %>%
  # remove unneeded variables
  select_if(is.numeric) %>%
  select(-c(ID, goal, pledged, usd.pledged))

### modeling ###

# create regression formula
formula_1 <- usd_pledged_real ~ backers + usd_goal_real + length

# classical regression
lm_mod <- linear_regression(formula_1, kickstarters_clean)

# bootstrap classical linear regression
n_boot <- 1000
n_samples <- 100
mod_boot <- data.frame()
for(i in 1:n_boot) {
  idx <- sample(1:nrow(kickstarters_clean), n_samples)
  mod_temp <- linear_regression(formula_1, kickstarters_clean %>% slice(idx))
  mod_boot <- rbind(mod_boot, t(mod_temp))
}

# bayesian linear regression
bayes_mod <- stan_glm(formula_1, data=kickstarters_clean, seed=1818)
summary(bayes_mod)

rm(kickstarters, mod_temp) # clear memory of uneeded variabels

# predictions
lm_coefs <- t(lm_mod)
preds_lm <- predict_lm(lm_coefs, kickstarters_clean)
print(preds_lm)

boot_coefs <- lm_summary(mod_boot)
preds_boot <- predict_lm(boot_coefs, kickstarters_clean)

bayes_coefs <- data.frame(mean = bayes_mod$stan_summary[1:length(kickstarters_clean),1],
                          median = bayes_mod$coefficients)

preds_bayes <- predict_lm(bayes_coefs, kickstarters_clean)

preds_df <- data.frame(
  boot_mean = preds_boot$mean,
  boot_median = preds_boot$median,
  bayes_mean = preds_bayes$mean,
  bayes_median = preds_bayes$median,
  true = kickstarters_clean$usd_pledged_real
)

metric_set(kickstarters_clean, preds_df$true, preds_boot$mean)
m_set <- metric_set(rsq, rmse, mae)
m_set(kickstarters_clean, preds_df$true, preds_df$boot_mean)
# .metric .estimator .estimate
# <chr>   <chr>          <dbl>
# 1 rsq     standard       0.353
# 2 rmse    standard   85129.   
# 3 mae     standard    7274.  

m_set(kickstarters_clean, preds_df$true, preds_df$boot_median)
# .metric .estimator .estimate
# <chr>   <chr>          <dbl>
# 1 rsq     standard       0.542
# 2 rmse    standard   61707.   
# 3 mae     standard    5461. 

m_set(kickstarters_clean, preds_df$true, preds_df$bayes_mean)
# A tibble: 3 x 3
# .metric .estimator .estimate
# <chr>   <chr>          <dbl>
# 1 rsq     standard       0.566
# 2 rmse    standard   59910.   
# 3 mae     standard    5893.   

m_set(kickstarters_clean, preds_df$true, preds_df$bayes_median)
# A tibble: 3 x 3
# .metric .estimator .estimate
# <chr>   <chr>          <dbl>
# 1 rsq     standard       0.566
# 2 rmse    standard   59910.   
# 3 mae     standard    5893.

# All regression methods seemed to 
