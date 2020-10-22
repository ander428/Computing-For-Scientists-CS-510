library(tidyverse)
library(rstanarm)

options(scipen = 999)
set.seed(1818)

kickstarters <- read.csv("Midterm/ks-projects-201801.csv")

# view data frame structure
kickstarters %>% glimpse()

kickstarters_clean <- kickstarters %>%
  # create length variable
  mutate(length = as.numeric(as.Date(deadline)-as.Date(launched))) %>%
  # remove unneeded variables
  select_if(is.numeric) %>%
  select(-c(ID, goal, pledged, usd.pledged))

# create linear regression algorithm using linear algebra
linear_regression <- function(f, df) {
  y <- model.frame(f, data=df)[,1]
  X <- model.matrix(f, data = df)
  
  # Calculate Coefficients
  # Betas = (X^T * X)^-1 * X^T * y
  Betas <- solve(t(X) %*% X) %*% t(X) %*% y
  
  Betas
}








      