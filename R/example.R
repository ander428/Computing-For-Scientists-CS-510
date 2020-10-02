library('tidyverse')

library("glmnet")
library("glmnetUtils")
options(scipen = 50)
set.seed(1861)
movies <- read.csv("./datasets/movie_metadata.csv")
getwd()
movies <- movies %>% filter(budget < 400000000) %>% 
  filter(content_rating != "",
         content_rating != "Not Rated",
         !is.na(gross)) 
movies <- movies %>% 
  mutate(genre_main = unlist(map(strsplit(as.character(movies$genres),"\\|"),1)),
         grossM = gross / 1000000,
         budgetM = budget / 1000000,
         profitM = grossM - budgetM)
movies <- movies %>% mutate(genre_main = fct_lump(genre_main,5),
                            content_rating = fct_lump(content_rating,3),
                            country = fct_lump(country,2),
                            cast_total_facebook_likes000s = 
                              cast_total_facebook_likes / 1000) %>%   drop_na()

train_idx <- sample(1:nrow(movies),size = floor(0.75*nrow(movies)))
movies_train <- movies %>% slice(train_idx)
movies_test <- movies %>% slice(-train_idx)

alpha_grid <- seq(0,1, length = 5)
alpha_grid

enet_mod <- cva.glmnet(profitM ~ ., data = movies_train, alpha = alpha_grid)
summary(enet_mod)
enet_mod$modlist[[2]]
minlossplot(enet_mod)

plot(enet_mod$modlist[[3]])
coef(enet_mod$modlist[[3]])

enet_coef <- data.frame(
  varname = rownames(coef(enet_mod$modlist[[1]])),
  ridge = as.matrix(coef(enet_mod[[1]])) %>% round(3),
  enet_025 = as.matrix(coef(enet_mod[[2]])) %>% round(3),
  enet_05 = as.matrix(coef(enet_mod[[3]])) %>% round(3),
  enet_075 = as.matrix(coef(enet_mod[[4]])) %>% round(3),
  lasso = as.matrix(coef(enet_mod[[5]])) %>% round(3),
) %>% remove_rownames() %>% 
          rename(varname = 1, ridge = 2, enet_025 = 3, enet_05 = 4, enet_075 = 5, lasso = 6)
enet_coef
