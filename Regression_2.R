library(dslabs)
library(tidyverse)
library(Lahman)
ds_theme_set()

# ------- 2.1. Introduction to linear models -------------
# lets see if bases on ball (BB) are more predictive - Confounding

#first the regression line for predicting runs from BBs
bb_slope <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(BB_per_game = BB/G, R_per_game = R/G) %>%
  lm(R_per_game ~ BB_per_game, data = .) %>%
  .$coef %>%
  .[2]
bb_slope

# then regression line for predicting runs from singles
singles_slope <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(Singles_per_game = (H - HR - X2B - X3B)/G, R_per_game = R/G) %>%
  lm(R_per_game ~ Singles_per_game, data = .) %>%
  .$coef %>%
  .[2]
singles_slope

# lastly the correlation between HR, BB and singles
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(Singles = (H - HR - X2B - X3B)/G, BB = BB/G, HR = HR/G) %>%
  summarize(cor(BB, HR), cor(Singles, HR), cor(BB, Singles))

# now lets find if BB is still useful to get runs, for that lets fix HR and see R vs BB --> stratify data HR to the tenths
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR_strata = round(HR/G, 1), 
         BB_per_game = BB/G, 
         R_per_game = R/G) %>%
  filter(HR_strata >= 0.4 & HR_strata <= 1.2)

# and make an scatter plot for each strata
dat %>% ggplot(aes(BB_per_game, R_per_game)) + 
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") + 
  facet_wrap(~ HR_strata)
# plot where we can see that slopes per strata is way smaller than the 0.74 obtained without considering HR

dat %>% group_by(HR_strata) %>%
  summarize(slope = cor(BB_per_game, R_per_game) * sd(R_per_game)/sd(BB_per_game))
# where we obtain a similar value of runs by BB than by Singles (0.44) which makes sense

# still, we can check if by stratifying by BB we can see if there is a HR effect or if its goes down
dat <- Teams %>% filter(yearID %in% 1961:2001) %>% 
  mutate(BB_strata = round(BB/G, 1), 
         HR_per_game = HR/G, 
         R_per_game = R/G) %>%
  filter(BB_strata >= 2.8 & BB_strata <= 3.9)
dat %>% ggplot(aes(HR_per_game, R_per_game)) + 
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  facet_wrap(~ BB_strata)
dat %>% group_by(BB_strata) %>%
  summarize(slope = cor(HR_pe_game, R_per_game) * sd(R_per_game)/sd(HR_per_game))


# ------- 2.2. Least Square Estimates (LSE) -------------
# to set up a successful linear model first needs to estimates the unknown parameters, the betas --> least square

rss <- function(beta0, beta1, data){
  resid <- galton_heights$son - (beta0 + beta1 * galton_heights$father)
  return(sum(resid^2))
}
# which yields a 3D plot where rss is z and beta0-1 are x-y. Keeping it simple, lets see 1D for beta0 = 25
beta1 = seq(0, 1, len = nrow(galton_heights))
results <- data.frame(beta1 = beta1, 
                      rss = sapply(beta1, rss, beta0 = 25))
results %>% ggplot(aes(beta1, rss)) + geom_line() + 
  geom_line(aes(beta1, rss), col = 2)

# to make the linear model use lm funtion this way
fit <- lm(son ~ father, data = galton_heights)
# which calculates the least square stimates
fit
summary(fit)

# the LSE are random variables since they're estimated from random variables, lets see it through a MC sim taking random set of population
B <- 1000
N <- 50
lse <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>% 
    lm(son ~ father, data = .) %>% .$coef
})
lse <- data.frame(beta_0 = lse[1,], beta_1 = lse[2,])

# to see the variability of the estimates
library(gridExtra)
p1 <- lse %>% ggplot(aes(beta_0)) + geom_histogram(binwidth = 5, color = "black")
p2 <- lse %>% ggplot(aes(beta_1)) + geom_histogram(binwidth = 0.1, color = "black")
grid.arrange(p1, p2, ncol = 2)

#summary from MC sim
sample_n(galton_heights, N, replace = TRUE) %>%
  lm(son ~ father, data = .) %>% summary
#now lets compare with lm
lse %>% summarize(se_0 = sd(beta_0), se_1 = sd(beta_1))

# advanced note on LSE, LSEs can be strongly correlated
lse %>% summarize(cor(beta_0, beta_1))
# but correlation depends on how the predictors are defined or transformed so lets change x_i to x_i - var{x}
B <- 1000
N <- 50
lse <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>%
    mutate(father = father - mean(father)) %>%
    lm(son ~ father, data = .) %>% .$coef
})
cor(lse[1,], lse[2,])
