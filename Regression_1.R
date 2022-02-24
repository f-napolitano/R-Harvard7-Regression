library(dslabs)
library(tidyverse)
library(Lahman)
ds_theme_set()

# ------- 1.1 Baseball as motivation -------------
# lets see if teams homeruns predicts teams runs

Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR_per_game = HR / G, R_per_game = R / G) %>%
  ggplot(aes(HR_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)

# lets see if stolen bases predicts runs

Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(SB_per_game = SB/G, R_per_game = R/G) %>%
  ggplot(aes(SB_per_game, R_per_game)) +
  geom_point(alpha = 0.5)

# lets see base on ball and runs

Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(BB_per_game = BB/G, R_per_game = R/G) %>%
  ggplot(aes(BB_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)

# ------- 1.2 Correlation -------------
# historical data from genetics

library(HistData)
data("GaltonFamilies")
galton_heights <- GaltonFamilies %>%
  filter(childNum == 1 & gender == "male") %>%
  select(father, childHeight) %>% 
  rename(son = childHeight)

# as fathers and sons heights are normal distributions then can be summarized using mean and sd

galton_heights %>% 
  summarize(mean(father), sd(father), mean(son), sd(son))
# but summize hide the correlation
galton_heights %>% ggplot(aes(father,son)) + 
  geom_point(alpha = 0.5)

# correlation between fathers and sons

galton_heights %>% summarize(cor(father,son))

# ------- 1.3 Sample Correlation is random variable -------------
# lets assume our list is the total population, take a sample of 25

set.seed(0)
R <- sample_n(galton_heights, 25, replace = TRUE) %>%
  summarize(cor(father,son))

# now lets run a montecarlo to see the distribution of this sample correlation
B <- 1000
N <- 25
R <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>%
    summarize(r = cor(father,son)) %>% .$r
})
data.frame(R) %>% ggplot(aes(R)) + geom_histogram(binwidth = 0.05, color = "black")
mean(R)
sd(R)
# QQ-plot to evaluate whether N is large enough
data.frame(R) %>%
  ggplot(aes(sample = R)) +
  stat_qq() +
  geom_abline(intercept = mean(R), slope = sqrt((1-mean(R)^2)/(N-2)))

# ------- 1.3 Stratification and variance -------------
# lets predict the son height with fathers heights, particularly from one 72 inch tall
# predicts from fathers with similar height, not exactly 72 inch

conditional_avg <- galton_heights %>% filter(round(father) == 72) %>%
  summarize(avg = mean(son)) %>% .$avg

# box plots for stratification for each group
galton_heights %>% mutate(father_strata = factor(round(father))) %>%
  ggplot(aes(father_strata, son)) +
  geom_boxplot() +
  geom_point()

# means of each group seems to follow a linnear relationship
galton_heights %>%  mutate(father = round(father)) %>%
  group_by(father) %>%
  summarize(son_conditional_avg = mean(son)) %>%
  ggplot(aes(father, son_conditional_avg)) + 
  geom_point()
# the slope of the following trend is similar to the heights correlation, 0.5

# lets plot the standarized heights with a line with slope equal to correlation
r <- galton_heights %>% summarize(r = cor(father, son)) %>% .$r
galton_heights %>% mutate(father = round(father)) %>%
  group_by(father) %>%
  summarize(son = mean(son)) %>%
  mutate(z_father = scale(father), z_son = scale(son)) %>%
  ggplot(aes(z_father, z_son)) + 
  geom_point() + 
  geom_abline(intercept = 0, slope = r)

# regression line for fathers and sons data
mu_x <- mean(galton_heights$father)
mu_y <- mean(galton_heights$son)
s_x <- sd(galton_heights$father)
s_y <- sd(galton_heights$son)
r <- cor(galton_heights$father, galton_heights$son)
m <- r * s_y / s_x
b <- mu_y - m * mu_x

galton_heights %>% ggplot(aes(father, son)) + 
  geom_point(alpha = 0.5) + 
  geom_abline(intercept = b, slope = m)

# same plot but in stardard units --> intercept 0 slope r
galton_heights %>%
  ggplot(aes(scale(father), scale(son))) +
  geom_point(alpha = 0.5) + 
  geom_abline(intercept = 0, slope = r)

# Bivariate Normal Distribution
# lets see if father-son heights follows a bivariate normal distribution

galton_heights %>%
  mutate(z_father = round((father - mean(father))/sd(father))) %>%
  filter(z_father %in% -2:2) %>%
  ggplot() + stat_qq(aes(sample = son)) + 
  facet_wrap(~z_father)

# ------- 1.3 Assigment -------------
# lets analyze mothers-daughters heights from GaltonFamilies data

set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
library(HistData)
data("GaltonFamilies")

female_heights <- GaltonFamilies%>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)

mu_xf <- mean(female_heights$mother)
mu_yf <- mean(female_heights$daughter)
sd_xf <- sd(female_heights$mother)
sd_yf <- sd(female_heights$daughter)
rf <- cor(female_heights$mother, female_heights$daughter)
mf <- rf * sd_yf / sd_xf
bf <- mu_yf - mf * mu_xf
# % of variability in daughter's heights is explained by mother's height
rf^2 * 100

# expected daughter's height for a mother 60 inch tall
mu_yf + rf * (60 - mu_xf) * sd_yf/sd_xf
