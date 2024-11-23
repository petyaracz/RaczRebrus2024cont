setwd('~/Github/RaczRebrus2024cont/')
library(tidyverse)
library(rstanarm)
library(broom.mixed)
library(performance)

d = read_tsv('dat/filtered_data.tsv')

# 4) How many and which conditions will participants be assigned to?
# within-participant conditions: word preference for back / front forms in corpus, word language of origin, word date of borrowing, word length, word frequency, word neighbourhood density, word similarity to front / back stems, Hayes' criteria of word behaviour: stem ends in a bilabial stop, a sibilant, a coronal sonorant, or a consonant cluster.
# 
# 5) Specify exactly which analyses you will conduct to examine the main question/hypothesis.
# (a) bayesian glm predicting yes/no from conditions with participant random intercept and word random intercept, weakly informative priors

# (b) bayesian glm predicting rt from conditions with participant random intercept and word random intercept, weakly informative priors
# loo used for model selection


# whoops
d = d |> 
  mutate(
    log_odds_back_total = ifelse(log_odds_back_total == -Inf, NA, log_odds_back_total),
    accept = as.factor(accept)
    )

length(unique(d$id))
d |> 
  count(is.na(log_odds_back_total))

aggr = d |> 
  count(stem,log_odds_back_total,log_odds_back_suffix,accept,suffix,llfpm10, sort = T) |> 
  pivot_wider(names_from = accept, values_from = n, values_fill = 0)

fit1 = glm(cbind(`TRUE`,`FALSE`) ~ log_odds_back_total + llfpm10, data = aggr, family = binomial)
tidy(fit1)
fit2 = glm(cbind(`TRUE`,`FALSE`) ~ log_odds_back_suffix + llfpm10, data = aggr, family = binomial)
tidy(fit2)
plot(compare_performance(fit1,fit2))

fit3 = lmer(rt ~ accept + log_odds_back_total + llfpm10 + (1|id) + (1|target), data = d)
fit4 = lmer(rt ~ log_odds_back_total + accept * llfpm10 + (1|id) + (1|target), data = d)
fit5 = lmer(rt ~ accept * log_odds_back_total + llfpm10 + (1|id) + (1|target), data = d)
check_collinearity(fit4)
fit6 = lmer(rt ~ accept * log_odds_back_total + accept * llfpm10 + (1|id) + (1|target), data = d)
check_collinearity(fit4) # ok...
check_collinearity(fit5) # ok...
plot(compare_performance(fit3,fit4,fit5,fit6))

fit3b = lmer(rt ~ accept + log_odds_back_total + llfpm10 + (1|id) + (1+accept|target), data = d)
fit3c = lmer(rt ~ accept + log_odds_back_suffix + llfpm10 + (1|id) + (1|target), data = d)
fit3d = lmer(rt ~ accept + log_odds_back_total + suffix + llfpm10 + (1|id) + (1+accept|target), data = d)
plot(compare_performance(fit3,fit3b,fit3c,fit3d))

sjPlot::plot_model(fit3d, 'pred', terms = c('log_odds_back_total','accept'))
sjPlot::plot_model(fit3d, 'pred', terms = c('llfpm10','suffix'))
