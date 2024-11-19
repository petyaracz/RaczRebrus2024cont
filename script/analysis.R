# 4) How many and which conditions will participants be assigned to?
# within-participant conditions: word preference for back / front forms in corpus, word language of origin, word date of borrowing, word length, word frequency, word neighbourhood density, word similarity to front / back stems, Hayes' criteria of word behaviour: stem ends in a bilabial stop, a sibilant, a coronal sonorant, or a consonant cluster.
# 
# 5) Specify exactly which analyses you will conduct to examine the main question/hypothesis.
# (a) bayesian glm predicting yes/no from conditions with participant random intercept and word random intercept, weakly informative priors
# (b) bayesian glm predicting rt from conditions with participant random intercept and word random intercept, weakly informative priors
# loo used for model selection

setwd('~/Github/RaczRebrus2024cont/')
library(tidyverse)
library(lme4)
library(broom.mixed)
library(performance)

d = read_tsv('dat/filtered_data.tsv')

d = d |> 
  mutate(log_odds_back_total = ifelse(log_odds_back_total == -Inf, NA, log_odds_back_total))

length(unique(d$id))

aggr = d |> 
  count(stem,log_odds_back_total,log_odds_back_suffix,accept,suffix, sort = T) |> 
  pivot_wider(names_from = accept, values_from = n, values_fill = 0)

fit1 = glm(cbind(`TRUE`,`FALSE`) ~ log_odds_back_total, data = aggr, family = binomial)
tidy(fit1)
fit2 = lmer(rt ~ accept + log_odds_back_total + llfpm10 + (1|id) + (1|target), data = d)
tidy(fit2)
fit3 = lmer(rt ~ accept * log_odds_back_total + llfpm10 + (1|id) + (1|target), data = d)
check_collinearity(fit3)
tidy(fit3)
fit4 = lmer(rt ~ accept * log_odds_back_total * llfpm10 + (1|id) + (1|target), data = d)
check_collinearity(fit4) # no

