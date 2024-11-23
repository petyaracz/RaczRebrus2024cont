setwd('~/Github/RaczRebrus2024cont/')
library(tidyverse)
library(rstanarm)
library(broom.mixed)
library(performance)

d = read_tsv('dat/filtered_data.tsv')

# 4) How many and which conditions will participants be assigned to?
# within-participant conditions: word preference for back / front forms in corpus, word language of origin, word date of borrowing, word length, word frequency, word neighbourhood density, word similarity to front / back stems, Hayes' criteria of word behaviour: stem ends in a bilabial stop, a sibilant, a coronal sonorant, or a consonant cluster.
#
count(d,id, sort = T)
count(d,language)
d$lang = ifelse(d$language %in% c('de','en','fr','la','yi'), d$language, 'other')
d$lang = fct_relevel(d$lang, 'other')

# 5) Specify exactly which analyses you will conduct to examine the main question/hypothesis.
# (a) bayesian glm predicting yes/no from conditions with participant random intercept and word random intercept, weakly informative priors
fit1 = stan_glmer(accept ~ log_odds_adj + lang + stem_length + llfpm10 + suffix + neighbourhood_size + stem_phonology + stem_final_consonant_cluster + (1|id) + (1|stem), data = d, family = binomial, cores = 4)
fit1
# (b) bayesian glm predicting rt from conditions with participant random intercept and word random intercept, weakly informative priors
# loo used for model selection
fit2 = stan_glmer(rt ~ accept + log_odds_adj + lang + stem_length + llfpm10 + suffix + neighbourhood_size + stem_phonology + stem_final_consonant_cluster + (1|id) + (1|stem), data = d, cores = 4)
# rank-deficient!
fit2

check_collinearity(fit1)
check_collinearity(fit2)
