setwd('~/Github/RaczRebrus2024cont/')
library(tidyverse)
library(rstanarm)
library(tidybayes)
library(broom.mixed)
library(scales)

d = read_tsv('dat/filtered_data.tsv')

# 4) How many and which conditions will participants be assigned to?
# within-participant conditions: word preference for back / front forms in corpus, word language of origin, word date of borrowing, word length, word frequency, word neighbourhood density, word similarity to front / back stems, Hayes' criteria of word behaviour: stem ends in a bilabial stop, a sibilant, a coronal sonorant, or a consonant cluster.
#
count(d,id, sort = T)
count(d,language)

d2 = d |> 
  mutate(
    lang = ifelse(language %in% c('de','en','fr','la','yi'), language, 'other'),
    lang = fct_relevel(lang, 'other'),
    knn_scaled = rescale(knn),
    yi_la_scaled = rescale(yi_la),
    date_scaled = rescale(date),
    n_size_scaled = rescale(neighbourhood_size),
    logfreq_scaled = rescale(llfpm10),
    stem_length_scaled = rescale(stem_length),
    logodds_scaled = rescale(log_odds_adj),
    stem_phonology = fct_relevel(stem_phonology, 'other')
  ) |> 
  select(rt,accept,log_odds_adj,lang,stem_length,llfpm10,suffix,neighbourhood_size,stem_phonology,stem_final_consonant_cluster,date_scaled,n_size_scaled,logfreq_scaled,stem_length_scaled,logodds_scaled,yi_la_scaled,knn_scaled,id,stem)

nrow(d2[!complete.cases(d2),]) == 0 # mm

d3 = d2[complete.cases(d2),]

# 5) Specify exactly which analyses you will conduct to examine the main question/hypothesis.
# (a) bayesian glm predicting yes/no from conditions with participant random intercept and word random intercept, weakly informative priors
# (b) bayesian glm predicting rt from conditions with participant random intercept and word random intercept, weakly informative priors
# loo used for model selection

lm1a = stan_glmer(accept ~ logodds_scaled + (1|id), data = d2, family = binomial, cores = 4)
lm1b = stan_glmer(accept ~ logodds_scaled + (1+logodds_scaled|id), data = d2, family = binomial, cores = 4)
lm1c = stan_glmer(accept ~ suffix + lang + stem_phonology + date_scaled + n_size_scaled + logfreq_scaled + stem_length_scaled + (1|id), data = d2, family = binomial, cores = 4)
lm1d = stan_glmer(accept ~ suffix + lang + stem_phonology + date_scaled + n_size_scaled + logfreq_scaled + stem_length_scaled + (1+ logfreq_scaled |id), data = d2, family = binomial, cores = 4)

loo1a = loo(lm1a)
loo1b = loo(lm1b)
loo1c = loo(lm1c)
loo1d = loo(lm1d)
loo_compare(loo1a,loo1b)
loo_compare(loo1a,loo1c)
loo_compare(loo1c,loo1d)
loo_compare(loo1b,loo1c)

plot(lm1c, 'areas', regex = 'lang|phonology|scaled')
plot(lm1b, 'areas', regex = 'odds_scaled$')

lm1a |> 
  add_epred_draws(newdata = d2, ndraws = 1000) |> 
  ungroup() |> 
  summarise(
    mean_epred = mean(.epred),
    mean_accept = mean(accept),
    .by = c(stem,log_odds_adj)
    ) |> 
  ggplot() +
  geom_point(aes(log_odds_adj,mean_accept))
  geom_point(aes(log_odds_adj,mean_epred))
