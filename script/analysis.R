setwd('~/Github/RaczRebrus2024cont/')
library(tidyverse)
library(rstanarm)
library(tidybayes)
library(broom.mixed)
library(scales)
library(randomForest)

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
  knn_scaled = rescale(knn_2_weight),
  yi_la_scaled = rescale(yi_la_weight),
  date_scaled = rescale(date),
  n_size_scaled = rescale(neighbourhood_size),
  logfreq_scaled = rescale(llfpm10),
  stem_length_scaled = rescale(stem_length),
  logodds_scaled = rescale(log_odds_adj),
  stem_phonology = fct_relevel(stem_phonology, 'other')
 ) |> 
 select(rt,accept,log_odds_adj,lang,stem_length,llfpm10,suffix,neighbourhood_size,stem_phonology,date_scaled,n_size_scaled,logfreq_scaled,stem_length_scaled,logodds_scaled,yi_la_scaled,knn_scaled,id,stem)

nrow(d2[!complete.cases(d2),]) == 0 # mm yes mmm

# 5) Specify exactly which analyses you will conduct to examine the main question/hypothesis.
# (a) bayesian glm predicting yes/no from conditions with participant random intercept and word random intercept, weakly informative priors
# (b) bayesian glm predicting rt from conditions with participant random intercept and word random intercept, weakly informative priors
# loo used for model selection

# hahaha no

d3 = d2 |> 
 count(accept,stem,log_odds_adj,lang,stem_length,llfpm10,suffix,neighbourhood_size,stem_phonology,date_scaled,n_size_scaled,logfreq_scaled,stem_length_scaled,logodds_scaled,yi_la_scaled,knn_scaled) |> 
 pivot_wider(names_from = accept, values_from = n, values_fill = 0) |> 
 mutate(log_odds_accept = log(`TRUE`+1)/log(`FALSE`+1))

d4 = d2 |> 
  summarise(rt = mean(rt), .by = c(stem,accept,log_odds_adj,lang,stem_length,llfpm10,suffix,neighbourhood_size,stem_phonology,date_scaled,n_size_scaled,logfreq_scaled,stem_length_scaled,logodds_scaled,yi_la_scaled,knn_scaled))

rf1 = randomForest(log_odds_accept ~ log_odds_adj + lang + stem_length + llfpm10 + suffix + neighbourhood_size + stem_phonology + date_scaled + n_size_scaled + logfreq_scaled + stem_length_scaled + logodds_scaled + yi_la_scaled + knn_scaled, data = d3)

rf2 = randomForest(rt ~ log_odds_adj + lang + stem_length + llfpm10 + suffix + neighbourhood_size + stem_phonology + date_scaled + n_size_scaled + logfreq_scaled + stem_length_scaled + logodds_scaled + yi_la_scaled + knn_scaled, data = d4)

importance(rf1)
rf1
importance(rf2)
rf2
# what