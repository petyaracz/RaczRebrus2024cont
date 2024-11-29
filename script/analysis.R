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
  accept = as.double(accept),
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
fit1 = stan_glmer(accept ~ 
                    logodds_scaled + # word preference for back / front forms in corpus, 
                    lang + # word language of origin,  
                    date_scaled + # word date of borrowing, 
                    stem_length_scaled + # word length, 
                    logfreq_scaled + # word frequency, 
                    n_size_scaled  +# word neighbourhood density, 
                    knn_scaled + # word similarity to front / back stems, 
                    stem_phonology  +# Hayes' criteria of word behaviour: stem ends in a bilabial stop, a sibilant, a coronal sonorant, or a consonant cluster
                    (1|id) + (1|stem),
                  family = binomial,
                  data = d2,
                  cores = 4
                    )
performance::check_collinearity(fit1)
pp_check(fit1)
fit1
sjPlot::plot_model(fit1, 'est', transform = NULL) +
  theme_bw() +
  geom_hline(yintercept = 0, alpha = .5)
sjPlot::plot_model(fit1, 'pred')
# (b) bayesian glm predicting rt from conditions with participant random intercept and word random intercept, weakly informative priors
d2a = d2[d2$accept == 1,]

fit2a = stan_glmer(rt ~ 
                    logodds_scaled + # word preference for back / front forms in corpus, 
                    lang + # word language of origin,  
                    date_scaled + # word date of borrowing, 
                    stem_length_scaled + # word length, 
                    logfreq_scaled + # word frequency, 
                    n_size_scaled  +# word neighbourhood density, 
                    knn_scaled + # word similarity to front / back stems, 
                    stem_phonology  +# Hayes' criteria of word behaviour: stem ends in a bilabial stop, a sibilant, a coronal sonorant, or a consonant cluster
                    (1|id) + (1|stem),
                  data = d2a,
                  cores = 4
)

pp_check(fit2a) # some effect missing
sjPlot::plot_model(fit2a, 'pred')

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