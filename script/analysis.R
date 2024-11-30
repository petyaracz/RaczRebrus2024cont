# todo: in the spirit of the prereg, fit top-down stepwise. and then also horseshoe. compare. accept is likely more interesting than rt

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
 select(rt,accept,suffix,log_odds_adj,lang,stem_length,llfpm10,suffix,neighbourhood_size,stem_phonology,date_scaled,n_size_scaled,logfreq_scaled,stem_length_scaled,logodds_scaled,yi_la_scaled,knn_scaled,id,stem)

nrow(d2[!complete.cases(d2),]) == 0 # mm yes mmm

# 5) Specify exactly which analyses you will conduct to examine the main question/hypothesis.
# (a) bayesian glm predicting yes/no from conditions with participant random intercept and word random intercept, weakly informative priors
fit1a = stan_glmer(accept ~ 
                    suffix +
                    logodds_scaled + # word preference for back / front forms in corpus, 
                    lang + # word language of origin,  
                    date_scaled + # word date of borrowing, 
                    stem_length_scaled + # word length, 
                    logfreq_scaled + # word frequency, 
                    n_size_scaled  +# word neighbourhood density, 
                    knn_scaled + # word similarity to front / back stems, 
                    stem_phonology +# Hayes' criteria of word behaviour: stem ends in a bilabial stop, a sibilant, a coronal sonorant, or a consonant cluster
                    (1|id) + (1|stem),
                  family = binomial,
                  data = d2,
                  cores = 4
                    )
performance::check_collinearity(fit1a)
pp_check(fit1a)
sjPlot::plot_model(fit1a, 'est', transform = NULL) +
  theme_bw() +
  geom_hline(yintercept = 0, alpha = .5)

# https://avehtari.github.io/modelselection/bodyfat.html#3_Regression_model_with_regularized_horseshoe_prior
p0 = 5 # prior guess for the number of relevant variables
p = 9 # number of variables
tau0 = p0/(p-p0) * 1/sqrt(nrow(d))
rhs_prior = hs(global_scale=tau0)

fit1hs = stan_glmer(accept ~ 
                      suffix +
                      logodds_scaled + # word preference for back / front forms in corpus,
                      lang + # word language of origin,  
                      date_scaled + # word date of borrowing, 
                      stem_length_scaled + # word length, 
                      logfreq_scaled + # word frequency, 
                      n_size_scaled  +# word neighbourhood density, 
                      knn_scaled + # word similarity to front / back stems, 
                      stem_phonology +# Hayes' criteria of word behaviour: stem ends in a bilabial stop, a sibilant, a coronal sonorant, or a consonant cluster
                      (1|id) + (1|stem),
                    family = binomial,
                    data = d2, 
                    prior=rhs_prior, QR=TRUE, chains = 4, cores = 4, iter = 4000)

loo1a = loo(fit1a)
loo1hs = loo(fit1hs)
loo_compare(loo1a,loo1hs)
fit1hs

fit1b = stan_glmer(accept ~ 
                     suffix +
                     logodds_scaled + # word preference for back / front forms in corpus, 
                     lang + # word language of origin,  
                     date_scaled + # word date of borrowing, 
                     knn_scaled + # word similarity to front / back stems, 
                     stem_phonology  +# Hayes' criteria of word behaviour: stem ends in a bilabial stop, a sibilant, a coronal sonorant, or a consonant cluster
                     (1|id) + (1|stem),
                   family = binomial,
                   data = d2,
                   cores = 4
)

sjPlot::plot_model(fit1b, 'est', transform = NULL) +
  theme_bw() +
  geom_hline(yintercept = 0, alpha = .5)

fit1c = stan_glmer(accept ~ 
                     logodds_scaled + # word preference for back / front forms in corpus, 
                     lang + # word language of origin,  
                     date_scaled * suffix + # word date of borrowing, 
                     knn_scaled + # word similarity to front / back stems, 
                     stem_phonology  +# Hayes' criteria of word behaviour: stem ends in a bilabial stop, a sibilant, a coronal sonorant, or a consonant cluster
                     (1|id) + (1|stem),
                   family = binomial,
                   data = d2,
                   cores = 4
)

fit1d = stan_glmer(accept ~ 
                     logodds_scaled + # word preference for back / front forms in corpus, 
                     lang + # word language of origin,  
                     date_scaled + # word date of borrowing, 
                     knn_scaled * suffix + # word similarity to front / back stems, 
                     stem_phonology  +# Hayes' criteria of word behaviour: stem ends in a bilabial stop, a sibilant, a coronal sonorant, or a consonant cluster
                     (1|id) + (1|stem),
                   family = binomial,
                   data = d2,
                   cores = 4
)

fit1e = stan_glmer(accept ~ 
                     logodds_scaled + # word preference for back / front forms in corpus, 
                     lang * suffix + # word language of origin,  
                     date_scaled + # word date of borrowing, 
                     knn_scaled + # word similarity to front / back stems, 
                     stem_phonology  +# Hayes' criteria of word behaviour: stem ends in a bilabial stop, a sibilant, a coronal sonorant, or a consonant cluster
                     (1|id) + (1|stem),
                   family = binomial,
                   data = d2,
                   cores = 4
)

fit1f = stan_glmer(accept ~ 
                     logodds_scaled + # word preference for back / front forms in corpus, 
                     lang + # word language of origin,  
                     date_scaled + # word date of borrowing, 
                     knn_scaled + # word similarity to front / back stems, 
                     stem_phonology * suffix  + # Hayes' criteria of word behaviour: stem ends in a bilabial stop, a sibilant, a coronal sonorant, or a consonant cluster
                     (1|id) + (1|stem),
                   family = binomial,
                   data = d2,
                   cores = 4
)

sjPlot::plot_model(fit1c, 'est', transform = NULL) +
  theme_bw() +
  geom_hline(yintercept = 0, alpha = .5)

loo1a = loo(fit1a)
loo1b = loo(fit1b)
loo1c = loo(fit1c)
loo1d = loo(fit1d)
loo1e = loo(fit1e)
loo1f = loo(fit1f)
loo_compare(loo1a,loo1b)
loo_compare(loo1b,loo1c)
loo_compare(loo1b,loo1d)
loo_compare(loo1b,loo1e)
loo_compare(loo1b,loo1f)

sjPlot::plot_model(fit1c, 'pred', terms = c('knn_scaled','suffix'))
sjPlot::plot_model(fit1f, 'pred', terms = c('stem_phonology','suffix'))

fit1g = stan_glmer(accept ~ 
                     logodds_scaled * suffix + # word preference for back / front forms in corpus, 
                     date_scaled + # word date of borrowing, 
                     (1|id) + (1|stem),
                   family = binomial,
                   data = d2,
                   cores = 4
)

fit1h = stan_glmer(accept ~ 
                     logodds_scaled + # word preference for back / front forms in corpus, 
                     date_scaled * suffix + # word date of borrowing, 
                     (1|id) + (1|stem),
                   family = binomial,
                   data = d2,
                   cores = 4
)

fit1i = stan_glmer(accept ~ 
                     logodds_scaled + # word preference for back / front forms in corpus, 
                     date_scaled +
                     suffix +
                     (1|id) + (1|stem),
                   family = binomial,
                   data = d2,
                   cores = 4
)

loo1g = loo(fit1g)
loo1h = loo(fit1h)
loo1i = loo(fit1i)

loo_compare(loo1i,loo1g)
loo_compare(loo1i,loo1h)
loo_compare(loo1h,loo1g)

fit1h2 = stan_glmer(accept ~ 
                     logodds_scaled + # word preference for back / front forms in corpus, 
                     date_scaled * suffix + # word date of borrowing, 
                     (1 + date_scaled|id) + (1|stem),
                   family = binomial,
                   data = d2,
                   cores = 4,
                   control = list(adapt_delta = 0.999, stepsize = 0.01, max_treedepth = 15)
)

loo1h2 = loo(fit1h2)
loo_compare(loo1h,loo1h2)

sjPlot::plot_model(fit1h, 'pred', terms = c('date_scaled','suffix'))
sjPlot::plot_model(fit1g, 'pred', terms = c('logodds_scaled','suffix'))

# (b) bayesian glm predicting rt from conditions with participant random intercept and word random intercept, weakly informative priors
d2accept = d2[d2$accept == 1,]

fit2a = stan_glmer(rt ~ 
                    suffix +
                    logodds_scaled + # word preference for back / front forms in corpus, 
                    lang + # word language of origin,  
                    date_scaled + # word date of borrowing, 
                    stem_length_scaled + # word length, 
                    logfreq_scaled + # word frequency, 
                    n_size_scaled  +# word neighbourhood density, 
                    knn_scaled + # word similarity to front / back stems, 
                    stem_phonology  +# Hayes' criteria of word behaviour: stem ends in a bilabial stop, a sibilant, a coronal sonorant, or a consonant cluster
                    (1|id) + (1|stem),
                  data = d2accept,
                  cores = 4
)

pp_check(fit2a) # some effect missing
sjPlot::plot_model(fit2a, 'pred')

fit2b = stan_glmer(rt ~ 
                     suffix +
                     logfreq_scaled + # word frequency, 
                     knn_scaled + # word similarity to front / back stems, 
                     (1|id) + (1|stem),
                   data = d2accept,
                   cores = 4
)

fit2c = stan_glmer(rt ~ 
                     logfreq_scaled * suffix + # word frequency, 
                     knn_scaled + # word similarity to front / back stems, 
                     (1|id) + (1|stem),
                   data = d2accept,
                   cores = 4
)

fit2d = stan_glmer(rt ~ 
                     logfreq_scaled + # word frequency, 
                     knn_scaled * suffix + # word similarity to front / back stems, 
                     (1|id) + (1|stem),
                   data = d2accept,
                   cores = 4
)

loo2a = loo(fit2a)
loo2b = loo(fit2b)
loo2c = loo(fit2c)
loo2d = loo(fit2d)

loo_compare(loo2a,loo2b)
loo_compare(loo2b,loo2c)
loo_compare(loo2b,loo2d)

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