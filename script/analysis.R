# todo: in the spirit of the prereg, fit top-down stepwise. and then also horseshoe. compare. accept is likely more interesting than rt

setwd('~/Github/RaczRebrus2024cont/')
library(tidyverse)
library(rstanarm)
library(tidybayes)
library(broom.mixed)
library(scales)
# library(randomForest)

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
  stem_phonology = fct_relevel(stem_phonology, 'other'),
  svm_scaled = rescale(svm_weight)
 ) |> 
 select(rt,accept,suffix,log_odds_adj,lang,stem_length,llfpm10,suffix,neighbourhood_size,stem_phonology,date_scaled,n_size_scaled,logfreq_scaled,stem_length_scaled,logodds_scaled,yi_la_scaled,knn_scaled,svm_category,svm_scaled,id,stem)

nrow(d2[!complete.cases(d2),]) == 0 # mm yes mmm

# pick similarity measure
sim1 = stan_glmer(accept ~ knn_scaled + (1+knn_scaled|id) + (1|stem), family = binomial, data = d2, cores = 4) # knn with k = 2 on phon dist and summed back/front
sim2 = stan_glmer(accept ~ yi_la_scaled + (1+yi_la_scaled|id) + (1|stem), family = binomial, data = d2, cores = 4) # gcm on yiddish vs latin
sim3 = stan_glmer(accept ~ svm_category + (1+svm_category|id) + (1|stem), family = binomial, data = d2, cores = 4) # svm using phon distance matrix, tuned sigma, and epsilon regression on log odds adj back, split to high/low
sim4 = stan_glmer(accept ~ svm_scaled + (1+svm_scaled|id) + (1|stem), family = binomial, data = d2, cores = 4) # svm using phon distance matrix, tuned sigma, and epsilon regression on log odds adj back

sloo1 = loo(sim1)
sloo2 = loo(sim2)
sloo3 = loo(sim3)
sloo4 = loo(sim4)
loo_compare(sloo1,sloo2,sloo3,sloo4)
performance::r2_bayes(sim1)
performance::r2_bayes(sim2)
performance::r2_bayes(sim3)
performance::r2_bayes(sim4)

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
                    svm_scaled + # word similarity to front / back stems, 
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
                      svm_scaled + # word similarity to front / back stems, 
                      stem_phonology +# Hayes' criteria of word behaviour: stem ends in a bilabial stop, a sibilant, a coronal sonorant, or a consonant cluster
                      (1|id) + (1|stem),
                    family = binomial,
                    data = d2, 
                    prior=rhs_prior, QR=TRUE, chains = 4, cores = 4, iter = 4000)

loo1a = loo(fit1a)
loo1hs = loo(fit1hs)
loo_compare(loo1a,loo1hs)
fit1hs

fit2 = stan_glmer(accept ~ svm_scaled * suffix + (1|id) + (1|stem), family = binomial, data = d2, cores = 4)
fit3 = stan_glmer(accept ~ logodds_scaled * suffix + (1|id) + (1|stem), family = binomial, data = d2, cores = 4)
fit4 = stan_glmer(accept ~ date_scaled * suffix + (1|id) + (1|stem), family = binomial, data = d2, cores = 4)
fit5 = stan_glmer(accept ~ date_scaled + suffix + (1|id) + (1|stem), family = binomial, data = d2, cores = 4)
fit6 = stan_glmer(accept ~ date_scaled * suffix + (1+date_scaled+suffix|id) + (1|stem), family = binomial, data = d2, cores = 4)

loo4 = loo(fit4)
loo5 = loo(fit5)
loo6 = loo(fit6)
loo_compare(loo5,loo4)
sjPlot::plot_model(fit5, 'pred', terms = c('date_scaled','suffix'))
