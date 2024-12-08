# -- head -- #

setwd('~/Github/RaczRebrus2024cont/')
library(tidyverse)
library(rstanarm)
library(tidybayes)
library(broom.mixed)
library(scales)
library(ggthemes)

# -- read -- #

all = read_tsv('dat/unfiltered_data.tsv')
d = read_tsv('dat/filtered_data.tsv')

length(unique(all$id))
length(unique(d$id))
1 - nrow(d) / nrow(all[all$id %in% d$id,])
ggplot(d, aes(id,fill = accept)) +
  geom_bar() +
  coord_flip() +
  theme_few()

# -- format -- #

ds = d |> 
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
    svm01_scaled = rescale(svm_weight_01),
    svm1_scaled = rescale(svm_weight_1),
    lang_de = lang == 'de',
    lang_en = lang == 'en',
    lang_fr = lang == 'fr',
    lang_la = lang == 'la',
    lang_yi = lang == 'yi'
  ) |> 
  select(rt,accept,suffix,log_odds_adj,lang,lang_de,lang_en,lang_fr,lang_la,lang_yi,stem_length,llfpm10,suffix,neighbourhood_size,stem_phonology,date_scaled,n_size_scaled,logfreq_scaled,stem_length_scaled,logodds_scaled,yi_la_scaled,knn_scaled,svm01_scaled,svm1_scaled,id,stem)

# 4) How many and which conditions will participants be assigned to?
# within-participant conditions: word preference for back / front forms in corpus, word language of origin, word date of borrowing, word length, word frequency, word neighbourhood density, word similarity to front / back stems, Hayes' criteria of word behaviour: stem ends in a bilabial stop, a sibilant, a coronal sonorant, or a consonant cluster.
#

# -- pick similarity measure -- #

sim1 = stan_glmer(accept ~ knn_scaled + (1+knn_scaled|id) + (1|stem), family = binomial, data = ds, cores = 4) # knn with k = 2 on phon dist and summed back/front
sim2 = stan_glmer(accept ~ yi_la_scaled + (1+yi_la_scaled|id) + (1|stem), family = binomial, data = ds, cores = 4) # gcm on yiddish vs latin
sim3 = stan_glmer(accept ~ svm1_scaled + (1+svm1_scaled|id) + (1|stem), family = binomial, data = ds, cores = 4) # svm using phon distance matrix, tuned sigma, and epsilon regression on log odds adj back, split to high/low
sim4 = stan_glmer(accept ~ svm01_scaled + (1+svm01_scaled|id) + (1|stem), family = binomial, data = ds, cores = 4) # svm using phon distance matrix, tuned sigma, and epsilon regression on log odds adj back

sloo1 = loo(sim1)
sloo2 = loo(sim2)
sloo3 = loo(sim3)
sloo4 = loo(sim4)
sloos = loo_compare(sloo1,sloo2,sloo3,sloo4) |> 
  as_tibble() |> 
  bind_cols(model = c('knn','gcm','svm1','svm01'))
sr1 = performance::r2_bayes(sim1)
sr2 = performance::r2_bayes(sim2)
sr3 = performance::r2_bayes(sim3)
sr4 = performance::r2_bayes(sim4)
srs = bind_rows(as_tibble(sr1),as_tibble(sr2),as_tibble(sr3),as_tibble(sr4)) |> 
  bind_cols(model = c('knn','knn','gcm','gcm','svm1','svm1','svm01','svm01'))

# -- fit models: accept -- #

## glm ##

# 5) Specify exactly which analyses you will conduct to examine the main question/hypothesis.
# (a) bayesian glm predicting yes/no from conditions with participant random intercept and word random intercept, weakly informative priors
fit1a = stan_glmer(accept ~ 
                    suffix +
                    logodds_scaled + # word preference for back / front forms in corpus, 
                    lang_de + # word language of origin,  
                    lang_en +
                    lang_fr +
                    lang_la +
                    lang_yi +
                    date_scaled + # word date of borrowing, 
                    stem_length_scaled + # word length, 
                    logfreq_scaled + # word frequency, 
                    n_size_scaled  +# word neighbourhood density, 
                    svm1_scaled + # word similarity to front / back stems, 
                    stem_phonology +# Hayes' criteria of word behaviour: stem ends in a bilabial stop, a sibilant, a coronal sonorant, or a consonant cluster
                    (1|id) + (1|stem),
                  family = binomial,
                  data = ds,
                  cores = 4
                    )

performance::check_collinearity(fit1a)
pp_check(fit1a)
sjPlot::plot_model(fit1a, 'est', transform = NULL) +
  theme_bw() +
  geom_hline(yintercept = 0, alpha = .5)

# lo, lang, date, svm

## reg prior ##

# https://avehtari.github.io/modelselection/bodyfat.html#3_Regression_model_with_regularized_horseshoe_prior
p0 = 6 # prior guess for the number of relevant variables
p = 14 # number of variables
tau0 = p0/(p-p0) * 1/sqrt(nrow(ds))
rhs_prior = hs(global_scale=tau0)

fit1hs = stan_glmer(accept ~ 
                      suffix +
                      logodds_scaled + # word preference for back / front forms in corpus,
                      lang_de + # word language of origin,  
                      lang_en +
                      lang_fr +
                      lang_la +
                      lang_yi +
                      date_scaled + # word date of borrowing, 
                      stem_length_scaled + # word length, 
                      logfreq_scaled + # word frequency, 
                      n_size_scaled  +# word neighbourhood density, 
                      svm1_scaled + # word similarity to front / back stems, 
                      stem_phonology +# Hayes' criteria of word behaviour: stem ends in a bilabial stop, a sibilant, a coronal sonorant, or a consonant cluster
                      (1|id) + (1|stem),
                    family = binomial,
                    data = ds, 
                    prior=rhs_prior, QR=TRUE, chains = 4, cores = 4, iter = 4000)

loo1a = loo(fit1a)
loo1hs = loo(fit1hs)
loos1 = as_tibble(loo_compare(loo1a,loo1hs)) |> 
  bind_cols(model = c('hs','reg'))
fit1hs
performance::check_collinearity(fit1hs)
sjPlot::plot_model(fit1hs, 'est', transform = NULL) +
  theme_bw() +
  geom_hline(yintercept = 0, alpha = .5)

## fun interactions ##

fit2a = stan_glmer(accept ~ logodds_scaled * suffix + (1|id) + (1|stem), family = binomial, data = ds, cores = 4)
fit2b = stan_glmer(accept ~ logodds_scaled + suffix + (1|id) + (1|stem), family = binomial, data = ds, cores = 4)
fit3a = stan_glmer(accept ~ lang * suffix + (1|id) + (1|stem), family = binomial, data = ds, cores = 4)
fit3b = stan_glmer(accept ~ lang + suffix + (1|id) + (1|stem), family = binomial, data = ds, cores = 4)
fit4a = stan_glmer(accept ~ date_scaled * suffix + (1|id) + (1|stem), family = binomial, data = ds, cores = 4)
fit4b = stan_glmer(accept ~ date_scaled + suffix + (1|id) + (1|stem), family = binomial, data = ds, cores = 4)
fit5a = stan_glmer(accept ~ svm1_scaled * suffix + (1|id) + (1|stem), family = binomial, data = ds, cores = 4)
fit5b = stan_glmer(accept ~ svm1_scaled + suffix + (1|id) + (1|stem), family = binomial, data = ds, cores = 4)

loo2a = loo(fit2a)
loo2b = loo(fit2b)
loo3a = loo(fit3a)
loo3b = loo(fit3b)
loo4a = loo(fit4a)
loo4b = loo(fit4b)
loo5a = loo(fit5a)
loo5b = loo(fit5b)

loo_compare(loo2a,loo2b)
loo_compare(loo3a,loo3b)
loo_compare(loo4a,loo4b)
loo_compare(loo5a,loo5b)

sjPlot::plot_model(fit3a, 'est', transform = NULL) +
  theme_bw() +
  geom_hline(yintercept = 0, alpha = .5)

sjPlot::plot_model(fit2a, 'pred', terms = c('logodds_scaled','suffix'))
sjPlot::plot_model(fit3a, 'pred', terms = c('lang','suffix'))
sjPlot::plot_model(fit4a, 'pred', terms = c('date_scaled','suffix'))
sjPlot::plot_model(fit5a, 'pred', terms = c('svm1_scaled','suffix'))

# kár

# -- fit models: rt -- #

fit6a = stan_glmer(rt ~ 
                     accept +
                     suffix +
                     logodds_scaled + # word preference for back / front forms in corpus, 
                     lang_de + # word language of origin,  
                     lang_en +
                     lang_fr +
                     lang_la +
                     lang_yi +
                     date_scaled + # word date of borrowing, 
                     stem_length_scaled + # word length, 
                     logfreq_scaled + # word frequency, 
                     n_size_scaled  +# word neighbourhood density, 
                     svm1_scaled + # word similarity to front / back stems, 
                     stem_phonology +# Hayes' criteria of word behaviour: stem ends in a bilabial stop, a sibilant, a coronal sonorant, or a consonant cluster
                     (1|id) + (1|stem),
                   data = ds,
                   cores = 4
)

performance::check_collinearity(fit6a)
sjPlot::plot_model(fit6a, 'est', transform = NULL) +
  theme_bw() +
  geom_hline(yintercept = 0, alpha = .5)

p0 = 3 # prior guess for the number of relevant variables
p = 15 # number of variables
tau0 = p0/(p-p0) * 1/sqrt(nrow(ds))
rhs_prior = hs(global_scale=tau0)

fit6hs = stan_glmer(rt ~ 
                     accept +
                     suffix +
                     logodds_scaled + # word preference for back / front forms in corpus, 
                     lang_de + # word language of origin,  
                     lang_en +
                     lang_fr +
                     lang_la +
                     lang_yi +
                     date_scaled + # word date of borrowing, 
                     stem_length_scaled + # word length, 
                     logfreq_scaled + # word frequency, 
                     n_size_scaled  +# word neighbourhood density, 
                     svm1_scaled + # word similarity to front / back stems, 
                     stem_phonology +# Hayes' criteria of word behaviour: stem ends in a bilabial stop, a sibilant, a coronal sonorant, or a consonant cluster
                     (1|id) + (1|stem),
                    data = ds, 
                    prior=rhs_prior, QR=TRUE, chains = 4, cores = 4, iter = 4000)

sjPlot::plot_model(fit6hs, 'est', transform = NULL) +
  theme_bw() +
  geom_hline(yintercept = 0, alpha = .5)

# -- write -- #

save(fit1a, file = 'models/fit1a.Rda')
save(fit1hs, file = 'models/fit1hs.Rda')
save(fit6a, file = 'models/fit6a.Rda')
save(fit6hs, file = 'models/fit6hs.Rda')

write_tsv(sloos, 'models/similarity_loos.tsv')
write_tsv(srs, 'models/similarity_r2s.tsv')
write_tsv(loos1, 'models/hs_loos.tsv')
