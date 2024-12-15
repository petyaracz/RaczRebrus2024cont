# -- head -- #

setwd('~/Github/RaczRebrus2024cont/')
library(tidyverse)
library(ggrain)
library(scales)
library(ggthemes)
library(knitr)
library(rstanarm)
library(patchwork)
library(sjPlot)

# -- read -- #

d = read_tsv('dat/filtered_data.tsv')
load('models/fit1a.Rda')
load('models/fit6a.Rda')
load('models/fit1hs.Rda')
load('models/fit6hs.Rda')

# -- wrangle -- #

d = d |> 
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
    svm1_category = ifelse(svm1_scaled > mean(svm1_scaled), 'back', 'front'),
    lang_de = lang == 'de',
    lang_en = lang == 'en',
    lang_fr = lang == 'fr',
    lang_la = lang == 'la',
    lang_yi = lang == 'yi',
    lang = ifelse(language %in% c('de','en','fr','la','yi'), language, 'other'),
    date2 = ntile(date, 5),
    category = ifelse(log_odds_adj > mean(log_odds_adj), 'back', 'front')
  ) |> 
  mutate(
    period = glue::glue('{min(date)}-{max(date)}'),
    .by = date2
  ) |> 
  select(-date2) |> 
  ungroup()

# -- summaries -- #

dsum = d |> 
  distinct(stem,category,date,period,lang,log_odds_adj,neighbourhood_size,llfpm10,stem_length,stem_phonology,svm_weight_1,svm1_category,x_phon,y_phon)

dexp = d |> 
  count(accept,category,suffix,stem,svm1_scaled,svm1_category,knn_2_weight,period,lang,log_odds_adj) |> 
  pivot_wider(names_from = accept, values_from = n, values_fill = 0) |> 
  mutate(
    log_odds_resp = log((`1`+1)/(`0`+1))
  )

# -- tables -- #

d |> 
  distinct(period,category,stem) |> 
  summarise(words = paste(stem, collapse = ', '), .by = c(period,category)) |> 
  pivot_wider(names_from = category, values_from = words) |> 
  googlesheets4::write_sheet('https://docs.google.com/spreadsheets/d/18jayTVDGODwlu8TJEQaqr9Wh8zywRjxKh_mRXgIrnd8/edit?usp=sharing', 'table1')

d |> 
  distinct(lang,category,stem) |> 
  summarise(words = paste(stem, collapse = ', '), .by = c(lang,category)) |> 
  pivot_wider(names_from = category, values_from = words) |> 
  mutate(lang = fct_relevel(lang, 'other', 'yi', 'de', 'fr', 'en', 'la')) |> 
  arrange(lang) |> 
  googlesheets4::write_sheet('https://docs.google.com/spreadsheets/d/18jayTVDGODwlu8TJEQaqr9Wh8zywRjxKh_mRXgIrnd8/edit?usp=sharing', 'table2')

d |> 
  count(accept,stem,log_odds_adj,lang,period,neighbourhood_size,svm1_scaled,stem_phonology) |> 
  pivot_wider(names_from = accept, values_from = n, values_fill = 0) |> 
  rename(accept = `1`, reject = `0`) |> 
  relocate(accept, .before = stem) |> 
  relocate(reject, .after = accept) |> 
  arrange(-accept) |> 
  mutate(
    log_odds_adj = round(log_odds_adj, 2),
    svm1_scaled = round(svm1_scaled, 2)
  ) |> 
  googlesheets4::write_sheet('https://docs.google.com/spreadsheets/d/18jayTVDGODwlu8TJEQaqr9Wh8zywRjxKh_mRXgIrnd8/edit?usp=sharing', 'table3')

# -- viz -- #

## models

plot_model(fit1a, 'est', transform = NULL) +
  geom_hline(yintercept = 0, lty = 3) +
  theme_bw() +
  theme(panel.grid.major.y = element_blank()) +
  ggtitle('Accept ~ ')

plot_model(fit6a, 'est', transform = NULL) +
  geom_hline(yintercept = 0, lty = 3) +
  theme_bw() +
  theme(panel.grid.major.y = element_blank()) +
  ggtitle('RT ~ ')


## word metadata

dsum |> 
  ggplot(aes(log_odds_adj, fill = category)) +
  geom_histogram() +
  theme_few()

dsum |> 
  mutate(
    lang_n = case_when(
      lang == 'la' ~ 1,
      lang == 'en' ~ 2,
      lang == 'fr' ~ 3,
      lang == 'other' ~ 4,
      lang == 'de' ~ 5,
      lang == 'yi' ~ 6
    ),
    phon_n = case_when(
      stem_phonology == 'other' ~ 1,
      stem_phonology == 'sibilant' ~ 2,
      stem_phonology == 'coronal_sonorant' ~ 3
    )
  ) |> 
  select(date,lang_n,log_odds_adj,neighbourhood_size,llfpm10,stem_length,phon_n,svm_weight_1) |> 
  psych::pairs.panels()
  # corrplot::corrplot()
  
## make pairwise combinations of cols
my_names = 
my_combinations = combn(my_names, 2, paste, collapse = ', ') |> 
  str_split(', ') |> 
  map(unlist)


dsum |> 
  mutate(lang = fct_relevel(lang, 'la','en','de','fr','yi','other')) |> 
  ggplot(aes(lang,svm_weight_1)) +
  geom_rain() +
  coord_flip() +
  theme_few()

dsum |> 
  ggplot(aes(period,log_odds_adj)) +
  geom_rain() +
  coord_flip() +
  theme_few() +
  ylab('log(back/front), corpus')

dsum |> 
  ggplot(aes(period,fill = category)) +
  geom_bar(position = position_dodge()) +
  coord_flip() +
  theme_few() +
  scale_fill_colorblind() +
  ylab('log(back/front), corpus')

dsum |> 
  ggplot(aes(svm_weight_1,log_odds_adj,label = stem)) +
  geom_label() +
  geom_smooth() +
  theme_few()

dsum |> 
  ggplot(aes(svm1_category,fill = category)) +
  geom_bar(position = position_dodge()) +
  scale_fill_colorblind() +
  coord_flip() +
  theme_few()

## results

d |> 
  mutate(`back suffix form` = ifelse(accept,'accept','reject')) |> 
  ggplot(aes(category,fill=`back suffix form`)) +
  geom_bar(position = position_dodge()) +
  # facet_wrap(~ suffix) +
  coord_flip() +
  theme_few() +
  scale_fill_brewer(palette = 'Pastel2') +
  xlab('corpus behaviour')

p1 = d |> 
  mutate(`back suffix form` = ifelse(accept,'accept','reject')) |> 
  ggplot(aes(period,fill=`back suffix form`)) +
  geom_bar(position = position_dodge()) +
  # facet_wrap(~ suffix) +
  coord_flip() +
  theme_few() +
  scale_fill_brewer(palette = 'Pastel2') +
  xlab('period of borrowing') +
  guides(fill = 'none')

p2 = d |> 
  mutate(
    `back suffix form` = ifelse(accept,'accept','reject'),
    lang = fct_relevel(lang, 'other', 'la', 'en', 'fr', 'de', 'yi')
  ) |> 
  ggplot(aes(lang,fill=`back suffix form`)) +
  geom_bar(position = position_dodge()) +
  # facet_wrap(~ suffix) +
  coord_flip() +
  theme_few() +
  scale_fill_brewer(palette = 'Pastel2') +
  xlab('language of borrowing')

p1 + p2

d |> 
  count(accept,suffix,stem,log_odds_adj) |> # also suffix!
  pivot_wider(names_from = accept, values_from = n, values_fill = 0) |> 
  mutate(
    log_odds_resp = log((`1`+1)/(`0`+1)),
    stem = fct_reorder(stem,log_odds_adj)
  ) |> 
  ggplot(aes(log_odds_resp,stem, group = stem, colour = suffix)) +
  geom_point() +
  geom_line() +
  scale_colour_grey() +
  theme_few() +
  ylab('ordered according to corpus preference') +
  xlab('log odds of accept/reject')

p3 = dexp |> 
  ggplot(aes(svm1_scaled,log_odds_resp,label=stem)) +
  geom_label() +
  geom_smooth(method = 'lm') +
  theme_few() +
  ylab('log(accept/reject)') +
  xlab('similarity, (SVM)') +
  facet_wrap( ~ suffix)

dexp |> 
  ggplot(aes(knn_2_weight,log_odds_resp, label = stem)) +
  geom_label() +
  geom_smooth(alpha = .5, method = 'gam') +
  theme_few() +
  xlab('similarity (KNN)') +
  ylab('log(accept/reject)')

p4 = dexp |> 
  ggplot(aes(log_odds_adj,log_odds_resp, label = stem)) +
  geom_label() +
  geom_smooth(method = 'lm') +
  theme_few() +
  xlab('corpus log odds') +
  ylab('log(accept/reject)') +
  facet_wrap( ~ suffix)

p4 / p3

## -- report -- ##

formula(fit1a)
fit1hs
