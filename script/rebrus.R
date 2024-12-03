setwd('~/Github/RaczRebrus2024cont/')
library(tidyverse)
library(glue)
library(ggthemes)

length(unique(d$id))

d = read_tsv('dat/filtered_data.tsv')

d = d |> 
  mutate(
    lang = ifelse(language %in% c('de','en','fr','la','yi'), language, 'other'),
    date2 = ntile(date, 5)
  ) |> 
  mutate(
    period = glue('{min(date)}-{max(date)}'),
    .by = date2
  ) |> 
  select(-date2) |> 
  ungroup()

d |> 
  mutate(`back suffix form` = ifelse(accept,'accept','reject')) |> 
  ggplot(aes(period,fill=`back suffix form`)) +
  geom_bar(position = position_dodge()) +
  # facet_wrap(~ suffix) +
  coord_flip() +
  theme_few() +
  scale_fill_colorblind() +
  xlab('period of borrowing')

d |> 
  mutate(
    `back suffix form` = ifelse(accept,'accept','reject'),
    lang = fct_relevel(lang, 'other', 'la', 'en', 'fr', 'de', 'yi')
         ) |> 
  ggplot(aes(lang,fill=`back suffix form`)) +
  geom_bar(position = position_dodge()) +
  # facet_wrap(~ suffix) +
  coord_flip() +
  theme_few() +
  scale_fill_colorblind() +
  xlab('language of borrowing')

d |> 
  count(accept,suffix,stem,log_odds_adj) |> 
  pivot_wider(names_from = accept, values_from = n, values_fill = 0) |> 
  mutate(
    log_odds_resp = log((`TRUE`+1)/(`FALSE`+1)),
    stem = fct_reorder(stem,log_odds_adj)
         ) |> 
  ggplot(aes(log_odds_resp,stem, group = stem, colour = suffix)) +
  geom_point() +
  geom_line() +
  scale_colour_viridis_d() +
  theme_few() +
  ylab('ordered according to corpus preference') +
  xlab('log odds of accept/reject')

d |> 
  mutate(
    `back suffix form` = ifelse(accept,'accept','reject'),
    svm_category = fct_relevel(svm_category, 'front')
         ) |> 
  ggplot(aes(svm_category,fill = `back suffix form`)) +
  geom_bar(position = position_dodge()) +
  coord_flip() +
  scale_fill_colorblind() +
  theme_few() +
  xlab('support vector machine category')

d |> 
  count(accept,suffix,stem,knn_2_weight) |> 
  pivot_wider(names_from = accept, values_from = n, values_fill = 0) |> 
  mutate(
    log_odds_resp = log((`TRUE`+1)/(`FALSE`+1))
  ) |> 
  ggplot(aes(knn_2_weight,log_odds_resp, label = stem)) +
  geom_label() +
  geom_smooth(alpha = .5, method = 'gam') +
  theme_few() +
  xlab('similarity (KNN)') +
  ylab('log(accept/reject)')
