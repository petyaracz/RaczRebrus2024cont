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
library(performance)

# -- read -- #

d = read_tsv('dat/filtered_data.tsv')
ds = read_tsv('dat/ds.tsv') # for model plots
c = read_tsv('dat/corpus_with_year_and_language.tsv')
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
    period = glue::glue('Group {date2}, {min(date)}-{max(date)}'),
    .by = date2
  ) |>
  select(-date2) |> 
  ungroup()

# -- summaries -- #

dsum = d |> 
  distinct(stem,category,date,lang,log_odds_adj,neighbourhood_size,llfpm10,stem_length,stem_phonology,svm_weight_1,knn_2_weight,yi_la_weight,svm1_category,x_phon,y_phon)

dexp = d |> 
  count(accept,stem,category,date,lang,log_odds_adj,neighbourhood_size,llfpm10,stem_length,stem_phonology,svm_weight_1,knn_2_weight,yi_la_weight,svm1_category,x_phon,y_phon) |> 
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

## corpus
c1 = count(c,language) |> 
  left_join(c) |> 
  mutate(
    language2 = ifelse(n > 14, language, NA) |> 
      fct_relevel('la','en','fr','de','yi')
  ) |> 
  filter(!is.na(language2))
  
pc1 = c1 |> ggplot(aes(language2,log_odds_back)) +
  geom_rain() +
  coord_flip() +
  theme_few() +
  xlab('source language') +
  scale_y_continuous(sec.axis = sec_axis(trans = ~ plogis(.), name = 'corpus: p(back)', breaks = c(.01,.1,.5,.9,.99)), name = 'corpus: log (back / front)')

pc2 = c1 |> ggplot(aes(language2,svm_weight_1)) +
  geom_rain() +
  coord_flip() +
  theme_few() +
  xlab('source language') +
  ylab('similarity weight') +
  theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())
  
c |> 
  filter(!is.na(year)) |> 
  ggplot(aes(year,log_odds_back)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  theme_few()
# I'm beyond chuffed I've spent two hours on getting these data to see the most horizontal regression line on the planet

## sim corr

dsum |> 
  summarise(
    r1 = cor(yi_la_weight,svm_weight_1, method = 'spearman'),
    r2 = cor(yi_la_weight,knn_2_weight, method = 'spearman'),
    r3 = cor(knn_2_weight,svm_weight_1, method = 'spearman')
  )
  
# -- models -- #

plot_model(fit1a, 'est', transform = NULL, show.intercept = T, ci.lvl = .95) +
  geom_hline(yintercept = 0, lty = 3) +
  theme_bw() +
  theme(panel.grid.major.y = element_blank()) +
  ggtitle('Accept ~ ')

ggsave('fig/model_accept.png', dpi = 600, width = 5, height = 3)

plot_model(fit6a, 'est', transform = NULL, show.intercept = T, ci.lvl = .95) +
  geom_hline(yintercept = 0, lty = 3) +
  theme_bw() +
  theme(panel.grid.major.y = element_blank()) +
  ggtitle('RT ~ ')

ggsave('fig/model_rt.png', dpi = 600, width = 5, height = 3)

broom.mixed::tidy(fit1a, conf.int = T)
plogis(c(-4.67,     -2.49)  )

plot_model(fit1hs, 'est', transform = NULL, show.intercept = T, ci.lvl = .95) +
  geom_hline(yintercept = 0, lty = 3) +
  theme_bw() +
  theme(panel.grid.major.y = element_blank()) +
  ggtitle('Regularised model: Accept ~ ')

ggsave('fig/model_accept_hs.png', dpi = 600, width = 5, height = 3)

plot_model(fit6hs, 'est', transform = NULL, show.intercept = T, ci.lvl = .95) +
  geom_hline(yintercept = 0, lty = 3) +
  theme_bw() +
  theme(panel.grid.major.y = element_blank()) +
  ggtitle('Regularised model: RT ~ ')

ggsave('fig/model_rt_hs.png', dpi = 600, width = 5, height = 3)

plot_model(fit7hs, 'est', transform = NULL, show.intercept = F, ci.lvl = .95) +
  geom_hline(yintercept = 0, lty = 3) +
  theme_bw() +
  theme(panel.grid.major.y = element_blank()) +
  ggtitle('Regularised model: RT ~ ')

ggsave('fig/model_rt_hs_ok.png', dpi = 600, width = 5, height = 3)

# -- corpus vs experiment -- #

p1 = dexp |> 
  ggplot(aes(log_odds_resp,log_odds_adj, label = stem)) +
  geom_label() +
  geom_smooth(method = 'loess') +
  theme_bw() +
  scale_x_continuous(sec.axis = sec_axis(trans = ~ plogis(.), name = 'experiment: p(accept)', breaks = c(.1,.25,.5,.75,.9)), name = 'experiment: log (accept / reject)', limits = c(-4,3)) +
  scale_y_continuous(sec.axis = sec_axis(trans = ~ plogis(.), name = 'corpus: p(back)', breaks = c(.01,.1,.25,.5,.75,.9,.99)), name = 'corpus: log (back / front)')

p2 = dexp |> 
  mutate(lang = fct_relevel(lang, 'other', 'la', 'en', 'fr', 'de', 'yi')) |> 
  ggplot(aes(lang,log_odds_adj)) +
  geom_rain() +
  coord_flip() +
  theme_bw() +
  scale_y_continuous(sec.axis = sec_axis(trans = ~ plogis(.), name = 'corpus: p(back)', breaks = c(.01,.1,.25,.5,.75,.9,.99)), name = 'corpus: log (back / front)')

p3 = dexp |> 
  mutate(lang = fct_relevel(lang, 'other', 'la', 'en', 'fr', 'de', 'yi')) |> 
  ggplot(aes(lang,log_odds_resp)) +
  geom_rain() +
  coord_flip() +
  theme_bw() +
  scale_y_continuous(sec.axis = sec_axis(trans = ~ plogis(.), name = 'experiment: p(accept)', breaks = c(.1,.25,.5,.75,.9)), name = 'experiment: log (accept / reject)', limits = c(-4,3))

p4 = dexp |> 
  ggplot(aes(log_odds_adj,date, label = stem)) +
  geom_label() +
  geom_smooth(method = 'loess') +
  theme_bw() +
  ylab('date of borrowing') +
  scale_x_continuous(sec.axis = sec_axis(trans = ~ plogis(.), name = 'corpus: p(back)', breaks = c(.01,.1,.25,.5,.75,.9,.99)), name = 'corpus: log (back / front)')

p5 = dexp |> 
  ggplot(aes(log_odds_resp,date, label = stem)) +
  geom_label() +
  geom_smooth(method = 'loess') +
  theme_bw() +
  ylab('date of borrowing') +
  scale_x_continuous(sec.axis = sec_axis(trans = ~ plogis(.), name = 'experiment: p(accept)', breaks = c(.1,.25,.5,.75,.9)), name = 'experiment: log (accept / reject)', limits = c(-4,3))

p6 = dexp |> 
  ggplot(aes(log_odds_adj,svm_weight_1, label = stem)) +
  geom_label() +
  geom_smooth(method = 'loess') +
  theme_bw() +
  ylab('similarity weight') +
  scale_x_continuous(sec.axis = sec_axis(trans = ~ plogis(.), name = 'corpus: p(back)', breaks = c(.01,.1,.25,.5,.75,.9,.99)), name = 'corpus: log (back / front)')

p7 = dexp |> 
  ggplot(aes(log_odds_resp,svm_weight_1, label = stem)) +
  geom_label() +
  geom_smooth(method = 'loess') +
  theme_bw() +
  ylab('similarity weight') +
  scale_x_continuous(sec.axis = sec_axis(trans = ~ plogis(.), name = 'experiment: p(accept)', breaks = c(.1,.25,.5,.75,.9)), name = 'experiment: log (accept / reject)', limits = c(-4,3))

p1 / (p2 + p3) / (p4 + p5) / (p6 + p7) + plot_annotation(tag_levels = 'i')

ggsave('fig/comp_corpus_exp.png', dpi = 600, width = 10, height = 12)

pc1 + pc2
ggsave('fig/corpus.png', dpi = 600, width = 8, height = 6)
