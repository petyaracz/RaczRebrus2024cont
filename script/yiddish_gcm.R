# use word distances to make figures and fit a gcm.
# gcm categories: word source of origin!

# -- setup -- #

library(tidyverse)
library(ggthemes)
library(patchwork)

# -- fun -- #

# Hungarian orthography: replace characters in digraphs with their IPA equivalents or vice versa
transcribeIPA = function(string, direction){
  if (direction == 'single'){
    stringr::str_replace_all(string, c(
      'ccs' = 'cscs', 'ssz' = 'szsz', 'zzs' = 'zszs', 'tty' = 'tyty', 'ggy' = 'gygy', 'nny' = 'nyny', 'lly' = 'jj', 'cs' = 'č', 'sz' = 'ß', 'zs' = 'ž', 'ty' = 'ṯ', 'gy' = 'ḏ', 'ny' = 'ṉ', 'ly' = 'j', 's' = 'š', 'ß' = 's', 'x' = 'ks'))
  } else if (direction == 'double'){
    stringr::str_replace_all(string, c('s' = 'ß', 'š' = 's', 'ṉ' = 'ny', 'ḏ' = 'gy', 'ṯ' = 'ty', 'ž' = 'zs', 'ß' = 'sz', 'č' = 'cs'))
  }
}

# build mds
buildMDS = function(dist){
  
  dat_matrix_phon = dist |>
    select(-edit_dist) |> 
    pivot_wider(names_from = test, values_from = phon_dist) |>
    select(-training) |>
    as.matrix()
  
  dat_matrix_edit = dist |>
    select(-phon_dist) |> 
    pivot_wider(names_from = test, values_from = edit_dist) |>
    select(-training) |>
    as.matrix()
  
  mds_phon = stats::cmdscale(dat_matrix_phon, k = 2)
  mds_edit = stats::cmdscale(dat_matrix_edit, k = 2)
  
  mds_table = tibble(
    transcription = unique(dist$training),
    x_phon = mds_phon[,1],
    y_phon = mds_phon[,2],
    x_edit = mds_edit[,1],
    y_edit = mds_edit[,2]
  )
}

# fit gcm on single word
singleGCM = function(dat, var_s, var_p){
  
  dists = dat |> 
    filter(
      training != test,
      category != 'other' # !!!! and I can't emphasise this enough, !!!!
    ) |> 
    mutate(
      pairwise_sim = exp ( - dist / var_s )^var_p,
      total_sim = sum(pairwise_sim)
    ) |> 
    group_by(category) |> 
    mutate(
      category_sim = sum(pairwise_sim),
    ) |> 
    ungroup() |> 
    mutate(
      category_weight = category_sim / total_sim
    ) |> 
    distinct(
      test,category,category_weight
    )
  return(dists)
}

# fit loo gcm
looGCM = function(dat, var_s, var_p){
  
  dats = dat |> 
    mutate(id = test) |> 
    nest(.by = id) |> 
    mutate(
      gcm = map(data, ~ singleGCM(., var_s, var_p))
    ) |> 
    select(-id,-data) |> 
    unnest(gcm)
  
  return(dats)
}

# get accuracy using a humble glm
getAccuracy = function(gcm_out,d){
  yi_la = gcm_out |> 
    filter(category %in% c('yi','la')) |> 
    pivot_wider(names_from = category, values_from = category_weight) |> 
    mutate(yi_la = yi/la) |> 
    select(test,yi_la)
  
  d2 = yi_la |> 
    rename(transcription = test) |> 
    left_join(d)
  fit1 = glm(cbind(back,front) ~ yi_la, data = d2, family = binomial)
  performance::r2_kullback(fit1)
  
}

# -- read -- #

dist = read_tsv('~/Github/RaczRebrus2024cont/dat/distance_haver.tsv')
c = read_tsv('~/Github/RaczRebrus2024/dat/dat_wide_stems.tsv')
k = read_tsv('~/Github/RaczRebrus2024/dat/dat_wide_stems_knn.tsv')
l = read_tsv('~/Github/RaczRebrus2024/dat/stemlanguage.tsv')

# -- do distance -- #

mds_table = buildMDS(dist)

d = c |> 
  mutate(
    transcription = transcribeIPA(stem, 'single'),
  ) |> 
  left_join(l) |> 
  left_join(mds_table)

# -- do GCM -- #

dist2 = d |> 
  mutate(category = ifelse(language %in% c('de','en','fr','yi','la'), language, 'other')) |> 
  select(transcription,category) |> 
  rename(training = transcription) |> 
  left_join(dist)

phond = dist2 |> 
  rename(dist = phon_dist) |> 
  select(-edit_dist)

editd = dist2 |> 
  rename(dist = edit_dist) |> 
  select(-phon_dist)

hyperparameters = crossing(
  p = c(1,2),
  s = seq(0.1,1,0.1)
)

# walk before you run
outp = looGCM(dat = phond, var_s = .5, var_p = 1)
getAccuracy(outp, d)

res = hyperparameters |> 
  mutate(
    phon_gcm = map2(s, p, 
                    ~ looGCM(dat = phond, var_s = .x, var_p = .y)
                    ),
    edit_gcm = map2(s, p, 
                    ~ looGCM(dat = editd, var_s = .x, var_p = .y)
    )
  ) |> 
  pivot_longer(-c(p,s))

res2 = res |> 
  mutate(
    accuracy = map_dbl(value, ~ getAccuracy(.,d))
  ) |> 
  arrange(-accuracy)

res2
# hmm

pred = res2$value[[1]] |> 
  filter(category %in% c('yi','la')) |> 
  pivot_wider(names_from = category, values_from = category_weight) |> 
  mutate(yi_la = yi/la) |> 
  select(test,yi_la)

k = k |> 
  select(stem,knn)

d = pred |> 
  rename(transcription = test) |> 
  left_join(d) |> 
  left_join(k)

# -- viz -- #

## mdc

d |> 
  ggplot(aes(x_edit,y_edit,fill = log_odds_back,label = stem)) +
  geom_label(colour = 'white') +
  geom_hline(yintercept = 0, alpha = .5, lty = 3) +
  geom_vline(xintercept = 0, alpha = .5, lty = 3) +
  theme_void() +
  scale_fill_viridis_b() +
  labs(fill = 'log(back/front)') +
  ggtitle('edit distance')

d |> 
  ggplot(aes(x_phon,y_phon,fill = log_odds_back,label = stem)) +
  geom_label(colour = 'white') +
  theme_void() +
  geom_hline(yintercept = 0, alpha = .5, lty = 3) +
  geom_vline(xintercept = 0, alpha = .5, lty = 3) +
  scale_fill_viridis_b() +
  labs(fill = 'log(back/front)') +
  ggtitle('phon distance')

d |> 
  mutate(lang = ifelse(language %in% c('de','en','fr','yi','la'), language, 'other') |> 
           fct_relevel('la','en','fr','de','yi','other')
  ) |> 
  ggplot(aes(x_phon,y_phon,fill = lang,label = stem)) +
  geom_label(colour = 'white') +
  geom_hline(yintercept = 0, alpha = .5, lty = 3) +
  geom_vline(xintercept = 0, alpha = .5, lty = 3) +
  theme_void() +
  scale_fill_colorblind() +
  labs(fill = 'src language') +
  ggtitle('phon distance')

d |> 
  mutate(lang = ifelse(language %in% c('yi','la'), language, 'other') |> 
           fct_relevel('yi')
  ) |> 
  filter(lang != 'other') |> 
  ggplot(aes(x_phon,y_phon,fill = lang,label = stem)) +
  geom_label(colour = 'white') +
  geom_hline(yintercept = 0, alpha = .5, lty = 3) +
  geom_vline(xintercept = 0, alpha = .5, lty = 3) +
  theme_void() +
  scale_fill_colorblind() +
  labs(fill = 'src language') +
  ggtitle('phon distance')

p4 = d |> 
  mutate(lang = ifelse(language %in% c('de','en','fr','yi','la'), language, 'other') |> 
           fct_relevel('la','en','fr','de','yi','other')
  ) |> 
  ggplot(aes(x_phon,y_phon,fill = lang,label = stem)) +
  geom_label(colour = 'white') +
  geom_hline(yintercept = 0, alpha = .5, lty = 3) +
  geom_vline(xintercept = 0, alpha = .5, lty = 3) +
  theme_void() +
  scale_fill_colorblind() +
  labs(fill = 'src language') +
  ggtitle('phon distance')

## gcm

d$yi_la = scales::rescale(d$yi_la)

fit1 = glm(cbind(back,front) ~ knn, data = d, family = binomial)
fit2 = glm(cbind(back,front) ~ yi_la, data = d, family = binomial)
fit3 = glm(cbind(back,front) ~ language, data = d, family = binomial)
fit4 = glm(cbind(back,front) ~ yi_la + language, data = d, family = binomial)

performance::r2_kullback(fit1)
performance::r2_kullback(fit2)
performance::r2_kullback(fit3)
performance::r2_kullback(fit4)
performance::check_collinearity(fit4)
performance::check_model(fit4)

sjPlot::plot_model(fit4, 'pred', terms = 'language')
sjPlot::plot_model(fit4, 'pred', terms = 'yi_la') +
  theme_bw() +
  ylab('p(back/front)') +
  xlab('similarity to yiddish / similarity to latin')
ggsave('~/Documents/lectures_apps/lectures/Siptár/figures/yiddish_latin.pdf', width = 6, height = 3)

# -- write -- #

d |> 
  select(stem,transcription,yi_la,knn,x_phon,y_phon) |> 
  write_tsv('dat/stems_with_similarity.tsv')
