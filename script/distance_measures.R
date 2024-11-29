setwd('~/Github/RaczRebrus2024cont')

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
      category %in% c('yi','la') # !!!! and I can't emphasise this enough, !!!!
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
      weight = category_sim / total_sim
    ) |> 
    filter(category == 'yi') |> 
    distinct(
      test,weight
    ) |> 
    rename(transcription = test)
  
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

# take target form, get nearest neighbours, calc acc
KNNphon = function(target_form,dist,my_k){
  f_d = dist |> 
    filter(
      target_form == test,
      target_form != training
    ) |> 
    arrange(phon_dist) |> 
    slice(1:my_k) |> 
    select(training) |> 
    rename(transcription = training) |> 
    left_join(d, by = join_by(transcription))
  
  f_d |> 
    summarise(
      back = sum(back), 
      front = sum(front)
    ) |> 
    mutate(weight = log((back+1)/(front+1))) |> 
    pull(weight)
  
}

# fit loo knn
looKNN = function(dat, my_k){
  
  dats = dat |> 
    rowwise() |> 
    mutate(
      weight = KNNphon(transcription, dist, my_k)
    ) |> 
    ungroup() |> 
    select(transcription,weight)
  
  return(dats)
}

# get accuracy for model
getAccuracy = function(dats,d){
  
  d2 = dats |> 
    left_join(d, by = join_by(transcription))
  
  fit1 = glm(cbind(back,front) ~ weight, data = d2, family = binomial)
  performance::r2_kullback(fit1)
  
}

# -- read -- #

dist = read_tsv('~/Github/RaczRebrus2024cont/dat/distance_haver.tsv')
c = read_tsv('~/Github/RaczRebrus2024/dat/dat_wide_stems.tsv')
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

res$value[[1]]

res2 = res |> 
  mutate(
    accuracy = map_dbl(value, ~ getAccuracy(.,d))
  ) |> 
  arrange(-accuracy)

pred_gcm = res2$value[[1]] |> 
  select(transcription,weight) |> 
  rename(yi_la_weight = weight)

d = pred_gcm |> 
  left_join(d) |> 
  mutate(origin = ifelse(language %in% c('de','en','fr','yi','la'), language, 'other'))

d |> 
  filter(origin %in% c('yi','la')) |> 
  mutate(gcm_yi = yi_la_weight > .5) |> 
  count(gcm_yi,origin) |> 
  pivot_wider(names_from = origin, values_from = n) # mm

# -- knn -- #

# walk before you run
looKNN(d, 3)

hyperparameters2 = 
  tibble(k = 1:25)

res3 = hyperparameters2 |> 
  mutate(
    knn = map(k, ~ looKNN(dat = d, my_k = .))
  )

res4 = res3 |> 
  mutate(
    accuracy = map_dbl(knn, ~ getAccuracy(., d))
  ) |> 
  arrange(-accuracy)

res4 # ho boy

pred_knn = res4$knn[[1]] |> 
  select(transcription,weight) |> 
  rename(knn_2_weight = weight)

d = pred_knn |> 
  left_join(d) 

d |> 
  ggplot(aes(log_odds_back,knn_2_weight)) +
  geom_point() +
  geom_smooth() # mhm

# -- write -- #

d |> 
  select(stem,transcription,knn_2_weight,yi_la_weight,x_phon,y_phon) |> 
  write_tsv('dat/stems_with_similarity.tsv')
