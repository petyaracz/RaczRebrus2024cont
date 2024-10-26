# -- head -- #

setwd('~/Github/RaczRebrus2024cont/')

set.seed(1337)

library(tidyverse)
library(patchwork)
library(ggthemes)
library(broom)

# -- read -- #

d = read_tsv('~/Github/RaczRebrus2024/dat/dat_wide_stems.tsv')
s = read_tsv('~/Github/RaczRebrus2024/dat/stemlanguage.tsv')
c = read_tsv('~/Github/published/Racz2024b/dat/tests.tz')

# -- setup -- #

word_yi = s[s$language == 'yi',]$stem
word_la = s[s$language == 'la',]$stem

real_words = d |> 
  filter(stem %in% c(word_yi,word_la)) |> 
  mutate(
    language = case_when(
      stem %in% word_yi ~ 'yi',
      stem %in% word_la ~ 'la'
    )
           )

non_words = c |> 
  filter(
    variation == 'hotelban/hotelben',
    vowel == 'e'
    )

# -- non words -- #

p1 = non_words |> 
  ggplot(aes(gcm,log_odds)) +
  geom_point() +
  geom_smooth()

p2 = non_words |> 
  ggplot(aes(mgl,log_odds)) +
  geom_point() +
  geom_smooth()

p3 = non_words |> 
  ggplot(aes(knn,log_odds)) +
  geom_point() +
  geom_smooth()

p1 + p2 + p3

summary(glm(cbind(resp1,resp2) ~ 1 + gcm + mgl, data = non_words, family = binomial))

fit1 = glm(cbind(resp1,resp2) ~ 1 + gcm, data = non_words, family = binomial)

non_words$resid = resid(fit1)

non_words |> 
  mutate(base = fct_reorder(base, resid)) |> 
  ggplot(aes(resid,base)) +
  geom_col()

non_words |> 
  ggplot(aes(gcm,log_odds,colour = resid)) +
  geom_point() +
  scale_colour_viridis_b()

# we're simulating

sim = tibble(
  id = 1:10000
) |> 
  rowwise() |> 
  mutate(
    data = list(sample_n(non_words, 50))
    ) |> 
  ungroup() |> 
  mutate(
    model = map(data, ~ glm(cbind(resp1,resp2) ~ gcm, data = ., family = binomial)),
    sum = map(model, tidy)
  ) |> 
  unnest(sum) |> 
  filter(term == 'gcm') |> 
  mutate(
    abs_est = abs(estimate)
  ) |> 
  filter(abs_est == max(abs_est))

non_words2 = sim$data[[1]]

non_words2 |> 
  ggplot(aes(gcm,log_odds)) +
  geom_point() +
  geom_smooth()

# -- real words -- #

real_words |> 
  mutate(stem = fct_reorder(stem,log_odds_back)) |> 
  ggplot(aes(log_odds_back,stem,fill = language)) +
  geom_col() +
  theme_few() +
  theme(axis.ticks.y = element_blank()) +
  scale_fill_colorblind()

# -- write -- #

non_words2 |> 
  write_tsv('dat/non_words.tsv')
real_words |> 
  write_tsv('dat/real_words.tsv')
