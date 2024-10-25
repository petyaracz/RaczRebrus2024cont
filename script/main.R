# -- head -- #

setwd('~/Github/RaczRebrus2024cont/')

library(tidyverse)
library(patchwork)
library(ggthemes)

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

non_words |> 
  ggplot(aes(gcm,log_odds)) +
  geom_point() +
  geom_smooth()

non_words |> 
  ggplot(aes(mgl,log_odds)) +
  geom_point() +
  geom_smooth()

non_words |> 
  ggplot(aes(knn,log_odds)) +
  geom_point() +
  geom_smooth()

summary(glm(cbind(resp1,resp2) ~ 1 + gcm + mgl, data = non_words, family = binomial))

real_words |> 
  mutate(stem = fct_reorder(stem,log_odds_back)) |> 
  ggplot(aes(log_odds_back,stem,fill = language)) +
  geom_col() +
  theme_few() +
  theme(axis.ticks.y = element_blank()) +
  scale_fill_colorblind()
