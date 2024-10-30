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
# c = read_tsv('~/Github/published/Racz2024b/dat/tests.tz')

# -- setup -- #

d2 = d |> 
  left_join(s)

d2a = d2 |> 
  filter(language %in% c('yi','la'))

d2b = d2 |> 
  filter(
    !language %in% c('yi','la'),
    log_odds_back > -1,
    !stem %in% c('duplex','komplex','groteszk','komplett','halef','korrekt')
  )

real_words = bind_rows(d2a,d2b)  

# -- real words -- #

real_words |> 
  mutate(
    stem = fct_reorder(stem,log_odds_back),
         ) |> 
  ggplot(aes(log_odds_back,stem,fill = language)) +
  geom_col() +
  theme_few() +
  theme(axis.ticks.y = element_blank()) +
  scale_fill_colorblind()

# -- write -- #

real_words |> 
  write_tsv('dat/real_words.tsv')
