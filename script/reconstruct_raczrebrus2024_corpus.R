# -- head -- #

setwd('~/Github/RaczRebrus2024cont')

library(tidyverse)

# -- read -- #

words = read_tsv('dat/stems_with_similarity.tsv')
languages = read_tsv('https://raw.githubusercontent.com/petyaracz/RaczRebrus2024/refs/heads/main/dat/stemlanguage.tsv')
log_odds = read_tsv('https://raw.githubusercontent.com/petyaracz/RaczRebrus2024/refs/heads/main/dat/dat_wide_stems.tsv')

# -- uesz -- #

uesz = read_lines('uesz/uesz.txt')
wordstem = paste(words$stem, collapse = '|')
matches = str_detect(uesz, paste0('^',wordstem))
indices = which(matches)
# keep = sort(c(indices,indices + 1, indices + 2))
uesz2 = uesz[matches]
write_lines(uesz2, 'uesz/mini_uesz.txt')
# I tried using the openai api but this literally looks easier by hand
uesz3 = uesz2[str_detect(uesz2, paste0('(', wordstem, ') A: [0-9]{4}'))]
uesz_w = str_extract(uesz3, '[^ ]+(?= A)') |> 
  str_replace('\\f', '') |> 
  str_replace('~', '')
uesz_y = str_extract(uesz3, '(?<=A: )[0-9]{4}')
years = tibble(
  stem = uesz_w, 
  year = uesz_y
  )

# -- combine -- #

everything = words |> 
  left_join(languages) |> 
  left_join(years) |> 
  arrange(year) |> 
  slice(1, .by = stem) |> 
  left_join(log_odds)

# -- write -- #

write_tsv(everything, 'dat/corpus_with_year_and_language.tsv')
