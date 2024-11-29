setwd('~/Github/RaczRebrus2024cont/')
library(tidyverse)

words = read_tsv('dat/word_metadata.tsv')
szotar = read_lines('uesz/uesz.txt')

targets = words |> 
  distinct(stem) |> 
  pull()

targets_regex = targets |> 
  paste(collapse = '|')

targets_regex = paste0('(',targets_regex,')')

dir = tibble(
  raw = szotar
) |> 
  mutate(
    word = str_extract(raw, targets_regex)
  ) |> 
  fill(word) |> 
  slice(1:5, .by = word) |> 
  mutate(
    date = str_extract(raw, '1[0-9]{3}')
  )

words = dir |> 
  filter(!is.na(word),!is.na(date))

words |> 
  select(raw,word,date) |> 
  arrange(word) |> 
  write_tsv('dat/word_etym.tsv')
