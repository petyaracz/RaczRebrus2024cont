# -- head -- #

setwd('~/Github/RaczRebrus2024cont/')

set.seed(1337)

library(tidyverse)
library(glue)
library(jsonlite)

# -- fun -- #

# Hungarian orthography: replace characters in digraphs with their IPA equivalents or vice versa
transcribeIPA = function(string, direction){
  if (direction == 'single'){
    stringr::str_replace_all(string, c(
      'ccs' = 'cscs', 'ssz' = 'szsz', 'zzs' = 'zszs', 'tty' = 'tyty', 'ggy' = 'gygy', 'nny' = 'nyny', 'lly' = 'jj', 'cs' = 'č', 'sz' = 'ß', 'zs' = 'ž', 'ty' = 'ṯ', 'gy' = 'ḏ', 'ny' = 'ṉ', 'ly' = 'j', 's' = 'š', 'ß' = 's'))
  } else if (direction == 'double'){
    stringr::str_replace_all(string, c('s' = 'ß', 'š' = 's', 'ṉ' = 'ny', 'ḏ' = 'gy', 'ṯ' = 'ty', 'ž' = 'zs', 'ß' = 'sz', 'č' = 'cs'))
  }
}

# -- read -- #

rw = read_tsv('dat/real_words.tsv')
nw = read_tsv('dat/non_words.tsv')

# -- setup -- #

rw = rw |> 
  select(stem,back,front,log_odds_back,stem_freq,language) |> 
  mutate(type = 'real word')
nw = nw |> 
  select(base,resp1,resp2,log_odds) |> 
  rename(
    stem = base,
    back = resp1,
    front = resp2,
    log_odds_back = log_odds
         ) |> 
  mutate(type = 'non word')

master = bind_rows(nw,rw) |> 
  mutate(transcription = transcribeIPA(stem, 'single'))

# -- prompt, target, json -- #

pl = master |> 
  rowwise() |> 
  mutate(
    prompt = glue('Ez egy {stem}. Ezek itt...'),
    target_words = list(c(glue('{stem}ok'), glue('{stem}ek')))
  ) |> 
  ungroup()

ine = master |> 
  rowwise() |> 
  mutate(
    z = ifelse(str_detect(stem, '^[aáeéiíoóöőuúüű]'),'z',''),
    prompt = glue('Ez egy {stem}. Bízom a{z}...'),
    target_words = list(c(glue('{stem}ban'), glue('{stem}ben')))
  ) |> 
  ungroup()

out = bind_rows(pl,ine)

# -- write -- #

write_tsv(out, 'dat/master.tsv')
stim = out |> 
  toJSON(pretty = TRUE)
stim = paste0('stim = ', stim)
write_lines(stim, 'dat/stim.js')
