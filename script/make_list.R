# -- head -- #

setwd('~/Github/RaczRebrus2024cont/')

set.seed(1337)

library(tidyverse)
library(patchwork)
library(ggthemes)
library(broom)
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

d = read_tsv('~/Github/RaczRebrus2024/dat/dat_wide_stems.tsv')
s = read_tsv('~/Github/RaczRebrus2024/dat/stemlanguage.tsv')
# c = read_tsv('~/Github/published/Racz2024b/dat/tests.tz')

# -- build master list -- #

d2 = d |> 
  left_join(s) |>
  mutate(
    log_odds_adj = log((back+1)/(front+1)),  
    stem = fct_reorder(stem, log_odds_adj),
    quantile_stem_freq = ntile(stem_freq, 4)
  ) |> 
  arrange(log_odds_adj)

master = d2 |> 
  filter(
    quantile_stem_freq > 1,
    !stem %in% c('mutter','charter','rocker','kadet')
         ) |> 
  slice(1:25,122:146) |> 
  select(-sd_back,-stem_varies) |> 
  mutate(transcription = transcribeIPA(stem, 'single'))

# -- stimulus, prompt, target, json -- #

# https://www.jspsych.org/latest/plugins/html-keyboard-response/

master = master |> 
  rowwise() |> 
  mutate(
    pl_back = glue('{stem}ok'),
    pl_front = glue('{stem}ek'),
    ine_back = glue('{stem}ban'),
    ine_front = glue('{stem}ben'),
    z = ifelse(str_detect(stem, '^[aáeéiíoóöőuúüű]'),'z','')
  ) |> 
  pivot_longer(c(pl_back,pl_front,ine_back,ine_front), names_to = 'trial_type', values_to = 'target')

master2 = master |> 
  mutate(
    prompt = case_when(
      str_detect(trial_type, 'pl_') ~ glue('<p>Ez egy {stem}. Ezek itt {target}.</p>'),
      str_detect(trial_type, 'ine_') ~ glue('<p>Ez egy {stem}. Bízom a{z} {target}.</p>')
    ),
    prompt = glue('{prompt}<p><span style="font-size:24px; color:red;">nem: "f"</span>&emsp;<span style="font-size:24px; color:green;">igen: "j"</span></p>'),
    stimulus = glue('<p style="font-size:48px;">{target}</p>'),
    trial_front = str_detect(trial_type, 'front$'),
    trial_pl = str_detect(trial_type, '^pl'),
    choices = list(c('f','j'))
  )

# -- write -- #

write_tsv(master2, 'dat/master.tsv')
stim = master2 |> 
  toJSON(pretty = TRUE)
stim = paste0('stim = ', stim)
write_lines(stim, 'dat/stim.js')
