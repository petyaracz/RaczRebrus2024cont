# -- head -- #

setwd('~/Github/RaczRebrus2024cont/')

set.seed(1337)

library(tidyverse)
library(patchwork)
library(ggthemes)
library(broom)
library(glue)
library(jsonlite)

# 4) How many and which conditions will participants be assigned to?
# within-participant conditions: word preference for back / front forms in corpus, word language of origin, word date of borrowing, word length, word frequency, word neighbourhood density, word similarity to front / back stems, Hayes' criteria of word behaviour: stem ends in a bilabial stop, a sibilant, a coronal sonorant, or a consonant cluster.

# -- fun -- #

transcribeIPA = function(string, direction){
  if (direction == 'single'){
    stringr::str_replace_all(string, c(
      'ccs' = 'cscs', 'ssz' = 'szsz', 'zzs' = 'zszs', 'tty' = 'tyty', 'ggy' = 'gygy', 'nny' = 'nyny', 'lly' = 'jj', 'cs' = 'č', 'sz' = 'ß', 'zs' = 'ž', 'ty' = 'ṯ', 'gy' = 'ḏ', 'ny' = 'ṉ', 'ly' = 'j', 's' = 'š', 'ß' = 's'))
  } else if (direction == 'double'){
    stringr::str_replace_all(string, c('s' = 'ß', 'š' = 's', 'ṉ' = 'ny', 'ḏ' = 'gy', 'ṯ' = 'ty', 'ž' = 'zs', 'ß' = 'sz', 'č' = 'cs'))
  }
}
# check n size
countNeighbours = function(string,neighbour_forms){
  vector = neighbour_forms[neighbour_forms != string]
  dists = stringdist::stringdist(string, vector, method = 'lv')  
  length(dists[dists==1])
}

# -- read -- #

d = read_tsv('dat/master.tsv')
w = read_tsv('~/Github/RaczRebrus2024/dat/dat_wide_stems.tsv')
s = read_tsv('~/Github/RaczRebrus2024/dat/stemlanguage.tsv')
f = read_tsv('~/Github/Raczrebrus2024/dat/dat_wide.tsv')
r = read_tsv('~/Github/RaczRebrus2025/dat/noun_webcorpus2_hunspell.gz')

# -- add -- #

# stem freq, log odds for suffix
f2 = f |> 
  filter(suffix %in% c("Dat",'Pl')) |> 
  mutate(
    log_odds_back_suffix = log((back+1)/(front+1)),
    trial_type = str_to_lower(suffix)
         ) |> 
  select(stem,llfpm10,trial_type,log_odds_back_suffix)

# neighbourhood density
neighbour_forms = r |> 
  mutate(trans = transcribeIPA(lemma, 'single')) |> 
  filter(
    nchar(trans) > 4,
    nchar(trans) < 8 # range of length for data +- 1
  ) |> 
  pull(trans) |> 
  unique()

# count stem neighbours on transcribed stems
d2 = d |> 
  rowwise() |> 
  mutate(
    neighbourhood_size = countNeighbours(transcription, neighbour_forms)
  ) |> 
  ungroup()

# join lang of origin, stem freq, suffix odds
d3 = d2 |> 
  left_join(s) |> 
  left_join(f) |> 
  mutate(
    stem_length = nchar(stem),
    stem_final = str_extract(transcription, '(?<=e)[^e]+$'),
    stem_phonology = case_when(
      str_detect(stem_final, '[bp]$') ~ 'bilabial_stop',
      str_detect(stem_final, '[sšzž]$') ~ 'sibilant',
      str_detect(stem_final, '[nlrj]$') ~ 'coronal_sonorant'
      ),
    stem_final_consonant_cluster = nchar(stem_final > 1)
    # set up hayes cats: stem ends in a bilabial stop, a sibilant, a coronal sonorant, or a consonant cluster.
         )

d3 |> 
  select(stem,log_odds_adj,llfpm10,language,trial_type,neighbourhood_size,stem_length,stem_final,stem_phonology,stem_final_consonant_cluster,form_varies) |> 
  write_tsv('dat/word_metadata.tsv')
