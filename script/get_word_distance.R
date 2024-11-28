# we calculate distance between words to use it in some sort of learning algorithm
# calculating edit distance is easy: bakter -> pakter == 1, bakter -> sakter == 1
# we also want phonological distance, where bakter is closer to sakter than batker is to sakter (first difference: manner of articulation between b/p. second one: manner and place of articulation between b/s.)
# we need to align words and use a phonological feature matrix to get this.
# luckily we've written all the code already in Racz2024b
# the words themselves, the original words, are in RaczRebrus2024
# let'sa go

# -- setup -- #

setwd('~/Github/RaczRebrus2024cont/')

library(tidyverse)

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

# -- get stuff -- #

# clone Racz2024b RaczRebrus2024 from github/petyaracz, then

# source functions
source('~/Github/Racz2024b/code/helper.R')
# get segmental distance lookup (based on feature matrix)
lookup_h = read_tsv('~/Github/Racz2024b/dat/segmental_distances/siptar_torkenczy_toth_racz_hungarian_dt.tsv')
# get words
words = read_tsv('~/Github/RaczRebrus2024/dat/dat_wide_stems.tsv')

# -- calc dist -- #

# transcribe words
words$string = transcribeIPA(words$stem, 'single') # needs to be called string bud

# align words, calc distances. RESOURCE INTENSIVE
alignments = runLookup(words, words, lookup_h)

# -- write -- #
write_tsv(alignments, 'dat/alignments_haver.tsv')

# dat_matrix = dat2 |>
#   pivot_wider(names_from = segment2, values_from = dist) |> 
#   select(-segment1) |> 
#   as.matrix()
# dat_mds = stats::cmdscale(dat_matrix, k = 2)