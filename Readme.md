## Lexical patterns in Hungarian vowel harmony 2: Electric Boogaloo

### Setup

The experiment is here: https://gitlab.pavlovia.org/petyaraczbme/noun-task
The previous paper is here: https://doi.org/10.5281/zenodo.12699305

### Workflow

0. make_list -> make exp stimuli
1. scrape_uesz -> convert_files -> find_dates: get etymology dates
2. get_word_distance -> distance_measures: get word distance based machine learner predictions (GCM, KNN, SVM)
3. setup_predictors -> parse_results: get word metadata from 1-2 and combine with _raw exp data from gitlab_ repository, remove outliers etc
4. analysis -> pre-registered analyses


### Files

| filename | desc |
| -- | -- |
|  script/analysis.R |  "analysis" | 
|  script/get_word_distance.R |  calculate phonological distance between back vowel + E nouns in Hungarian. see https://github.com/petyaracz/Racz2024b for exegesis | 
|  script/distance_measures.R |  use distance measures to calculate similarity to yiddish / latin words and similarity to back / front stems, save output | 
|  script/make_list.R |   |  make list for gitlab exp input | 
|  script/set_up_predictors.R create metadata file w/ all word-level predictors to combine w/ output | 
|  script/parse_results.R  |  parse results from the gitlab repo, add filters, add word data | 
|  script/find_dates.R |  parse the UESZ pdf downloads for year of borrowing x word | 
|  convert_files.sh |  this is for downloading and parsing the UESZ pdfs. this is a mess | 
|  script/scrape_uesz.py   |  download UESZ data | 
|  dat/filtered_data.tsv  |  filtered exp data | 
|  dat/unfiltered_data.tsv  |  all exp data | 
|  dat/distance_haver.tsv  |  word-aligned phonological distance table | 
|  dat/stim.js  |  gitlab exp input | 
|  dat/stems_with_similarity.tsv  |  similarity measures made from word-alignment table | 
|  dat/word_metadata.tsv  |  word metadata for target words in exp | 

### Data dictionary

There are 200 varying stems in RaczRebrus2024. We sampled these down to 50 for our experiment.

Dict for tidy data (filtered_dat/unfiltered_dat)

| col | desc |
| -- | -- |
| id |  participant id | 
| yob  |  participant year of birth | 
| age |    |  2024-yob | 
| gender |  part gend | 
| total_time |   participant sum rt | 
| i |  trial n / participant | 
| stem |  word stem | 
| target |  word target | 
| suffix |  suffix: pl or acc (v- or c-init) | 
| accept |  did part accept word | 
| rt |  response time | 
| transcription |  simple ipa transcription (one sound is one segment, assimilations and all that malarkey not marked: useful for calculating word dist) | 
| language |  source of borrowing: yiddish for haver, latin for kodex | 
| log_odds_adj |  adjusted log odds: nat log of back+1 / front+1 in corpus for stem | 
| neighbourhood_size |  n hungarian nouns that are not hapaxes in webcorpus2 and are at distance of 1 from stem | 
| llfpm10 |  log lemma freq per 10 million in webcorpus 2 | 
| stem_length |  stem length! "This is a door." | 
| stem_final |  final c cluster | 
| stem_phonology |  phonology cat of final c cluster | 
| stem_final_consonant_cluster |  is it two consonants? subset of stem_phonology | 
| log_odds_back_suffix |  log odds of stem with this part suffix in webcorpus 2 | 
| upper_time |  median + 3 mad total time across part | 
| lower_time |  median - 3 mad total time across part | 
| date |  date of word borrowing. either from uesz or I estimated it using the source lang. you'd think this is like date of recording? no | 
| yi_la |  output of generalised context model with grid search for s and p (but like both are 1) prediction for label "yiddish word" / "latin word". so like is this word more like the yiddish words (>.5) or more like the latin words (<.5) see script/distance_measures.R  | 
| knn |  the summed log odds of the 2 nearest neighbour stems of the stem in the webcorpus 2 (we grid searched for k) see script/distance_measures.R  | 
| x_phon |  x for 2-dim mds of phonological distance between words see script/distance_measures.R  | 
| y_phon |  y for 2-dim mds of phonological distance between words see script/distance_measures.R  | 