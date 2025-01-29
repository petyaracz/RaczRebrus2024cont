setwd('~/Github/RaczRebrus2024cont/')
library(tidyverse)

all = read_tsv('dat/unfiltered_data.tsv')
d = read_tsv('dat/filtered_data.tsv')

# number of participants dropped from data
length(unique(all$id)) - length(unique(d$id))

# all[!all$id %in% d$id,] |> 
  # count(id,accept)

# number of observations dropped from data
nrow(all[all$id %in% d$id,]) - nrow(d)

# number of participants in reported data
length(unique(d$id))

# number of men in reported data
length(unique(d[str_detect(d$gender, 'f'),]$id))

# median age in reported data
d |> 
  distinct(id,age) |> 
  filter(!is.na(age)) |> 
  summarise(median = median(age)) |> 
  pull(median)

# median response time in reported data w/ range
quantile(d$rt, c(.25,.5,.75)) / 1000

  