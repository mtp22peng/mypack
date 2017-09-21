experiment1 <-
  read.csv('accident_2015.csv.bz2') %>%
  mutate(experiment = 1)
devtools::use_data(experiment1)
