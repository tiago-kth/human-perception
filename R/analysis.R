library(tidyverse)

data_raw <- read.csv2('./api/output.txt', header = FALSE)

colnames(data_raw) <- c(
  'timestamp',
  'age',
  'country',
  'gender',
  'instrument_xp',
  'instrument_desc',
  'music_xp',
  'music_desc',
  'genres',
  'colors'
)

color_numbers <- 0:8
colors <- c('red', 'orange', 'yellow', 'green', 'cyan', 'blue', 'violet', 'magenta')

data_pre <- data_raw %>%
  tidyr::separate(colors, into = c('colors_ex1', 'colors_ex2', 'colors_ex3', 'colors_ex4', 'colors_ex5', 'colors_ex6'), sep = ' ')

data_colors <- data_pre %>%
  separate(colors_ex1, into = paste0('color_ex1_', color_numbers), sep = ',') %>%
  separate(colors_ex2, into = paste0('color_ex2_', color_numbers), sep = ',') %>%
  separate(colors_ex3, into = paste0('color_ex3_', color_numbers), sep = ',') %>%
  separate(colors_ex4, into = paste0('color_ex4_', color_numbers), sep = ',') %>%
  separate(colors_ex5, into = paste0('color_ex5_', color_numbers), sep = ',') %>%
  separate(colors_ex6, into = paste0('color_ex6_', color_numbers), sep = ',') %>%
  select(-ends_with('_0')) 

data_long <- data_colors %>%
  mutate(id = row_number()) %>%
  gather(starts_with('color_ex1_'), key = 'color1', value = 'value1') %>%

