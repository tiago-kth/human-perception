library(tidyverse)
library(forcats)

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

# Clean-up countries

#dput(data_raw$country %>% unique())
countries <- data.frame(
  country = c("Brazil", "Dutch", "Swedish", "German", "China", "Spanish", 
              "Croatian", "Irish", "Taiwan", "Brazilian", "indian", "TESTE", 
              "Sweden", "Singapore ", "Italian ", "Italian", "Indian", "Indian ", 
              "India", "Brasileira", "Brasileiro ", "Brasileira ", "Brasil", 
              "brasileira", "Brasileiro", "Brasil ", "Bra", "French", "brasileiro", 
              "Honduras ", "Chile", "Mexican", "Mexico", "Uruguayan", "Uruguay ", 
              "brasil", "", "PERU", "Bradileira", "Mexican ", "mexican", "Brazilian "
  ),
  
  country_clean = c("Brazil", "Netherlands", "Sweden", "Germany", "China", "Spain", 
                    "Croatia", "Ireland", "Taiwan", "Brazil", "India", "TESTE", 
                    "Sweden", "Singapore", "Italia", "Italia", "India", "India", 
                    "India", "Brazil", "Brazil", "Brazil", "Brazil", 
                    "Brazil", "Brazil", "Brazil", "Brazil", "France", "Brazil", 
                    "Honduras", "Chile", "Mexico", "Mexico", "Uruguay", "Uruguay", 
                    "Brazil", "", "Peru", "Brazil", "Mexico", "Mexico", "Brazil"
  )
)

excertps <- data.frame(
  excerpt = c('01', '02', '03', '04', '05', '06'),
  expression = c('Tender', 'Tender', 'Sorrow', 'Sorrow', 'Joy', 'Joy')
)

color_numbers <- 0:8
colors <- c('red', 'orange', 'yellow', 'green', 'cyan', 'blue', 'violet', 'magenta')

data_clean <- data_raw %>%
  filter(!(country %in% c('TESTE', ''))) %>%
  distinct() %>%
  left_join(countries) 

ggplot(data_pre, aes(y = fct_rev(fct_infreq(country_clean)))) + 
  geom_bar() +
  geom_text(aes(label = ..count.., x = ..count..), stat = 'count', nudge_x = 1, hjust = 'left')

ggplot(data_pre, aes(y = instrument_xp)) + 
  geom_bar() +
  geom_text(aes(label = ..count.., x = ..count..), stat = 'count', nudge_x = 1, hjust = 'left')

ggplot(data_pre, aes(y = music_xp)) + 
  geom_bar() +
  geom_text(aes(label = ..count.., x = ..count..), stat = 'count', nudge_x = 1, hjust = 'left')

ggplot(data_pre, aes(y = fct_rev(fct_infreq(gender)))) + 
  geom_bar() +
  geom_text(aes(label = ..count.., x = ..count..), stat = 'count', nudge_x = 1, hjust = 'left')

ggplot(data_pre) +
  geom_histogram(aes(x = age), binwidth = 1)

data_pre <- data_clean %>%
  tidyr::separate(colors, into = c('colors_ex1', 'colors_ex2', 'colors_ex3', 'colors_ex4', 'colors_ex5', 'colors_ex6'), sep = ' ') %>%
  mutate(id = row_number())
    
# data_colors <- data_pre %>%
#   separate(colors_ex1, into = paste0('color_ex1_', color_numbers), sep = ',') %>%
#   separate(colors_ex2, into = paste0('color_ex2_', color_numbers), sep = ',') %>%
#   separate(colors_ex3, into = paste0('color_ex3_', color_numbers), sep = ',') %>%
#   separate(colors_ex4, into = paste0('color_ex4_', color_numbers), sep = ',') %>%
#   separate(colors_ex5, into = paste0('color_ex5_', color_numbers), sep = ',') %>%
#   separate(colors_ex6, into = paste0('color_ex6_', color_numbers), sep = ',') %>%
#   select(-ends_with('_0')) 
# 
# data_long <- data_colors %>%
#   gather(starts_with('color_ex1_'), key = 'color1', value = 'value1')

# replaces that for a map

process_colors <- function(excerpt_number, data) {
  
  data_temp <- data %>% 
    select(
      id, 
      timestamp, 
      age, 
      -country, 
      country = country_clean,
      gender, 
      instrument_xp, 
      instrument_desc, 
      music_xp,
      music_desc,
      genres,
      paste0('colors_ex', excerpt_number)
    ) %>%
    separate(paste0('colors_ex', excerpt_number), into = paste0('color_', color_numbers), sep = ',') %>%
    rename(excerpt = color_0)
    
  return(data_temp)
  
}

excerpts_numbers <- 1:6

data_excerpts <- purrr::map(excerpts_numbers, process_colors, data_pre)

data_joined <- data_excerpts %>% bind_rows() 

data_long <- data_joined %>%
  gather(starts_with('color_'), key = 'color_position', value = 'color') %>%
  filter(!is.na(color)) %>%
  left_join(excertps)

ggplot(data_long, aes(y = fct_rev(fct_infreq(excerpt)))) + 
  geom_bar() +
  geom_text(aes(label = ..count.., x = ..count..), stat = 'count', nudge_x = 1, hjust = 'left')

ggplot(data_long, aes(y = fct_rev(fct_infreq(color)))) + 
  geom_bar(aes(fill = color)) +
  geom_text(aes(label = ..count.., x = ..count..), stat = 'count', nudge_x = 1, hjust = 'left') +
  scale_fill_identity() +
  facet_wrap(~excerpt)

ggplot(data_long, aes(y = fct_rev(fct_infreq(expression)))) + 
  geom_bar() +
  geom_text(aes(label = ..count.., x = ..count..), stat = 'count', nudge_x = 1, hjust = 'left')


ggplot(data_long, aes(y = fct_rev(fct_infreq(color)))) + 
  geom_bar(aes(fill = color)) +
  geom_text(aes(label = ..count.., x = ..count..), stat = 'count', nudge_x = 1, hjust = 'left') +
  scale_fill_identity() +
  facet_wrap(~expression)


ggplot(data_long, aes(y = fct_rev(fct_infreq(color)), group = expression)) + 
  geom_bar(aes(fill = color), stat='count') +
  geom_text(aes(label = scales::percent(..prop..), x = ..prop..), stat = 'count', nudge_x = 1, hjust = 'left') +
  scale_fill_identity() +
  facet_wrap(~expression)




data_expression <- data_long %>%
  mutate(country = ifelse(country == 'Brazil', 'Brazil', 'Other')) %>%
  group_by(country, expression) %>%
  mutate(sum_country_expression = n()) %>%
  group_by(country, expression, color) %>%
  mutate(n = n(), pct = n / sum_country_expression) %>%
  select(country, expression, color, pct) %>%
  distinct()

#data_expression %>% group_by(country, expression) %>% summarise(sum(pct))

ggplot(data_expression, aes(x = pct, y = color)) + 
  geom_col(aes(fill = color)) +
  #geom_text(aes(label = pct), nudge_x = 1, hjust = 'left') +
  scale_fill_identity() +
  scale_x_continuous(labels = scales::percent) +
  facet_grid(vars(country), vars(expression)) + 
  theme_bw()

# color combinations


data_expression_instrument <- data_long %>%
  group_by(instrument_xp, expression) %>%
  mutate(sum_inst_expression = n()) %>%
  group_by(instrument_xp, expression, color) %>%
  mutate(n = n(), pct = n / sum_inst_expression) %>%
  select(instrument_xp, expression, color, pct) %>%
  distinct()

#data_expression %>% group_by(country, expression) %>% summarise(sum(pct))

ggplot(data_expression_instrument, aes(x = pct, y = color)) + 
  geom_col(aes(fill = color)) +
  #geom_text(aes(label = pct), nudge_x = 1, hjust = 'left') +
  scale_fill_identity() +
  scale_x_continuous(labels = scales::percent) +
  facet_grid(vars(instrument_xp), vars(expression)) + 
  theme_bw()



data_expression_gender <- data_long %>%
  mutate(gender = ifelse(gender == 'nonbinary', 'female', gender)) %>%
  group_by(gender, expression) %>%
  mutate(sum_gender_expression = n()) %>%
  group_by(gender, expression, color) %>%
  mutate(n = n(), pct = n / sum_gender_expression) %>%
  select(gender, expression, color, pct) %>%
  distinct()

#data_expression %>% group_by(country, expression) %>% summarise(sum(pct))

ggplot(data_expression_gender, aes(x = pct, y = color)) + 
  geom_col(aes(fill = color)) +
  #geom_text(aes(label = pct), nudge_x = 1, hjust = 'left') +
  scale_fill_identity() +
  scale_x_continuous(labels = scales::percent) +
  facet_grid(vars(gender), vars(expression)) + 
  theme_bw()


write.csv(data_long, 'data_clean.csv')
