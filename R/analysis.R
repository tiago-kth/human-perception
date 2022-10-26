library(tidyverse)
library(forcats)
library(showtext)
font_add_google('Arvo')

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
              "brasil", "", "PERU", "Bradileira", "Mexican ", "mexican", "Brazilian ", 
              "Costarican ", "BRASILEIRO "
  ),
  
  country_clean = c("Brazil", "Netherlands", "Sweden", "Germany", "China", "Spain", 
                    "Croatia", "Ireland", "Taiwan", "Brazil", "India", "TESTE", 
                    "Sweden", "Singapore", "Italia", "Italia", "India", "India", 
                    "India", "Brazil", "Brazil", "Brazil", "Brazil", 
                    "Brazil", "Brazil", "Brazil", "Brazil", "France", "Brazil", 
                    "Honduras", "Chile", "Mexico", "Mexico", "Uruguay", "Uruguay", 
                    "Brazil", "", "Peru", "Brazil", "Mexico", "Mexico", "Brazil",
                    "Costa Rica", "Brazil"
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
  filter(row_number() <= 165)

data_pre <- data_clean %>%
  left_join(countries) %>%
  mutate(
    country_d = ifelse(country_clean == 'Brazil', 'Brazil', 'Other'),
    age_d = ifelse(age < median(data_clean$age), 'Under 42', 'Over 42'),
    gender_d = ifelse(gender == 'nonbinary', 'female', gender)
  ) %>%
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
      age_d,
      -country, 
      country = country_clean,
      country_d,
      gender,
      gender_d,
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


# Random plotting / exploring ---------------------------------------------

summary(data_clean$age)
ggplot(data_clean) + geom_boxplot(aes(y = age))

ggplot(data_clean, aes(y = fct_rev(fct_infreq(country_clean)))) + 
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

ggplot(data_long, aes(y = fct_rev(fct_infreq(excerpt)))) + 
  geom_bar() +
  geom_text(aes(label = ..count.., x = ..count..), stat = 'count', nudge_x = 1, hjust = 'left')

ggplot(data_long, aes(y = fct_rev(fct_infreq(expression)))) + 
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

# de todas as vezes que <Cor> foi escolhida, <tantos>% foram em trechos Tender, tantos com Sorrow, tantos com Joy.
ggplot(data_long, aes(y = expression, group = color)) +
  geom_bar(aes(fill = color, x = ..prop..), stat = 'count') +
  scale_fill_identity() +
  facet_wrap(~color)

# Counting ----------------------------------------------------------------

## General

n_participants <- nrow(data_pre)

color_presence <- data_long %>%
  select(id, expression, color) %>%
  distinct() %>%
  count(expression, color) %>%
  mutate(pct = n / n_participants)

ggplot(color_presence, aes(y = color, x = pct, fill = color, alpha = ifelse(pct >= .45, 'strong', 'weak'))) +
  geom_col() +
  scale_fill_identity() +
  scale_x_continuous(labels = scales::percent) +
  scale_alpha_manual(values = c('strong' = 1, 'weak' = .1)) +
  facet_wrap(~expression) +
  theme_minimal() +
  labs(x = NULL, y = NULL, 
       subtitle = '(colors picked by less then 45% of the users are transparent to highlight the main picked colors)',
       title = 'Percentage of users that chose a given color to an excerpt of a given expression') +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = 'none',
        text = element_text(family = 'Arvo'))

ggsave('./plots/pct_colors_all.png', plot = last_plot(), width = 9, height = 4)
  
ggplot(color_presence, aes(x = color, y = pct, fill = color, group = expression)) +
  geom_col() +
  scale_fill_identity()

## For different criteria

get_color_count <- function(data, crit){
  
  count_crit <- data_pre %>%
    count(!!sym(crit)) %>%
    rename(n_crit = n)
  
  df <- data %>%
    #filter(excerpt != '06') %>%
    select(id, expression, color, !!sym(crit)) %>%
    distinct() %>% #with that, we're focusing on different colors chosen by the user for a given expression
    count(expression, color, !!sym(crit)) %>%
    left_join(count_crit) %>%
    mutate(pct = n / n_crit)
  
  # p <- ggplot(df, aes(y = color, x = pct, fill = color)) +
  #   geom_col() +
  #   scale_fill_identity() +
  #   scale_x_continuous(labels = scales::percent) +
  #   facet_grid(vars(expression), vars(!!sym(crit)))
  
  return(df)
  
}

plot_color_count <- function(data, crit) {
  
  df <- get_color_count(data, crit)
  
  ggplot(df, aes(y = color, x = pct, fill = color, alpha = ifelse(pct >= .4, 'strong', 'weak'))) +
    geom_col() +
    scale_fill_identity() +
    scale_x_continuous(labels = scales::percent) +
    scale_alpha_manual(values = c('strong' = 1, 'weak' = .1)) +
    facet_wrap(~expression) +
    theme_minimal() +
    labs(x = NULL, y = NULL)+#, 
         #subtitle = '(colors picked by less then 40% of the users are transparent to highlight the main picked colors)',
         #title = 'Percentage of users that chose a given color to an excerpt of a given expression') +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.line.y = element_blank(),
          axis.line.x = element_blank(),
          legend.position = 'none',
          text = element_text(family = 'Arvo'),
          panel.spacing = unit(2, "lines")
          #,strip.text.y = element_blank()
          ) +
    facet_grid(vars(!!sym(crit)), vars(expression), switch = "y")
    
}

# gender

plot_color_count(data_long, 'gender_d')

gender_difs <- get_color_count(data_long, 'gender_d') %>%
  select(-n, -n_crit) %>%
  spread(gender_d, pct) %>%
  mutate(dif = male - female)

ggplot(gender_difs, aes(y = color, x = dif * 100, fill = abs(dif * 100) > 10)) +
  geom_col() +
  #scale_x_continuous(labels = scales::percent) +
  facet_wrap(~expression)

# country

country_count <- get_color_count(data_long, 'country_d')
plot_color_count(data_long, 'country_d')
ggsave('./plots/pct_colors_brazil_other.png', plot = last_plot(), width = 9, height = 6)

countries <- get_color_count(data_long, 'country_d')

# ggplot(countries %>% filter(country_d == 'Brazil'), aes(y = color, x = pct, fill = color, alpha = ifelse(pct >= .45, 'strong', 'weak'))) +
#   geom_col() +
#   scale_fill_identity() +
#   scale_x_continuous(labels = scales::percent) +
#   scale_alpha_manual(values = c('strong' = 1, 'weak' = .1)) +
#   facet_wrap(~expression) +
#   theme_minimal() +
#   labs(x = NULL, y = NULL, 
#        subtitle = '(colors picked by less then 45% of the users are transparent to highlight the main picked colors)',
#        title = 'BRAZIL - Percentage of users that chose a given color to an excerpt of a given expression') +
#   theme(panel.grid.major.y = element_blank(),
#         panel.grid.minor.x = element_blank(),
#         panel.grid.minor.y = element_blank(),
#         axis.line.y = element_blank(),
#         legend.position = 'none',
#         text = element_text(family = 'Arvo'))
# 
# ggsave('./plots/pct_colors_brazil.png', plot = last_plot(), width = 9, height = 4)
# ggplot(countries %>% filter(country_d == 'Other'), aes(y = color, x = pct, fill = color, alpha = ifelse(pct >= .45, 'strong', 'weak'))) +
#   geom_col() +
#   scale_fill_identity() +
#   scale_x_continuous(labels = scales::percent) +
#   scale_alpha_manual(values = c('strong' = 1, 'weak' = .1)) +
#   facet_wrap(~expression) +
#   theme_minimal() +
#   labs(x = NULL, y = NULL, 
#        subtitle = '(colors picked by less then 45% of the users are transparent to highlight the main picked colors)',
#        title = 'OTHER - Percentage of users that chose a given color to an excerpt of a given expression') +
#   theme(panel.grid.major.y = element_blank(),
#         panel.grid.minor.x = element_blank(),
#         panel.grid.minor.y = element_blank(),
#         legend.position = 'none',
#         text = element_text(family = 'Arvo'))
# 
# ggsave('./plots/pct_colors_other.png', plot = last_plot(), width = 9, height = 4)

# parecida com a do exemplo

ggplot(data_long %>% mutate(color = factor(color, levels = colors)), aes(fill = color, y = country_d)) +
  geom_bar(position = position_fill()) +
  scale_fill_identity() + 
  facet_grid(vars(expression)) +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.line.y = element_blank(),
        legend.position = 'none',
        text = element_text(family = 'Arvo'))
  

# highlighting the diferences

country_difs <- get_color_count(data_long, 'country_d') %>%
  select(-n, -n_crit) %>%
  spread(country_d, pct) %>%
  mutate(dif = Brazil - Other)

ggplot(country_difs, aes(x = color, y = dif * 100, fill = abs(dif * 100) > 20)) +
  geom_col() +
  #scale_x_continuous(labels = scales::percent) +
  facet_grid(vars(expression))

ggplot(country_difs, aes(y = color, yend= color, x = 0, xend=dif * 100, color = color,
                         alpha = abs(dif * 100) > 15)) +#color = abs(dif * 100) > 20)) +
  geom_segment(arrow = arrow(length = unit(0.05, "npc")), size = 1.5) +
  scale_color_identity() +
  #scale_x_continuous(labels = scales::percent) +
  #facet_grid(vars(expression)) +
  scale_alpha_manual(values = c('TRUE' = 1, 'FALSE' = .25)) +
  labs(x = NULL, y = NULL) +
  facet_wrap(~expression) +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.line.y = element_blank(),
        legend.position = 'none',
        panel.spacing = unit(2, "lines"),
        text = element_text(family = 'Arvo'))

ggsave('./plots/pct_colors_difs.png', plot = last_plot(), width = 9, height = 2)



# Analysis of Brasileirinho -----------------------------------------------

joy_01 <- get_color_count(data_long %>% filter(excerpt == '05'), 'country_d')
plot_color_count(data_long %>% filter(excerpt == '05'), 'country_d')

country_difs_joy_01 <- get_color_count(data_long %>% filter(excerpt == '05'), 'country_d') %>%
  select(-n, -n_crit) %>%
  spread(country_d, pct) %>%
  mutate(dif = Brazil - Other)

ggplot(country_difs_joy_01, aes(y = color, yend= color, x = 0, xend=dif * 100, color = color,
                         alpha = abs(dif * 100) > 15)) +#color = abs(dif * 100) > 20)) +
  geom_segment(arrow = arrow(length = unit(0.05, "npc")), size = 1.5) +
  scale_color_identity() +
  #scale_x_continuous(labels = scales::percent) +
  #facet_grid(vars(expression)) +
  scale_alpha_manual(values = c('TRUE' = 1, 'FALSE' = .25)) +
  labs(x = NULL, y = NULL) +
  facet_wrap(~expression) +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.line.y = element_blank(),
        legend.position = 'none',
        panel.spacing = unit(2, "lines"),
        text = element_text(family = 'Arvo'))



## brasileirinho

joy_02 <- get_color_count(data_long %>% filter(excerpt == '06'), 'country_d')
plot_color_count(data_long %>% filter(excerpt == '06'), 'country_d')

country_difs_joy_02 <- get_color_count(data_long %>% filter(excerpt == '06'), 'country_d') %>%
  select(-n, -n_crit) %>%
  spread(country_d, pct) %>%
  mutate(dif = Brazil - Other)

ggplot(country_difs_joy_02, aes(y = color, yend= color, x = 0, xend=dif * 100, color = color,
                                alpha = abs(dif * 100) > 15)) +#color = abs(dif * 100) > 20)) +
  geom_segment(arrow = arrow(length = unit(0.05, "npc")), size = 1.5) +
  scale_color_identity() +
  #scale_x_continuous(labels = scales::percent) +
  #facet_grid(vars(expression)) +
  scale_alpha_manual(values = c('TRUE' = 1, 'FALSE' = .25)) +
  labs(x = NULL, y = NULL) +
  facet_wrap(~expression) +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.line.y = element_blank(),
        legend.position = 'none',
        panel.spacing = unit(2, "lines"),
        text = element_text(family = 'Arvo'))

# só joy06 e verde e amarelo

count_country <- data_pre %>%
  count(country_d) %>%
  rename(n_crit = n)

excertps_joy_names <- data.frame(
  excerpt = c('05', '06'),
  name = c('Apanhei-te Cavaquinho', 'Brasileirinho')
)

joy_01_green_yellow <- data_long %>% filter(expression == 'Joy', color %in% c('green', 'yellow')) %>%
  group_by(country_d, id, excerpt) %>%
  mutate(duas_juntas = n() == 2) %>%
  summarise(duas_juntas = first(duas_juntas)) %>%
  filter(duas_juntas) %>%
  ungroup() %>%
  count(country_d, excerpt) %>%
  left_join(count_country) %>%
  mutate(pct = n / n_crit) %>%
  left_join(excertps_joy_names)


ggplot(joy_01_green_yellow, aes(y = country_d, x = pct, color = country_d)) +
  geom_col(aes(fill = country_d), width = .7) +
  geom_text(aes(label = scales::percent(pct)), nudge_x = .015, family = 'Arvo') +
  scale_x_continuous(labels = scales::percent, expand = c(0,0), limits = c(0, .43)) +
  #facet_grid(vars(name), switch = 'y') +
  facet_wrap(~name, nrow = 2) +
  labs(x = NULL, y = NULL, title = '% of participants that chose a Green-Yellow combination for the "Joy" excerpts') +
  theme_minimal() +
  scale_fill_manual(values = c('Brazil' = 'Forest Green', 'Other' = 'Gray')) +
  scale_color_manual(values = c('Brazil' = 'Forest Green', 'Other' = 'Gray')) +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.line.y = element_blank(),
        legend.position = 'none',
        panel.spacing = unit(2, "lines"),
        text = element_text(family = 'Arvo'),
        strip.text.x = element_text(hjust = 0, margin = margin(3,0,3,0)))

ggsave('./plots/green-yellow-joy.png', plot = last_plot(), width = 8, height = 3, bg = 'white')


# Plot: average number of colors ------------------------------------------

count_colors_excerpts <- data_long %>%
  count(excerpt, expression) %>%
  group_by(expression) %>%
  mutate(excerpt_exp = paste(expression, row_number(), sep = '-')) %>%
  ungroup() %>%
  mutate(qty = n / n_participants)

ggplot(count_colors_excerpts, aes(y = reorder(excerpt_exp, qty), x = qty)) +
  geom_col(fill = "#EA5F94", width = .5) + 
  geom_text(aes(label = scales::number(qty, accuracy = .01)), nudge_x = .1, family = 'Arvo') +
  labs(x = NULL, y = NULL)+ #, 
       #title = 'Average number of different colors chosen by participants for each excerpt') +
  scale_x_continuous(expand = c(0,0), limits = c(0,2.5)) +
  theme_minimal() + 
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_blank(),
        axis.line = element_blank(),
        legend.position = 'none',
        text = element_text(family = 'Arvo'))

ggsave('./plots/avg_different_colors.png', plot = last_plot(), width = 8, height = 2)






ggplot(ct_colors, aes(x = excerpt)) + geom_boxplot() 

ct_colors %>% filter(excerpt == '06') %>% .$n %>% summary

ggplot(data_long, aes(y = fct_rev(fct_infreq(excerpt)))) + 
  geom_bar() +
  geom_text(aes(label = ..count.., x = (..count.. / n_participants)), family = 'Arvo', stat = 'count', nudge_x = 1, hjust = 'left') +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = 'none',
        text = element_text(family = 'Arvo'))


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


write.csv2(data_long, 'data_clean2.csv', fileEncoding = 'UTF-8')

teste <- data_long %>% filter(expression =='Sorrow', color == 'blue') %>%
  select(id) %>%
  distinct() %>%
  unlist()


# o fator brasileirinho ---------------------------------------------------
data_long %>% filter(excerpt == '06', country_d == 'Brazil') %>% count(color)
60/120
data_long %>% filter(excerpt == '06', country_d == 'Other') %>% count(color)
7/45

data_long %>% filter(excerpt == '05', country_d == 'Brazil') %>% count(color)
35/120
data_long %>% filter(excerpt == '05', country_d == 'Other') %>% count(color)
8/45

