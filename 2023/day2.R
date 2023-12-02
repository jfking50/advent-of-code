library(stringr)
library(dplyr)
library(tidyr)

df <- read.delim2("2023/day2_data.txt", header=FALSE)

df <- df %>% 
  mutate(
    game = row_number(),
    V1 = str_replace(V1, "Game \\d*:\\s", "")
  )

df2 <- tibble(V2 = str_split(df$V1, "; "),
              game = 1:100)
  
df2 <- df2 %>% unnest_longer(V2) 
df2 <- df2 %>% mutate(show = 1:nrow(df2))
  
df3 <- tibble(V3 = str_split(df2$V2, ", "),
              game = df2$game,
              show = 1:nrow(df2))

df3 <- df3 %>% unnest_longer(V3)
df3 <- df3 %>% separate(V3, c("cubes", "color")) %>% mutate(cubes = as.numeric(cubes))

not_poss <- df3 %>% 
  pivot_wider(names_from = color, values_from = cubes) %>%
  filter(red>12 | green>13 | blue>14) %>%.$game %>% unique()

df %>% filter(!(game %in% not_poss)) %>% summarize(s = sum(game))

# max: 12 red
#      13 green
#      14 blue

# part 2
df3 %>% 
  pivot_wider(names_from = color, values_from = cubes) %>%
  group_by(game) %>%
  summarize(
    maxg = max(green, na.rm = TRUE),
    maxr = max(red, na.rm = TRUE),
    maxb = max(blue, na.rm = TRUE)
  ) %>%
  mutate(
    maxg = replace_na(maxg, 1),
    maxr = replace_na(maxr, 1),
    maxb = replace_na(maxb, 1),
    power = maxg * maxr * maxb
  ) %>% summarize(s = sum(power))

#27299 is too low