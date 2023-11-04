df <- read.table("~/GitHub/advent-of-code/2022/d02_data.txt", quote="\"", comment.char="")

library(tidyverse)

df %>%
  mutate(
    score1 = case_when(
      V2 == "X" ~ 1,
      V2 == "Y" ~ 2,
      V2 == "Z" ~ 3
    ),
    score2 = case_when(
      V1 == "A" & V2 == "Y" ~ 6,
      V1 == "A" & V2 == "Z" ~ 0,
      V1 == "B" & V2 == "Z" ~ 6,
      V1 == "B" & V2 == "X" ~ 0,
      V1 == "C" & V2 == "X" ~ 6,
      V1 == "C" & V2 == "Y" ~ 0,
      TRUE ~ 3
    ),
    total = score1 + score2
  ) %>%
  summarize(score = sum(total))

df %>%
  mutate(
    score1 = case_when(
      V2 == "X" & V1 == "A" ~ 3,
      V2 == "X" & V1 == "B" ~ 1,
      V2 == "X" & V1 == "C" ~ 2,
      V2 == "Y" & V1 == "A" ~ 1,
      V2 == "Y" & V1 == "B" ~ 2,
      V2 == "Y" & V1 == "C" ~ 3,
      V2 == "Z" & V1 == "A" ~ 2,
      V2 == "Z" & V1 == "B" ~ 3,
      V2 == "Z" & V1 == "C" ~ 1,
      
    ),
    score2 = case_when(
      V2 == "X" ~ 0,
      V2 == "Y" ~ 3,
      V2 == "Z" ~ 6
    ),
    total = score1 + score2
  ) %>%
  summarize(score = sum(total))
