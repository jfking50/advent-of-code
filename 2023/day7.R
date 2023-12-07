library(stringr)
library(dplyr)
library(tidyr)

df <- read.delim2("2023/day7_data.txt", header=FALSE)

df <- df %>%
  mutate(
    c1 = str_sub(V1, 1, 1),
    c2 = str_sub(V1, 2, 2),
    c3 = str_sub(V1, 3, 3),
    c4 = str_sub(V1, 4, 4),
    c5 = str_sub(V1, 5, 5),
    value = as.numeric(str_sub(V1, 7, 9))
    ) %>%
  mutate(
    c1 = case_when(c1 == "T" ~ "10",
                   c1 == "J" ~ "11",
                   c1 == "Q" ~ "12",
                   c1 == "K" ~ "13",
                   c1 == "A" ~ "14",
                   TRUE ~ c1),
    c2 = case_when(c2 == "T" ~ "10",
                   c2 == "J" ~ "11",
                   c2 == "Q" ~ "12",
                   c2 == "K" ~ "13",
                   c2 == "A" ~ "14",
                   TRUE ~ c2),
    c3 = case_when(c3 == "T" ~ "10",
                   c3 == "J" ~ "11",
                   c3 == "Q" ~ "12",
                   c3 == "K" ~ "13",
                   c3 == "A" ~ "14",
                   TRUE ~ c3),
    c4 = case_when(c4 == "T" ~ "10",
                   c4 == "J" ~ "11",
                   c4 == "Q" ~ "12",
                   c4 == "K" ~ "13",
                   c4 == "A" ~ "14",
                   TRUE ~ c4),
    c5 = case_when(c5 == "T" ~ "10",
                   c5 == "J" ~ "11",
                   c5 == "Q" ~ "12",
                   c5 == "K" ~ "13",
                   c5 == "A" ~ "14",
                   TRUE ~ c5)
  ) %>% 
  mutate(
    c1 = as.numeric(c1),
    c2 = as.numeric(c2),
    c3 = as.numeric(c3),
    c4 = as.numeric(c4),
    c5 = as.numeric(c5)
  )

df %>% mutate(
  hand = case_when(
    c1 == c2 & c1 == c3 & c1 == c4 & c1 == c5 ~ "five"
  )
)

df %>%
  mutate(
    uniq = apply(df[, 2:6],1,function(x) length(unique(x))),
    hand = case_when(
      uniq == 1 ~ "five",
      uniq == 2 ~ "four"
    )
  )
