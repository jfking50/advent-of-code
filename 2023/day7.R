library(stringr)
library(dplyr)
library(tidyr)

df <- read.delim2("day7_data.txt", header=FALSE)
df <- df %>%
  mutate(
    c1 = str_sub(V1, 1, 1),
    c2 = str_sub(V1, 2, 2),
    c3 = str_sub(V1, 3, 3),
    c4 = str_sub(V1, 4, 4),
    c5 = str_sub(V1, 5, 5),
    value = as.numeric(str_extract(V1,"\\d*$"))
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

df <- df %>%
  mutate(
    uniq = apply(df[, 2:6], 1, function(x) length(unique(x))),
    hand = case_when(
      uniq == 1 ~ 7, # 5 of a kind
      uniq == 2 & ((c1==c2 & c1==c3 & c1==c4) | (c1==c3 & c1==c4 & c1==c5) | (c1==c2 & c1==c4 & c1==c5) | (c1==c2 & c1==c3 & c1==c5) |
                     (c2==c3 & c2==c4 & c2==c5) ) ~6, # 4 of a kind
      uniq == 2 & !((c1==c2 & c1==c3 & c1==c4) | (c1==c3 & c1==c4 & c1==c5) | (c1==c2 & c1==c4 & c1==c5) | (c1==c2 & c1==c3 & c1==c5) |
                      (c2==c3 & c2==c4 & c2==c5) ) ~ 5, # full house
      uniq == 3 & ((c1==c2 & c1==c3) | (c1==c2 & c1==c4) | (c1==c2 & c1==c5) | (c1==c3 & c1==c4 | (c1==c3 & c1==c5) | (c1==c4 & c1==c5 |
                      (c2==c3 & c2==c4) | (c2==c3 & c2==c5) | (c2==c4 & c2==c5) |(c3==c4 & c3==c5)))) ~ 4, # 3 of a kind
      uniq == 4 ~ 2, # pair
      uniq == 5 ~ 1, # high card
      TRUE ~ 3 # two pair
    )
  ) %>%
  group_by(hand, c1, c2, c3, c4, c5) %>%
  arrange(hand, c1, c2, c3, c4, c5) %>%
  ungroup() %>%
  mutate(
    rank = row_number(),
    score = rank * value
  )

df %>% summarize(s = sum(score))

# part 2

df <- read.delim2("day7_data.txt", header=FALSE)

df <- df %>%
  mutate(
    c1 = str_sub(V1, 1, 1),
    c2 = str_sub(V1, 2, 2),
    c3 = str_sub(V1, 3, 3),
    c4 = str_sub(V1, 4, 4),
    c5 = str_sub(V1, 5, 5),
    value = as.numeric(str_extract(V1,"\\d*$"))
  ) %>%
  mutate(
    c1 = case_when(c1 == "T" ~ "10",
                   c1 == "J" ~ "1",
                   c1 == "Q" ~ "12",
                   c1 == "K" ~ "13",
                   c1 == "A" ~ "14",
                   TRUE ~ c1),
    c2 = case_when(c2 == "T" ~ "10",
                   c2 == "J" ~ "1",
                   c2 == "Q" ~ "12",
                   c2 == "K" ~ "13",
                   c2 == "A" ~ "14",
                   TRUE ~ c2),
    c3 = case_when(c3 == "T" ~ "10",
                   c3 == "J" ~ "1",
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
                   c5 == "J" ~ "1",
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

df2 <- df %>%
  mutate(
    uniq = apply(df[, 2:6], 1, function(x) length(unique(x))),
    hand = case_when(
      uniq == 1 ~ 7, # 5 of a kind
      uniq == 2 & ((c1==c2 & c1==c3 & c1==c4) | (c1==c3 & c1==c4 & c1==c5) | (c1==c2 & c1==c4 & c1==c5) | (c1==c2 & c1==c3 & c1==c5) |
                     (c2==c3 & c2==c4 & c2==c5) ) ~6, # 4 of a kind
      uniq == 2 & !((c1==c2 & c1==c3 & c1==c4) | (c1==c3 & c1==c4 & c1==c5) | (c1==c2 & c1==c4 & c1==c5) | (c1==c2 & c1==c3 & c1==c5) |
                      (c2==c3 & c2==c4 & c2==c5) ) ~ 5, # full house
      uniq == 3 & ((c1==c2 & c1==c3) | (c1==c2 & c1==c4) | (c1==c2 & c1==c5) | (c1==c3 & c1==c4 | (c1==c3 & c1==c5) | (c1==c4 & c1==c5 |
                     (c2==c3 & c2==c4) | (c2==c3 & c2==c5) | (c2==c4 & c2==c5) |(c3==c4 & c3==c5)))) ~ 4, # 3 of a kind
      uniq == 4 ~ 2, # pair
      uniq == 5 ~ 1, # high card
      TRUE ~ 3 # two pair
    )
  )

df2 <- df2 %>% mutate(
  j = apply(df[, 2:6], 1, function(x) sum(x==1)),
  new_hand = case_when(
    (c1==1 | c2==1 | c3==1 | c4==1 | c5==1) & hand==6 ~ 7,
    (c1==1 | c2==1 | c3==1 | c4==1 | c5==1) & hand==5 ~ 7,
    (c1==1 | c2==1 | c3==1 | c4==1 | c5==1) & hand==4 & j!=3 ~ 6,
    (c1==1 | c2==1 | c3==1 | c4==1 | c5==1) & hand==3 & j!=2 ~ 5,
    (c1==1 | c2==1 | c3==1 | c4==1 | c5==1) & hand==3 & j==2 ~ 4,
    (c1==1 | c2==1 | c3==1 | c4==1 | c5==1) & hand==2 & j!=2 ~ 3,
    (c1==1 | c2==1 | c3==1 | c4==1 | c5==1) & hand==1 ~ 2,
    TRUE ~ hand
  )) %>%
  group_by(new_hand, c1, c2, c3, c4, c5) %>%
  arrange(new_hand, c1, c2, c3, c4, c5) %>%
  ungroup() %>%
  mutate(
    rank = row_number(),
    score = rank * value
  )

df2 %>% summarize(s = sum(score))

# 248953870 too low
df2 %>% filter((c1==1 | c2==1 | c3==1 | c4==1 | c5==1) & hand==6) # change 4 of a kind to hand 7
df2 %>% filter((c1==1 | c2==1 | c3==1 | c4==1 | c5==1) & hand==5) # change full house these to hand 7
df2 %>% filter((c1==1 | c2==1 | c3==1 | c4==1 | c5==1) & hand==4) # change 3 of kind to hand 6 unless there are 3 jokers, then keep at 4
df2 %>% filter((c1==1 | c2==1 | c3==1 | c4==1 | c5==1) & hand==3) # change 2 pair to hand 5 (full house) unless there are 2 jokers, then change to hand 4
df2 %>% filter((c1==1 | c2==1 | c3==1 | c4==1 | c5==1) & hand==2) # change pair to hand 3 unless 2 jokers, then keep at 2
df2 %>% filter((c1==1 | c2==1 | c3==1 | c4==1 | c5==1) & hand==1) # change high card to hand 2

