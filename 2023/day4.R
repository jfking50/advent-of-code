library(stringr)
library(dplyr)
library(tidyr)
library(purrr)

df <- read.delim2("2023/day4_data.txt", header=FALSE)

df <- df %>% 
  mutate(
    card = row_number(),
    V1 = str_replace(V1, "Card\\s*\\d*:\\s*", ""),
    V1 = str_replace(V1, "\\| ", "")
  ) %>%
  separate(V1, into = paste0("x", 1:35), fill = 'right') %>%
  mutate_if(is.character, as.numeric)

1:205 %>% map(function(x) length(t(unique(df[x, 1:35])))) %>% unlist() != 35 # numbers don't repeat

df %>% 
  mutate(
    matches = 1:205 %>% map(function(x) sum(df[x, 11:35] %in% df[x, 1:10])) %>% unlist(),
    score = case_when(
      matches == 0 ~ 0,
      matches == 1 ~ 1,
      TRUE ~ 2^(matches - 1)
    )
  ) %>% summarize(s = sum(score))

# 23837 too low
# 25000 too high

# part 2

m <- 1:205 %>% map(function(x) sum(df[x, 11:35] %in% df[x, 1:10]))
m2 <- m
i <- 1
app_list <- function(x, m, m2){
  nxt <- m[(x+1):(m[[x]] + x)]
  nxt <- nxt[nxt > 0]
  for (j in 1:length(nxt)){
    m2[[x+j]] <- append(m2[[x+j]], m[[x+j]])
  }
  m2
}

m2 <- app_list(i, m, m2)

for (i in 2:205){
  print(paste(i, length(m2[[i]])))
  for (j in 1:length(m2[[i]])){
    m2 <- app_list(i, m, m2)
  }
}

1:205 %>% map(~length(m2[[.x]])) %>% unlist() %>% sum()
