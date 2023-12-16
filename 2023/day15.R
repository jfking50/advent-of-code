library(stringr)
library(dplyr)
library(tidyr)
library(purrr)

df <- read.csv("2023/day15_data.txt", header = FALSE)
df <- t(df)

asci <- data.frame(
  char = c(letters, 0, 1:9, "=", "-"),
  asci = c(97:122, 48:57, 61, 45)
)

1:length(df) %>% map(function(x){
  test <- str_split_1(df[x], "")
  ans <- 0
  for (i in test){ans <- ((asci[asci$char == i, "asci"] + ans) * 17) %% 256}
  ans
}) %>% unlist() %>% sum()

# part 2

#df2 <- c("rn=1", "cm-", "qp=3", "cm=2", "qp-", "pc=4", "ot=9", "ab=5", "pc-", "pc=6", "ot=7")

get_hash <- function(lens){
  ans <- 0
  for (i in lens){ans <- ((asci[asci$char == i, "asci"] + ans) * 17) %% 256}
  ans
}

bx <- vector("list", 256)

for (i in 1:4000){
  lens <- str_split_1(df[i], "")
  lab <- lens[1:(which(lens == "=" | lens == "-")-1)]
  foc <- lens[length(lens)]
  idx <- get_hash(lab) + 1
  lab <- str_flatten(lab)

  if (length(bx[[idx]]) > 0){box_labs <- 1:length(bx[[idx]]) %>% map(function(x) bx[[idx]][[x]][1]) %>% unlist()}
  
  if ("=" %in% lens){
    if(!(lab %in% box_labs)){bx[[idx]] <- append(bx[[idx]], list(c(lab, foc)))}else{
    bx[[idx]][[which(box_labs == lab)]] <- c(lab, foc)
    }
    }
  if ("-" %in% lens){
    if(lab %in% box_labs){bx[[idx]][[which(box_labs == lab)]] <- NULL}
  }
}

df3 <- tibble(res = bx, box = 1:256)

df3 %>% 
  unnest_longer(res) %>% 
  unnest_longer(res) %>% 
  filter(res %in% as.character(0:9)) %>% 
  group_by(box) %>% 
  mutate(
    Count = row_number(),
    prod = as.numeric(res) * box * Count) %>%
  ungroup() %>%
  summarise(s = sum(prod))
