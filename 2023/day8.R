library(stringr)
library(dplyr)
library(tidyr)

move <- "LRRLRLRRRLLRLRRRLRLLRLRLRRLRLRRLRRLRLRLLRRRLRRLLRRRLRRLRRRLRRLRLRLLRRLRLRRLLRRRLLLRRRLLLRRLRLRRLRLLRRRLRRLRRRLRRLLRRRLRRRLRRRLRLRRLRLRRRLRRRLRRLRLRRLLRRRLRRLLRRLRRLRLRLRRRLRLLRRRLRRLRRRLLRRLLLLLRRRLRRLLLRRRLRRRLRRLRLLLLLRLRRRLRRRLRLRRLLLLRLRRRLLRRRLRRRLRLRLRRLRRLRRLRLRLLLRLRRLRRLRRRLRRRLLRRRR"

move <- str_split(move, "") %>% unlist() %>% tolower()

df <- read.delim2("2023/day8_data.txt", header=FALSE) %>% 
  mutate(c1 = str_sub(V1, 1, 3),
         l = str_sub(V1, 8, 10),
         r = str_sub(V1, 13, 15))

count <- 1
i <- 1
nxt <- df[1, move[1]]
keepgoing <- TRUE
while (keepgoing){
  count <- count + 1
  i <- i + 1
  nxt <- df[df$c1 == nxt, move[i]]
  if (nxt == "ZZZ"){keepgoing <- FALSE}
  if (i == 293){i <- 0}
}
