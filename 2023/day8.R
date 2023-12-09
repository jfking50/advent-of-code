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
nxt <- df[df$c1 == "AAA", move[1]]
keepgoing <- TRUE
while (keepgoing){
  count <- count + 1
  i <- i + 1
  nxt <- df[df$c1 == nxt, move[i]]
  if (nxt == "ZZZ"){keepgoing <- FALSE}
  if (i == 293){i <- 0}
}

# part 2
soln <- rep(NA, 6)
start <- df %>% filter(str_sub(c1, 3, 3) == "A") %>% .$c1
count <- 1
i <- 1
nxt <- df[df$c1 == start[6], move[1]] # do this for each item in start
keepgoing <- TRUE
while (keepgoing){
  count <- count + 1
  i <- i + 1
  nxt <- df[df$c1 == nxt, move[i]]
if (str_sub(nxt, 3, 3) == "Z"){keepgoing <- FALSE}
  if (i == 293){i <- 0}
}
soln[6] <- count

options(scipen = 999)
numbers::mLCM(soln) # use least common multiple function
