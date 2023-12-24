library(stringr)
library(dplyr)
library(tidyr)

df <- read.delim2("2023/day23_data.txt", header=FALSE)

m <- matrix(NA, ncol=nchar(df[1, 1]), nrow = nrow(df))
for (i in 1:nrow(df)){m[i, ] <- str_split_1(df[i,1], "")}
m[1, 2] <- "S"

dfl <- expand.grid(ro=1:nrow(df), co=1:nrow(df)) %>% mutate(smbl = as.vector(m), node = row_number())

steps <- 0
nxt <- 0
from <- 142
w <- ncol(m)
g <- data.frame()
todo <- data.frame()
visited <- c()

# get to the first new node
while(length(visited) == 0){
  if (from == 142 & steps == 0){
    steps <- steps + 1
    nxt <- from + 1
  }else{
    ro <- dfl[nxt, "ro"]
    co <- dfl[nxt, "co"]
    if (m[ro, co] == "."){m[ro, co] <- "O"}
    if (m[ro+1, co] == "v" & m[ro, co+1] == ">"){ # at a new node
      g <- rbind(g, data.frame(from=from, to=nxt, d=steps+1))
      from <- nxt
      if (!(nxt %in% visited)){
        visited <- c(visited, nxt)
        todo <- rbind(todo, data.frame(from=c(from, from), dir=c("r", "d")))}
      steps <- 0
    }else{
      if(m[ro+1, co] == "." | m[ro+1, co] == "v")
      {nxt <- nxt + 1
      steps <- steps + 1} # down
      if(m[ro-1, co] == "."){
        nxt <- nxt - 1
        steps <- steps + 1} # up
      if(m[ro, co-1] == "."){
        nxt <- nxt - w
        steps <- steps + 1} # left
      if(m[ro, co+1] == "." | m[ro, co+1] == ">"){
        nxt <- nxt + w
        steps <- steps + 1} # right
    }
  }
}

# while todo has at least 1 row
while(nrow(todo) > 0){
  if (steps == 0){
    #print(todo)
    from <- todo[1, "from"]
    if(todo[1, "dir"] == "r"){nxt <- from + w}else{nxt <- from + 1}
    if(nrow(todo) == 1){todo <- data.frame()}else{todo <- todo[2:nrow(todo), ]}}
  
  ro <- dfl[nxt, "ro"]
  co <- dfl[nxt, "co"]
  if(m[ro, co] == "."){m[ro, co] <- "O"}
  
  if(m[ro-1, co] == "v" & m[ro, co-1] == ">" & m[ro, co+1] == ">" & m[ro+1, co] == "#"){ # at a node with 1 in & 1 out
    g <- rbind(g, data.frame(from=from, to=nxt, d=steps+1))
    from <- nxt
    if (!(nxt %in% visited)){
      visited <- c(visited, nxt)
      todo <- rbind(todo, data.frame(from=from, dir="r"))}
    steps <- 0
  }else{  
    if(m[ro+1, co] == "v" & m[ro, co-1] == ">" & m[ro, co+1] == "#"){ # at a node with 1 in & 1 out
      g <- rbind(g, data.frame(from=from, to=nxt, d=steps+1))
      from <- nxt
      if (!(nxt %in% visited)){
        visited <- c(visited, nxt)
        todo <- rbind(todo, data.frame(from=from, dir="d"))}
      steps <- 0
    }else{
      if(m[ro+1, co] == "v" & m[ro, co+1] == ">"){ # at a node
        g <- rbind(g, data.frame(from=from, to=nxt, d=steps+1))
        from <- nxt
        if (!(nxt %in% visited)){
          visited <- c(visited, nxt)
          todo <- rbind(todo, data.frame(from=c(from, from), dir=c("r", "d")))}
        steps <- 0
      }else{
        if (steps > 0){
          if(m[ro+1, co] %in% c(".", "v") | dfl[dfl$ro == ro+1 & dfl$co == co, "node"] %in% visited){
            nxt <- nxt + 1
            steps <- steps + 1} # down
          if(m[ro-1, co] == "." | dfl[dfl$ro == ro-1 & dfl$co == co, "node"] %in% visited){
            nxt <- nxt - 1
            steps <- steps + 1} # up
          if(m[ro, co-1] == "." | dfl[dfl$ro == ro & dfl$co == co-1, "node"] %in% visited){
            nxt <- nxt - w
            steps <- steps + 1} # left
          if(m[ro, co+1] %in% c(".", ">") | dfl[dfl$ro == ro & dfl$co == co+1, "node"] %in% visited){
            nxt <- nxt + w
            steps <- steps + 1} # right
        }else{
          if(m[ro+1, co] %in% c(".", "v")){
            nxt <- nxt + 1
            steps <- steps + 1} # down
          if(m[ro-1, co] == "."){
            nxt <- nxt - 1
            steps <- steps + 1} # up
          if(m[ro, co-1] == "."){
            nxt <- nxt - w
            steps <- steps + 1} # left
          if(m[ro, co+1] %in% c(".", ">")){
            nxt <- nxt + w
            steps <- steps + 1} # right
        }
        
      }
    }
  }
  
  if (nxt == w^2 - w){ # at the end node
    g <- rbind(g, data.frame(from=from, to=nxt, d=steps+1))
    steps <- 0}
}

library(igraph)

g2 <- 
  graph_from_data_frame(
    g %>% 
      mutate(d=-d, from=as.character(from), to=as.character(to)) %>% 
      rename("weight" = "d"),
    directed = TRUE)
visited
spath <- shortest_paths(g2, from="142", to="19740")
pth <- as.numeric(names(spath$vpath[[1]]))

res <- data.frame(
  f = pth[1:(length(pth)-1)],
  t = pth[2:length(pth)]
)

res %>% 
  left_join(g, by = c("f"="from", "t"="to")) %>%
  summarize(s=sum(d))
