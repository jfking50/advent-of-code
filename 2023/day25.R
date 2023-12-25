library(stringr)
library(dplyr)
library(tidyr)
library(igraph)

df <- read.delim2("2023/day25_data.txt", header=FALSE)

df <- df |> 
  mutate(
    left = str_sub(V1, 1, 3),
    right = str_sub(V1, 6, nchar(V1))
  ) |> 
  select(-V1)

nodes <- c()
for (i in 1:nrow(df)){
  splt <- str_split(df[i, "right"], " ") %>% unlist()
  for (j in 1:length(splt)){
    nodes <- c(nodes, c(df[i, "left"], splt[j]))
  }
}

g <- make_graph(nodes, directed = F)
min_cut(g, source="jts", target=sample(nodes, 1), value.only = FALSE, capacity = rep(1, vcount(g)))
#plot(g, edge.arrow.size = 0.1, edge.curved = 0.5, vertex.size = 3)
# edges to cut: jlt--sjr mzb--fjn mhb--zqg
797*798
