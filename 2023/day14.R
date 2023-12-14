library(stringr)
library(dplyr)
library(tidyr)

df <- read.delim2("2023/test.txt", header=FALSE)

# convert to matrix
m <- matrix(NA, ncol=nchar(df[1, 1]), nrow = nrow(df))
for (i in 1:nrow(df)){m[i, ] <- str_split_1(df[i,1], "")}
m

res <- c()
for (j in 1:ncol(m)){
  hs <- c(0, which(m[, j] == "#"), 11)
  os <- which(m[, j] == "O")
  for (i in 1:(length(hs)-1)){
    if (length(os[os > hs[i] & os < hs[i+1]]) > 0){res <- c(res, (hs[i]+1):(hs[i] + length(os[os > hs[i] & os < hs[i+1]])))}
  }
}
sum(nrow(m) + 1 - res)
