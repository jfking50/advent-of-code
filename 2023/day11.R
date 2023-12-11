library(stringr)
library(dplyr)
library(tidyr)

df <- read.delim2("2023/day11_data.txt", header=FALSE)

# convert to matrix
mat <- matrix(NA, ncol=nchar(df[1, 1]), nrow = nrow(df))
empty_r <- c() # empty rows
for (i in 1:nrow(df)){
  mat[i, ] <- str_split_1(df[i,1], "")
  if (sum(mat[i, ] == ".") == 140){empty_r <- c(empty_r, i)}}

# empty columns
empty_c <- c()
for (i in 1:140){if (sum(mat[, i] == ".") == 140){empty_c <- c(empty_c, i)}}

# bigger matrix
m2 <- matrix(NA, ncol = 140, nrow = 148)
empty_r2 <- empty_r + 0:7
empty_c2 <- empty_c + 0:4

j <- 1
for (i in 1:nrow(m2)){
  if (i == empty_r[j]){
    if (j == 1) {m2[1:empty_r2[j], ] <- mat[1:empty_r[j], ]
    m2[empty_r2[j]+1, ] <- "."
    m2[(empty_r2[j]+2):(140+j), ] <- mat[(empty_r2[j]+1):140, ]
    j <- j + 1}
  }
  if (j > 1){
    
  }
}
