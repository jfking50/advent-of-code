library(stringr)
library(dplyr)
library(tidyr)

df <- read.delim2("2023/day3_data.txt", header=FALSE)

df <- df %>% mutate(V1 = str_replace_all(V1, "\\.", "x"))

idx <- rep(NA, 140)
numlen <- idx
nums <- idx

for (i in 1:nrow(df)){
  n <- str_extract_all(df[i,1], "\\d*")
  m <- str_locate_all(df[i,1], "\\d")[[1]][, 1]
  for (j in 1:length(m)){
    if (j == 1){mi = m[1]}else{
      if(m[j] - m[j-1] > 1){
        mi <- c(mi, m[j])
      }
    }
  }
  idx[i] <- list(mi) # indices
  numlen[i] <- list(nchar(n[[1]][which(!is.na(as.numeric(n[[1]])))])) # number length
  nums[i] <- list(as.numeric(n[[1]][which(!is.na(as.numeric(n[[1]])))])) # the numbers
}

df2 <- tibble(
  idx = idx,
  numlen = numlen,
  nums = nums,
  line = 1:nrow(df)
  )
df2 <- df2 %>% unnest_longer(c(idx, numlen, nums)) %>% mutate(keep = FALSE)

# convert data to matrix
mat <- matrix(NA, ncol=nchar(df[1, 1]), nrow = nrow(df))
for (i in 1:nrow(df)){mat[i, ] <- str_split_1(df[i,1], "")}

for (i in 1:nrow(df2)){ # loop through dataframe of numbers and indices
  row <- df2 %>% slice(i) %>% .$line
  lefti <- df2 %>% slice(i) %>% .$idx - 1
  if (lefti > 0){
    righti <- lefti + 1 + df2 %>% slice(i) %>% .$numlen
  }else{
    righti <- lefti + 1 + df2 %>% slice(i) %>% .$numlen
    lefti <- 1
  }
  if (righti == 141){righti <- 140}
  if (row == 1){ # first row
    if (str_detect(paste0(c(mat[row, lefti], mat[row, righti], mat[row+1, lefti:righti]), collapse=""), "\\W")){
      df2[i, "keep"] <- TRUE}
  }
  if (row > 1 & row < 140){ # middle rows
    if (lefti > 0 & righti != 141){
      if (str_detect(paste0(c(mat[row, lefti], mat[row, righti], mat[row+1, lefti:righti], mat[row-1, lefti:righti]), collapse=""), "\\W")){
        df2[i, "keep"] <- TRUE}
      }
  }
  if (row == 140){ # last row
    if (str_detect(paste0(c(mat[row, lefti], mat[row, righti], mat[row-1, lefti:righti]), collapse=""), "\\W")){
      df2[i, "keep"] <- TRUE}
  }
}

df2 %>% filter(keep) %>% summarize(s = sum(nums))
# 533769 too low
# 534744 too low


