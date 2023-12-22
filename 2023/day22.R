library(stringr)
library(dplyr)
library(tidyr)

df <- read.delim2("2023/day22_data.txt", header=FALSE)

df <- df |> 
  separate(V1, into = c("x1", "y1", "z1", "x2", "y2", "z2")) |>
  mutate_if(is.character, as.numeric) |>
  mutate(x1=x1+1, y1=y1+1, x2=x2+1, y2=y2+1)

# fill array with blocks
a <- array(0, c(10, 10, max(max(df[, "z1"]), max(df[, "z2"]))))
for (i in 1:nrow(df)){
  if (sum(df[i, 1:3] == df[i, 4:6]) == 3){
    a[df[i, "x1"], df[i, "y1"], df[i, "z1"]] <- i
  }else{
    dif <- df[i, 4:6] - df[i, 1:3]
    if (which(dif > 0) == 1){
      xmin <- min(df[i, "x1"], df[i, "x2"])
      xmax <- max(df[i, "x1"], df[i, "x2"])
      a[xmin:xmax, df[i, "y1"], df[i, "z1"]] <- i}
    if (which(dif > 0) == 2){
      ymin <- min(df[i, "y1"], df[i, "y2"])
      ymax <- max(df[i, "y1"], df[i, "y2"])
      a[df[i, "x1"], ymin:ymax, df[i, "z1"]] <- i}
    if (which(dif > 0) == 3){
      zmin <- min(df[i, "z1"], df[i, "z2"])
      zmax <- max(df[i, "z1"], df[i, "z2"])
      a[df[i, "x1"], df[i, "y1"], zmin:zmax] <- i} 
    }
}
rm(dif, xmax, xmin, ymax, ymin, zmax, zmin)
b <- a

# move blocks down
count <- 1
while (count > 0){
  count <- 0
  for (i in 2:dim(b)[3]){
    lev <- as.vector(b[, , i])
    levbel <- as.vector(b[, , i-1])
    bs <- unique(lev)
    bs <- bs[bs != 0]
    if (length(bs > 0)){
      for (j in 1:length(bs)){
        if (sum(levbel[which(lev == bs[j])]) == 0){
          levbel[which(lev == bs[j])] <- bs[j]
          lev[which(lev == bs[j])] <- 0
          b[, , i-1] <- matrix(levbel, 10, 10)
          b[, , i] <- matrix(lev, 10, 10)
          count <- count + 1
        }
      }
    }
  }
  print(count)
}

move_down <- function(bnew){
  count <- 0
  for (i in 2:dim(bnew)[3]){
    lev <- as.vector(bnew[, , i])
    levbel <- as.vector(bnew[, , i-1])
    bs <- unique(lev)
    bs <- bs[bs != 0]
    if (length(bs > 0)){
      for (j in 1:length(bs)){
        if (sum(levbel[which(lev == bs[j])]) == 0){
          count <- count + 1
        }
      }
    }
    if (count > 0){break}
  }
  count
}

res <- rep(NA, 1210)
for (k in 1:1210){
  b1 <- b[, , 1:176]
  flat <- as.vector(b1)
  flat[flat == k] <- 0
  bnew <- array(flat, dim = c(10, 10, 176))
  res[k] <- move_down(bnew)
}
sum(res == 0)

# part 2
move_down2 <- function(b){
  count <- 1
  moved <- c()
  while (count > 0){
    count <- 0
    for (i in 2:dim(b)[3]){
      lev <- as.vector(b[, , i])
      levbel <- as.vector(b[, , i-1])
      bs <- unique(lev)
      bs <- bs[bs != 0]
      if (length(bs > 0)){
        for (j in 1:length(bs)){
          if (sum(levbel[which(lev == bs[j])]) == 0){
            moved <- c(moved, bs[j])
            levbel[which(lev == bs[j])] <- bs[j]
            lev[which(lev == bs[j])] <- 0
            b[, , i-1] <- matrix(levbel, 10, 10)
            b[, , i] <- matrix(lev, 10, 10)
            count <- count + 1
          }
        }
      }
    }
  }
  length(unique(moved))
}

res <- rep(NA, 1210)
for (k in 1:1210){
  print(k)
  b1 <- b[, , 1:176]
  flat <- as.vector(b1)
  flat[flat == k] <- 0
  bnew <- array(flat, dim = c(10, 10, 176))
  res[k] <- move_down2(bnew)
}
sum(res)
