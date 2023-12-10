library(stringr)
library(dplyr)
library(tidyr)

df <- read.delim2("2023/day10_data.txt", header=FALSE)

# convert to matrix
mat <- matrix(NA, ncol=nchar(df[1, 1]), nrow = nrow(df))
for (i in 1:nrow(df)){mat[i, ] <- str_split_1(df[i,1], "")}

# find starting point: 21, 104
for (i in 1:nrow(mat)){
  if(sum(mat[i, ] == "S") == 1){
    r <- i 
    c <- which(mat[i, ] == "S")}
}

moves <- data.frame(
  rows = 21,
  cols = 104,
  l = "S",
  from = "start"
)

move_left <- function(df){
  r <- df[nrow(df), "rows"]
  c <- df[nrow(df), "cols"]
  nxt <- data.frame(
    rows = r,
    cols = c-1,
    l = mat[r, c-1],
    from = "right"
  )
  nxt
}

# move down
move_down <- function(df){
  r <- df[nrow(df), "rows"]
  c <- df[nrow(df), "cols"]
  nxt <- data.frame(
    rows = r+1,
    cols = c,
    l = mat[r+1, c],
    from = "up"
  )
  nxt
}

# move up
move_up <- function(df){
  r <- df[nrow(df), "rows"]
  c <- df[nrow(df), "cols"]
  nxt <- data.frame(
    rows = r-1,
    cols = c,
    l = mat[r-1, c],
    from = "down"
  )
  nxt
}

# move right
move_right <- function(df){
  r <- df[nrow(df), "rows"]
  c <- df[nrow(df), "cols"]
  nxt <- data.frame(
    rows = r,
    cols = c+1,
    l = mat[r, c+1],
    from = "left"
  )
  nxt
}

moves <- rbind(moves, move_left(moves))
moves <- rbind(moves, move_down(moves))
moves <- rbind(moves, move_left(moves))

keepgoing <- TRUE
while(keepgoing){
  if (moves[nrow(moves), "l"] == "S") break
  if (moves[nrow(moves), "l"] == "L" & moves[nrow(moves), "from"] == "right"){moves <- rbind(moves, move_up(moves))}
  if (moves[nrow(moves), "l"] == "L" & moves[nrow(moves), "from"] == "up"){moves <- rbind(moves, move_right(moves))}
  if (moves[nrow(moves), "l"] == "|" & moves[nrow(moves), "from"] == "down"){moves <- rbind(moves, move_up(moves))}
  if (moves[nrow(moves), "l"] == "|" & moves[nrow(moves), "from"] == "up"){moves <- rbind(moves, move_down(moves))}
  if (moves[nrow(moves), "l"] == "-" & moves[nrow(moves), "from"] == "right"){moves <- rbind(moves, move_left(moves))}
  if (moves[nrow(moves), "l"] == "-" & moves[nrow(moves), "from"] == "left"){moves <- rbind(moves, move_right(moves))}
  if (moves[nrow(moves), "l"] == "J" & moves[nrow(moves), "from"] == "up"){moves <- rbind(moves, move_left(moves))}
  if (moves[nrow(moves), "l"] == "J" & moves[nrow(moves), "from"] == "left"){moves <- rbind(moves, move_up(moves))}
  if (moves[nrow(moves), "l"] == "7" & moves[nrow(moves), "from"] == "left"){moves <- rbind(moves, move_down(moves))}
  if (moves[nrow(moves), "l"] == "7" & moves[nrow(moves), "from"] == "down"){moves <- rbind(moves, move_left(moves))}
  if (moves[nrow(moves), "l"] == "F" & moves[nrow(moves), "from"] == "right"){moves <- rbind(moves, move_down(moves))}
  if (moves[nrow(moves), "l"] == "F" & moves[nrow(moves), "from"] == "down"){moves <- rbind(moves, move_right(moves))}
}

(nrow(moves) - 1) / 2

# part 2

m2 <- matrix(0, 140, 140)

for (i in 1:nrow(moves)){
  r <- moves[i, "rows"]
  c <- moves[i, "cols"]
  f <- moves[i, "from"]
  if (mat[r, c] == "S"){m2[r+1, c] <- 1}
  if (mat[r, c] == "F" & f == "right"){m2[r+1, c+1] <- 1}
  if (mat[r, c] == "F" & f == "down"){
    m2[r, c-1] <- 1
    m2[r+1, c-1] <- 1
    m2[r+1, c] <- 1
  }
  if (mat[r, c] == "7" & f == "down"){m2[r+1, c-1] <- 1}
  if (mat[r, c] == "7" & f == "left"){
    m2[r-1, c] <- 1
    m2[r-1, c+1] <- 1
    m2[r, c+1] <- 1
  }
  if (mat[r, c] == "L" & f == "top"){m2[r-1, c+1] <- 1}
  if (mat[r, c] == "L" & f == "right"){
    m2[r+1, c] <- 1
    m2[r+1, c-1] <- 1
    m2[r, c-1] <- 1
  }
  if (mat[r, c] == "J" & f == "left"){m2[r-1, c-1] <- 1}
  if (mat[r, c] == "J" & f == "top"){
    m2[r, c+1] <- 1
    m2[r+1, c+1] <- 1
    m2[r, c+1] <- 1
  }
  if (mat[r, c] == "|" & f == "down"){m2[r, c-1] <- 1}
  if (mat[r, c] == "|" & f == "up"){m2[r, c+1] <- 1}
  if (mat[r, c] == "-" & f == "left"){m2[r-1, c] <- 1}
  if (mat[r, c] == "-" & f == "right"){m2[r+1, c] <- 1}
}

for (i in 1:nrow(moves)){
  r <- moves[i, "rows"]
  c <- moves[i, "cols"]
  m2[r, c] <- 0
}

m2[63:71, 59:78] <- 1
m2[72:80, 62:67] <- 1

xy <- expand.grid(x = 1:140, y = 1:140)
z <- as.data.frame.table(m2, responseName = "value") %>% select(value)

m2_long <- data.frame(
  x = xy$y,
  y = xy$x,
  value = z
)

library(ggplot2)

ggplot(m2_long) +
  geom_tile(aes(x=x, y=y, fill = value)) +
  geom_path(data = moves, aes(x=cols, y=rows), linewidth = 3, color="yellow", linejoin = "mitre") +
  geom_point(aes(x=104, y=21), color="red", size=2) +
  geom_point(data = expand.grid(x = 1:140, y = 1:140), aes(x=x, y=y), color="white", size=1) +
  coord_fixed(xlim = c(50, 100), ylim = c(50, 100))

sum(m2 == 1) + 81
