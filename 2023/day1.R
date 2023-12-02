library(stringr)
library(dplyr)

df <- read.table("2023/day1_data.txt")

# part 1
df %>% mutate(
  num1 = str_extract(V1, "\\d"),
  temp = str_extract(V1, "\\d\\D*$"),
  num2 = str_sub(temp, 1, 1),
  s = as.numeric(str_c(num1, num2))
) %>%
  summarize(total = sum(s))

# part2 
m <- str_locate(df$V1, "twone|sevenine|eightwo|eighthree|oneight|threight|fiveight|nineight\\w*$")

#length(m[!is.na(m[, 1]), 1])
# df %>%
#   mutate(
#     temp = str_extract(V1, "twone|sevenine|eightwo|eighthree|oneight|threight|fiveight|nineight\\w*$")
#   ) %>% View()

df <- 
  df %>% mutate(
  start = m[, "start"],
  end = m[, "end"],
  V2 = case_when(
    str_detect(V1, "twone") ~ paste0(str_sub(V1, 1, start+2), str_sub(V1, start+2, nchar(V1))),
    str_detect(V1, "eightwo") ~ paste0(str_sub(V1, 1, start+4), str_sub(V1, start+4, nchar(V1))),
    str_detect(V1, "oneight") ~ paste0(str_sub(V1, 1, start+2), str_sub(V1, start+2, nchar(V1))),
    TRUE ~ V1),
  num = str_extract_all(V2, "\\d|one|two|three|four|five|six|seven|eight|nine")
  )

df %>% 
  mutate(
    test = str_extract_all(V1, "twone|sevenine|eightwo|eighthree")) %>% View()

left <- rep(NA, nrow(df))
right <- left

for (i in 1:nrow(df)){
  vec <- df[i, "num"]
  if (length(vec[[1]]) == 1){
    left[i] <- vec[[1]]
    right[i] <- vec [[1]]}else{
      left[i] <- vec[[1]][1]
      right[i] <- vec[[1]][length(vec[[1]])]
    }
}

df %>%
  mutate(
    left = left,
    right = right,
  ) %>%
  mutate(
    left = case_when(
      left == "one" ~ "1",
      left == "two" ~ "2",
      left == "three" ~ "3",
      left == "four" ~ "4",
      left == "five" ~ "5",
      left == "six" ~ "6",
      left == "seven" ~ "7",
      left == "eight" ~ "8",
      left == "nine" ~ "9",
      TRUE ~ left),
    right = case_when(
      right == "one" ~ "1",
      right == "two" ~ "2",
      right == "three" ~ "3",
      right == "four" ~ "4",
      right == "five" ~ "5",
      right == "six" ~ "6",
      right == "seven" ~ "7",
      right == "eight" ~ "8",
      right == "nine" ~ "9",
      TRUE ~right)
  ) %>%
  mutate(
    s = as.numeric(str_c(left, right))
  ) %>%
  summarize(total = sum(s))

# 54110 is too high
# 54052 is too low
