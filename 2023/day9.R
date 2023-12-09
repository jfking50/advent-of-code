library(dplyr)

df <- read.delim2("2023/day9_data.txt", header = FALSE, sep = " ")

get_vec <- function(vec){
  v2 <- lead(vec) - vec
  v2[!is.na(v2)]
}

soln <- rep(NA, nrow(df))

for (i in 1:200){
  v <- as.vector(df[i, ]) %>% unlist()
  v2 <- get_vec(v)
  if (sum(v2 == 0) != length(v2)){v3 <- get_vec(v2)}else{
    soln[i] <- v[length(v)]
    next
  }
  if (sum(v3 == 0) != length(v3)){v4 <- get_vec(v3)}else{
    v2n <- v2[length(v2)]
    soln[i] <- v[length(v)] + v2n
    next
  }
  if (sum(v4 == 0) != length(v4)){v5 <- get_vec(v4)}else{
    v3n <- v3[length(v3)]
    v2n <- v2[length(v2)] + v3n
    soln[i] <- v[length(v)] + v2n
    next
  }
  if (sum(v5 == 0) != length(v5)){v6 <- get_vec(v5)}else{
    v4n <- v4[length(v4)]
    v3n <- v3[length(v3)] + v4n
    v2n <- v2[length(v2)] + v3n
    soln[i] <- v[length(v)] + v2n
    next
  }
  if (sum(v6 == 0) != length(v6)){v7 <- get_vec(v6)}else{
    v5n <- v5[length(v5)]
    v4n <- v4[length(v4)] + v5n
    v3n <- v3[length(v3)] + v4n
    v2n <- v2[length(v2)] + v3n
    soln[i] <- v[length(v)] + v2n
    next
  }
  if (sum(v7 == 0) != length(v7)){v8 <- get_vec(v7)}else{
    v6n <- v6[length(v6)]
    v5n <- v5[length(v5)] + v6n
    v4n <- v4[length(v4)] + v5n
    v3n <- v3[length(v3)] + v4n
    v2n <- v2[length(v2)] + v3n
    soln[i] <- v[length(v)] + v2n
    next
  }
  if (sum(v8 == 0) != length(v8)){v9 <- get_vec(v8)}else{
    v7n <- v7[length(v7)] 
    v6n <- v6[length(v6)] + v7n
    v5n <- v5[length(v5)] + v6n
    v4n <- v4[length(v4)] + v5n
    v3n <- v3[length(v3)] + v4n
    v2n <- v2[length(v2)] + v3n
    soln[i] <- v[length(v)] + v2n
    next
  }
  if (sum(v9 == 0) != length(v9)){v10 <- get_vec(v9)}else{
    v8n <- v8[length(v8)] 
    v7n <- v7[length(v7)] + v8n
    v6n <- v6[length(v6)] + v7n
    v5n <- v5[length(v5)] + v6n
    v4n <- v4[length(v4)] + v5n
    v3n <- v3[length(v3)] + v4n
    v2n <- v2[length(v2)] + v3n
    soln[i] <- v[length(v)] + v2n
    next
  }
  if (sum(v10 == 0) != length(v10)){v11 <- get_vec(v10)}else{
    v9n <- v9[length(v9)]
    v8n <- v8[length(v8)] + v9n
    v7n <- v7[length(v7)] + v8n
    v6n <- v6[length(v6)] + v7n
    v5n <- v5[length(v5)] + v6n
    v4n <- v4[length(v4)] + v5n
    v3n <- v3[length(v3)] + v4n
    v2n <- v2[length(v2)] + v3n
    soln[i] <- v[length(v)] + v2n
    next
  }
  if (sum(v11 == 0) != length(v11)){v12 <- get_vec(v11)}else{
    v10n <- v10[length(v10)]
    v9n <- v9[length(v9)] + v10n
    v8n <- v8[length(v8)] + v9n
    v7n <- v7[length(v7)] + v8n
    v6n <- v6[length(v6)] + v7n
    v5n <- v5[length(v5)] + v6n
    v4n <- v4[length(v4)] + v5n
    v3n <- v3[length(v3)] + v4n
    v2n <- v2[length(v2)] + v3n
    soln[i] <- v[length(v)] + v2n
    next
  }
  if (sum(v12 == 0) != length(v12)){v13 <- get_vec(v12)}else{
    v11n <- v11[length(v11)] 
    v10n <- v10[length(v10)] + v11n
    v9n <- v9[length(v9)] + v10n
    v8n <- v8[length(v8)] + v9n
    v7n <- v7[length(v7)] + v8n
    v6n <- v6[length(v6)] + v7n
    v5n <- v5[length(v5)] + v6n
    v4n <- v4[length(v4)] + v5n
    v3n <- v3[length(v3)] + v4n
    v2n <- v2[length(v2)] + v3n
    soln[i] <- v[length(v)] + v2n
    next
  }
  if (sum(v13 == 0) != length(v13)){v14 <- get_vec(v13)}else{
    v12n <- v12[length(v12)]
    v11n <- v11[length(v11)] + v12n
    v10n <- v10[length(v10)] + v11n
    v9n <- v9[length(v9)] + v10n
    v8n <- v8[length(v8)] + v9n
    v7n <- v7[length(v7)] + v8n
    v6n <- v6[length(v6)] + v7n
    v5n <- v5[length(v5)] + v6n
    v4n <- v4[length(v4)] + v5n
    v3n <- v3[length(v3)] + v4n
    v2n <- v2[length(v2)] + v3n
    soln[i] <- v[length(v)] + v2n
    next
  }
  if (sum(v14 == 0) != length(v14)){v15 <- get_vec(v14)}else{
    v13n <- v13[length(v13)]
    v12n <- v12[length(v12)] + v13n
    v11n <- v11[length(v11)] + v12n
    v10n <- v10[length(v10)] + v11n
    v9n <- v9[length(v9)] + v10n
    v8n <- v8[length(v8)] + v9n
    v7n <- v7[length(v7)] + v8n
    v6n <- v6[length(v6)] + v7n
    v5n <- v5[length(v5)] + v6n
    v4n <- v4[length(v4)] + v5n
    v3n <- v3[length(v3)] + v4n
    v2n <- v2[length(v2)] + v3n
    soln[i] <- v[length(v)] + v2n
    next
  }
  if (sum(v15 == 0) != length(v15)){v16 <- get_vec(v15)}else{
    v14n <- v14[length(v14)] 
    v13n <- v13[length(v13)] + v14n
    v12n <- v12[length(v12)] + v13n
    v11n <- v11[length(v11)] + v12n
    v10n <- v10[length(v10)] + v11n
    v9n <- v9[length(v9)] + v10n
    v8n <- v8[length(v8)] + v9n
    v7n <- v7[length(v7)] + v8n
    v6n <- v6[length(v6)] + v7n
    v5n <- v5[length(v5)] + v6n
    v4n <- v4[length(v4)] + v5n
    v3n <- v3[length(v3)] + v4n
    v2n <- v2[length(v2)] + v3n
    soln[i] <- v[length(v)] + v2n
    next
  }
  if (sum(v16 == 0) != length(v16)){v17 <- get_vec(v16)}else{
    v15n <- v15[length(v15)]
    v14n <- v14[length(v14)] + v15n
    v13n <- v13[length(v13)] + v14n
    v12n <- v12[length(v12)] + v13n
    v11n <- v11[length(v11)] + v12n
    v10n <- v10[length(v10)] + v11n
    v9n <- v9[length(v9)] + v10n
    v8n <- v8[length(v8)] + v9n
    v7n <- v7[length(v7)] + v8n
    v6n <- v6[length(v6)] + v7n
    v5n <- v5[length(v5)] + v6n
    v4n <- v4[length(v4)] + v5n
    v3n <- v3[length(v3)] + v4n
    v2n <- v2[length(v2)] + v3n
    soln[i] <- v[length(v)] + v2n
    next
  }
  if (sum(v17 == 0) != length(v17)){v18 <- get_vec(v17)}else{
    v16n <- v16[length(v16)]
    v15n <- v15[length(v15)] + v16n
    v14n <- v14[length(v14)] + v15n
    v13n <- v13[length(v13)] + v14n
    v12n <- v12[length(v12)] + v13n
    v11n <- v11[length(v11)] + v12n
    v10n <- v10[length(v10)] + v11n
    v9n <- v9[length(v9)] + v10n
    v8n <- v8[length(v8)] + v9n
    v7n <- v7[length(v7)] + v8n
    v6n <- v6[length(v6)] + v7n
    v5n <- v5[length(v5)] + v6n
    v4n <- v4[length(v4)] + v5n
    v3n <- v3[length(v3)] + v4n
    v2n <- v2[length(v2)] + v3n
    soln[i] <- v[length(v)] + v2n
    next
  }
  if (sum(v18 == 0) != length(v18)){v19 <- get_vec(v18)}else{
    v17n <- v17[length(v17)]
    v16n <- v16[length(v16)] + v17n
    v15n <- v15[length(v15)] + v16n
    v14n <- v14[length(v14)] + v15n 
    v13n <- v13[length(v13)] + v14n
    v12n <- v12[length(v12)] + v13n
    v11n <- v11[length(v11)] + v12n
    v10n <- v10[length(v10)] + v11n
    v9n <- v9[length(v9)] + v10n
    v8n <- v8[length(v8)] + v9n
    v7n <- v7[length(v7)] + v8n
    v6n <- v6[length(v6)] + v7n
    v5n <- v5[length(v5)] + v6n
    v4n <- v4[length(v4)] + v5n
    v3n <- v3[length(v3)] + v4n
    v2n <- v2[length(v2)] + v3n
    soln[i] <- v[length(v)] + v2n
    next
  }
  if (sum(v19 == 0) != length(v19)){v20 <- get_vec(v19)}else{
    v18n <- v18[length(v18)]
    v17n <- v17[length(v17)] + v18n
    v16n <- v16[length(v16)] + v17n
    v15n <- v15[length(v15)] + v16n
    v14n <- v14[length(v14)] + v15n
    v13n <- v13[length(v13)] + v14n
    v12n <- v12[length(v12)] + v13n
    v11n <- v11[length(v11)] + v12n
    v10n <- v10[length(v10)] + v11n
    v9n <- v9[length(v9)] + v10n
    v8n <- v8[length(v8)] + v9n
    v7n <- v7[length(v7)] + v8n
    v6n <- v6[length(v6)] + v7n
    v5n <- v5[length(v5)] + v6n
    v4n <- v4[length(v4)] + v5n
    v3n <- v3[length(v3)] + v4n
    v2n <- v2[length(v2)] + v3n
    soln[i] <- v[length(v)] + v2n
    next
  }
  if (sum(v20 == 0) != length(v20)){v21 <- get_vec(v20)}else{
    v19n <- v19[length(v19)]
    v18n <- v18[length(v18)] + v19n
    v17n <- v17[length(v17)] + v18n
    v16n <- v16[length(v16)] + v17n
    v15n <- v15[length(v15)] + v16n
    v14n <- v14[length(v14)] + v15n
    v13n <- v13[length(v13)] + v14n
    v12n <- v12[length(v12)] + v13n
    v11n <- v11[length(v11)] + v12n
    v10n <- v10[length(v10)] + v11n
    v9n <- v9[length(v9)] + v10n
    v8n <- v8[length(v8)] + v9n
    v7n <- v7[length(v7)] + v8n
    v6n <- v6[length(v6)] + v7n
    v5n <- v5[length(v5)] + v6n
    v4n <- v4[length(v4)] + v5n
    v3n <- v3[length(v3)] + v4n
    v2n <- v2[length(v2)] + v3n
    soln[i] <- v[length(v)] + v2n
    next
  }
}

sum(soln)

# 1749672519 too low
# 1841201636 too high
# 1834108701

# part 2

soln <- rep(NA, nrow(df))

for (i in 1:200){
  v <- as.vector(df[i, ]) %>% unlist()
  v2 <- get_vec(v)
  if (sum(v2 == 0) != length(v2)){v3 <- get_vec(v2)}else{
    soln[i] <- v[1]
    next
  }
  if (sum(v3 == 0) != length(v3)){v4 <- get_vec(v3)}else{
    v2n <- v2[1]
    soln[i] <- v[1] - v2n
    next
  }
  if (sum(v4 == 0) != length(v4)){v5 <- get_vec(v4)}else{
    v3n <- v3[1]
    v2n <- v2[1] - v3n
    soln[i] <- v[1] - v2n
    next
  }
  if (sum(v5 == 0) != length(v5)){v6 <- get_vec(v5)}else{
    v4n <- v4[1]
    v3n <- v3[1] - v4n
    v2n <- v2[1] - v3n
    soln[i] <- v[1] - v2n
    next
  }
  if (sum(v6 == 0) != length(v6)){v7 <- get_vec(v6)}else{
    v5n <- v5[1]
    v4n <- v4[1] - v5n
    v3n <- v3[1] - v4n
    v2n <- v2[1] - v3n
    soln[i] <- v[1] - v2n
    next
  }
  if (sum(v7 == 0) != length(v7)){v8 <- get_vec(v7)}else{
    v6n <- v6[1]
    v5n <- v5[1] - v6n
    v4n <- v4[1] - v5n
    v3n <- v3[1] - v4n
    v2n <- v2[1] - v3n
    soln[i] <- v[1] - v2n
    next
  }
  if (sum(v8 == 0) != length(v8)){v9 <- get_vec(v8)}else{
    v7n <- v7[1] 
    v6n <- v6[1] - v7n
    v5n <- v5[1] - v6n
    v4n <- v4[1] - v5n
    v3n <- v3[1] - v4n
    v2n <- v2[1] - v3n
    soln[i] <- v[1] - v2n
    next
  }
  if (sum(v9 == 0) != length(v9)){v10 <- get_vec(v9)}else{
    v8n <- v8[1] 
    v7n <- v7[1] - v8n
    v6n <- v6[1] - v7n
    v5n <- v5[1] - v6n
    v4n <- v4[1] - v5n
    v3n <- v3[1] - v4n
    v2n <- v2[1] - v3n
    soln[i] <- v[1] - v2n
    next
  }
  if (sum(v10 == 0) != length(v10)){v11 <- get_vec(v10)}else{
    v9n <- v9[1]
    v8n <- v8[1] - v9n
    v7n <- v7[1] - v8n
    v6n <- v6[1] - v7n
    v5n <- v5[1] - v6n
    v4n <- v4[1] - v5n
    v3n <- v3[1] - v4n
    v2n <- v2[1] - v3n
    soln[i] <- v[1] - v2n
    next
  }
  if (sum(v11 == 0) != length(v11)){v12 <- get_vec(v11)}else{
    v10n <- v10[1]
    v9n <- v9[1] - v10n
    v8n <- v8[1] - v9n
    v7n <- v7[1] - v8n
    v6n <- v6[1] - v7n
    v5n <- v5[1] - v6n
    v4n <- v4[1] - v5n
    v3n <- v3[1] - v4n
    v2n <- v2[1] - v3n
    soln[i] <- v[1] - v2n
    next
  }
  if (sum(v12 == 0) != length(v12)){v13 <- get_vec(v12)}else{
    v11n <- v11[1] 
    v10n <- v10[1] - v11n
    v9n <- v9[1] - v10n
    v8n <- v8[1] - v9n
    v7n <- v7[1] - v8n
    v6n <- v6[1] - v7n
    v5n <- v5[1] - v6n
    v4n <- v4[1] - v5n
    v3n <- v3[1] - v4n
    v2n <- v2[1] - v3n
    soln[i] <- v[1] - v2n
    next
  }
  if (sum(v13 == 0) != length(v13)){v14 <- get_vec(v13)}else{
    v12n <- v12[1]
    v11n <- v11[1] - v12n
    v10n <- v10[1] - v11n
    v9n <- v9[1] - v10n
    v8n <- v8[1] - v9n
    v7n <- v7[1] - v8n
    v6n <- v6[1] - v7n
    v5n <- v5[1] - v6n
    v4n <- v4[1] - v5n
    v3n <- v3[1] - v4n
    v2n <- v2[1] - v3n
    soln[i] <- v[1] - v2n
    next
  }
  if (sum(v14 == 0) != length(v14)){v15 <- get_vec(v14)}else{
    v13n <- v13[1]
    v12n <- v12[1] - v13n
    v11n <- v11[1] - v12n
    v10n <- v10[1] - v11n
    v9n <- v9[1] - v10n
    v8n <- v8[1] - v9n
    v7n <- v7[1] - v8n
    v6n <- v6[1] - v7n
    v5n <- v5[1] - v6n
    v4n <- v4[1] - v5n
    v3n <- v3[1] - v4n
    v2n <- v2[1] - v3n
    soln[i] <- v[1] - v2n
    next
  }
  if (sum(v15 == 0) != length(v15)){v16 <- get_vec(v15)}else{
    v14n <- v14[1] 
    v13n <- v13[1] - v14n
    v12n <- v12[1] - v13n
    v11n <- v11[1] - v12n
    v10n <- v10[1] - v11n
    v9n <- v9[1] - v10n
    v8n <- v8[1] - v9n
    v7n <- v7[1] - v8n
    v6n <- v6[1] - v7n
    v5n <- v5[1] - v6n
    v4n <- v4[1] - v5n
    v3n <- v3[1] - v4n
    v2n <- v2[1] - v3n
    soln[i] <- v[1] - v2n
    next
  }
  if (sum(v16 == 0) != length(v16)){v17 <- get_vec(v16)}else{
    v15n <- v15[1]
    v14n <- v14[1] - v15n
    v13n <- v13[1] - v14n
    v12n <- v12[1] - v13n
    v11n <- v11[1] - v12n
    v10n <- v10[1] - v11n
    v9n <- v9[1] - v10n
    v8n <- v8[1] - v9n
    v7n <- v7[1] - v8n
    v6n <- v6[1] - v7n
    v5n <- v5[1] - v6n
    v4n <- v4[1] - v5n
    v3n <- v3[1] - v4n
    v2n <- v2[1] - v3n
    soln[i] <- v[1] - v2n
    next
  }
  if (sum(v17 == 0) != length(v17)){v18 <- get_vec(v17)}else{
    v16n <- v16[1]
    v15n <- v15[1] - v16n
    v14n <- v14[1] - v15n
    v13n <- v13[1] - v14n
    v12n <- v12[1] - v13n
    v11n <- v11[1] - v12n
    v10n <- v10[1] - v11n
    v9n <- v9[1] - v10n
    v8n <- v8[1] - v9n
    v7n <- v7[1] - v8n
    v6n <- v6[1] - v7n
    v5n <- v5[1] - v6n
    v4n <- v4[1] - v5n
    v3n <- v3[1] - v4n
    v2n <- v2[1] - v3n
    soln[i] <- v[1] - v2n
    next
  }
  if (sum(v18 == 0) != length(v18)){v19 <- get_vec(v18)}else{
    v17n <- v17[1]
    v16n <- v16[1] - v17n
    v15n <- v15[1] - v16n
    v14n <- v14[1] - v15n 
    v13n <- v13[1] - v14n
    v12n <- v12[1] - v13n
    v11n <- v11[1] - v12n
    v10n <- v10[1] - v11n
    v9n <- v9[1] - v10n
    v8n <- v8[1] - v9n
    v7n <- v7[1] - v8n
    v6n <- v6[1] - v7n
    v5n <- v5[1] - v6n
    v4n <- v4[1] - v5n
    v3n <- v3[1] - v4n
    v2n <- v2[1] - v3n
    soln[i] <- v[1] - v2n
    next
  }
  if (sum(v19 == 0) != length(v19)){v20 <- get_vec(v19)}else{
    v18n <- v18[1]
    v17n <- v17[1] - v18n
    v16n <- v16[1] - v17n
    v15n <- v15[1] - v16n
    v14n <- v14[1] - v15n
    v13n <- v13[1] - v14n
    v12n <- v12[1] - v13n
    v11n <- v11[1] - v12n
    v10n <- v10[1] - v11n
    v9n <- v9[1] - v10n
    v8n <- v8[1] - v9n
    v7n <- v7[1] - v8n
    v6n <- v6[1] - v7n
    v5n <- v5[1] - v6n
    v4n <- v4[1] - v5n
    v3n <- v3[1] - v4n
    v2n <- v2[1] - v3n
    soln[i] <- v[1] - v2n
    next
  }
  if (sum(v20 == 0) != length(v20)){v21 <- get_vec(v20)}else{
    v19n <- v19[1]
    v18n <- v18[1] - v19n
    v17n <- v17[1] - v18n
    v16n <- v16[1] - v17n
    v15n <- v15[1] - v16n
    v14n <- v14[1] - v15n
    v13n <- v13[1] - v14n
    v12n <- v12[1] - v13n
    v11n <- v11[1] - v12n
    v10n <- v10[1] - v11n
    v9n <- v9[1] - v10n
    v8n <- v8[1] - v9n
    v7n <- v7[1] - v8n
    v6n <- v6[1] - v7n
    v5n <- v5[1] - v6n
    v4n <- v4[1] - v5n
    v3n <- v3[1] - v4n
    v2n <- v2[1] - v3n
    soln[i] <- v[1] - v2n
    next
  }
}

sum(soln)

v11
v10
v9
v8

