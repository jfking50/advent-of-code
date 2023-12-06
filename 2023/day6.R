#Time:        61     70     90     66
#Distance:   643   1184   1362   1041

library(purrr)

r1 <- 1:61 %>% map(function(x)x * (61-x)) %>% unlist()
r2 <- 1:70 %>% map(function(x)x * (70-x)) %>% unlist()
r3 <- 1:90 %>% map(function(x)x * (90-x)) %>% unlist()
r4 <- 1:66 %>% map(function(x)x * (66-x)) %>% unlist()

sum(r1 > 643) * sum(r2 > 1184) * sum(r3 > 1362) * sum(r4 > 1041)

# part 2
r5 <- 1:61709066 %>% map(function(x)x * (61709066-x)) %>% unlist()
sum(r5 > 643118413621041)
