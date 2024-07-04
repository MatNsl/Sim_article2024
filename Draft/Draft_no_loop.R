source("~/work/Sim_article2024/Useful_functions/Circle.R")

n <- nrow(random_points)

start.time <- Sys.time()
a <- c() # Target: the initial sample alone
for (i in 1:n) {
  a <- append(a, circle3_nb(random_points[i, 1], random_points[i, 2], 15, 9, 6))
}
end.time <- Sys.time()
time.taken1 <- end.time - start.time
time.taken1

start.time <- Sys.time()
b <- mapply(circle3_nb, random_points[, 1], random_points[, 2], MoreArgs = list(15, 9, 6))
end.time <- Sys.time()
time.taken2 <- end.time - start.time
time.taken2


# Error
a <- circle3_nb(random_points[, 1], random_points[, 2], 15, 9, 6)

a1 <- c(1,2)
b1 <- c(3,4)
a2 <- c(5,6)
b2 <- c(7,8)
vec_try <- data.frame(x = c(a1,b1), y = c(a2,b2))
# vec_try <- tibble(.rows = 2)
# colnames(vec_try) <- c("x", "y")
# vec_try$x <- c(a1,b1)
mapply(circle_nb, vec_try)

mapply(circle3_nb, vec_try[, 1], vec_try[, 2], MoreArgs = list(15, 9, 6))

a <- c()
for (i in 1:n) {
  a <- append(a, fun(df[i, 1], df[i, 2], 15, 9, 6))
}

### apply + mapply

start.time <- Sys.time()
est_boot_b <- c()
for (c in 1:B) {
  a <- mapply(circle3_nb, df_boot[, (2*c)-1], df_boot[, 2*c], MoreArgs = list(15, 9, 6))
  est_boot_b <- append(est_boot_b, sum(a/p))
}
est_boot_b <- sort(est_boot_b)
end.time <- Sys.time()
time.taken1 <- end.time - start.time
time.taken1
# Time difference of 11.80462 mins

start.time <- Sys.time()
est_boot <- apply(as.array(1:B), 1, function(c){
  sum(
    (mapply(circle3_nb, df_boot[, (2*c)-1], df_boot[, 2*c], MoreArgs = list(15, 9, 6)))
    /p)
      })
est_boot <- sort(est_boot)
end.time <- Sys.time()
time.taken2 <- end.time - start.time
time.taken2
# Time difference of 12.32548 mins


##### Ancienne version: ----------
a <- c()
for (i in 1:n) {
  a <- append(a, circle3_nb(df_boot[i, (2*c)-1], df_boot[i, 2*c], 15, 9, 6))
}

a <- c() # Target: the initial sample alone
for (i in 1:n) {
  a <- append(a, circle3_nb(df_init[i, 1], df_init[i, 2], 15, 9, 6))
}



