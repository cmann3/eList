## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
x <- c("apples", "bananas", "pears")

fruit_chars <- vector("list", length(x))
for (i in seq_along(x)){
  fruit_chars[[i]] <- 1:nchar(x[i])
}
fruit_chars

## -----------------------------------------------------------------------------
fruit_chars <- lapply(x, function(i) 1:nchar(i))

## -----------------------------------------------------------------------------
library(eList)
fruit_chars <- List(for (i in x) 1:nchar(i))

## -----------------------------------------------------------------------------
List(for (i in x) i = 1:nchar(i))

## -----------------------------------------------------------------------------
List(for (i in x) (if (i == "bananas") "good" else "bad") = 1:nchar(i))

## -----------------------------------------------------------------------------
List(for (i in x) if (i == "bananas") 1:nchar(i))

## -----------------------------------------------------------------------------
List(for (i in x) if (i == "bananas") "delicious" else "ewww")

## -----------------------------------------------------------------------------
List(for (i in x) i = {
  n <- nchar(i)
  if (n > 6) "delicious"
  else if (n > 5) "ok"
  else "ewww"
})

## -----------------------------------------------------------------------------
List(for (i.j in enum(x)) j = i)

## -----------------------------------------------------------------------------
enum(x)

## -----------------------------------------------------------------------------
y <- list(a = 1:3, b = 4:6, c = 7:9)
List(for (i.j.k in y) (i+j)/k)


## -----------------------------------------------------------------------------
List(for (i..j in y) c(i, j))

## -----------------------------------------------------------------------------
List(for (i.j in items(y)) paste0(i, j))

## -----------------------------------------------------------------------------
Num(for (i.j.k in y) (i+j)/k)
Chr(for (i.j.k in y) (i+j)/k)

## -----------------------------------------------------------------------------
..[for (i.j.k in y) (i+j)/k]

## -----------------------------------------------------------------------------
Mat(for (i in 1:3) for (j in 1:6) i*j)

## -----------------------------------------------------------------------------
Num(for (i in 1:3) for (j in 1:6) if (i==j) i*j)

## -----------------------------------------------------------------------------
Chr(for (i in Num(for (j in 1:3) j^2)) paste0(letters[sqrt(i)], i))

## ----eval=FALSE---------------------------------------------------------------
#  cluster <- auto_cluster()
#  Num(for (i in 1:100) sample(1:100, 1), clust=cluster)
#  close_cluster(cluster)

## -----------------------------------------------------------------------------
# Are all values greater than 0?
All(for (i in 1:10) i > 0)

## -----------------------------------------------------------------------------
# Are no values TRUE? (combines comprehension with other values)
None(for (i in 1:10) i < 0, TRUE, FALSE)

## -----------------------------------------------------------------------------
# factorial(5)
Prod(for (i in 1:5) i)

## -----------------------------------------------------------------------------
# Summary statistics from a random draw of 1000 observations from normal distribution
Stats(for (i in rnorm(1000)) i)

## -----------------------------------------------------------------------------
# Every other letter in the alphabet as a single character
Paste(for (i in seq(1,26,2)) letters[i], collapse=", ")

## -----------------------------------------------------------------------------
time_for <- mean(sapply(1:3, function(i){
  system.time(
    {
      nums <- numeric(0)
      for (j in 1:100000) nums <- c(nums, log(rnorm(1, 1000, 10)) + 1)
    }
  )
}))

time_sapply <- mean(sapply(1:3, function(i){
  system.time(
    sapply(1:100000, function(j) log(rnorm(1, 1000, 10)) + 1)
  )
}))

time_comp <- mean(sapply(1:3, function(i){
  system.time(
    Num(for (i in 1:100000) log(rnorm(1, 1000, 10)) + 1)
  )
}))

cluster <- auto_cluster()
time_parallel <- mean(sapply(1:3, function(i){
  system.time(
    Num(for (i in 1:100000) log(rnorm(1, 1000, 10)) + 1, clust=cluster)
  )
}))
close_cluster(cluster)

cat(paste0("for Loop: ", time_for, "\nsapply:   ", time_sapply, "\nNum:      ", 
           time_comp, "\nNum-Prll: ", time_parallel, "\n"))

