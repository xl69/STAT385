# Exercise 1
# 1. A vector of the first nine Fibonacci numbers, starting from 0.
x1 <- c(0, 1, 1, 2, 3, 5, 8, 13, 21)
# 2. A vector of the integers from 1 to 25. Hint: Use the : operator.
x2 <- 1 : 25
# 3. A vector of the even numbers from 2 to 20
x3 <- seq(2, 20 ,2)
# 4. A vector that stores the following strings: rock, paper, scissors.
x4 <- c("rock", "paper", "scissors")

# Exercise 2
vector_1 <- 1 : 10
vector_2 <- c(0, 5)
vector_1 + vector_2

# Exercise 3
add_maxes <- function(x, y) {
  return(max(x) + max(y))
}

# Exercise 4
rock_paper_scissors <- function() {
  return(sample(c("rock", "paper", "scissors"), 2, replace=T))
}

# Exercise 5
rps_extended <- function(shapes = c("rock", "paper", "scissors"), n_players = 2) {
  return(sample(shapes, n_players, replace=T))
}