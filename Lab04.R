# Exercise 1 (Fizz Buzz)
fizz_buzz <- function(x) {
  vector <- c()
  for (i in 1 : length(x)) {
    if (x[i] %% 3 == 0 && x[i] %% 5 == 0) {
      vector <- c(vector, "fizzbuzz")
    } else if (x[i] %% 3 == 0) {
      vector <- c(vector, "fizz")
    } else if (x[i] %% 5 == 0) {
      vector <- c(vector, "buzz")
    } else {
      vector <- c(vector, as.character(x[i]))
    }
  }
  return (vector)
}

fizz_buzz(x = c(1 : 15))
fizz_buzz(x = c(1 : 100))
fizz_buzz(x = c(1 : 1000))

# Exercise 2 (Leap Years)
is_leap <- function(year) {
  if (year %% 400 == 0) {
    return (TRUE)
  }
  if (year %% 4 == 0 && year %% 100 != 0) {
    return (TRUE)
  }
  return (FALSE)
}

is_leap(year = 1822)
is_leap(year = 1900)
is_leap(year = 2000)
is_leap(year = 2100)
is_leap(year = 2200)
is_leap(year = 2300)
is_leap(year = 2400)

# Exercise 3 (Multiplicative Sequences)
gen_mult_seq <- function(first, multi, length) {
  vector <- c()
  for (i in 1 : length) {
    vector <- c(vector, first)
    first <- first * multi
  }
  return (vector)
}

gen_mult_seq(1, 2, 7)
gen_mult_seq(3, 4, 15)
gen_mult_seq(12, 3, 9)
