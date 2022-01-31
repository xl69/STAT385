find_divisors = function(x) {
  if (x == 1) {
    return (NULL)
  } else {
    vec <- c()
    for (i in 1 : (x - 1)) {
      if (x %% i == 0) {
        vec <- c(vec, i)
      }
    }
    return(vec)
  }
}

is_perfect = function(x) {
  return(sum(find_divisors(x)) == x)
}

is_abundant = function(x) {
  return(sum(find_divisors(x)) > x)
}

is_deficient = function(x) {
  return(sum(find_divisors(x)) < x)
}

data.frame(
  x = 1:25,
  perfect = sapply(1:25, is_perfect),
  abundant = sapply(1:25, is_abundant),
  deficient = sapply(1:25, is_deficient)
)

split_digits = function(x) {
  
  if (!is.numeric(x) | length(x) != 1 | trunc(x) != x) {
    stop("x stop be a numeric vector (that represents an \"integer\") of length 1")
  }
  
  return(as.numeric(strsplit(as.character(x), split = "")[[1]]))
  
}

is_valid = function(num) {
  
  if (length(split_digits(x = num)) < 2) {
    stop("input numbermust have at least two digits")
  }
  
  # delete this comment and place your code here
  x <- split_digits(num)
  lastDigit <- x[length(x)]
  x <- x[-length(x)]
  index <- 0
  for (i in 1 : length(x)) {
    if (index %% 2 == 0) {
      x[length(x) - i + 1] <- x[length(x) - i + 1] * 2
    }
    index <- index + 1
  }
  for (i in 1 : length(x)) {
    if (x[i] >= 10) {
      sum <- 0
      while (x[i] > 0) {
        sum <- sum + x[i] %% 10
        x[i] <- x[i] %/% 10
      }
      x[i] <- sum
    }
  }
  s <- sum(x)
  return(10 - (s %% 10) == lastDigit)
}

is_valid(num = 79927398713)
is_valid(num = 4539319503436467)
is_valid(num = 8273123273520569)

long_vec = seq(1, 200, 1)
even = c()
for (i in 1 : 200) {
  if (i %% 2 == 0) {
    even = c(even, long_vec[i])
  }
}
large = c()
for (i in 1 : 200) {
  if (long_vec[i] > 64 ) {
    large = c(large, long_vec[i])
  }
}

some_list = list(
  g = 42,
  m = "Exams are fun!",
  s = as.integer(seq(8, 314, 1))
)

check_vec_total = function(x) {
  s = sum(x)
  if (s <= 12) {
    return ("small total")
  } else if (s > 12 && s <= 26) {
    return ("medium total")
  } else if (s > 26) {
    return ("large total")
  }
}

positive = c(vec[6], vec[11], vec[30])
negative = c(vec[1], vec[3:14], vec[16:26], vec[28:100])

some_df = data.frame(
  d = rep(117, 38),
  j = rep("STAT 385", 38),
  f = seq(1, 38, 1)
)

gen_seq = function(first, len) {
  vec = c(first)
  for (i in 2 : len) {
    nextElement = sqrt(first *3 + 14)
    vec = c(vec, nextElement)
    first = nextElement
  }
  return (vec)
}