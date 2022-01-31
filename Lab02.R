# Exercise 1 (Creating More Vectors)
#1. A integer vector of the first nine Fibonacci numbers, starting from 0. Hint: Use the c() function.
x1 <- c(0L, 1L, 1L, 2L, 3L, 5L, 8L, 13L, 21L)
#2. A double vector containing the numbers 42, 42, and 3.14. Give the elements names one, two, and three respectively.
x2 <- c(one = 42, two = 42, three = 3.14)
#3. A character vector containing “University of Illinois at Urbana-Champaign” where each word is an element of the vector. (Urbana-Champaign being one word.)
x3 <- c("University", "of", "Illinois", "at", "Urbana-Champaign")
#4. A logical vector containing the sequence TRUE, TRUE, FALSE repeated 10 times.
x4 <- rep(c(TRUE, TRUE, FALSE), 10)

# Exercise 2 (Creating A Matrix)
c_matrix = matrix(c(1: 100), nrow = 25, ncol = 4)
r_matrix = matrix(c(1: 100), nrow = 25, ncol = 4, byrow = TRUE)

# Exercise 3 (Creating Lists)
#1. A vector named x which contains only the number 42. (Double of integer is fine.)
x = c(42)
#2. A matrix named y which has 5 rows and 2 columns. It can contain any values.
y = matrix(c(1: 10), nrow = 5, ncol = 2)
#3. A vector names z that contains the months of the year, that is January, February, etc. Hint: Run ?letters.
z = month.name

list <- list(x = x, y = y, z = z)

# Exercise 4 (First Generation Starter Pokemon)
starter_pokemon = data.frame(
  pokedex_num = c(1: 9), 
  name = c("Bulbasaur", "Ivysaur", "Venusaur", "Charmander", "Charmeleon", "Charizard", "Squirtle", "Wartortle", "Blastoise"),
  type_primary = c("Grass", "Grass", "Grass", "Fire", "Fire", "Fire", "Water", "Water", "Water"),
  type_secondary = c("Poison", "Poison", "Poison", "", "", "", "", "", "")
  )

# Exercise 5 (External Data)
pokemon = read.csv("https://stat385.org/data/pokemon.csv")
head(pokemon)
tail(pokemon)
pokemon[1:25, 1:4]
pokemon[pokemon$legendary == TRUE, ]



split_odd_even <- function(x) {
  vec_odd <- c()
  vec_even <- c()
  for (i in seq(1, length(x), 2)) {vec_odd <- c(vec_odd, x[i])}
  for (i in seq(2, length(x), 2)) {vec_even <- c(vec_even, x[i])}
  return (list(odd = vec_odd, even = vec_even))
}

calc_olympic_score = function(judges_scores) {
  
  # delete this comment and place your code here
  res <- 0
  for (i in 1 : length(judges_scores)) res <- res + judges_scores[i]
  
  res <- res - max(judges_scores)
  res <- res - min(judges_scores)
  
  return (res / (length(x) - 2))
}


contain_word <- function(x, word) {
  for (i in 1 : length(x)) {
    if (x[i] == word) {
      return (TRUE)
    }
  }
  return (FALSE)
}

count_word <- function(x, word) {
  count <- 0
  for (i in 1 : length(x)) {
    if (x[i] %in% word == TRUE) {
      count = count + 1
    }
  }
  return (count)
}

list_to_mod_df <- function(lst, row_mod, row_delete) {
  
  
  lst$df[row_mod, 1] <- lst$rep_1
  lst$df[row_mod, 2] <- lst$rep_2
  lst$df[row_mod, 3] <- lst$rep_3
  
  lst$df <- lst$df[-c(row_delete),]
  
  return (lst$df)
}

replace_an_element <- function(x, to_rep, rep_with) {
  for (i in 1 : length(x)) {
    if (x[i] == to_rep) {
      x[i] <- rep_with
    }
  }
  return (x);
}




