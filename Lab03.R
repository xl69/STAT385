# Exercise 1

set.seed(42)
x = rnorm(100)
y = sample(letters, size = 1000, replace = TRUE)
z = sample(c(TRUE, FALSE, NA), size = 100, replace = TRUE)

# A line of code that sums the elements of x that are stored in even indexes.
sum(x[seq(2, length(x), 2)])

# A line of code that calculates the proportion of vowels (a, e, i, o, u) in y.
sum2 <- 0
for (i in seq(1, length(y), 1)) {if (y[i] == "a" || y[i] == "e" || y[i] == "i" || y[i] == "o" || y[i] == "u") sum2 <- sum2 + 1}
sum2 / length(y)

# A line of code that counts the number of elements which are NA in z.
sum3 <- 0
for (i in seq(1, length(z), 1)) {if (is.na(z[i])) sum3 <- sum3 + 1}
sum3

# Exercise 2

absurd_list = list(
  q = "Look elsewhere.", 
  y = list(
    z = list(x = 1),
    x = list(x = 2),
    zzz = list(
      zzz = "Not here.",
      zz = list(qq = "gg"),
      a = list(
        b = list(
          q = list(
            answer = list(
              answer = "Hello World!")
          )))
    )
  ))
absurd_list[[2]][[3]][[3]][[1]][[1]][[1]][[1]]

# Exercise 3

temp = airquality[-which(is.na(airquality$Ozone)), ]
answer = temp[-which(is.na(temp$Solar.R)), ]

all(answer == na.omit(airquality))

# Exercise 4

library(nflreadr)
rosters_2021 = as.data.frame(load_rosters(seasons = 2021))
rosters_2021 = rosters_2021[-which(is.na(rosters_2021$college)), ]
nfl_illini_2021 = subset(rosters_2021, college %in% "Illinois", select = c(team, position, jersey_number, full_name, height, weight))
nfl_illini_2021["height"][nfl_illini_2021["full_name"] == "Nate Hobbs"] <- "6-0"
nfl_illini_2021

# Exercise 5

nfl_2021 = as.data.frame(load_schedules(seasons = 2021))
answer = subset(nfl_2021, home_team %in% "CHI" | away_team %in% "CHI", select = c(week, away_team, home_team))
answer