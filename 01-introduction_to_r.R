## -----------------------------------------------------------------------------
#| output-location: fragment
#| echo: true
#| fig-width: 4
#| fig-height: 4
#| fig-align: center
print("Hello, WU!")
paste0("One plus two is: ", 1 + 2)
c <- data.frame(p = seq(0, 2 * pi, by = 0.001))
c$h_x <- 16 * sin(c$p)^3
c$h_y <- 13 * cos(c$p) - 5 * cos(2 * c$p) - 2 * cos(3 * c$p) - cos(4 * c$p)
plot(c$h_x, c$h_y, type = "l", main = "I <3 R", frame = F, xlab = NA, ylab = NA)


## -----------------------------------------------------------------------------
#| echo: true
# This is a comment
print("Hi") # also a comment
## Assignment of varibale names
x <- 1
x
## Missing values
NA
## Vectors
y <- c(1, 2, 3, NA)
y


## -----------------------------------------------------------------------------
#| echo: true
## built-in
sum(y)
sum(y, na.rm = FALSE)
sum(y, na.rm = TRUE)
## User functions
a_plus_b <- function(a, b = 1) {
    return(a + b)
}
a_plus_b(y)
a_plus_b(y, 2)
a_plus_b(b = 2, a = y)
## Functions provided by packages
## Installation
#install.packages("ineq")
ineq::Gini(y)
## or
library(ineq)
Gini(y)
## Help 
?Gini


## -----------------------------------------------------------------------------
#| echo: true
y[1]
y[-1]
y[2:3]
y[c(1, 3, 4)]

set.seed(1)
x <- y / 2 + rnorm(length(y))
cbind(y, x)
y > 2
y > 2 & x > 0
y > 2 | x > 0

y[y > 2 | x > 0]


## -----------------------------------------------------------------------------
#| echo: true
## 'elem' is a temporary variable
for (elem in y) {
    print(paste("Current y value is:", elem))
}
## 'seq_along' returns a vector which indexes the argument
for (i in seq_along(y)) {
    print(paste("Current y value is:", y[i]))
}
## set.seed guarantees the same random numbers every time
set.seed(1)
total <- 0
while (total < 1) {
    ## runif generates random numbers between 0 and 1
    total <- total + runif(1)
    print(paste("Current total value is:", total))
}
## ranges
1:3
10:3
seq(3, 11, by = 2)


## -----------------------------------------------------------------------------
#| echo: true
z <- -2:3
for (x in z) {
  print(paste("x =", x))
    if (x > 0) {
        print("x is positive")
    } else if (x > 2) {
        print("x is greater than 2")
    } else if (x < 0) {
        print("x is negative")
    } else if (x == 0) {
        print("x is zero")
    }
}
z[z <= 0]
z[z >= 0]
z[z != 0]
z[! z < 0]


## -----------------------------------------------------------------------------
a <- c(1, 2, 3)
b <- c(1, 2, 3, 4)
cat("a is:", a, "\n")
cat("b is:", b, "\n")
cat("Result:\n")
NA


## -----------------------------------------------------------------------------
a <- c(1, 2, 3)
b <- c(0, 2, 4)
cat("a is:", a, "\n")
cat("b is:", b, "\n")
cat("Result:\n")
print("1 a larger")
print("2 equal")
print("3 b larger")


## -----------------------------------------------------------------------------
#| echo: true
data <- data.frame(x = -1:1, y = 3:1, z = c("a", "b", NA))
data
class(data)
## Variable access
data$x
data$x + data$y

row_summaries <- with(data, 
  data.frame(
    rsum = x + y,
    rdiff = x - y
  ))
row_summaries


## -----------------------------------------------------------------------------
#| echo: true
str(data)
summary(data)
head(data)


## -----------------------------------------------------------------------------
#| echo: true
## 2D structure of data
## Empty argument means "all"#| 
data[, c("x", "y")]
data[1:3, c("x", "y")]
data[1, ]
data[c(1, 3), c("x", "z")]
data[data$x < 3,]


## -----------------------------------------------------------------------------
#| echo: true

## new data has to have the same number of elements
data$a <- 2 * data$x
data
data$b <- c("one", "two", "three")
data

data$x <- NULL
data

data$a <- log(data$a)
data

data$b[data$b == "two"] <- "TWO!"
data$z[is.na(data$z)] <- "c"
data$a[is.nan(data$a)] <- 0
data


## -----------------------------------------------------------------------------
#| echo: true
## CSV
penguins_raw <- readr::read_csv("data/penguins/penguins_raw.csv")
head(penguins_raw, 2)


## -----------------------------------------------------------------------------
#| echo: true
penguins_raw[1,1]


## -----------------------------------------------------------------------------
#| echo: true
## CSV
penguins_raw <- readr::read_csv(
  "data/penguins/penguins_raw.csv",
  comment = "#")
head(penguins_raw, 2)
penguins_raw[1,1]
str(penguins_raw)


## -----------------------------------------------------------------------------
#| echo: true
penguins_raw <- readxl::read_excel("data/penguins/penguins_raw.xlsx")
head(penguins_raw, 2)
## Read a subset 
penguins_subset <- readxl::read_excel("data/penguins/penguins_raw.xlsx", sheet = "Sheet1", range = "B1:O345")
head(penguins_subset, 2)


## -----------------------------------------------------------------------------
#| echo: true
penguins_raw <- haven::read_sav("data/penguins/penguins_raw.sav")
head(penguins_raw, 2)


## -----------------------------------------------------------------------------
#| echo: true
penguins_raw <- arrow::read_parquet("data/penguins/penguins_raw.parquet")
penguins_raw <- arrow::read_feather("data/penguins/penguins_raw.feather")
head(penguins_raw, 2)


## -----------------------------------------------------------------------------
#| echo: true
penguin_species_island <- arrow::read_parquet('data/penguins/penguin_species_nested.parquet')
head(penguin_species_island, 2)
head(tidyr::unnest(penguin_species_island), 2)


## -----------------------------------------------------------------------------
#| echo: true
#| warnings: false
#| cache: true
library(microbenchmark)
microbenchmark(
  csv = readr::read_csv("data/penguins/penguins_raw.csv", 
   show_col_types = FALSE, name_repair = 'minimal'),
  parquet = arrow::read_parquet("data/penguins/penguins_raw.parquet"),
  feather = arrow::read_feather("data/penguins/penguins_raw.feather")
) 


## -----------------------------------------------------------------------------
#| echo: true
str(penguins_raw)


## -----------------------------------------------------------------------------
#| echo: true
penguins <- janitor::clean_names(penguins_raw)
str(penguins)


## -----------------------------------------------------------------------------
#| echo: true
penguins |>
  head(2)
head(penguins, 2)


## -----------------------------------------------------------------------------
#| echo: true
library(dplyr)
library(stringr)
penguins_subset <- penguins |>
  mutate(
    species = str_split(species, " ", n = 2, simplify = TRUE)[,1],
    is_adult = str_detect(str_to_lower(stage), "adult"),
    is_female = str_detect(str_to_lower(sex), "female"),
    sex = str_to_lower(sex)) |>
  select(species, island, sex, is_adult,  culmen_length_mm, culmen_depth_mm, is_female)
penguins_subset |> head(2)


## -----------------------------------------------------------------------------
#| echo: true
#penguins_subset <- 
penguins_subset <- penguins_subset |>
  mutate(
    across(starts_with('culmen'), \(x) x / 10),
    across(species:sex, as.factor),
    across(c('is_adult', 'is_female'), as.numeric)
    ) |>
  mutate_if(is.numeric,
    list(scaled = \(x) (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE))
  ) |>
  rename_with(
    \(name) str_replace(name, "mm", "cm"),
    starts_with('culmen'))
penguins_subset |> select(-starts_with('is')) |> str()


## -----------------------------------------------------------------------------
#| echo: true
\(name) str_replace(name, "mm", "cm")
str_replace("ammm", "mm", "cm")


## -----------------------------------------------------------------------------
#| echo: true
?str_replace


## -----------------------------------------------------------------------------
#| echo: true
typeof(\(name) str_replace(name, "mm", "cm"))


## -----------------------------------------------------------------------------
#| echo: true
my_function <- \(name) str_replace(name, "mm", "cm")
my_function('here are some mms')


## -----------------------------------------------------------------------------
#| echo: true
str_replace


## -----------------------------------------------------------------------------
#| error: true
#| echo: true
means <- c(4,5,6)
mean[1]


## -----------------------------------------------------------------------------
#| echo: true
adelies <- penguins_subset |>
  filter(species == "Adelie")
unique(adelies$species)
female_adelies <- penguins_subset |>
  filter(species == "Adelie", is_female == 1)
female_adelies |> select(species, sex) |> summary()


## -----------------------------------------------------------------------------
#| echo: true
library(tidyr)
penguins |>
  drop_na(body_mass_g) |>
  summarize(avg_weight = mean(body_mass_g))


## -----------------------------------------------------------------------------
#| echo: true
penguins_summary <- penguins_subset |>
  drop_na(culmen_length_cm) |>
  group_by(species, sex) |>
  summarize(avg_clength = mean(culmen_length_cm))
penguins_summary


## -----------------------------------------------------------------------------
#| echo: true
penguins_summary |>
  pivot_wider(names_from = species, values_from = avg_clength) 

penguins_wide <- penguins_subset |>
  drop_na(culmen_length_cm) |>
  select(culmen_length_cm, species, sex) |>
  pivot_wider(values_from = culmen_length_cm, names_from = species, values_fn = mean) |>
  arrange(sex) |>
  select(sex, Adelie, Chinstrap, Gentoo)
penguins_wide

pivot_longer(penguins_wide, cols = -sex, names_to = "species", values_to = "avg_clength")


## -----------------------------------------------------------------------------
#| echo: true
charts <- readr::read_csv("data/chart_data/top10_charts.csv")
songs <- readr::read_csv("data/chart_data/top10_meta.csv")
str(charts, give.attr=FALSE)
str(songs, give.attr=FALSE)


## -----------------------------------------------------------------------------
#| echo: true
data1 <- data.frame(group = c('a', 'a', 'b','c'), value = c(1,2,3,4)) # missing group 'd'
data2 <- data.frame(group2 = c('a', 'c', 'd'), value2 = factor(c("abc", "def", "ghi"))) # missing group 'b'

left_join(data1, data2, by = c("group" = "group2"))
right_join(data1, data2, by = c("group" = "group2"))
inner_join(data1, data2, by = c("group" = "group2"))


## -----------------------------------------------------------------------------
#| echo: true
full_join(data1, data2, by = c("group" = "group2"))
semi_join(data1, data2, by = c("group" = "group2"))
filter(data1, group %in% data2$group2)
anti_join(data1, data2, by = c("group" = "group2"))

