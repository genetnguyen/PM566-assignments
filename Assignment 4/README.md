Assignment 4 - HPC and SQL
================
Eugene Nguyen
11/16/2021

# HPC

## Problem 1: Make sure your code is nice

Rewrite the following R functions to make them faster. It is OK (and
recommended) to take a look at Stackoverflow and Google.

``` r
# Total row sums
fun1 <- function(mat) {
  n <- nrow(mat)
  ans <- double(n) 
  for (i in 1:n) {
    ans[i] <- sum(mat[i, ])
  }
  ans
}

fun1alt <- function(mat) {
  # YOUR CODE HERE
  rowSums(mat)
}

# Cumulative sum by row
fun2 <- function(mat) {
  n <- nrow(mat)
  k <- ncol(mat)
  ans <- mat
  for (i in 1:n) {
    for (j in 2:k) {
      ans[i,j] <- mat[i, j] + ans[i, j - 1]
    }
  }
  ans
}

fun2alt <- function(mat) {
  # YOUR CODE HERE
  ans <- as.data.table(t(mat))
  ans <- cumsum(ans)
  ans <- t(ans)
  ans
}


# Use the data with this code
set.seed(2315)
dat <- matrix(rnorm(200 * 100), nrow = 200)

# Test for the first
microbenchmark::microbenchmark(
  fun1(dat),
  fun1alt(dat), check = "equivalent"
)
```

    ## Unit: microseconds
    ##          expr   min     lq    mean median     uq    max neval cld
    ##     fun1(dat) 333.2 437.25 574.934 541.35 684.80 1456.1   100   b
    ##  fun1alt(dat)  49.8  53.10  86.821  59.00  79.75 1855.2   100  a

``` r
# Test for the second
microbenchmark::microbenchmark(
  fun2(dat),
  fun2alt(dat), check = "equivalent"
)
```

    ## Unit: milliseconds
    ##          expr    min     lq     mean  median     uq     max neval cld
    ##     fun2(dat) 2.3121 2.9402 4.649514 3.94690 5.6446 20.0147   100  a 
    ##  fun2alt(dat) 3.3541 4.3601 6.495860 5.65745 7.6310 17.6508   100   b

The last argument, check = “equivalent”, is included to make sure that
the functions return the same result.

# Problem 2: Make things run faster with parallel computing

The following function allows simulating PI

``` r
sim_pi <- function(n = 1000, i = NULL) {
  p <- matrix(runif(n*2), ncol = 2)
  mean(rowSums(p^2) < 1) * 4
}

# Here is an example of the run
set.seed(156)
sim_pi(1000) # 3.132
```

    ## [1] 3.132

In order to get accurate estimates, we can run this function multiple
times, with the following code:

``` r
# This runs the simulation a 4,000 times, each with 10,000 points
set.seed(1231)
system.time({
  ans <- unlist(lapply(1:4000, sim_pi, n = 10000))
  print(mean(ans))
})
```

    ## [1] 3.14124

    ##    user  system elapsed 
    ##    4.86    0.00    4.91

Rewrite the previous code using parLapply() to make it run faster. Make
sure you set the seed using clusterSetRNGStream():

``` r
# YOUR CODE HERE
cluster <- makeCluster(4L)
clusterSetRNGStream(cluster,iseed = 1111)
system.time({
  # YOUR CODE HERE
  ans <- unlist(parLapply(cluster, 1:4000, sim_pi, n=1000))
  print(mean(ans))
  # YOUR CODE HERE
})
```

    ## [1] 3.140899

    ##    user  system elapsed 
    ##    0.00    0.00    0.36

# SQL

Setup a temporary database by running the following chunk

``` r
# install.packages(c("RSQLite", "DBI"))

library(RSQLite)
library(DBI)

# Initialize a temporary in memory database
con <- dbConnect(SQLite(), ":memory:")

# Download tables
film <- read.csv("https://raw.githubusercontent.com/ivanceras/sakila/master/csv-sakila-db/film.csv")
film_category <- read.csv("https://raw.githubusercontent.com/ivanceras/sakila/master/csv-sakila-db/film_category.csv")
category <- read.csv("https://raw.githubusercontent.com/ivanceras/sakila/master/csv-sakila-db/category.csv")

# Copy data.frames to database
dbWriteTable(con, "film", film)
dbWriteTable(con, "film_category", film_category)
dbWriteTable(con, "category", category)
```

When you write a new chunk, remember to replace the r with sql,
connection=con. Some of these questions will reqruire you to use an
inner join. Read more about them here
<https://www.w3schools.com/sql/sql_join_inner.asp>

# Question 1

How many many movies is there avaliable in each rating catagory.

``` sql
select rating, count(*) as N
from film
group by rating
order by count(*) desc
```

<div class="knitsql-table">

| rating |   N |
|:-------|----:|
| PG-13  | 223 |
| NC-17  | 210 |
| R      | 195 |
| PG     | 194 |
| G      | 180 |

5 records

</div>

# Question 2

What is the average replacement cost and rental rate for each rating
category.

``` sql
select rating, avg(replacement_cost) as replacement_cost_AVG, avg(rental_rate) as rental_rate_AVG
from film
group by rating
```

<div class="knitsql-table">

| rating | replacement\_cost\_AVG | rental\_rate\_AVG |
|:-------|-----------------------:|------------------:|
| G      |               20.12333 |          2.912222 |
| NC-17  |               20.13762 |          2.970952 |
| PG     |               18.95907 |          3.051856 |
| PG-13  |               20.40256 |          3.034843 |
| R      |               20.23103 |          2.938718 |

5 records

</div>

# Question 3

Use table film\_category together with film to find the how many films
there are with each category ID

``` sql
select a.category_id, count(*) as N
from film_category as a
left join film as b on a.film_id = b.film_id
group by a.category_id
```

<div class="knitsql-table">

| category\_id |   N |
|:-------------|----:|
| 1            |  64 |
| 2            |  66 |
| 3            |  60 |
| 4            |  57 |
| 5            |  58 |
| 6            |  68 |
| 7            |  62 |
| 8            |  69 |
| 9            |  73 |
| 10           |  61 |

Displaying records 1 - 10

</div>

# Question 4

Incorporate table category into the answer to the previous question to
find the name of the most popular category.

``` sql
select a.category_id, c.name, count(*) as N
from film_category as a
left join film as b on a.film_id = b.film_id
left join category as c on a.category_id = c.category_id
group by a.category_id, c.name
order by count(*) desc
limit 1
```

<div class="knitsql-table">

| category\_id | name   |   N |
|-------------:|:-------|----:|
|           15 | Sports |  74 |

1 records

</div>
