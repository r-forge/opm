library(testthat)
context("Testing the coding functions of the pkgutils package")


################################################################################


## assert
test_that("we can make assertions", {

  x <- 1:10

  got <- assert(cond = x > 5L, quiet = TRUE)
  expect_equal(got, "assertion 'x > 5L' failed in 5 of 10 cases")
  expect_error(assert(cond = x > 5L, quiet = FALSE))

  got <- assert(cond = function(x) x > 2L, orig = x, quiet = TRUE)
  expect_is(got, "character")
  expect_equal(length(got), 2L)
  expect_error(assert(function(x) x > 2L, orig = x, quiet = FALSE))

  got <- assert(cond = x > 5L, msg = "not larger than 5", quiet = TRUE)
  expect_equal(got, "not larger than 5 in 5 of 10 cases")
  expect_error(assert(cond = x > 5L, msg = "not larger than 5", quiet = FALSE))

  got <- assert(cond = x > 5L, msg = "%s is not larger than 5", orig = x,
    quiet = TRUE)
  expect_is(got, "character")
  expect_equal(length(got), 5L)
  expect_error(assert(cond = x > 5L, msg = "%s is not larger than 5", orig = x,
    quiet = FALSE))

  cond <- x > 5L
  expect_warning(got <- assert(cond = cond, msg = "%s is not larger than 5",
    orig = x, quiet = NA))
  expect_equal(got, cond)

})


## case
test_that("case works as expected", {
  expect_equal(case(0, "a", "b"), "a")
  expect_equal(case(1, "a", "b"), "b")
  expect_equal(case(10, "a", "b"), "b")
  expect_equal(case("a", a = "a", b = "b"), "a")
  expect_equal(case("b", a = "a", b = "b"), "b")
  expect_error(case("c", a = "a", b = "b"))
  expect_error(case(NA_character_, a = "a", b = "b"))
})

## must
test_that("must works as expected", {
  expect_error(must(warning("abc")))
})

## L
## UNTESTED

## LL
test_that("LL works as expected", {
  x <- 3
  y <- 9:10
  z <- 'a'
  expect_equal(c("x", "z"), LL(x, z))
  expect_error(LL(x, y))
  expect_error(LL(x, y, .wanted = 2L))
  expect_error(LL(y, z, .wanted = 2L))
  expect_equal("y", LL(y, .wanted = 2L))
})

## listing
## UNTESTED

## flatten
## UNTESTED

## unnest
## UNTESTED

## collect
test_that("we can collect rows", {
  x <- list(data.frame(A = 1:3, B = letters[1:3]),
    data.frame(B = letters[4:6], A = 4:6))
  got <- collect(x = x, what = "rows", dataframe = TRUE)
  expect_is(got, "data.frame")
  expect_equal(dim(got), c(6L, 2L))
  expect_equal(got[, "A"], 1:6)
})


## map_values
test_that("values in character vectors can be mapped", {
  map <- c(a = '1', b = '2', c = '3')
  x <- c("d", "c", "b", "a", "A")
  names(x) <- LETTERS[1L:5L]
  exp <- c("d", "3", "2", "1", "A")
  names(exp) <- names(x)
  got <- map_values(x, map)
  expect_equal(exp, got)
  map.2 <- as.character(1L:3L) # no names => all mappings unsuccessful
  got <- map_values(x, map.2)
  expect_equal(x, got)
})


## map_values
test_that("values in logical vectors can be mapped", {
  x <- c(i = TRUE, j = FALSE, k = NA, l = TRUE)
  got <- map_values(x, NULL)
  expect_equal(x, got)
  got <- map_values(x)
  expect_is(got, "integer")
  expect_equal(names(got), names(x))
  got <- map_values(x, LETTERS)
  expect_equal(names(got), names(x))
  expect_equivalent(got, c("C", "A", "B", "C"))
  expect_error(map_values(x, LETTERS[1:2]))
})


## map_values
test_that("values in lists can be mapped using character vectors", {

  map <- c(a = '1', b = '2', c = '3')
  x <- c("d", "c", "b", "a", "A")
  names(x) <- LETTERS[1L:5L]
  exp <- c("d", "3", "2", "1", "A")
  names(exp) <- names(x)

  xy <- list(x = x, y = 1:10)
  got <- map_values(xy, map)
  expect_is(got, "list")
  expect_equal(got[[1L]], exp)
  expect_equal(got[[2L]], 1:10)
  expect_equal(names(got), names(xy))

  got <- map_values(xy, map, coerce = "integer")
  expect_is(got, "list")
  expect_equal(got[[1L]], exp)
  expect_equal(got[[2L]], as.character(1:10))
  expect_equal(names(got), names(xy))

})


## map_values
test_that("values in lists can be mapped by cleaning", {
  x <- list(A = 13, B = list(B1 = NULL, B2 = -5), C = "z", character(),
    D = list(D1 = NULL, D2 = list()))
  got <- map_values(x, NULL)
  expect_equal(got, list(A = 13, B = list(B2 = -5), C = "z"))
  got <- map_values(x, NULL, "numeric")
  expect_equal(got, list(A = "13", B = list(B2 = "-5"), C = "z"))
})


## map_values
test_that("values in lists can be mapped using expressions", {
  x <- list(a = 1:5, b = letters[1:3], K = list(K1 = 3, 89))
  assign("z", 7.5, 1)
  # 1
  got <- map_values(x, expression(a <- a, u <- a + z))
  expect_equal(got, c(x, list(u = x$a + z)))
  # 2
  expect_error(map_values(x, expression(u <- a + z), baseenv()))
  # 3
  got <- map_values(x, expression(u <- a + z, v <- u))
  expect_equivalent(got, c(x, list(u = x$a + z, v = x$a + z)))
  # 4
  b <- 4
  got <- map_values(x, expression(rm(b)))
  x$b <- NULL
  expect_equal(got, x)
  expect_equal(b, 4)
  # 5
  got <- map_values(x, expression(b <- NULL))
  expect_equal(got, c(x, list(b = NULL)))
})


## map_names
test_that("names in lists can be mapped and received", {
  x <- list(a = 99, b = list(xx = c(a = "NA", b = "99.5", c = "10e+06")),
    c = 8, d = "Z")

  # Using a character vector
  map <- c(a = "b", b = "a", xx = "yy", c = "d", d = "e")
  got <- map_names(x, map)
  exp <- list(b = 99, a = list(yy = c(a = "NA", b = "99.5", c = "10e+06")),
    d = 8, e = "Z")
  expect_equal(got, exp)

  # Using a function
  got <- map_names(x, identity)
  expect_equal(got, x)

  # Conducting just a query
  got <- map_names(x)
  exp <- c("a", "b", "c", "d", "xx")
  names(exp) <- exp
  expect_equal(got, exp)
})

## map_names
test_that("names in lists with missing names can be mapped", {
  x <- list(a = list(1:2, 5:6), b = 3:8)
  map <- c(a = "A", b = "B")
  got <- map_names(x, map)
  expect_equal(got, list(A = list(1:2, 5:6), B = 3:8))
  got <- map_names(x, toupper)
  expect_equal(got, list(A = list(1:2, 5:6), B = 3:8))
  x <- list(list(), list())
  expect_equivalent(character(), map_names(x))
})


################################################################################


## contains
test_that("a list can be queried with a list with exact matches", {

  x <- list(a = 99, list(i = 1, j = 2), d = 1:2, c = list(99, y = 100, z = 101))

  query <- list(c = list(y = 100), d = 1:2)
  expect_true(contains(x, query, values = TRUE, exact = TRUE))
  expect_true(contains(x, query, values = FALSE, exact = TRUE))
  expect_true(contains(x, query, values = TRUE, exact = FALSE))
  expect_true(contains(x, query, values = FALSE, exact = FALSE))

  query <- list(a = 99, c = list(z = 101))
  expect_true(contains(x, query, values = TRUE, exact = TRUE))
  expect_true(contains(x, query, values = FALSE, exact = TRUE))
  expect_true(contains(x, query, values = TRUE, exact = FALSE))
  expect_true(contains(x, query, values = FALSE, exact = FALSE))

  query <- list()
  expect_true(contains(x, query, values = TRUE, exact = TRUE))
  expect_true(contains(x, query, values = FALSE, exact = TRUE))
  expect_true(contains(x, query, values = TRUE, exact = FALSE))
  expect_true(contains(x, query, values = FALSE, exact = FALSE))

})

## contains
test_that("a list can be queried with a list without matches", {

  x <- list(a = 99, list(i = 1, j = 2), d = 1:2, c = list(99, y = 100, z = 101))

  query <- list(b = 99, c = list(z = 101))
  expect_false(contains(x, query, values = TRUE, exact = TRUE))
  expect_false(contains(x, query, values = FALSE, exact = TRUE))
  expect_false(contains(x, query, values = TRUE, exact = FALSE))
  expect_false(contains(x, query, values = FALSE, exact = FALSE))

})

## contains
test_that("a list can be queried with a list with only non-exact matches", {

  x <- list(a = 99, list(i = 1, j = 2), d = 1:2, c = list(99, y = 100, z = 101))

  query <- list(c = list(y = c(100, 101)), d = 1:3)
  expect_false(contains(x, query, values = TRUE, exact = TRUE))
  expect_true(contains(x, query, values = FALSE, exact = TRUE))
  expect_true(contains(x, query, values = TRUE, exact = FALSE))
  expect_true(contains(x, query, values = FALSE, exact = FALSE))

  query <- list(c = list(y = 101), d = list(1:2))
  expect_false(contains(x, query, values = TRUE, exact = TRUE))
  expect_true(contains(x, query, values = FALSE, exact = TRUE))
  expect_false(contains(x, query, values = TRUE, exact = FALSE))
  expect_true(contains(x, query, values = FALSE, exact = FALSE))

  query <- list(a = "99", c = list(z = 101))
  expect_false(contains(x, query, values = TRUE, exact = TRUE))
  expect_true(contains(x, query, values = FALSE, exact = TRUE))
  expect_true(contains(x, query, values = TRUE, exact = FALSE))
  expect_true(contains(x, query, values = FALSE, exact = FALSE))

  query <- list(c = list(y = 100), d = list(1:2))
  expect_false(contains(x, query, values = TRUE, exact = TRUE))
  expect_true(contains(x, query, values = FALSE, exact = TRUE))
  expect_false(contains(x, query, values = TRUE, exact = FALSE))
  expect_true(contains(x, query, values = FALSE, exact = FALSE))

})

## contains
test_that("a list can be queried with a list with missing names", {

  x <- list(a = 99, list(i = 1, j = 2), d = 1:2, c = list(99, y = 100, z = 101))

  query <- list(list(i = 1, j = 2))
  expect_false(contains(x, query, values = TRUE, exact = TRUE))
  expect_false(contains(x, query, values = FALSE, exact = TRUE))
  expect_false(contains(x, query, values = TRUE, exact = FALSE))
  expect_false(contains(x, query, values = FALSE, exact = FALSE))

  query <- list(1, 2)
  expect_false(contains(x, query, values = TRUE, exact = TRUE))
  expect_false(contains(x, query, values = FALSE, exact = TRUE))
  expect_false(contains(x, query, values = TRUE, exact = FALSE))
  expect_false(contains(x, query, values = FALSE, exact = FALSE))

  query <- list(13, a = 99, 2)
  expect_false(contains(x, query, values = TRUE, exact = TRUE))
  expect_false(contains(x, query, values = FALSE, exact = TRUE))
  expect_false(contains(x, query, values = TRUE, exact = FALSE))
  expect_false(contains(x, query, values = FALSE, exact = FALSE))

})


################################################################################


## check
test_that("data-frame columns are correctly checked", {
  x <- data.frame(A = letters, B = 1:2, C = 9.3, stringsAsFactors = FALSE)
  e <- c(B = "integer", C = "numeric", A = "character")
  got <- check(x, e)
  expect_equal(length(got), 0L)
  e <- c(B = "integer", C = "numeric", D = "character", A = "character")
  got <- check(x, e)
  expect_equal(length(got), 1L)
  e <- c(B = "integer", C = "factor", A = "character")
  got <- check(x, e)
  expect_equal(length(got), 1L)
})


################################################################################


## match_parts
## UNTESTED


## set
## UNTESTED


## sql
test_that("SQL generation from formulas works", {

  # without selection arguments
  x <- sql(~ mytable)
  expect_equal(x, "SELECT * FROM mytable;")
  x <- sql(~ mytable(col1, col2))
  expect_equal(x, "SELECT col1, col2 FROM mytable;")
  x <- sql(~ mytable(`col 1`, col2))
  expect_equal(x, "SELECT \"col 1\", col2 FROM mytable;")

  # named selection arguments
  x <- sql(mytable ~ foo == NULL)
  expect_equal(x, "SELECT * FROM mytable WHERE foo IS NULL;")
  x <- sql(mytable ~ foo == NULL & bar != NULL)
  expect_equal(x,
    "SELECT * FROM mytable WHERE foo IS NULL AND bar IS NOT NULL;")
  x <- sql(mytable ~ foo & !baz)
  expect_equal(x, "SELECT * FROM mytable WHERE foo AND NOT baz;")
  x <- sql(mytable ~ !(foo & baz))
  expect_equal(x, "SELECT * FROM mytable WHERE NOT (foo AND baz);")
  x <- sql(mytable ~ foo & baz)
  x <- sql(mytable ~ if (foo > 0) bar else baz)
  expect_equal(x,
    "SELECT * FROM mytable WHERE CASE WHEN foo > 0 THEN bar ELSE baz END;")

  expect_warning(x <- sql(~ my_table(id = my_id,
    value = function(c, a = 1, ..., b = 10) NULL)))
  expect_equal(x, paste("SELECT my_id AS id, CASE WHEN c OR a THEN 1",
    "WHEN b THEN 10 ELSE NULL END AS value FROM my_table;"))

  x <- sql(~ sites(id, convert(b = name, k), coalesce(location, "here")))
  expect_equal(x,
    "SELECT id, convert(b := name, k), coalesce(location, 'here') FROM sites;")

  x <- sql(~ my_table(id = my_id, value = while (TRUE) next))
  expect_equal(x,
    "SELECT my_id AS id, while(TRUE, next()) AS value FROM my_table;")
  x <- sql(~ my_table(id = my_id, value = for (i in 1:10) print(i)))
  expect_equal(x,
    "SELECT my_id AS id, for(i, 1 : 10, print(i)) AS value FROM my_table;")

})

