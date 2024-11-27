test_that("empty call keeps rows with all NA", {
  df <- tibble(x = c(1, 2, NA, NA), y = c("a", NA, "b", NA))
  res <- keep_na(df)
  expect_true(all(vapply(res, \(x) all(is.na(x)), TRUE)))
})

test_that("tidyselection that selects no columns doesn't drop any rows (#1227)", {
  df <- tibble(x = c(1, 2, NA), y = c("a", NA, "b"))
  expect_identical(keep_na(df, starts_with("foo")), df)
})

test_that("specifying (a) variables considers only that variable(s)", {
  df <- tibble(x = c(1, 2, NA, NA), y = c("a", NA, "b", NA))

  exp <- tibble(x = c(NA_real_, NA_real_), y = c("b", NA))
  res <- keep_na(df, x)
  expect_identical(res, exp)

  exp <- tibble(x = c(NA_real_), y = c(NA_character_))
  res <- keep_na(df, x:y)
  expect_identical(res, exp)
})

test_that("groups are preserved", {
  df <- tibble(g = c("A", "A", "B"), x = c(1, 2, NA), y = c("a", NA, "b"))
  exp <- tibble(g = c("A"), x = c(2), y = c(NA_character_))

  gdf <- dplyr::group_by(df, "g")
  gexp <- dplyr::group_by(exp, "g")

  res <- keep_na(gdf, y)

  expect_identical(res, gexp)
  expect_identical(dplyr::group_vars(res), dplyr::group_vars(gexp))
})

test_that("errors are raised", {
  df <- tibble(x = c(1, 2, NA), y = c("a", NA, "b"))
  expect_snapshot(error = TRUE, {
    keep_na(df, list())
  })
  expect_snapshot(error = TRUE, {
    keep_na(df, "z")
  })
})

test_that("single variable data.frame doesn't lose dimension", {
  df <- data.frame(x = c(1, 2, NA))
  res <- keep_na(df, "x")
  exp <- data.frame(x = NA_real_)
  expect_identical(res, exp)
})

test_that("works with list-cols", {
  df <- tibble(x = list(1L, NULL, 3L), y = c(1L, 2L, NA))
  rs <- keep_na(df, y)

  expect_identical(rs, tibble(x = list(3L), y = NA_integer_))
})

test_that("doesn't drop empty atomic elements of list-cols (#1228)", {
  df <- tibble(x = list(1L, NULL, integer()))
  expect_identical(keep_na(df), df[c(2), ])
})

test_that("preserves attributes", {
  df <- tibble(x = structure(c(1, NA), attr = "!"))
  rs <- keep_na(df)

  expect_identical(rs$x, structure(NA_real_, attr = "!"))
})

test_that("works with df-cols", {
  # if any packed row contains a missing value, it is incomplete
  df <- tibble(a = tibble(x = c(1, 1, NA, NA), y = c(1, NA, 1, NA)))
  expect_identical(keep_na(df, a), tibble(a = tibble(x = NA_real_, y = NA_real_)))
})

test_that("works with rcrd cols", {
  # if any rcrd field contains a missing value, it is incomplete
  col <- new_rcrd(list(x = c(1, 1, NA, NA), y = c(1, NA, 1, NA)))
  df <- tibble(col = col)

  expect_identical(
    keep_na(df, col),
    tibble(col = new_rcrd(list(x = NA_real_, y = NA_real_)))
  )
})
