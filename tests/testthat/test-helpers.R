test_that(".is_discrete() works", {
  expect_equal(.is_discrete(1:3), FALSE)
  expect_equal(.is_discrete(10:9, n_unique = 2), TRUE)
  expect_equal(.is_discrete(c(TRUE, FALSE), n_unique = 3L), TRUE)
  expect_equal(.is_discrete(c("a", "b"), n_unique = 3L), TRUE)
  expect_equal(.is_discrete(factor(c("a", "b")), n_unique = 3L), TRUE)
})

test_that(".all_identical() works", {
  expect_true(.all_identical(list(1, 1, 1)))
  expect_true(.all_identical(c(1, 1, 1)))
  expect_false(.all_identical(list(1, 2, 1)))
  expect_false(.all_identical(c(1, 2, 1)))
  expect_true(.all_identical(list(NULL, NULL)))
  expect_true(.all_identical(list(NULL, 1, 1)))
  expect_false(.all_identical(list(NULL, 1, 1), ignore_null = FALSE))
  expect_false(.all_identical(list(NULL, 1, 2)))
  expect_true(.all_identical(list(NA, NA)))
  expect_true(.all_identical(list("a", "a", "a")))
  expect_false(.all_identical(list("a", "a", "b")))
  expect_true(.all_identical(list(factor("a"), factor("a"))))
  expect_false(.all_identical(list(factor("a"), factor("a", levels = c("b", "a")))))
})

test_that(".collect_xy() works", {
  expect_equal(.collect_xy(list(x = TRUE, y = TRUE)), "collect")
  expect_equal(.collect_xy(list(x = TRUE, y = FALSE)), "collect_x")
  expect_equal(.collect_xy(list(x = FALSE, y = TRUE)), "collect_y")
  expect_equal(.collect_xy(list(x = FALSE, y = FALSE)), "keep")
})

test_that(".collect() works", {
  set.seed(1)

  p1 <- ggplot2::ggplot(
    iris, ggplot2::aes(x = Sepal.Length, y = Sepal.Width, color = Species)
  ) +
    ggplot2::geom_point()
  p2 <- ggplot2::ggplot(
    iris, ggplot2::aes(x = Sepal.Length, y = Sepal.Width, color = Petal.Width)
  ) +
    ggplot2::geom_jitter()
  expect_equal(
    .collect(list(p1, p2)),
    list(axis_titles = "collect", axes = "keep", guides = "keep")
  )

  p2 <- ggplot2::ggplot(iris, ggplot2::aes(x = Sepal.Length, y = Sepal.Width)) +
    ggplot2::geom_point()
  expect_equal(
    .collect(list(p1, p2)),
    list(axis_titles = "collect", axes = "collect", guides = "collect")
  )

  p2 <- ggplot2::ggplot(iris, ggplot2::aes(x = Petal.Length, y = Sepal.Width)) +
    ggplot2::geom_point()
  expect_equal(
    .collect(list(p1, p2)),
    list(axis_titles = "collect_y", axes = "collect_y", guides = "collect")
  )

  p2 <- ggplot2::ggplot(iris, ggplot2::aes(x = Sepal.Length, y = Petal.Width)) +
    ggplot2::geom_point()
  expect_equal(
    .collect(list(p1, p2)),
    list(axis_titles = "collect_x", axes = "collect_x", guides = "collect")
  )
})
