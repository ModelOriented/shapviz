# Helpers in sv_dependence()

test_that(".is_discrete() works", {
  expect_equal(.is_discrete(1:3), FALSE)
  expect_equal(.is_discrete(10:9, n_unique = 2), TRUE)
  expect_equal(.is_discrete(c(TRUE, FALSE), n_unique = 3L), TRUE)
  expect_equal(.is_discrete(c("a", "b"), n_unique = 3L), TRUE)
  expect_equal(.is_discrete(factor(c("a", "b")), n_unique = 3L), TRUE)
})

test_that(".general_range() works", {
  expect_equal(.general_range(3:1), c(1L, 3L))
  expect_equal(.general_range(c(TRUE, FALSE, NA)), c(FALSE, TRUE))
  expect_equal(.general_range(c("b", "a")), c("a", "b"))
  expect_equal(.general_range(factor(c("a", "b", "a"))), factor(c("a", "b")))
})

test_that(".all_identical() works", {
  expect_true(.all_identical(list(1, 1, 1)))
  expect_true(.all_identical(c(1, 1, 1)))
  expect_false(.all_identical(list(1, 2, 1)))
  expect_false(.all_identical(c(1, 2, 1)))
  expect_true(.all_identical(list(NULL, NULL)))
  expect_false(.all_identical(list(NULL, 1, 2)))
  expect_true(.all_identical(list(NA, NA)))
  expect_true(.all_identical(list("a", "a", "a")))
  expect_false(.all_identical(list("a", "a", "b")))
  expect_true(.all_identical(list(factor("a"), factor("a"))))
  expect_false(.all_identical(list(factor("a"), factor("a", levels = c("b", "a")))))
})
