test_that("convert scales", {
  expect_equal(convert_scale(6, 7, 100)$new_value, 85.714)
  expect_equal(convert_scale(6, 7, 100, digits=5)$new_value, 85.71429)
  expect_equal(convert_scale(10, 100, 7)$new_value, 0.7)

  converted <- convert_scale(26, 100, 7)
  expect_equal(converted$new_value, 1.82)
  expect_equal(converted$old_value, 26)
  expect_equal(converted$from_scale, 100)
  expect_equal(converted$to_scale, 7)
})


test_that("coefplot", {
  set.seed(5653)
  df1 <- data.frame(x = rnorm(1000), y = rnorm(1000), z = rnorm(1000))
  model1 <- lm(y ~ x, df1)
  df2 <- data.frame(x = rnorm(1000), y = rnorm(1000), z = rnorm(1000))
  model2 <- lm(y ~ x, df2)
  expect_type(coefplot_compare(model1, model2), "list")
})


test_that("confidence_interval", {
  expect_equal(round(confidence_interval(100, 15, 1000)$upper, 4), 100.9297)
  expect_equal(round(confidence_interval(m=100, s=15, n=1000)$upper, 4), 100.9297)
})
