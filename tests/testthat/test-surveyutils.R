test_that("generate correlation R code", {
  set.seed(5653)
  data <- data.frame(x = rnorm(100),
                     y = rnorm(100),
                     z = rnorm(100),
                     w = rnorm(100),
                     q = rnorm(100))

  # Generate R code for the data.
  code <- generate_corr_code(data, method = "cor", target_variable="tmp_var")

  # Run the generate code.
  eval(parse(text = code))

  # All variables should be the same (when rounding to the same number of digits).
  total_true <- sum(round(as.data.frame(cor(data)), 5) == round(as.data.frame(tmp_var), 5))

  # There should be 25 identical values.
  expect_equal(25, total_true)

  # Same thing again, but with covariance.
  code <- generate_corr_code(data, method = "cov", target_variable="tmp_var", early_line_break = TRUE)
  eval(parse(text = code))
  total_true <- sum(round(as.data.frame(cov(data)), 5) == round(as.data.frame(tmp_var), 5))
  expect_equal(25, total_true)

  # Same thing again, but with correlation matrix input.
  cor_mat <- cor(data)
  code <- generate_corr_code(cor_mat, target_variable="tmp_var", early_line_break = TRUE)
  eval(parse(text = code))
  total_true <- sum(round(as.data.frame(cor(data)), 5) == round(as.data.frame(tmp_var), 5))
  expect_equal(25, total_true)
})


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
