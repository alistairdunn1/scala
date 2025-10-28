test_that("get_default_lw_params returns correct structure", {
  lw_params <- get_default_lw_params()

  expect_type(lw_params, "list")
  expect_named(lw_params, c("male", "female", "unsexed"))

  # Check each sex has correct structure
  for (sex in names(lw_params)) {
    expect_type(lw_params[[sex]], "double")
    expect_named(lw_params[[sex]], c("a", "b"))
    expect_true(lw_params[[sex]]["a"] > 0)
    expect_true(lw_params[[sex]]["b"] > 0)
  }

  # Check reasonable values for toothfish
  expect_true(lw_params$male["b"] > 2.5 && lw_params$male["b"] < 3.5)
  expect_true(lw_params$female["b"] > 2.5 && lw_params$female["b"] < 3.5)
  expect_true(lw_params$unsexed["b"] > 2.5 && lw_params$unsexed["b"] < 3.5)
})

test_that("get_abundance_lw_params returns different values", {
  lw_params <- get_abundance_lw_params()
  default_params <- get_default_lw_params()

  expect_type(lw_params, "list")
  expect_named(lw_params, c("male", "female", "unsexed"))

  # Should be different from default params
  expect_false(identical(lw_params, default_params))

  # Check structure is the same
  for (sex in names(lw_params)) {
    expect_type(lw_params[[sex]], "double")
    expect_named(lw_params[[sex]], c("a", "b"))
    expect_true(lw_params[[sex]]["a"] > 0)
    # For abundance calculation, b should be exactly 0
    expect_equal(lw_params[[sex]]["b"], 0, check.names = FALSE)
  }
})
