test_that("get_ecampus_colors returns valid hex codes", {
  result <- get_ecampus_colors("eCore_Green")
  expect_true(grepl("^#[0-9A-Fa-f]{6}$", result))
})
