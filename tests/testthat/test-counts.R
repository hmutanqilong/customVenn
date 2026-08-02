test_that("venn_region_counts cleans inputs and counts two-set regions", {
  x <- list(
    A = c("a", "b", "b", NA, "c"),
    B = c("b", "c", "d", NA)
  )

  counts <- venn_region_counts(x)

  expect_equal(counts$count[match("A_only", counts$region)], 1)
  expect_equal(counts$count[match("B_only", counts$region)], 1)
  expect_equal(counts$count[match("A&B", counts$region)], 2)
  expect_equal(sum(counts$count), 4)
  expect_equal(counts$prop[match("A&B", counts$region)], 2 / 4)
})

test_that("venn_region_counts counts three-set regions", {
  x <- list(
    A = letters[1:5],
    B = letters[4:8],
    C = letters[c(1, 4, 6, 9)]
  )

  counts <- venn_region_counts(x)

  expected <- c(
    A_only = 2,
    B_only = 2,
    C_only = 1,
    "A&B" = 1,
    "A&C" = 1,
    "B&C" = 1,
    "A&B&C" = 1
  )

  expect_equal(setNames(counts$count, counts$region)[names(expected)], expected)
  expect_equal(sum(counts$count), length(Reduce(union, x)))
})

test_that("venn_region_counts validates input shape", {
  expect_error(venn_region_counts(list(A = 1:3)), "2 or 3")
  expect_error(venn_region_counts(list(1:3, 2:4)), "named")
  expect_error(venn_region_counts(list(A = character(), B = 1:3)), "empty")
})
