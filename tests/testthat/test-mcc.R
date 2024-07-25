data <- matchedcc::mccxmpl
data$case_fctr <- factor(data$case, levels = c(1, 0),
                         labels = c("6+ cups", "0 cups"))
data$control_fctr <- factor(data$control, levels = c(1, 0),
                            labels = c("6+ cups", "0 cups"))

# Successful `mcc`/`mcci` calls ------------------------------------------------

test_that("`mcc` and `mcci` accept the same data in multiple formats", {
  mcc_from_vctrs <- mcc(cases = data$case, controls = data$control)
  mcc_from_table <- mcc(table = table(data$case_fctr, data$control_fctr,
                                      dnn = c("Cases", "Controls")))
  mcc_from_cells <- with(data, {
    mcci(a = sum(control_fctr == "0 cups" & case_fctr == "0 cups"),
         b = sum(control_fctr == "0 cups" & case_fctr == "6+ cups"),
         c = sum(control_fctr == "6+ cups" & case_fctr == "0 cups"),
         d = sum(control_fctr == "6+ cups" & case_fctr == "6+ cups"))
  })

  testthat::expect_equal(mcc_from_vctrs, mcc_from_table)
  testthat::expect_equal(mcc_from_cells, mcc_from_table)
})

# Unsuccessful `mcc`/`mcci` calls ----------------------------------------------

## Error reason: wrong data types ----------------------------------------------
test_that("`mcc` and `mcci` fail with incorrect data types", {
  testthat::expect_error(
    mcc(cases = data$case, controls = data$control, conf_level = "0.95")
  )

  testthat::expect_error(
    mcci(8, 8, 3, 8, conf_level = "0.95")
  )

  testthat::expect_error(
    mcci("8", 8, 3, 8, conf_level = 0.95)
  )
})

## Error reason: right types; bad values ---------------------------------------

test_that("`mcc` and `mcci` fail with bad data values", {
  # Add 0.5 to make `control` non-integerish
  testthat::expect_error(
    mcc(cases = data$case, controls = data$control + 0.5, conf_level = 0.95)
  )
  # Include a non-integerish value in `mcci()` call
  testthat::expect_error(
    mcci(100, 200, 200, 203.1, conf_level = 0.95)
  )


  # Confidence level expected to be >= 0.1 and <1
  testthat::expect_error(
    mcc(cases = data$case, controls = data$control, conf_level = 0.05)
  )
  testthat::expect_error(
    # Just below 0.1
    mcc(cases = data$case, controls = data$control, conf_level = 0.0999)
  )
  testthat::expect_error(
    # Just above 0.9999
    mcc(cases = data$case, controls = data$control, conf_level = 0.99999)
  )
  testthat::expect_error(
    mcc(cases = data$case, controls = data$control, conf_level = 1.25)
  )
})

## Error reason: Vector length issues ------------------------------------------

test_that("`mcc` fails if vector lengths are wrong", {
  # Length should be >= 1
  testthat::expect_error(
    mcc(cases = integer(), controls = integer())
  )
  # Lengths should be equal
  testthat::expect_error(
    mcc(cases = data$case[1:5], controls = data$control[1:6]),
    class = "matchedcc_cases_controls_diff_lengths"
  )

  # Inputs to `mcci()` should be scalars
  testthat::expect_error(
    mcci(8, 8, 3, c(8, 1))
  )
})

## Error reason: All inputs are `NULL` -----------------------------------------
test_that("`mcc` fails if vector + table inputs are `NULL`", {
  testthat::expect_error(
    mcc(cases = NULL, controls = NULL, table = NULL),
    class = "matchedcc_all_data_null"
  )
})

## Error reason: Giving vector + table input -----------------------------------
test_that("`mcc` fails if given both vector + table inputs", {
  testthat::expect_error(
    mcc(cases = data$case, controls = data$control,
        table = table(data$case_fctr, data$control_fctr)),
    class = "matchedcc_too_much_input"
  )
})

# Coverage: case where no. of obs is 1/2 total discrepant pairs ----------------
test_that("`mcci` uses 1 for McNemar exact p if b = 1/2 * (b + c)", {
  results <- mcci(2, 5, 5, 9)
  testthat::expect_equivalent(results$mcnemar_exact_p, expected = 1)
})

# Directly compare R vs. Stata matched case-control output ---------------------
test_that(desc = "Test R implementation against Stata outputs", {
  stata_testers <- readr::read_lines(file = "../testdata/stata-output.log")
  stata_testers_split <- split(stata_testers,
                               ceiling(seq_along(stata_testers) / 22))

  test_r_stata_mcc_equivalence <- function(stata_output) {
    results_r <- get_matchedcc_equivalent(stata_output)
    results_stata <- list(
      data = NULL,
      mcnemar_chi2 = get_stata_mcnemar_results(stata_output),
      mcnemar_exact_p = get_stata_mcnemar_exact(stata_output),
      proportions = get_stata_proportions(stata_output),
      statistics = get_stata_statistics(stata_output)
    )

    expect_equal(
      results_r$mcnemar_chi2, results_stata$mcnemar_chi2, tolerance = 1e-2
    )
    expect_equal(
      results_r$mcnemar_exact_p, results_stata$mcnemar_exact_p, tolerance = 1e-4
    )
    expect_equal(
      results_r$proportions, results_stata$proportions, tolerance = 1e-5
    )
    expect_equal(
      results_r$statistics, results_stata$statistics, tolerance = 1e-5
    )
  }

  purrr::walk(
    .x = stata_testers_split,
    .f = test_r_stata_mcc_equivalence
  )
})
