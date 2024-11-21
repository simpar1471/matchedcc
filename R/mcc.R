# matchedcc: Stata-like matched case-control analysis
# Copyright (C) 2024 Simon Parker
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

#' Stata-like analysis of unstratified matched-case control data
#'
#' @description
#' Using data from vectors, data from a 2x2 contingency table, or individual
#' cell counts, `mcc()` and `mcci()` will calculate McNemar's \eqn{\chi^{2}};
#' point estimates and confidence intervals for the difference, ratio, and
#' relative difference of proportion of pairs with the exposure; and the odds
#' ratio with a confidence interval.
#' @param cases,controls Numeric vectors of the same length, with values of `0`
#'   (unexposed) and `1` (exposed). The default for these variables is `NULL`,
#'   and an error will be thrown if you attempt to provide these parameters as
#'   well as `table`. If provided, these variables are used to construct a 2x2
#'   matrix in the same format as `table`.
#' @param table A 2x2 integerish (see `checkmate::check_integerish()`) matrix
#'   with matched case-control data. The default value of `table` is `NULL`, and
#'   an error will be thrown if you provide `table` as well as `cases` and
#'   `controls`.
#'
#'   The table should have the following format, where each cell represents a
#'   pair of a matched case and control:
#'
#'   | **Cases**     | **Controls** |           |
#'   |---------------|--------------|-----------|
#'   |               | Exposed      | Unexposed |
#'   | Exposed       | a            | b         |
#'   | Unexposed     | c            | d         |
#' @param conf_level Numeric scalar from `0.1` to `0.9999`. Controls level at
#'   which to calculate confidence intervals. Default = `0.95` (95% confidence
#'   intervals).
#' @returns A named list with 5 elements:
#' \describe{
#'   \item{`data`}{A 3x3 matrix generated using the data provided, formatted for
#'     matched case-control analysis and with row/column totals.}
#'   \item{`mcnemar_chi2`}{Results from analysing the matched case-control data
#'     with `mcnemar.test()`, *without* Yates' continuity correction.}
#'   \item{`mcnemar_exact_p`}{Result of an exact test of \eqn{{H}_{0}}:
#'     \eqn{OR = 1}, calculated using the binomial distribution.}
#'   \item{`proportions`}{A two-element numeric vector with the proportion of
#'     of cases and controls with the exposure.}
#'   \item{`statistics`}{A 4 row, 3 column numeric matrix with point estimates
#'     and confidence intervals for the ratio, difference, and relative
#'     difference in the proportion of cases/controls with the exposure, and
#'     the odds ratio.}
#' }
#' @references
#' Exact Chi-squared statistic:<br>
#' McNemar, Q. (1947) *Note on the sampling error of the difference between
#' correlated proportions or percentages* **Psychometrika** 12(2): 153–157.
#' \doi{10.1007/bf02295996}
#'
#' Other steps:<br>
#' Agresti, A. (2013) *Categorical Data Analysis* 3rd ed. Hoboken, NJ: Wiley.
#' pp. 414-417.
#' @examples
#' data <- matchedcc::mccxmpl
#' mcc(cases = data$case, controls = data$control)
#'
#' # Convert data into 2x2 table
#' data$case_fctr <- factor(data$case, levels = c(1, 0),
#'                          labels = c("6+ cups", "0 cups"))
#' data$control_fctr <- factor(data$control, levels = c(1, 0),
#'                             labels = c("6+ cups", "0 cups"))
#' mcc(table = table(data$control_fctr, data$case_fctr))
#'
#' # Alternatively, provide cell counts to `mcci()`
#' table <- table(data$control_fctr, data$case_fctr)
#' mcci(a = table[1,1],
#'      b = table[1,2],
#'      c = table[2,1],
#'      d = table[2,2])
#' @rdname mcc
#' @export
mcc <- function(cases = NULL,
                controls = NULL,
                table = NULL,
                conf_level = 0.95) {
  cases_null <- is.null(cases)
  controls_null <- is.null(controls)
  table_null <- is.null(table)

  if (all(cases_null, controls_null, table_null)) {
    cli::cli_abort(
      c("Error:",
        "!" = paste0("All of {.var cases}, {.var controls}, and {.var table} ",
                     "were {.var NULL}."),
        "i" = paste0("You must provide data for either {.var cases} and ",
                     "{.var controls}, or {.var table}.")),
      class = "matchedcc_all_data_null"
    )
  }

  if (!cases_null & !controls_null) {
    if (!is.null(table)) {
      cli::cli_abort(
        c("Error:",
          "!" = paste0("Data was provided for {.var cases}/{.var controls} as ",
                       "well as {.var table}."),
          "i" = paste0("You must provide data for either {.var cases} and ",
                       "{.var controls}, or {.var table}.")),
        class = "matchedcc_too_much_input"
      )
    }
    validate_mcc_cases_controls(cases, controls)
    table <- table(controls, cases)
  }

  validate_mcc_table(table)
  validate_conf_level(conf_level)
  mcci_internal(a = table[1,1], b = table[1,2], c = table[2,1], d = table[2,2],
                conf_level = conf_level)
}

#' @param a,b,c,d Single integerish values with cell counts that correspond to
#'   a 2x2 table of matched case control data.
#' @rdname mcc
#' @export
mcci <- function(a,
                 b,
                 c,
                 d,
                 conf_level = 0.95) {
  checkmate::assert_integerish(a, lower = 0, len = 1)
  checkmate::assert_integerish(b, lower = 0, len = 1)
  checkmate::assert_integerish(c, lower = 0, len = 1)
  checkmate::assert_integerish(d, lower = 0, len = 1)
  validate_conf_level(conf_level)
  mcci_internal(a, b, c, d, conf_level)
}

# Matched case-control internal function ---------------------------------------
#' @noRd
mcci_internal <- function(a, b, c, d, conf_level = 0.95) {
  cells <- mcc_cells(a, b, c, d)
  mcc_data <- mcc_data(cells)
  mcc_table <- mcc_data[1:2, 1:2]

  list(
    data = mcc_data(cells),
    mcnemar_chi2 = stats::mcnemar.test(mcc_table, correct = FALSE),
    mcnemar_exact_p = mcc_mcnemar_exact(cells),
    proportions = mcc_proportions(cells),
    statistics = mcc_statistics(cells, conf_level)
  )
}

# Matched case-control internal helpers ----------------------------------------

#' @noRd
mcc_cells <- function(a, b, c, d) {
  a <- as.integer(a)
  b <- as.integer(b)
  c <- as.integer(c)
  d <- as.integer(d)
  list(a = a, b = b, c = c, d = d,
       m1 = a + b, m0 = c + d,
       n1 = a + c, n0 = b + d,
       t = sum(c(a, b, c, d))) |>
    lapply(FUN = as.double)
}

mcc_data <- function(cells) {
  abcd <- vapply(X = 1:4,
                 FUN = \(idx) cells[[idx]],
                 FUN.VALUE = double(length = 1L))
  Total <- sum
  matrix(data = abcd, nrow = 2, byrow = TRUE,
                dimnames = list("Cases" = c("Unexposed", "Exposed"),
                                "Controls" = c("Unexposed", "Exposed"))) |>
    stats::addmargins(FUN = Total, quiet = TRUE)
}

#' @noRd
mcc_statistics <- function(cells, conf_level) {
  statistics <- list(
    diff = mcc_diff(cells, conf_level = conf_level),
    ratio = mcc_ratio(cells, conf_level = conf_level),
    rel_diff = mcc_rel_diff(cells, conf_level = conf_level),
    or = mcc_oddsratio(cells, conf_level = conf_level)
  ) |> do.call(what = "rbind")
  rownames(statistics) <- c("difference", "ratio", "rel. diff.", "odds ratio")
  names(dimnames(statistics))[[1]] <- "statistic"
  names(dimnames(statistics))[[2]] <- paste0(
    "estimate [", round(conf_level * 100, 2), "% CI]"
  )
  statistics
}

# Matched case-control analysis statistical functions --------------------------

#' @noRd
mcc_mcnemar_exact <- function(cells) {
  min_discrepant <- with(cells, min(c(b, c)))
  tot_discrepant <- with(cells, b + c)
  pval_exact <- if (min_discrepant == tot_discrepant / 2) {
     1
  } else {
    suppressMessages(suppressWarnings(
      2 * (1 - stats::pbinom(min_discrepant, size = tot_discrepant, prob = .5,
                             lower.tail = FALSE))
    ))
  }
  if (is.na(pval_exact)) {
    cli::cli_inform(message = c(
      "!" = "Too many obs for exact test; asymptotic result should hold"
    ))
  }
  c("Exact McNemar significance probability" = pval_exact)
}

#' @noRd
mcc_proportions <- function(cells) {
  p1 <- with(cells, n1 / t)
  p2 <- with(cells, m1 / t)
  matrix(c(p2, p1), nrow = 1,
         dimnames = list("", "Proportion with factor" = c("Cases", "Controls")))
}

#' @noRd
mcc_diff <- function(cells, conf_level = 0.95) {
  p1 <- with(cells, n1 / t)
  p2 <- with(cells, m1 / t)
  diff <- p2 - p1

  diff_se <- with(cells, sqrt((a + d) * (b + c) + 4 * b * c) / t^(3/2))
  crit <- stats::qnorm(1 - (1 - conf_level) / 2)
  diff_lower <- diff - crit * diff_se - 1 / cells$t
  diff_upper <- diff + crit * diff_se + 1 / cells$t

  output_matrix(diff, diff_lower, diff_upper, "difference", conf_level)
}

#' @noRd
mcc_ratio <- function(cells, conf_level = 0.95) {
  p1 <- with(cells, n1 / t)
  p2 <- with(cells, m1 / t)
  ratio <- p2 / p1

  ratio_se <- with(cells, sqrt((b + c) / (m1 * n1)))
  crit <- stats::qnorm(1 - (1 - conf_level) / 2)
  ratio_lower <- exp(log(ratio) - crit * ratio_se)
  ratio_upper <- exp(log(ratio) + crit * ratio_se)

  output_matrix(ratio, ratio_lower, ratio_upper, "ratio", conf_level)
}

#' @noRd
mcc_rel_diff <- function(cells, conf_level = 0.95) {
  rel_diff <- with(cells, (b - c) / (b + d))

  rel_diff_se <- with(
    cells,
    (b + d)^(-2) * sqrt((b + c + d) * (b * c + b * d + c * d) - b * c * d)
  )
  crit <- stats::qnorm(1 - (1 - conf_level) / 2)
  rel_diff_lower <- rel_diff - crit * rel_diff_se
  rel_diff_upper <- rel_diff + crit * rel_diff_se

  output_matrix(rel_diff, rel_diff_lower, rel_diff_upper, "rel. diff.",
                conf_level)
}

#' @noRd
mcc_oddsratio <- function(cells, conf_level = 0.95) {
  oddsratio <- with(cells, b / c)
  or_ci <- mcc_oddsratio_ci(cells, conf_level)
  out <- output_matrix(oddsratio, min(or_ci), max(or_ci), "odds ratio",
                       conf_level)
  out
}

#' @noRd
mcc_oddsratio_ci <- function(cells, conf_level) {
  clopper_pearson_ci <- with(
    cells,
    binom::binom.confint(x = b, n = b + c,
                         conf.level = conf_level,
                         methods = "exact")
  )
  with(
    clopper_pearson_ci, c(lower / (1 - lower), upper / (1 - upper))
  )
}

# Output helper ----------------------------------------------------------------

#' @noRd
output_matrix <- function(estimate, lower, upper, statistic, conf_level) {
  varname <- paste0(statistic, " (", round(conf_level * 100, 2), "% CI)")
  mat <- matrix(c(estimate, lower, upper), nrow = 1,
                dimnames = list("", temp = c("estimate", "lower", "upper")))
  names(dimnames(mat))[[2]] <- varname
  mat
}

# Parameter validation helpers -------------------------------------------------

#' @noRd
validate_mcc_cases_controls <- function(cases, controls) {
  if (length(cases) != length(controls)) {
    cli::cli_abort(
      message = c("",
                  "!" = paste0("{.var cases} and {.var controls} must have ",
                               "the same length."),
                  "i" = paste0("{.var cases} had {length(cases)} elements."),
                  "i" = paste0("{.var controls} had {length(controls)} ",
                               "elements.")),
      class = "matchedcc_cases_controls_diff_lengths"
    )
  }
  checkmate::assert_integerish(cases, min.len = 1, lower = 0, upper = 1,
                               any.missing = FALSE)
  checkmate::assert_integerish(controls, min.len = 1, lower = 0, upper = 1,
                               any.missing = FALSE)
}

#' @noRd
validate_mcc_table <- function(table) {
  checkmate::assert_matrix(table, mode = "numeric", nrows = 2, ncols = 2)
  checkmate::assert_integerish(table, lower = 0)
}

#' @noRd
validate_conf_level <- function(conf_level) {
  checkmate::assert_numeric(conf_level, lower = 0.1, upper = 0.9999)
}