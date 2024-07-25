# Extract matched case-control analysis results from Stata output --------------

extract_stata_number <- function(str) {
  stringr::str_extract(str, pattern = "(\\d+|\\s)\\.\\d+") |>
    stringr::str_replace(pattern = "^\\s", replacement =  "0") |>
    as.numeric()
}

extract_statistic <- function(str, indexes) {
  stringr::str_split_1(str, pattern = "\\s+") |>
    vctrs::vec_slice(i = indexes) |>
    as.numeric()
}

get_stata_mcnemar_results <- function(stata_output) {
  stringr::str_split_1(stata_output[11], pattern = " Prob") |>
    extract_stata_number() |>
    setNames(c("McNemar's chi-squared", "p.value"))
}
get_stata_mcnemar_exact <- function(stata_output) {
  extract_stata_number(stata_output[12]) |>
    setNames("Exact McNemar significance probability")
}
get_stata_proportions <- function(stata_output) {
  matrix(c(extract_stata_number(stata_output[15]),
           extract_stata_number(stata_output[16])),
         nrow = 1,
         dimnames = list("", "Proportion with factor" = c("Cases", "Controls")))
}

get_stata_statistics <- function(stata_output) {
  li_1 <- purrr::map(.x = 18:19,
                     .f = \(x) extract_statistic(stata_output[x], indexes = 3:5))
  li_2 <- purrr::map(.x = c(20, 22),
                     .f = \(x) extract_statistic(stata_output[x], indexes = 4:6))
  stata_statistics <- append(li_1, li_2) |>
    do.call(what = "rbind")
  colnames(stata_statistics) <- c("estimate", "lower", "upper")
  rownames(stata_statistics) <- c("difference", "ratio", "rel. diff.",
                                  "odds ratio")
  names(dimnames(stata_statistics))[[1]] <- "statistic"
  conf_str <- stringr::str_extract(stata_output[16],
                                   pattern = "(?<=\\[).+(?=%)")
  names(dimnames(stata_statistics))[[2]] <- paste0(
    "estimate [", conf_str, "% CI]"
  )
  stata_statistics
}

# Get matchedcc analysis results corresponding to Stata output -----------------

get_matchedcc_equivalent <- function(stata_output) {
  mcc_ints <- stringr::str_extract_all(stata_output[1], "\\s\\d+") |>
    unlist() |>
    as.integer() |>
    as.list()
  mcc_conf <- stringr::str_extract(stata_output[1],
                                   pattern = "(?<=\\().*(?=\\))")
  mcc_conf <- as.double(mcc_conf) / 100
  mcc_output <- do.call(args = append(mcc_ints, mcc_conf), what = "mcci")
  chi2_results <- mcc_output$mcnemar_chi2
  mcc_output$mcnemar_chi2 <- c(chi2_results[[1]], "p.value" = chi2_results[[3]])
  mcc_output
}
