# Write Stata output to .log file for use in unit tests ------------------------

# The code does NOT work unless being run in interactive mode, as
# `capture.output()` captures more than just the RStata call.

if (interactive()) {
  if (!"RStata" %in% installed.packages()) {
    devtools::install_cran("RStata")
  }

  options("RStata.StataPath" = "\"C:\\Program Files\\Stata18\\StataSE-64\"")
  options("RStata.StataVersion" = 18)

  n_commands <- 1000
  commands <- withr::with_seed(2800, {
    idxs <- 1:4
    samples_vec <- 1L:1000L
    conf_level_vec <- seq(10, 99.99, 0.01)
    purrr::map_chr(
      .x = seq_len(n_commands),
      .f = \(x) {
        samples <- vapply(
          idxs,
          FUN = \(x) sample(samples_vec, size = 1, replace = FALSE),
          FUN.VALUE = integer(length = 1)
        )
        conf_level <-  sample(conf_level_vec, size = 1, replace = FALSE)
        glue::glue(
          "mcci {paste(samples, collapse = \" \")}, level({conf_level})"
        )
      })
  })

  stata_testers <- utils::capture.output(
    RStata::stata(commands, stata.echo = TRUE)
  )
  readr::write_lines(stata_testers, file = "tests/testdata/stata-output.log")
}