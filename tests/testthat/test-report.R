test_that("download filenames are derived from the project title", {
  config <- ftp_load_config()
  state <- ftp_example_state()

  name <- ftp_export_filename(state, config, "csv")
  expect_match(name, "^optimising-nitrogen-and-sowing-strategies")
  expect_match(name, "-[0-9]{8}[.]csv$")
})

test_that("an untitled plan falls back to the configured basename", {
  config <- ftp_load_config()
  state <- ftp_new_state()

  expect_match(ftp_export_filename(state, config, "csv"), "^field_trial_plan-")
})

test_that("punctuation in a title does not leak into the filename", {
  config <- ftp_load_config()
  state <- ftp_new_state()
  state$project$project_title <- "Nitrogen & sowing: WA trial (2026)"

  name <- ftp_export_filename(state, config, "html")
  expect_equal(name, paste0("nitrogen-sowing-wa-trial-2026-",
                            format(Sys.Date(), "%Y%m%d"), ".html"))
})

test_that("the summary renders the whole plan", {
  skip_if_not(ftp_ensure_pandoc(), "pandoc not available")

  config <- ftp_load_config()
  out <- withr::local_tempfile(fileext = ".html")
  ftp_render_summary(ftp_example_state(), config, out)

  html <- paste(readLines(out, warn = FALSE), collapse = "\n")

  expect_true(file.size(out) > 5000)
  # Both trials, their treatment structure, and the generated questions.
  expect_match(html, "Trial E1")
  expect_match(html, "Trial E2")
  expect_match(html, "120 kg N/ha")
  expect_match(html, "What level of Nitrogen rate maximises grain yield")
  # The under-replication note reaches the reader.
  expect_match(html, "residual degrees of freedom")
  # Styling is inlined rather than linked, so the file travels on its own.
  expect_match(html, "at-a-glance")
  expect_false(grepl('<link[^>]+href="summary.css"', html))
})

test_that("an empty plan still renders rather than erroring", {
  skip_if_not(ftp_ensure_pandoc(), "pandoc not available")

  config <- ftp_load_config()
  out <- withr::local_tempfile(fileext = ".html")
  expect_no_error(ftp_render_summary(ftp_new_state(), config, out))

  html <- paste(readLines(out, warn = FALSE), collapse = "\n")
  expect_match(html, "Untitled project")
})
