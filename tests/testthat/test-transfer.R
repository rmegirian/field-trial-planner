test_that("an empty plan round trips to an empty plan", {
  state <- ftp_new_state()
  path <- withr::local_tempfile(fileext = ".csv")
  ftp_write_csv(state, path)
  back <- ftp_read_csv(path)

  expect_equal(names(back), names(state))
  expect_equal(nrow(back$trials), 0L)
  expect_equal(nrow(back$project), 1L)
})

test_that("the worked example survives a CSV round trip unchanged", {
  state <- ftp_example_state()
  path <- withr::local_tempfile(fileext = ".csv")
  ftp_write_csv(state, path)
  back <- ftp_read_csv(path)

  for (key in names(state)) {
    expect_equal(nrow(back[[key]]), nrow(state[[key]]), info = key)
    for (col in names(state[[key]])) {
      expect_equal(back[[key]][[col]], state[[key]][[col]],
                   info = paste(key, col, sep = "$"))
    }
  }
})

test_that("integer fields come back as integers, not strings", {
  state <- ftp_example_state()
  path <- withr::local_tempfile(fileext = ".csv")
  ftp_write_csv(state, path)
  back <- ftp_read_csv(path)

  expect_type(back$factors$n_levels, "integer")
  expect_type(back$implementations$year, "integer")
  expect_type(back$implementations$n_reps, "integer")
})

test_that("blank answers stay absent rather than becoming empty strings", {
  state <- ftp_example_state()
  long <- ftp_state_to_long(state)

  expect_false(any(is.na(long$value)))
  expect_false(any(trimws(long$value) == ""))

  back <- ftp_long_to_state(long)
  expect_true(is.na(back$implementations$notes[1]))
})

test_that("the export carries a schema version", {
  long <- ftp_state_to_long(ftp_example_state())
  meta <- long[long$section == "meta", ]

  expect_true("schema_version" %in% meta$field)
  expect_equal(meta$value[meta$field == "schema_version"], FTP_SCHEMA_VERSION)
})

test_that("unrecognised sections and fields are dropped with a warning", {
  long <- ftp_state_to_long(ftp_example_state())
  long <- rbind(long, tibble::tibble(
    section = "sponsorship", entity_id = "X", parent_id = NA_character_,
    field = "internal_code", value = "ABC-123"
  ))

  expect_warning(back <- ftp_long_to_state(long), "sponsorship")
  expect_equal(nrow(back$trials), 2L)
})

test_that("a file that is not a plan is rejected clearly", {
  expect_error(
    ftp_long_to_state(tibble::tibble(a = 1, b = 2)),
    "Not a Field Trial Planner file"
  )
})

test_that("trial and factor ordering survives the round trip", {
  state <- ftp_example_state()
  path <- withr::local_tempfile(fileext = ".csv")
  ftp_write_csv(state, path)
  back <- ftp_read_csv(path)

  expect_equal(back$trials$trial_id, c("E1", "E2"))
  expect_equal(back$levels$level_id, state$levels$level_id)
})
