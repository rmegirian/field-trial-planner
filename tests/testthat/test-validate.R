test_that("an empty plan asks for trials rather than erroring", {
  config <- ftp_load_config()
  issues <- ftp_validate(ftp_new_state(), config)

  expect_gt(nrow(issues), 0L)
  expect_true(any(grepl("No trials", issues$message)))
  expect_false(any(issues$severity == "problem"))
})

test_that("the worked example has no contradictions", {
  config <- ftp_load_config()
  issues <- ftp_validate(ftp_fill_question_text(ftp_example_state(), config), config)

  problems <- issues[issues$severity == "problem", ]
  expect_equal(nrow(problems), 0L)

  # It does carry notes: both nitrogen sites are under the residual df target.
  expect_true(any(issues$severity == "note"))
  expect_true(any(grepl("residual degrees of freedom", issues$message)))
})

test_that("a declared level count that does not match the levels is caught", {
  config <- ftp_load_config()
  state <- ftp_example_state()
  state$factors$n_levels[1] <- 7L

  issues <- ftp_validate(state, config)
  hit <- issues[issues$section == "factors", ]
  expect_equal(nrow(hit), 1L)
  expect_match(hit$message, "declared 7 level\\(s\\), described 5")
})

test_that("a level pointing at a factor that does not exist is a problem", {
  config <- ftp_load_config()
  state <- ftp_example_state()
  state$levels$factor_id[1] <- "E9_F9"

  issues <- ftp_validate(state, config)
  hit <- issues[issues$severity == "problem", ]
  expect_true(any(grepl("does not exist", hit$message)))
})

test_that("duplicate identifiers are a problem", {
  config <- ftp_load_config()
  state <- ftp_example_state()
  state$trials$trial_id[2] <- "E1"

  issues <- ftp_validate(state, config)
  expect_true(any(issues$severity == "problem" &
                    grepl("used more than once", issues$message)))
})

test_that("a trial with no responses is flagged", {
  config <- ftp_load_config()
  state <- ftp_example_state()
  state$responses <- state$responses[state$responses$trial_id != "E2", ]

  issues <- ftp_validate(state, config)
  hit <- issues[issues$section == "responses", ]
  expect_equal(hit$entity_id, "E2")
  expect_match(hit$message, "nothing to analyse")
})

test_that("under-replication is a note with a concrete remedy", {
  config <- ftp_load_config()
  issues <- ftp_validate(ftp_example_state(), config)

  hit <- issues[issues$section == "implementations", ]
  expect_true(all(hit$severity == "note"))
  expect_match(hit$message[1], "5 replicates would reach it")
})

test_that("issues are ordered most severe first", {
  config <- ftp_load_config()
  state <- ftp_example_state()
  state$trials$trial_id[2] <- "E1"
  state$factors$n_levels[1] <- 7L

  issues <- ftp_validate(state, config)
  expect_equal(as.character(issues$severity[1]), "problem")
  expect_false(is.unsorted(as.integer(issues$severity)))
})

test_that("the summary line reads sensibly in both states", {
  config <- ftp_load_config()

  expect_equal(ftp_validation_summary(ftp_no_issues()), "No inconsistencies found.")
  expect_match(ftp_validation_summary(ftp_validate(ftp_example_state(), config)),
               "note\\(s\\)")
})
