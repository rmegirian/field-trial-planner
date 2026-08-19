test_that("question sentences are built from the user's selections", {
  config <- ftp_load_config()

  expect_equal(
    ftp_question_text("Main effect", "Optimum or dose-response",
                      "Nitrogen rate", "Grain yield", config),
    "What level of Nitrogen rate maximises grain yield?"
  )
  expect_equal(
    ftp_question_text("Main effect", "Difference between levels",
                      "Sowing depth", "Established plants", config),
    "Does Sowing depth affect established plants?"
  )
  expect_equal(
    ftp_question_text("Two-way interaction", "Difference between levels",
                      "Sowing depth and variety", "Grain yield", config),
    "Does the interaction between Sowing depth and variety affect grain yield?"
  )
})

test_that("an incomplete question yields no sentence rather than a broken one", {
  config <- ftp_load_config()

  expect_true(is.na(ftp_question_text(NA, "Threshold", "N rate", "Yield", config)))
  expect_true(is.na(ftp_question_text("Main effect", NA, "N rate", "Yield", config)))
  expect_true(is.na(ftp_question_text("Nonsense", "Threshold", "N rate", "Yield", config)))
})

test_that("filling in question text populates every question in the example", {
  config <- ftp_load_config()
  state <- ftp_fill_question_text(ftp_example_state(), config)

  expect_false(any(is.na(state$questions$question_text)))
  expect_match(state$questions$question_text[1], "^What level of Nitrogen rate")
  expect_match(state$questions$question_text[2], "cross the target threshold")
})

test_that("a factor name containing 'and' is matched whole, not split", {
  declared <- c("Sowing depth", "Variety and herbicide combination")
  hit <- ftp_match_factor_names("Sowing depth and variety and herbicide combination",
                                declared)

  expect_setequal(hit$matched, declared)
  expect_equal(hit$leftover, "")
})

test_that("text naming an undeclared factor is left over", {
  hit <- ftp_match_factor_names("Sowing depth and row spacing", "Sowing depth")

  expect_equal(hit$matched, "Sowing depth")
  expect_equal(hit$leftover, "row spacing")
})

test_that("the worked example contains no unestimable questions", {
  expect_equal(nrow(ftp_unestimable_questions(ftp_example_state())), 0L)
})

test_that("a question about an undeclared factor is flagged with a reason", {
  state <- ftp_example_state()
  state$questions$factors_involved[1] <- "Row spacing"

  bad <- ftp_unestimable_questions(state)
  expect_equal(nrow(bad), 1L)
  expect_match(bad$problem, "No declared factor")
})

test_that("an interaction naming only one factor is flagged", {
  state <- ftp_example_state()
  state$questions$effect_type[3] <- "Two-way interaction"  # names Sowing depth only

  bad <- ftp_unestimable_questions(state)
  expect_equal(nrow(bad), 1L)
  expect_match(bad$problem, "involves 2 factor\\(s\\), but 1 named")
})
