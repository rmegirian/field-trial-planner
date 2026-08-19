test_that("residual degrees of freedom follow the RCBD formula", {
  expect_equal(ftp_error_df(5L, 4L), 12)
  expect_equal(ftp_error_df(2L, 3L), 2)
  expect_true(is.na(ftp_error_df(1L, 4L)))
  expect_true(is.na(ftp_error_df(5L, 1L)))
  expect_true(is.na(ftp_error_df(NA_integer_, 4L)))
})

test_that("the calculator reports how many replicates would meet the target", {
  # 5 treatments: (5-1)(r-1) >= 15 needs r >= 4.75, so 5 replicates.
  expect_equal(ftp_reps_for_df(5L, 15), 5L)
  expect_equal(ftp_error_df(5L, ftp_reps_for_df(5L, 15)) >= 15, TRUE)

  # 3 treatments needs more replication to reach the same target.
  expect_equal(ftp_reps_for_df(3L, 15), 9L)
})

test_that("treatments are the product of level counts across factors", {
  state <- ftp_example_state()
  expect_equal(ftp_n_treatments(state, "E1"), 5L)
  expect_equal(ftp_n_treatments(state, "E2"), 12L)
})

test_that("a trial with no factors yet reports NA rather than 1", {
  state <- ftp_new_state()
  state$trials <- tibble::tibble(trial_id = "T1", trial_name = "x",
                                 trial_aim = "y", initial_questions = NA_character_)
  expect_true(is.na(ftp_n_treatments(state, "T1")))
})

test_that("per-implementation metrics flag the under-replicated site", {
  config <- ftp_load_config()
  im <- ftp_implementation_metrics(ftp_example_state(), "E1", config$app$min_error_df)

  expect_equal(nrow(im), 2L)
  expect_equal(im$n_plots, c(20L, 15L))
  expect_equal(im$error_df, c(12, 8))
  # Both sit below the default target of 15.
  expect_false(any(im$meets_df_target))
  expect_equal(unique(im$reps_needed), 5L)
})

test_that("site count is inferred from implementations within a year", {
  # Two implementations in one season can only be two sites.
  expect_equal(ftp_n_sites(data.frame(year = c(2025L, 2025L))), 2L)
  # One site revisited reads as one site, which is the intended inference.
  expect_equal(ftp_n_sites(data.frame(year = c(2025L, 2026L))), 1L)
  # Three sites over two seasons, unevenly.
  expect_equal(ftp_n_sites(data.frame(year = c(2025L, 2025L, 2025L, 2026L))), 3L)
  expect_equal(ftp_n_sites(data.frame(year = integer(0))), 0L)
  # Years not yet recorded: fall back to the row count rather than reporting none.
  expect_equal(ftp_n_sites(data.frame(year = c(NA_integer_, NA_integer_))), 2L)
})

test_that("flags describe the structure of the example trials", {
  config <- ftp_load_config()

  e1 <- ftp_trial_flags(ftp_example_state(), "E1", config)
  expect_false(e1$multi_factor)
  expect_false(e1$interaction)
  expect_true(e1$multi_site)
  expect_true(e1$advanced_claim)          # optimum and threshold
  expect_true(e1$unbalanced_replication)  # 4 replicates at one site, 3 at the other
  expect_false(e1$non_standard_response)

  e2 <- ftp_trial_flags(ftp_example_state(), "E2", config)
  expect_true(e2$multi_factor)
  expect_true(e2$interaction)
  expect_true(e2$restricted_layout)       # split-plot
  expect_true(e2$non_standard_response)   # count data
  expect_false(e2$multi_site)
})

test_that("the profile summarises each trial", {
  config <- ftp_load_config()
  profile <- ftp_profile(ftp_example_state(), config)

  expect_equal(names(profile), c("E1", "E2"))
  expect_equal(profile$E1$n_treatments, 5L)
  expect_equal(profile$E1$total_plots, 35L)   # 20 + 15
  expect_equal(profile$E2$n_treatments, 12L)
  expect_equal(profile$E2$total_plots, 48L)   # 12 treatments x 4 replicates
  expect_true(profile$E2$score >= profile$E1$score)
  expect_true(all(profile$E2$drivers %in% names(profile$E2$flags)))
})

test_that("scoring bands come from config and cover every score", {
  config <- ftp_load_config()
  flags_none <- lapply(ftp_trial_flags(ftp_new_state(), "none", config), function(x) FALSE)

  expect_equal(ftp_trial_score(flags_none, config)$score, 0)
  expect_equal(ftp_trial_score(flags_none, config)$band, "Straightforward")

  flags_all <- lapply(flags_none, function(x) TRUE)
  expect_gt(ftp_trial_score(flags_all, config)$score, 5)
  expect_equal(ftp_trial_score(flags_all, config)$band, "Involved")
})
