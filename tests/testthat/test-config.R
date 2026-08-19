test_that("the shipped config loads", {
  config <- ftp_load_config()

  expect_named(config$app)
  expect_false(config$is_customised)
  expect_equal(config$app$app_name, "Field Trial Planner")
  expect_equal(config$app$min_error_df, 15)
})

test_that("every schema vocabulary exists in lists.yml", {
  config <- ftp_load_config()

  for (section in ftp_schema()) {
    for (field in section$fields) {
      if (is.null(field$choices)) next
      expect_true(
        length(ftp_choices(config, field$choices)) > 0,
        info = paste0(section$key, "$", field$name, " -> ", field$choices)
      )
    }
  }
})

test_that("a local file replaces only the keys it restates", {
  dir <- withr::local_tempdir()
  dir.create(file.path(dir, "local"))
  file.copy(list.files(ftp_config_dir(), pattern = "[.]yml$", full.names = TRUE), dir)

  writeLines(c("app_name: Trial Design Planner",
               "min_error_df: 20"),
             file.path(dir, "local", "app.yml"))

  config <- ftp_load_config(dir)
  expect_true(config$is_customised)
  expect_equal(config$app$app_name, "Trial Design Planner")
  expect_equal(config$app$min_error_df, 20)
  # Untouched keys fall through to the shipped defaults.
  expect_true(nzchar(config$app$privacy_note))
  expect_true(nzchar(config$app$tagline))
})

test_that("nested keys merge rather than replacing their whole branch", {
  dir <- withr::local_tempdir()
  dir.create(file.path(dir, "local"))
  file.copy(list.files(ftp_config_dir(), pattern = "[.]yml$", full.names = TRUE), dir)

  writeLines(c("weights:", "  multi_factor: 5"),
             file.path(dir, "local", "complexity.yml"))

  config <- ftp_load_config(dir)
  expect_equal(config$complexity$weights$multi_factor, 5)
  # Sibling weights survive, as do sibling branches.
  expect_equal(config$complexity$weights$interaction, 1)
  expect_length(config$complexity$bands, 3)
})

test_that("guidance lookups tolerate a trimmed file", {
  config <- ftp_load_config()

  expect_true(nzchar(ftp_guidance(config, "factors.what_is_a_factor")))
  expect_equal(ftp_guidance(config, "factors.no_such_key"), "")
  expect_equal(ftp_guidance(config, "nothing.at.all"), "")
})

test_that("every question template combination is defined", {
  config <- ftp_load_config()
  templates <- config$lists$question_templates

  for (effect in ftp_choices(config, "effect_type")) {
    for (answer in ftp_choices(config, "answer_type")) {
      expect_true(
        !is.null(templates[[effect]][[answer]]),
        info = paste(effect, "/", answer)
      )
    }
  }
})

test_that("an unknown vocabulary warns rather than failing silently", {
  config <- ftp_load_config()

  expect_warning(result <- ftp_choices(config, "not_a_vocabulary"), "not_a_vocabulary")
  expect_length(result, 0)
  expect_null(ftp_choices(config, NULL))
})
