# Worked example ---------------------------------------------------------------
#
# The two trials carried through the guidance. Loading this into an empty app
# gives someone a complete, coherent plan to read before they write their own,
# which is the fastest way to convey what a good answer looks like.
#
# It doubles as the fixture for the round-trip and metrics tests, so the example
# people learn from is the same one that is checked on every run.

ftp_example_state <- function(schema = ftp_schema()) {
  state <- ftp_new_state(schema)

  state$project <- tibble::tibble(
    project_title = "Optimising nitrogen and sowing strategies for wheat productivity",
    project_objective = paste(
      "To identify nitrogen and sowing strategies that improve wheat productivity",
      "across contrasting rainfall zones, and determine whether variety responses",
      "to sowing depth are consistent across seasons."
    )
  )

  state$trials <- tibble::tribble(
    ~trial_id, ~trial_name, ~trial_aim, ~initial_questions,
    "E1", "Nitrogen rate trial",
    paste("To determine the nitrogen rate required to maximise wheat yield and",
          "grain protein content under different seasonal conditions."),
    "How much nitrogen is needed, and does the answer change between seasons?",
    "E2", "Sowing depth by variety-herbicide trial",
    paste("To determine whether sowing depth affects wheat establishment and yield,",
          "and whether the response differs among commonly used variety and",
          "herbicide combinations."),
    "Can we sow deeper without losing establishment, and does it depend on variety?"
  )

  state$factors <- tibble::tribble(
    ~factor_id, ~trial_id, ~factor_name, ~n_levels,
    "E1_F1", "E1", "Nitrogen rate", 5L,
    "E2_F1", "E2", "Sowing depth", 4L,
    "E2_F2", "E2", "Variety and herbicide combination", 3L
  )

  state$levels <- tibble::tribble(
    ~level_id, ~factor_id, ~level_no, ~level_desc,
    "E1_F1_L1", "E1_F1", 1L, "0 kg N/ha (nil applied)",
    "E1_F1_L2", "E1_F1", 2L, "30 kg N/ha as urea, single application at GS30",
    "E1_F1_L3", "E1_F1", 3L, "60 kg N/ha as urea, single application at GS30",
    "E1_F1_L4", "E1_F1", 4L, "90 kg N/ha as urea, split 45 kg at sowing + 45 kg at GS30",
    "E1_F1_L5", "E1_F1", 5L, "120 kg N/ha as urea, split 60 kg at sowing + 60 kg at GS30",
    "E2_F1_L1", "E2_F1", 1L, "20 mm, standard press wheel setting",
    "E2_F1_L2", "E2_F1", 2L, "40 mm",
    "E2_F1_L3", "E2_F1", 3L, "60 mm",
    "E2_F1_L4", "E2_F1", 4L, "80 mm, deep sowing with modified boot",
    "E2_F2_L1", "E2_F2", 1L, "Variety A sown at 70 kg/ha with pre-emergent herbicide 1 at label rate",
    "E2_F2_L2", "E2_F2", 2L, "Variety B sown at 70 kg/ha with pre-emergent herbicide 2 at label rate",
    "E2_F2_L3", "E2_F2", 3L, "Variety C sown at 70 kg/ha with post-sowing pre-emergent herbicide 3"
  )

  state$responses <- tibble::tribble(
    ~response_id, ~trial_id, ~response_name, ~units, ~data_type,
    ~measurement_method, ~sampling_within_unit, ~repeated_measures, ~measurement_timing,
    "E1_R1", "E1", "Grain yield", "t/ha", "Continuous",
    "Plot harvester at maturity", "Whole plot harvested", "No", "Once at harvest",
    "E1_R2", "E1", "Grain protein", "%", "Continuous",
    "NIR on subsample of harvested grain", "One subsample per plot from the bulked harvest",
    "No", "Once at harvest",
    "E2_R1", "E2", "Established plants", "plants/m2", "Count",
    "Manual count", "Two 1 m2 quadrats per plot at randomly generated coordinates",
    "No", "28 days after sowing",
    "E2_R2", "E2", "Grain yield", "t/ha", "Continuous",
    "Plot harvester at maturity", "Whole plot harvested", "No", "Once at harvest"
  )

  state$questions <- tibble::tribble(
    ~question_id, ~trial_id, ~response_id, ~effect_type, ~factors_involved, ~answer_type, ~question_text,
    "E1_Q1", "E1", "E1_R1", "Main effect", "Nitrogen rate", "Optimum or dose-response", NA_character_,
    "E1_Q2", "E1", "E1_R2", "Main effect", "Nitrogen rate", "Threshold", NA_character_,
    "E2_Q1", "E2", "E2_R1", "Main effect", "Sowing depth", "Difference between levels", NA_character_,
    "E2_Q2", "E2", "E2_R2", "Two-way interaction",
    "Sowing depth and variety and herbicide combination", "Difference between levels", NA_character_
  )

  state$implementations <- tibble::tribble(
    ~impl_id, ~trial_id, ~year, ~n_reps, ~layout, ~notes,
    "E1_I1", "E1", 2025L, 4L, "Randomised complete block", NA_character_,
    "E1_I2", "E1", 2025L, 3L, "Randomised complete block",
    "Reduced to 3 replicates due to available trial area",
    "E2_I1", "E2", 2025L, 4L, "Split-plot",
    "Sowing depth as main plots. Waterlogging affected two plots in block 3"
  )

  state
}
