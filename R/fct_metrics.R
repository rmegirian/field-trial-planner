# Design metrics ---------------------------------------------------------------
#
# What is structurally true about a described design: how many treatments, plots
# and analyses it involves, how much residual information the replication leaves,
# and which features tend to complicate design or analysis.
#
# Weighting and banding are configuration rather than code, since how much each
# feature matters depends on who is doing the work - see inst/config/complexity.yml.

#' Residual degrees of freedom for a randomised complete block design
#'
#' @param n_treatments Number of treatment combinations.
#' @param n_reps Number of replicates (blocks).
#' @return (t - 1) * (r - 1), or NA if either input is missing or below 2.
ftp_error_df <- function(n_treatments, n_reps) {
  ok <- !is.na(n_treatments) & !is.na(n_reps) & n_treatments >= 2 & n_reps >= 2
  ifelse(ok, (n_treatments - 1L) * (n_reps - 1L), NA_integer_)
}

#' Smallest number of replicates meeting a residual df target
#'
#' Useful for telling a user what would fix an under-replicated design rather
#' than only that it is under-replicated.
ftp_reps_for_df <- function(n_treatments, target_df) {
  if (is.na(n_treatments) || n_treatments < 2) return(NA_integer_)
  as.integer(ceiling(target_df / (n_treatments - 1)) + 1)
}

#' Treatment count for one trial
#'
#' The product of the level counts across the trial's factors. A factor with a
#' missing or implausible level count is skipped rather than collapsing the
#' product to NA, so a partly filled plan still reports something useful.
ftp_n_treatments <- function(state, trial_id) {
  f <- state$factors
  n <- f$n_levels[f$trial_id %in% trial_id]
  n <- n[!is.na(n) & n >= 1]
  if (length(n) == 0) return(NA_integer_)
  as.integer(prod(n))
}

#' Per-implementation metrics for one trial
#'
#' @return A data frame with one row per implementation, carrying the treatment
#'   count, plot count and residual degrees of freedom implied by its replication.
ftp_implementation_metrics <- function(state, trial_id, min_error_df = 15) {
  impl <- state$implementations
  impl <- impl[impl$trial_id %in% trial_id, , drop = FALSE]
  t_n <- ftp_n_treatments(state, trial_id)

  if (nrow(impl) == 0) {
    return(data.frame(
      impl_id = character(0), year = integer(0),
      n_reps = integer(0), n_treatments = integer(0), n_plots = integer(0),
      error_df = integer(0), meets_df_target = logical(0),
      reps_needed = integer(0), stringsAsFactors = FALSE
    ))
  }

  err_df <- ftp_error_df(t_n, impl$n_reps)
  data.frame(
    impl_id         = impl$impl_id,
    year            = impl$year,
    n_reps          = impl$n_reps,
    n_treatments    = rep(t_n, nrow(impl)),
    n_plots         = as.integer(t_n * impl$n_reps),
    error_df        = err_df,
    meets_df_target = !is.na(err_df) & err_df >= min_error_df,
    reps_needed     = rep(ftp_reps_for_df(t_n, min_error_df), nrow(impl)),
    stringsAsFactors = FALSE
  )
}

#' How many sites a trial runs at
#'
#' Sites are not labelled, so the count is inferred: the most implementations
#' recorded in any single year. Three implementations in 2025 means three sites,
#' because a trial cannot run at the same place three times in one season.
#'
#' The inference cannot tell a site revisited across years from two distinct
#' sites, which matters for the correlation structure of a multi-environment
#' analysis. That is a conversation to have rather than a field to collect.
ftp_n_sites <- function(impl) {
  years <- impl$year[!is.na(impl$year)]
  if (nrow(impl) == 0) return(0L)
  if (length(years) == 0) return(nrow(impl))
  max(as.integer(table(years)))
}

#' Complexity flags for one trial
#'
#' Each flag names a structural feature of the design. Flags are descriptive:
#' none of them means a design is wrong, only that it involves something whose
#' handling should be agreed before the trial runs rather than after.
ftp_trial_flags <- function(state, trial_id, config) {
  cx <- config$complexity
  factors <- state$factors[state$factors$trial_id %in% trial_id, , drop = FALSE]
  responses <- state$responses[state$responses$trial_id %in% trial_id, , drop = FALSE]
  questions <- state$questions[state$questions$trial_id %in% trial_id, , drop = FALSE]
  impl <- state$implementations[state$implementations$trial_id %in% trial_id, , drop = FALSE]

  reps <- impl$n_reps[!is.na(impl$n_reps)]
  im <- ftp_implementation_metrics(state, trial_id, config$app$min_error_df)

  list(
    multi_factor           = nrow(factors) > 1,
    interaction            = any(questions$effect_type %in%
                                   c("Two-way interaction", "Three-way interaction")),
    three_way_interaction  = any(questions$effect_type == "Three-way interaction", na.rm = TRUE),
    multi_site             = ftp_n_sites(impl) > 1,
    multi_year             = length(unique(impl$year[!is.na(impl$year)])) > 1,
    non_standard_response  = any(responses$data_type %in% cx$non_standard_data_types),
    repeated_measures      = any(responses$repeated_measures == "Yes", na.rm = TRUE),
    restricted_layout      = any(impl$layout %in% cx$restricted_layouts),
    advanced_claim         = any(questions$answer_type %in% cx$advanced_answer_types),
    unbalanced_replication = length(unique(reps)) > 1,
    low_error_df           = nrow(im) > 0 && any(!im$meets_df_target)
  )
}

#' Complexity score and band for one trial
ftp_trial_score <- function(flags, config) {
  weights <- config$complexity$weights
  score <- sum(vapply(names(flags), function(f) {
    if (isTRUE(flags[[f]])) as.numeric(weights[[f]] %||% 0) else 0
  }, numeric(1)))

  bands <- config$complexity$bands
  band <- bands[[length(bands)]]
  for (b in bands) {
    if (score <= b$max_score) { band <- b; break }
  }
  list(score = score, band = band$label, note = band$note)
}

#' Full profile for one trial
ftp_trial_profile <- function(state, trial_id, config) {
  trial <- state$trials[state$trials$trial_id %in% trial_id, , drop = FALSE]
  im <- ftp_implementation_metrics(state, trial_id, config$app$min_error_df)
  flags <- ftp_trial_flags(state, trial_id, config)
  scored <- ftp_trial_score(flags, config)

  list(
    trial_id        = trial_id,
    trial_name      = if (nrow(trial)) trial$trial_name[1] else NA_character_,
    n_factors       = sum(state$factors$trial_id %in% trial_id),
    n_treatments    = ftp_n_treatments(state, trial_id),
    n_responses     = sum(state$responses$trial_id %in% trial_id),
    n_questions     = sum(state$questions$trial_id %in% trial_id),
    n_implementations = nrow(im),
    n_sites         = ftp_n_sites(im),
    n_years         = length(unique(im$year[!is.na(im$year)])),
    total_plots     = if (nrow(im)) sum(im$n_plots, na.rm = TRUE) else NA_integer_,
    error_df_range  = if (nrow(im)) range(im$error_df, na.rm = TRUE) else c(NA_integer_, NA_integer_),
    implementations = im,
    flags           = flags,
    score           = scored$score,
    band            = scored$band,
    band_note       = scored$note,
    drivers         = names(Filter(isTRUE, flags))
  )
}

#' Profiles for every trial in the plan
ftp_profile <- function(state, config) {
  ids <- state$trials$trial_id
  ids <- ids[!is.na(ids)]
  stats::setNames(lapply(ids, function(id) ftp_trial_profile(state, id, config)), ids)
}

`%||%` <- function(x, y) if (is.null(x)) y else x
