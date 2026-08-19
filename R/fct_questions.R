# Question builder -------------------------------------------------------------
#
# A research question is assembled from things the user has already entered: a
# response, an effect type, the factors involved, and the kind of answer wanted.
# Writing the sentence back to them is the point - it makes an abstract set of
# dropdowns land as something they can agree or disagree with.

#' Render one research question sentence
#'
#' @param effect_type,answer_type Values from the corresponding vocabularies.
#' @param factors_involved Factor name, or names, as entered.
#' @param response_name The response variable name.
#' @return The question sentence, or NA if the combination has no template.
ftp_question_text <- function(effect_type, answer_type, factors_involved,
                              response_name, config) {
  if (any(is.na(c(effect_type, answer_type, factors_involved, response_name)))) {
    return(NA_character_)
  }
  template <- config$lists$question_templates[[effect_type]][[answer_type]]
  if (is.null(template)) return(NA_character_)

  out <- gsub("{factors}", factors_involved, template, fixed = TRUE)
  gsub("{response}", tolower(response_name), out, fixed = TRUE)
}

#' Fill in question_text for every question in a plan
#'
#' Generated rather than stored: if a user changes the effect type, the sentence
#' follows. The text is written into the state (and so into the export) because
#' the sentence is the part a reader of the plan actually needs.
ftp_fill_question_text <- function(state, config) {
  q <- state$questions
  if (nrow(q) == 0) return(state)

  response_name <- state$responses$response_name[
    match(q$response_id, state$responses$response_id)
  ]

  q$question_text <- vapply(seq_len(nrow(q)), function(i) {
    ftp_question_text(q$effect_type[i], q$answer_type[i], q$factors_involved[i],
                      response_name[i], config)
  }, character(1))

  state$questions <- q
  state
}

#' Identify which declared factors a "factors involved" entry refers to
#'
#' Splitting on " and " is not safe: a legitimate factor may be called
#' "Variety and herbicide combination". So declared names are matched whole,
#' longest first, and consumed from the string. Whatever survives is text that
#' names no declared factor.
#'
#' @return A list with `matched` (declared factor names referred to) and
#'   `leftover` (residual text, empty when everything was accounted for).
ftp_match_factor_names <- function(x, declared) {
  empty <- list(matched = character(0), leftover = "")
  if (is.na(x) || !nzchar(trimws(x))) return(empty)

  declared <- declared[!is.na(declared) & nzchar(declared)]
  declared <- declared[order(nchar(declared), decreasing = TRUE)]

  remaining <- tolower(x)
  matched <- character(0)
  for (d in declared) {
    if (grepl(tolower(d), remaining, fixed = TRUE)) {
      matched <- c(matched, d)
      remaining <- sub(tolower(d), " ", remaining, fixed = TRUE)
    }
  }

  # Connectives are expected residue; anything else names something undeclared.
  leftover <- gsub("\\band\\b|\\bwith\\b|[,;&]", " ", remaining)
  leftover <- trimws(gsub("\\s+", " ", leftover))

  list(matched = matched, leftover = leftover)
}

#' How many factors an effect type implies
ftp_expected_factor_count <- function(effect_type) {
  switch(effect_type %||% "",
    "Main effect"           = 1L,
    "Two-way interaction"   = 2L,
    "Three-way interaction" = 3L,
    NA_integer_
  )
}

#' Questions the described design cannot answer
#'
#' The design can only estimate effects involving factors that were declared. A
#' question referring to anything else is not a wording problem: it is a sign the
#' design cannot answer what is being asked, which is worth surfacing early
#' rather than at analysis time.
#'
#' @return The offending question rows with a `problem` column explaining each.
ftp_unestimable_questions <- function(state) {
  q <- state$questions
  out <- q[0, , drop = FALSE]
  out$problem <- character(0)
  if (nrow(q) == 0) return(out)

  problem <- vapply(seq_len(nrow(q)), function(i) {
    declared <- state$factors$factor_name[state$factors$trial_id %in% q$trial_id[i]]
    hit <- ftp_match_factor_names(q$factors_involved[i], declared)

    if (length(hit$matched) == 0) {
      return(paste0("No declared factor of trial ", q$trial_id[i],
                    " matches \"", q$factors_involved[i], "\"."))
    }
    if (nzchar(hit$leftover)) {
      return(paste0("\"", hit$leftover, "\" is not a factor declared for trial ",
                    q$trial_id[i], "."))
    }
    expected <- ftp_expected_factor_count(q$effect_type[i])
    if (!is.na(expected) && length(hit$matched) != expected) {
      return(paste0(q$effect_type[i], " involves ", expected,
                    " factor(s), but ", length(hit$matched), " named."))
    }
    ""
  }, character(1))

  bad <- nzchar(problem)
  out <- q[bad, , drop = FALSE]
  out$problem <- problem[bad]
  out
}
