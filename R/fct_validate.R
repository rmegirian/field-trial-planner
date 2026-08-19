# Validation -------------------------------------------------------------------
#
# Checks that run across sections, because that is where the mistakes are. A
# section on its own usually looks fine; it is the relationships between them
# that go wrong - a question about a factor nobody declared, a factor with six
# levels and two of them described, a trial that is never implemented anywhere.
#
# Severity is deliberately three-valued. "problem" means the plan contradicts
# itself. "check" means something that is probably wrong but might be
# intentional. "note" means a design feature worth a conversation, not a defect.

FTP_SEVERITIES <- c("problem", "check", "note")

ftp_issue <- function(severity, section, entity_id, message) {
  tibble::tibble(
    severity  = factor(severity, levels = FTP_SEVERITIES),
    section   = section,
    entity_id = as.character(entity_id),
    message   = message
  )
}

ftp_no_issues <- function() ftp_issue(character(0), character(0), character(0), character(0))

#' Validate a whole plan
#'
#' @return A tibble of issues, most severe first. Zero rows means the plan is
#'   internally consistent - which is not the same as the design being a good
#'   one, and the summary says so.
ftp_validate <- function(state, config, schema = ftp_schema()) {
  issues <- list(
    ftp_check_required(state, schema),
    ftp_check_duplicate_ids(state, schema),
    ftp_check_orphans(state, schema),
    ftp_check_level_counts(state),
    ftp_check_completeness(state),
    ftp_check_questions(state),
    ftp_check_replication(state, config)
  )
  out <- do.call(rbind, issues)
  if (is.null(out) || nrow(out) == 0) return(ftp_no_issues())
  out[order(out$severity), , drop = FALSE]
}

#' Required fields that were left blank
ftp_check_required <- function(state, schema) {
  out <- lapply(names(schema), function(key) {
    section <- schema[[key]]
    tbl <- state[[key]]
    if (nrow(tbl) == 0) return(NULL)

    required <- Filter(function(f) f$required, section$fields)
    per_field <- lapply(required, function(f) {
      blank <- is.na(tbl[[f$name]])
      if (!any(blank)) return(NULL)
      ids <- if (is.null(section$id_field)) rep(NA_character_, nrow(tbl)) else tbl[[section$id_field]]
      ftp_issue("check", key, ids[blank], paste0(f$label, " is blank."))
    })
    do.call(rbind, per_field)
  })
  do.call(rbind, out)
}

#' Two records sharing an identifier
ftp_check_duplicate_ids <- function(state, schema) {
  out <- lapply(names(schema), function(key) {
    section <- schema[[key]]
    if (is.null(section$id_field) || nrow(state[[key]]) == 0) return(NULL)
    ids <- state[[key]][[section$id_field]]
    dup <- unique(ids[duplicated(ids) & !is.na(ids)])
    if (length(dup) == 0) return(NULL)
    ftp_issue("problem", key, dup,
              paste0("Identifier is used more than once, so these records cannot be told apart."))
  })
  do.call(rbind, out)
}

#' Records pointing at a parent that does not exist
ftp_check_orphans <- function(state, schema) {
  out <- lapply(names(schema), function(key) {
    section <- schema[[key]]
    if (is.null(section$parent_field) || nrow(state[[key]]) == 0) return(NULL)

    parent_tbl <- state[[section$parent_section]]
    parent_ids <- parent_tbl[[schema[[section$parent_section]]$id_field]]
    child_parent <- state[[key]][[section$parent_field]]

    missing <- !is.na(child_parent) & !(child_parent %in% parent_ids)
    if (!any(missing)) return(NULL)

    ftp_issue("problem", key, state[[key]][[section$id_field]][missing],
              paste0("Refers to ", section$parent_field, " \"",
                     child_parent[missing], "\", which does not exist."))
  })

  # Questions link to a response as well as to a trial.
  q <- state$questions
  if (nrow(q) > 0) {
    missing <- !is.na(q$response_id) & !(q$response_id %in% state$responses$response_id)
    if (any(missing)) {
      out <- c(out, list(ftp_issue(
        "problem", "questions", q$question_id[missing],
        paste0("Refers to response \"", q$response_id[missing], "\", which does not exist.")
      )))
    }
  }
  do.call(rbind, out)
}

#' Factors whose declared level count does not match the levels described
#'
#' The equivalent of the workbook's "Declared 6, described 2" check, and the one
#' that catches the most real errors.
ftp_check_level_counts <- function(state) {
  f <- state$factors
  if (nrow(f) == 0) return(NULL)

  described <- vapply(f$factor_id, function(id) {
    lv <- state$levels[state$levels$factor_id %in% id, , drop = FALSE]
    sum(!is.na(lv$level_desc) & nzchar(trimws(lv$level_desc)))
  }, integer(1))

  mismatch <- !is.na(f$n_levels) & described != f$n_levels
  if (!any(mismatch)) return(NULL)

  ftp_issue("check", "factors", f$factor_id[mismatch],
            paste0(f$factor_name[mismatch], ": declared ", f$n_levels[mismatch],
                   " level(s), described ", described[mismatch], "."))
}

#' Trials missing a whole section
ftp_check_completeness <- function(state) {
  ids <- state$trials$trial_id
  ids <- ids[!is.na(ids)]
  if (length(ids) == 0) {
    return(ftp_issue("check", "trials", NA, "No trials have been described yet."))
  }

  wanted <- list(
    factors         = "no treatment factors, so there is nothing to compare",
    responses       = "no response variables, so there is nothing to analyse",
    questions       = "no research questions, so it is unclear what the trial must answer",
    implementations = "no sites or years, so the amount of replication is unknown"
  )

  out <- lapply(names(wanted), function(key) {
    empty <- ids[!(ids %in% state[[key]]$trial_id)]
    if (length(empty) == 0) return(NULL)
    ftp_issue("check", key, empty, paste0("Trial has ", wanted[[key]], "."))
  })
  do.call(rbind, out)
}

#' Questions the design cannot answer
ftp_check_questions <- function(state) {
  bad <- ftp_unestimable_questions(state)
  if (nrow(bad) == 0) return(NULL)
  ftp_issue("problem", "questions", bad$question_id,
            paste0(bad$problem,
                   " The design can only estimate effects involving factors it varies."))
}

#' Replication that leaves little residual information
ftp_check_replication <- function(state, config) {
  ids <- state$trials$trial_id
  ids <- ids[!is.na(ids)]
  target <- config$app$min_error_df

  out <- lapply(ids, function(id) {
    im <- ftp_implementation_metrics(state, id, target)
    if (nrow(im) == 0) return(NULL)

    short <- im[!is.na(im$error_df) & !im$meets_df_target, , drop = FALSE]
    if (nrow(short) == 0) return(NULL)

    ftp_issue("note", "implementations", short$impl_id,
              paste0(short$n_treatments, " treatments at ", short$n_reps,
                     " replicates leaves ", short$error_df,
                     " residual degrees of freedom, below the ", target,
                     " commonly aimed for. ", short$reps_needed,
                     " replicates would reach it."))
  })
  do.call(rbind, out)
}

#' One-line summary of a validation result, for the UI header
ftp_validation_summary <- function(issues) {
  if (nrow(issues) == 0) return("No inconsistencies found.")
  counts <- table(factor(as.character(issues$severity), levels = FTP_SEVERITIES))
  parts <- c(
    if (counts[["problem"]] > 0) paste(counts[["problem"]], "problem(s)"),
    if (counts[["check"]] > 0)   paste(counts[["check"]], "to check"),
    if (counts[["note"]] > 0)    paste(counts[["note"]], "note(s)")
  )
  paste(parts, collapse = ", ")
}
