# Workflow steps ---------------------------------------------------------------
#
# The steps a user moves through, in the order they are presented. Order is a
# convenience for working through a complex task, not a dependency chain: any
# step can be revisited at any time, and later answers regularly send people
# back to earlier ones.
#
# The sidebar, the navigation and the progress markers are all generated from
# this list, so adding a step means adding an entry here and a module.

ftp_steps <- function() {
  list(
    list(id = "welcome",        label = "Welcome",            number = NA),
    # The id stays "project" because it is the section name in exported plan
    # files; only the label shown to a user changes. This step now covers the
    # project objective and the trials that serve it, so there is no separate
    # trials step.
    list(id = "project",        label = "Research Overview",  number = 1),
    list(id = "factors",        label = "Treatment factors",  number = 2),
    list(id = "replication",    label = "Replication",        number = 3),
    list(id = "responses",      label = "Measurements",       number = 4),
    list(id = "questions",      label = "Question confirmation", number = 5),
    list(id = "implementation", label = "Implementation",     number = 6),
    list(id = "summary",        label = "Summary",            number = 7)
  )
}

ftp_step_ids <- function() vapply(ftp_steps(), function(s) s$id, character(1))

ftp_step <- function(id) {
  hit <- Filter(function(s) s$id == id, ftp_steps())
  if (length(hit) == 0) NULL else hit[[1]]
}

#' The step before or after a given one, or NULL at either end
ftp_step_neighbour <- function(id, offset) {
  ids <- ftp_step_ids()
  i <- match(id, ids) + offset
  if (is.na(i) || i < 1 || i > length(ids)) return(NULL)
  ftp_step(ids[i])
}

#' How much of a step has been filled in
#'
#' Three states rather than a percentage: a plan is not a progress bar, and
#' implying it can be "100% done" works against revisiting. "empty" means
#' untouched, "started" means something is there, "ready" means the step has
#' what the next ones need from it.
ftp_step_status <- function(state, id) {
  filled <- function(x) length(x) > 0 && !all(is.na(x)) && any(nzchar(trimws(as.character(x[!is.na(x)]))))

  switch(
    id,
    welcome = "ready",
    project = {
      have <- c(state$project$project_title, state$project$project_objective)
      if (!filled(have)) "empty"
      else if (all(!is.na(have) & nzchar(trimws(have)))) "ready" else "started"
    },
    trials = ftp_rows_status(state$trials, "trial_aim"),
    factors = ftp_rows_status(state$factors, "factor_name"),
    replication = if (nrow(state$factors) > 0) "ready" else "empty",
    responses = ftp_rows_status(state$responses, "response_name"),
    questions = ftp_rows_status(state$questions, "answer_type"),
    implementation = ftp_rows_status(state$implementations, "n_reps"),
    summary = if (nrow(state$trials) > 0) "ready" else "empty",
    "empty"
  )
}

#' Status for a table-backed step: empty, started, or ready
ftp_rows_status <- function(tbl, key_field) {
  if (nrow(tbl) == 0) return("empty")
  key <- tbl[[key_field]]
  if (all(is.na(key))) "started" else "ready"
}
