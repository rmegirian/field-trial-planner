# Data model -----------------------------------------------------------------
#
# Single source of truth for the trial plan. Export, import, validation and the
# UI are all derived from these definitions, so a field is added in one place
# only.

FTP_SCHEMA_VERSION <- "1"

#' Define one field in a section
#'
#' @param name Column name in the internal table and in the exported CSV.
#' @param type One of "character", "integer", "logical".
#' @param label Human-readable label used in the UI and the rendered summary.
#' @param required Whether validation should flag this field when empty.
#' @param choices Name of a vocabulary in lists.yml, or NULL for free text.
#' @param multiline Whether the UI should render a text area rather than an input.
ftp_field <- function(name,
                      type = "character",
                      label = name,
                      required = FALSE,
                      choices = NULL,
                      multiline = FALSE) {
  list(
    name      = name,
    type      = type,
    label     = label,
    required  = required,
    choices   = choices,
    multiline = multiline
  )
}

ftp_section <- function(key, title, fields,
                        id_field = NULL,
                        parent_field = NULL,
                        parent_section = NULL,
                        single = FALSE) {
  list(
    key            = key,
    title          = title,
    fields         = fields,
    id_field       = id_field,
    parent_field   = parent_field,
    parent_section = parent_section,
    single         = single
  )
}

#' The trial plan schema
#'
#' Sections are listed in workflow order. `single = TRUE` marks a section that
#' holds exactly one record; every other section holds zero or more, keyed by
#' `id_field` and linked upward by `parent_field`.
ftp_schema <- function() {
  list(

    project = ftp_section(
      key    = "project",
      title  = "Project",
      single = TRUE,
      fields = list(
        ftp_field("project_title", label = "Project title", required = TRUE),
        ftp_field("project_objective", label = "Overall project objective",
                  required = TRUE, multiline = TRUE)
      )
    ),

    trials = ftp_section(
      key      = "trials",
      title    = "Trials",
      id_field = "trial_id",
      fields = list(
        ftp_field("trial_id", label = "Trial ID", required = TRUE),
        ftp_field("trial_name", label = "Trial name", required = TRUE),
        ftp_field("trial_aim", label = "Trial aim", required = TRUE, multiline = TRUE),
        ftp_field("initial_questions", label = "Research questions you want answered",
                  multiline = TRUE)
      )
    ),

    factors = ftp_section(
      key            = "factors",
      title          = "Treatment factors",
      id_field       = "factor_id",
      parent_field   = "trial_id",
      parent_section = "trials",
      fields = list(
        ftp_field("factor_id", label = "Factor ID", required = TRUE),
        ftp_field("trial_id", label = "Trial", required = TRUE),
        ftp_field("factor_name", label = "Factor", required = TRUE),
        ftp_field("n_levels", type = "integer", label = "Number of levels", required = TRUE)
      )
    ),

    levels = ftp_section(
      key            = "levels",
      title          = "Factor levels",
      id_field       = "level_id",
      parent_field   = "factor_id",
      parent_section = "factors",
      fields = list(
        ftp_field("level_id", label = "Level ID", required = TRUE),
        ftp_field("factor_id", label = "Factor", required = TRUE),
        ftp_field("level_no", type = "integer", label = "Level number", required = TRUE),
        ftp_field("level_desc", label = "Level", required = TRUE, multiline = TRUE)
      )
    ),

    responses = ftp_section(
      key            = "responses",
      title          = "Response variables",
      id_field       = "response_id",
      parent_field   = "trial_id",
      parent_section = "trials",
      fields = list(
        ftp_field("response_id", label = "Response ID", required = TRUE),
        ftp_field("trial_id", label = "Trial", required = TRUE),
        ftp_field("response_name", label = "Response variable", required = TRUE),
        ftp_field("units", label = "Units"),
        ftp_field("data_type", label = "Data type", required = TRUE, choices = "data_type"),
        ftp_field("measurement_method", label = "Measurement method", multiline = TRUE),
        ftp_field("sampling_within_unit", label = "Sampling within experimental unit",
                  multiline = TRUE),
        ftp_field("repeated_measures", label = "Repeated measures?", choices = "yes_no"),
        ftp_field("measurement_timing", label = "Measurement timing")
      )
    ),

    questions = ftp_section(
      key            = "questions",
      title          = "Research questions",
      id_field       = "question_id",
      parent_field   = "trial_id",
      parent_section = "trials",
      fields = list(
        ftp_field("question_id", label = "Question ID", required = TRUE),
        ftp_field("trial_id", label = "Trial", required = TRUE),
        ftp_field("response_id", label = "Response variable", required = TRUE),
        ftp_field("effect_type", label = "Effect type", required = TRUE, choices = "effect_type"),
        ftp_field("factors_involved", label = "Factor(s) involved", required = TRUE),
        ftp_field("answer_type", label = "What you want to know", required = TRUE,
                  choices = "answer_type"),
        ftp_field("question_text", label = "Research question (generated)", multiline = TRUE)
      )
    ),

    implementations = ftp_section(
      key            = "implementations",
      title          = "Implementation",
      id_field       = "impl_id",
      parent_field   = "trial_id",
      parent_section = "trials",
      fields = list(
        ftp_field("impl_id", label = "Implementation ID", required = TRUE),
        ftp_field("trial_id", label = "Trial", required = TRUE),
        # Sites are counted, not named. One row per site-by-year establishes how
        # many environments there are, and carries the replication, layout and
        # deviations that differ between them - which is what the analysis needs.
        ftp_field("year", type = "integer", label = "Year", required = TRUE),
        ftp_field("n_reps", type = "integer", label = "Number of replicates", required = TRUE),
        ftp_field("layout", label = "Experimental layout", choices = "layout"),
        ftp_field("notes", label = "Notes or deviations", multiline = TRUE)
      )
    )
  )
}

# Helpers --------------------------------------------------------------------

ftp_section_keys <- function(schema = ftp_schema()) names(schema)

ftp_field_names <- function(section) {
  vapply(section$fields, function(f) f$name, character(1))
}

ftp_field_def <- function(section, name) {
  hit <- Filter(function(f) f$name == name, section$fields)
  if (length(hit) == 0) NULL else hit[[1]]
}

ftp_field_labels <- function(section) {
  stats::setNames(
    vapply(section$fields, function(f) f$label, character(1)),
    ftp_field_names(section)
  )
}

ftp_field_types <- function(section) {
  stats::setNames(
    vapply(section$fields, function(f) f$type, character(1)),
    ftp_field_names(section)
  )
}

#' Coerce a character vector to the type declared in the schema
#'
#' Import always arrives as character (CSV), so every field passes through here
#' on the way back into the state. Blank strings become NA rather than 0 or
#' FALSE, so "not answered" survives a round trip intact.
ftp_coerce <- function(x, type) {
  x <- as.character(x)
  x[!is.na(x) & trimws(x) == ""] <- NA_character_
  switch(
    type,
    integer   = suppressWarnings(as.integer(x)),
    logical   = as.logical(toupper(x)),
    character = x,
    stop("Unsupported field type: ", type, call. = FALSE)
  )
}

#' An empty, correctly typed table for one section
ftp_empty_table <- function(section) {
  types <- ftp_field_types(section)
  cols <- lapply(types, function(ty) {
    switch(ty,
      integer   = integer(0),
      logical   = logical(0),
      character = character(0)
    )
  })
  tibble::as_tibble(cols)
}

#' A single blank row for a section, typed per the schema
ftp_blank_row <- function(section) {
  types <- ftp_field_types(section)
  cols <- lapply(types, function(ty) {
    switch(ty,
      integer   = NA_integer_,
      logical   = NA,
      character = NA_character_
    )
  })
  tibble::as_tibble(cols)
}

#' A complete, empty trial plan
#'
#' Every section starts as a zero-row table except the project section, which
#' always holds exactly one record and so starts with a single blank row.
ftp_new_state <- function(schema = ftp_schema()) {
  state <- lapply(schema, ftp_empty_table)
  state$project <- ftp_blank_row(schema$project)
  state
}
