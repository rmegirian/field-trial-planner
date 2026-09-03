# Step 1: Project --------------------------------------------------------------
#
# Completed once for the whole project. Two fields, because only two of them
# change what the design has to do: what the project is called, and what it is
# trying to achieve. The objective is the reference point every later trade-off
# is settled against, which is why it gets a text area rather than a one-liner.

ftp_project_ui <- function(id) {
  ns <- shiny::NS(id)

  htmltools::tagList(
    ftp_step_header(ftp_step("project")),

    htmltools::div(
      class = "ftp-fields",

      htmltools::div(
        class = "ftp-field",
        shiny::textAreaInput(
          ns("project_objective"),
          label = ftp_labelled(
            "Research Objective",
            paste("A research objective provides a broad overview of what the",
                  "research is intended to achieve or establish, setting its",
                  "overall purpose and direction.")
          ),
          width = "100%",
          placeholder = paste(
            "e.g., To identify management strategies that improve wheat",
            "productivity across contrasting rainfall zones and seasons."
          )
        ),
        shiny::uiOutput(ns("objective_feedback"))
      )
    ),

    # Trials -------------------------------------------------------------------
    # One block per trial, generated from the plan. A project may be a single
    # trial, or several that each contribute to the objective above.
    shiny::uiOutput(ns("trials")),

    htmltools::p(
      class = "ftp-hint ftp-add-note",
      paste("If your research project includes multiple trials with distinct aims",
            "or research questions contributing to the overall research objective,",
            "you can add each trial below.")
    ),
    shiny::actionButton(ns("add_trial"), "+ Add trial",
                        class = "btn-outline-secondary ftp-add"),

    shiny::uiOutput(ns("guidance")),
    ftp_step_nav(ns, "project")
  )
}

#' The fields for one trial
#'
#' @param n Position in the list, used for the visible "Trial 1" heading. Trial
#'   identifiers are internal and never shown.
ftp_trial_block <- function(ns, trial, questions, n, removable) {
  tid <- trial$trial_id

  # With several trials the fields repeat, so the guidance is shown once on the
  # first and the name is numbered to tell the blocks apart.
  multiple <- removable
  first <- n == 1
  hint <- function(text) if (first) text else NULL

  htmltools::div(
    class = "ftp-trial",
    # No "Trial N" heading: the trial name is the first field, so it identifies
    # the block itself. Only the remove control sits above the fields.
    if (removable) {
      htmltools::div(
        class = "ftp-trial-head",
        shiny::actionLink(ns(paste0("remove_", tid)), "Remove trial",
                          class = "ftp-remove")
      )
    },

    htmltools::div(
      class = "ftp-field",
      shiny::textInput(
        ns(paste0("name_", tid)),
        label = ftp_labelled(if (multiple) paste("Trial", n, "name") else "Trial name"),
        value = ftp_blank_to_empty(trial$trial_name),
        width = "100%",
        placeholder = "e.g., Nitrogen rate trial"
      )
    ),

    htmltools::div(
      class = "ftp-field",
      shiny::textAreaInput(
        ns(paste0("aim_", tid)),
        label = ftp_labelled(
          "Aim",
          hint(paste("The aim describes the trial's purpose and what it is intended",
                     "to contribute to the overall objective."))
        ),
        value = ftp_blank_to_empty(trial$trial_aim),
        width = "100%",
        placeholder = paste("e.g., To determine the nitrogen rate required to maximise",
                            "wheat yield and grain protein under different seasonal",
                            "conditions.")
      )
    ),

    htmltools::div(
      class = "ftp-field",
      ftp_labelled(
        "Research Question(s)",
        hint(paste("Research questions specify what the trial is intended to find out.",
                   "In quantitative research, a well-defined question identifies the",
                   "outcome and the factor, relationship, comparison or effect of",
                   "interest."))
      ),
      if (first) {
        htmltools::p(
          class = "ftp-hint",
          paste("Most trials will address one or two primary research questions. Each",
                "question should describe a distinct thing the trial is intended to",
                "find out. A question may involve more than one outcome.")
        )
      },

      lapply(seq_len(nrow(questions)), function(i) {
        qid <- questions$rq_id[i]
        htmltools::div(
          class = "ftp-question",
          shiny::textAreaInput(
            ns(paste0("rq_", qid)),
            label = NULL,
            value = ftp_blank_to_empty(questions$question[i]),
            width = "100%",
            placeholder = if (i == 1) {
              paste("e.g., How does nitrogen rate affect wheat yield and grain",
                    "protein under different seasonal conditions?")
            } else NULL
          ),
          if (nrow(questions) > 1) {
            shiny::actionLink(ns(paste0("rmq_", qid)), "Remove", class = "ftp-remove")
          }
        )
      }),

      # A link rather than a button: Bootstrap's button padding would indent the
      # text away from the left edge of the fields above it.
      shiny::actionLink(ns(paste0("addq_", tid)), "+ Add research question",
                        class = "ftp-add-question")
    )
  )
}

ftp_project_server <- function(id, plan, config, go_to) {
  shiny::moduleServer(id, function(input, output, session) {

    # Populate from the plan, including after a file is loaded or the example
    # opened. `ignoreInit = FALSE` so a plan already in memory shows on arrival.
    shiny::observeEvent(plan$state$project, {
      p <- plan$state$project
      shiny::updateTextAreaInput(session, "project_objective",
                                 value = ftp_blank_to_empty(p$project_objective[1]))
    }, ignoreInit = FALSE)

    shiny::observeEvent(input$project_objective, {
      plan$state$project$project_objective <- ftp_empty_to_na(input$project_objective)
    }, ignoreInit = TRUE)

    # A gentle nudge rather than validation. An objective of four words is
    # usually a topic, not an objective, but it is not the app's place to insist.
    output$objective_feedback <- shiny::renderUI({
      text <- input$project_objective
      if (is.null(text) || !nzchar(trimws(text))) return(NULL)
      words <- lengths(strsplit(trimws(text), "\\s+"))
      if (words >= 12) return(NULL)
      htmltools::div(
        class = "ftp-nudge",
        "That reads more like a topic than an objective. A useful objective says",
        "what would count as a successful result, so that later design decisions",
        "can be judged against it."
      )
    })

    # Trials -------------------------------------------------------------------
    #
    # The trial blocks are redrawn only when trials are added, removed or loaded
    # from a file - never while typing. `layout` holds the ids currently drawn;
    # keeping it separate from the plan is what stops a keystroke from rebuilding
    # the inputs and moving the cursor.
    # `layout` is a list of trial ids, each holding the ids of its questions. The
    # UI is redrawn only when that structure changes.
    layout <- shiny::reactiveVal(list())
    wired <- shiny::reactiveValues()

    shiny::observe({
      trials <- plan$state$trials

      # A plan always has at least one trial, with one question, ready to fill
      # in. Seeding here rather than in the empty state keeps an untouched plan
      # genuinely empty on export.
      if (nrow(trials) == 0) {
        new <- ftp_blank_row(ftp_schema()$trials)
        new$trial_id <- ftp_next_trial_id(character(0))
        plan$state$trials <- new
        return()
      }

      ids <- trials$trial_id[!is.na(trials$trial_id)]
      rq <- plan$state$research_questions

      missing <- setdiff(ids, rq$trial_id)
      if (length(missing) > 0) {
        plan$state$research_questions <- rbind(rq, ftp_blank_question(missing[1], rq$rq_id))
        return()
      }

      structure_now <- lapply(stats::setNames(ids, ids), function(tid) {
        rq$rq_id[rq$trial_id == tid]
      })
      if (!identical(structure_now, shiny::isolate(layout()))) layout(structure_now)
    })

    output$trials <- shiny::renderUI({
      structure_now <- layout()
      if (length(structure_now) == 0) return(NULL)

      trials <- shiny::isolate(plan$state$trials)
      rq <- shiny::isolate(plan$state$research_questions)
      ids <- names(structure_now)

      htmltools::tagList(
        lapply(seq_along(ids), function(i) {
          ftp_trial_block(
            session$ns,
            trial     = trials[match(ids[i], trials$trial_id), ],
            questions = rq[rq$rq_id %in% structure_now[[i]], , drop = FALSE],
            n         = i,
            removable = length(ids) > 1
          )
        })
      )
    })

    # Inputs get their own observers the first time they are drawn, once each.
    # Writes are guarded by a comparison so that echoing a value back into the
    # plan cannot start a loop.
    shiny::observe({
      structure_now <- layout()
      for (tid in names(structure_now)) {
        if (!isTRUE(wired[[tid]])) {
          wired[[tid]] <- TRUE
          ftp_wire_trial(input, plan, tid)
        }
        for (qid in structure_now[[tid]]) {
          if (isTRUE(wired[[qid]])) next
          wired[[qid]] <- TRUE
          ftp_wire_question(input, plan, qid)
        }
      }
    })

    shiny::observeEvent(input$add_trial, {
      trials <- plan$state$trials
      new <- ftp_blank_row(ftp_schema()$trials)
      new$trial_id <- ftp_next_trial_id(trials$trial_id)
      plan$state$trials <- rbind(trials, new)
    })

    # One "+ add question" button per trial, so they are registered as the
    # trials themselves appear.
    shiny::observe({
      for (tid in names(layout())) {
        key <- paste0("addq_wired_", tid)
        if (isTRUE(wired[[key]])) next
        wired[[key]] <- TRUE
        local({
          this <- tid
          shiny::observeEvent(input[[paste0("addq_", this)]], {
            rq <- plan$state$research_questions
            plan$state$research_questions <- rbind(rq, ftp_blank_question(this, rq$rq_id))
          }, ignoreInit = TRUE)
        })
      }
    })

    output$guidance <- shiny::renderUI(NULL)

    shiny::observeEvent(input$back, go_to("welcome"))
    shiny::observeEvent(input$`next`, go_to("trials"))
  })
}

# Helpers ----------------------------------------------------------------------

#' Per-step copy from guidance.yml
#'
#' @param field "matters" for the why-this-matters callout, "provide" for a
#'   short statement of what the step asks for.
ftp_section_copy <- function(config, id, field = "matters") {
  hit <- Filter(function(s) identical(s$id, id), config$guidance$sections)
  if (length(hit) == 0) return("")
  paste(as.character(hit[[1]][[field]]), collapse = " ")
}

ftp_section_matters <- function(config, id) ftp_section_copy(config, id, "matters")

#' Wire one trial's inputs to the plan
#'
#' Called once per trial. Each field writes back only when the value actually
#' differs from what is stored, so echoing state into an input cannot trigger an
#' endless write-render-write cycle. Removing the trial is handled here too, so
#' everything to do with one trial is registered in one place.
ftp_wire_trial <- function(input, plan, tid) {
  fields <- c(name = "trial_name", aim = "trial_aim")

  for (prefix in names(fields)) {
    local({
      input_id <- paste0(prefix, "_", tid)
      column <- fields[[prefix]]

      shiny::observeEvent(input[[input_id]], {
        trials <- plan$state$trials
        i <- match(tid, trials$trial_id)
        if (is.na(i)) return()

        value <- ftp_empty_to_na(input[[input_id]])
        if (!identical(trials[[column]][i], value)) {
          trials[[column]][i] <- value
          plan$state$trials <- trials
        }
      }, ignoreInit = TRUE)
    })
  }

  # Removing a trial takes its questions with it, so nothing is left pointing at
  # a trial that no longer exists.
  shiny::observeEvent(input[[paste0("remove_", tid)]], {
    trials <- plan$state$trials
    plan$state$trials <- trials[trials$trial_id != tid, , drop = FALSE]
    rq <- plan$state$research_questions
    plan$state$research_questions <- rq[rq$trial_id != tid, , drop = FALSE]
  }, ignoreInit = TRUE)
}

#' Wire one research question's input to the plan
ftp_wire_question <- function(input, plan, qid) {
  shiny::observeEvent(input[[paste0("rq_", qid)]], {
    rq <- plan$state$research_questions
    i <- match(qid, rq$rq_id)
    if (is.na(i)) return()

    value <- ftp_empty_to_na(input[[paste0("rq_", qid)]])
    if (!identical(rq$question[i], value)) {
      rq$question[i] <- value
      plan$state$research_questions <- rq
    }
  }, ignoreInit = TRUE)

  shiny::observeEvent(input[[paste0("rmq_", qid)]], {
    rq <- plan$state$research_questions
    plan$state$research_questions <- rq[rq$rq_id != qid, , drop = FALSE]
  }, ignoreInit = TRUE)
}

#' A blank research question belonging to a trial
ftp_blank_question <- function(trial_id, existing_ids) {
  row <- ftp_blank_row(ftp_schema()$research_questions)
  row$trial_id <- trial_id
  n <- suppressWarnings(as.integer(sub("^Q", "", existing_ids[grepl("^Q[0-9]+$", existing_ids)])))
  n <- n[!is.na(n)]
  row$rq_id <- paste0("Q", if (length(n) == 0) 1 else max(n) + 1)
  row
}

#' Next unused trial identifier
#'
#' Identifiers are internal - they link trials to their factors, responses and
#' questions in the exported file - so they only have to be unique and stable,
#' never meaningful to read.
ftp_next_trial_id <- function(existing) {
  n <- suppressWarnings(as.integer(sub("^T", "", existing[grepl("^T[0-9]+$", existing)])))
  n <- n[!is.na(n)]
  paste0("T", if (length(n) == 0) 1 else max(n) + 1)
}

#' NA becomes an empty input box
ftp_blank_to_empty <- function(x) if (length(x) == 0 || is.na(x)) "" else as.character(x)

#' An empty input box becomes NA, so "not answered" is stored as not answered
ftp_empty_to_na <- function(x) {
  if (is.null(x) || !nzchar(trimws(x))) NA_character_ else trimws(x)
}
