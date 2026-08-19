# Step 1: Project --------------------------------------------------------------
#
# Completed once for the whole project. Two fields, because only two of them
# change what the design has to do: what the project is called, and what it is
# trying to achieve. The objective is the reference point every later trade-off
# is settled against, which is why it gets a text area rather than a one-liner.

ftp_project_ui <- function(id) {
  ns <- shiny::NS(id)

  htmltools::tagList(
    ftp_step_header(
      ftp_step("project"),
      "Describe the project once. Individual trials come next."
    ),
    shiny::uiOutput(ns("why")),

    htmltools::div(
      class = "ftp-fields",

      htmltools::div(
        class = "ftp-field",
        shiny::textInput(
          ns("project_title"),
          label = ftp_labelled(
            "Project title",
            "The working title, as you would recognise it."
          ),
          width = "100%",
          placeholder = "e.g. Optimising nitrogen and sowing strategies for wheat productivity"
        )
      ),

      htmltools::div(
        class = "ftp-field",
        shiny::textAreaInput(
          ns("project_objective"),
          label = ftp_labelled(
            "Overall project objective",
            "The broad goal or intended outcome. What would count as a successful result?"
          ),
          width = "100%",
          height = "9rem",
          placeholder = paste(
            "e.g. To identify nitrogen and sowing strategies that improve wheat",
            "productivity across contrasting rainfall zones, and determine whether",
            "variety responses to sowing depth are consistent across seasons."
          )
        ),
        shiny::uiOutput(ns("objective_feedback"))
      )
    ),

    shiny::uiOutput(ns("guidance")),
    ftp_step_nav(ns, "project")
  )
}

ftp_project_server <- function(id, plan, config, go_to) {
  shiny::moduleServer(id, function(input, output, session) {

    output$why <- shiny::renderUI({
      ftp_why_panel(ftp_section_matters(config, "project"))
    })

    # Populate from the plan, including after a file is loaded or the example
    # opened. `ignoreInit = FALSE` so a plan already in memory shows on arrival.
    shiny::observeEvent(plan$state$project, {
      p <- plan$state$project
      shiny::updateTextInput(session, "project_title",
                             value = ftp_blank_to_empty(p$project_title[1]))
      shiny::updateTextAreaInput(session, "project_objective",
                                 value = ftp_blank_to_empty(p$project_objective[1]))
    }, ignoreInit = FALSE)

    shiny::observeEvent(input$project_title, {
      plan$state$project$project_title <- ftp_empty_to_na(input$project_title)
    }, ignoreInit = TRUE)

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

    output$guidance <- shiny::renderUI({
      ftp_guidance_accordion(
        session$ns("guide"),
        list(
          "Why the objective carries so much weight" =
            ftp_guidance(config, "project.objective_matters"),
          "What if the project has several distinct aims?" =
            ftp_guidance(config, "trials.what_is_a_trial")
        )
      )
    })

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

#' NA becomes an empty input box
ftp_blank_to_empty <- function(x) if (length(x) == 0 || is.na(x)) "" else as.character(x)

#' An empty input box becomes NA, so "not answered" is stored as not answered
ftp_empty_to_na <- function(x) {
  if (is.null(x) || !nzchar(trimws(x))) NA_character_ else trimws(x)
}
