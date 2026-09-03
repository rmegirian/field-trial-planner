# Start ------------------------------------------------------------------------
#
# Orientation before any data entry: why planning determines what an experiment
# can show, how the planner is organised, and what happens to what you enter.
#
# All the copy comes from guidance.yml. Deliberately no per-section rationale
# here - each section carries its own, where there is context to make sense of it.

ftp_welcome_ui <- function(id) {
  ns <- shiny::NS(id)

  htmltools::tagList(
    shiny::uiOutput(ns("header")),
    shiny::uiOutput(ns("body"))
  )
}

ftp_welcome_server <- function(id, plan, config, go_to) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    welcome <- config$guidance$welcome

    output$header <- shiny::renderUI({
      htmltools::h2(class = "ftp-step-title ftp-app-title", config$app$app_name)
    })

    output$body <- shiny::renderUI({
      htmltools::tagList(
        # Blocks are rendered only if guidance.yml still defines them, so the
        # copy can be restructured - a section added, dropped or reordered -
        # without the page erroring.
        ftp_welcome_block(welcome, "what_this_is", lede = TRUE),
        ftp_welcome_block(welcome, "how_it_works"),
        ftp_welcome_block(welcome, "privacy", callout_first = TRUE),

        htmltools::div(
          class = "ftp-start-actions",
          shiny::actionButton(ns("start"), "Start a new plan",
                              class = "btn-primary btn-lg"),
          shiny::actionButton(ns("example"), "Open the worked example",
                              class = "btn-outline-secondary btn-lg")
        )
      )
    })

    shiny::observeEvent(input$start, go_to("project"))


    shiny::observeEvent(input$example, {
      plan$state <- ftp_example_state()
      go_to("project")
      shiny::showNotification(
        "Loaded the worked example: two trials you can read through or edit.",
        type = "message"
      )
    })
  })
}

#' Render one titled block of the Start page
#'
#' Looks for `<key>_title` and `<key>` in the welcome config. Returns NULL when
#' the block is absent, so removing a section from guidance.yml removes it from
#' the page rather than breaking it.
#'
#' @param lede Style the paragraphs as the opening statement.
#' @param callout_first Give the first paragraph the highlighted treatment, for
#'   the one thing on the page a reader might need to have noticed.
ftp_welcome_block <- function(welcome, key, lede = FALSE, callout_first = FALSE) {
  paragraphs <- as.character(unlist(welcome[[key]]))
  paragraphs <- paragraphs[nzchar(trimws(paragraphs))]
  if (length(paragraphs) == 0) return(NULL)

  title <- welcome[[paste0(key, "_title")]]

  htmltools::tagList(
    if (!is.null(title) && nzchar(title)) htmltools::h3(class = "ftp-h3", title),
    if (callout_first) {
      htmltools::tagList(
        htmltools::div(class = "ftp-note ftp-note-strong", paragraphs[1]),
        ftp_paragraphs(paragraphs[-1])
      )
    } else {
      ftp_paragraphs(paragraphs, class = if (lede) "ftp-lede" else NULL)
    }
  )
}
