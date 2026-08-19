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
      htmltools::tagList(
        htmltools::h2(class = "ftp-step-title ftp-app-title", config$app$app_name),
        ftp_paragraphs(welcome$what_this_is, class = "ftp-lede")
      )
    })

    output$body <- shiny::renderUI({
      privacy <- as.character(unlist(welcome$privacy))

      htmltools::tagList(

        htmltools::h3(class = "ftp-h3", welcome$how_it_works_title),
        ftp_paragraphs(welcome$how_it_works),

        htmltools::h3(class = "ftp-h3", welcome$privacy_title),
        # The first statement gets the callout treatment; it is the one thing on
        # this page someone might need to have noticed.
        htmltools::div(class = "ftp-note ftp-note-strong", privacy[1]),
        ftp_paragraphs(privacy[-1]),

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
