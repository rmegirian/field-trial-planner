# Not yet built ----------------------------------------------------------------
#
# Steps the workflow reaches but which have not been implemented. Shows what the
# step will ask for and why, taken from the same section map as everything else,
# so the shape of the plan is legible before it is all built.

ftp_placeholder_ui <- function(id, step_id) {
  ns <- shiny::NS(id)
  step <- ftp_step(step_id)

  htmltools::tagList(
    ftp_step_header(step),
    shiny::uiOutput(ns("why")),
    htmltools::div(
      class = "ftp-note",
      htmltools::strong("Not built yet. "),
      "This step is next in line. In the meantime the data model behind it is ",
      "complete, so a plan loaded from a file keeps everything this step will show."
    ),
    shiny::uiOutput(ns("provides")),
    ftp_step_nav(ns, step_id)
  )
}

ftp_placeholder_server <- function(id, step_id, config, go_to) {
  shiny::moduleServer(id, function(input, output, session) {

    output$why <- shiny::renderUI({
      ftp_why_panel(ftp_section_matters(config, step_id))
    })

    output$provides <- shiny::renderUI({
      provide <- ftp_section_copy(config, step_id, "provide")
      if (!nzchar(provide)) return(NULL)
      htmltools::p(class = "ftp-hint",
                   htmltools::strong("Will ask for: "), provide)
    })

    prev_step <- ftp_step_neighbour(step_id, -1)
    next_step <- ftp_step_neighbour(step_id, +1)
    if (!is.null(prev_step)) shiny::observeEvent(input$back, go_to(prev_step$id))
    if (!is.null(next_step)) shiny::observeEvent(input$`next`, go_to(next_step$id))
  })
}
