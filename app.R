# Field Trial Planner ----------------------------------------------------------
#
# The shell: holds the plan in memory, draws the step list, and shows one step
# module at a time. Steps are defined in R/fct_steps.R; each has a module in
# R/mod_*.R. Steps not yet built fall through to the placeholder module.

library(shiny)
library(bslib)

for (f in list.files("R", pattern = "[.]R$", full.names = TRUE)) source(f)

config <- ftp_load_config()

# Steps with a real module. Anything else gets the placeholder.
FTP_BUILT <- c("welcome", "project")

ui <- page_sidebar(
  title = config$app$app_name,
  window_title = config$app$app_name,

  theme = bs_theme(
    version = 5,
    primary = config$app$theme$primary %||% "#3d6b52",
    base_font = font_google("Source Sans 3", local = FALSE),
    heading_font = font_google("Source Sans 3", local = FALSE)
  ),

  tags$head(tags$link(rel = "stylesheet", href = "styles.css")),

  sidebar = sidebar(
    width = 290,
    class = "ftp-sidebar",

    uiOutput("step_list"),

    hr(),
    div(
      class = "ftp-plan-controls",
      downloadButton("download_csv", "Download plan", class = "btn-primary w-100"),
      downloadButton("download_html", "Download summary",
                     class = "btn-outline-secondary w-100 mt-2"),
      div(class = "mt-3",
          fileInput("upload", "Load a plan", accept = ".csv",
                    buttonLabel = "Browse", placeholder = "No file"))
    ),
    uiOutput("plan_state_note")
  ),

  div(class = "ftp-main", uiOutput("step_body"))
)

server <- function(input, output, session) {

  # The whole plan lives here. Modules read plan$state and write back to it.
  plan <- reactiveValues(state = ftp_new_state())

  current <- reactiveVal("welcome")
  go_to <- function(id) {
    current(id)
    session$sendCustomMessage("ftp-scroll-top", list())
  }

  # Step modules -------------------------------------------------------------
  ftp_welcome_server("welcome", plan, config, go_to)
  ftp_project_server("project", plan, config, go_to)

  for (step in ftp_steps()) {
    if (step$id %in% FTP_BUILT) next
    local({
      sid <- step$id
      ftp_placeholder_server(sid, sid, config, go_to)
    })
  }

  output$step_body <- renderUI({
    id <- current()
    if (id %in% FTP_BUILT) {
      switch(id,
        welcome = ftp_welcome_ui("welcome"),
        project = ftp_project_ui("project")
      )
    } else {
      ftp_placeholder_ui(id, id)
    }
  })

  # Step list ----------------------------------------------------------------
  output$step_list <- renderUI({
    here <- current()
    state <- plan$state

    tags$nav(
      class = "ftp-steps",
      lapply(ftp_steps(), function(step) {
        status <- ftp_step_status(state, step$id)
        actionLink(
          paste0("goto_", step$id),
          class = paste("ftp-step-link",
                        if (step$id == here) "is-current",
                        if (!(step$id %in% FTP_BUILT)) "is-pending"),
          label = tagList(
            ftp_status_dot(status),
            span(class = "ftp-step-link-label", step$label)
          )
        )
      })
    )
  })

  for (step in ftp_steps()) {
    local({
      sid <- step$id
      observeEvent(input[[paste0("goto_", sid)]], go_to(sid), ignoreInit = TRUE)
    })
  }

  # Plan file ----------------------------------------------------------------
  observeEvent(input$upload, {
    req(input$upload)
    tryCatch({
      plan$state <- ftp_read_csv(input$upload$datapath)
      showNotification("Plan loaded.", type = "message")
    }, error = function(e) {
      showNotification(paste("Could not read that file:", conditionMessage(e)),
                       type = "error", duration = NULL)
    })
  })

  output$plan_state_note <- renderUI({
    n <- nrow(plan$state$trials)
    p(class = "ftp-hint mt-2",
      if (n == 0) "Nothing entered yet."
      else paste(n, if (n == 1) "trial" else "trials", "in this plan."))
  })

  saved <- reactive(ftp_fill_question_text(plan$state, config))

  output$download_csv <- downloadHandler(
    filename = function() ftp_export_filename(saved(), config, "csv"),
    content  = function(file) ftp_write_csv(saved(), file)
  )

  output$download_html <- downloadHandler(
    filename = function() ftp_export_filename(saved(), config, "html"),
    content  = function(file) ftp_render_summary(saved(), config, file)
  )
}

shinyApp(ui, server)
