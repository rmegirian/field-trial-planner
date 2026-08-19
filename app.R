# Field Trial Planner ----------------------------------------------------------
#
# This is currently the foundation harness, not the finished planner. It
# exercises everything underneath the UI - the schema, the configuration,
# validation, the design metrics, and the CSV round trip - so that the plumbing
# can be seen working before the section-by-section interface is built on top.
#
# The guided workflow (trials, factors, replication, responses, questions,
# implementation) replaces the "Plan" tab below.

library(shiny)
library(bslib)

for (f in list.files("R", pattern = "[.]R$", full.names = TRUE)) source(f)

config <- ftp_load_config()

ui <- page_sidebar(
  title = config$app$app_name,
  theme = bs_theme(
    version = 5,
    primary = config$theme$primary %||% "#3d6b52",
    base_font = font_google("Source Sans 3", local = FALSE)
  ),

  sidebar = sidebar(
    width = 330,
    h5("Your plan"),
    p(class = "text-muted small", config$app$export_instruction),

    fileInput("upload", "Load a plan (.csv)", accept = ".csv", buttonLabel = "Browse"),
    actionButton("load_example", "Load the worked example", class = "btn-outline-secondary w-100"),
    actionButton("clear", "Start empty", class = "btn-outline-secondary w-100 mt-2"),

    hr(),
    downloadButton("download_csv", "Download plan (.csv)", class = "btn-primary w-100"),
    downloadButton("download_html", "Download summary (.html)",
                   class = "btn-outline-primary w-100 mt-2"),

    hr(),
    p(class = "text-muted small", config$app$privacy_note),
    if (isTRUE(config$is_customised)) {
      span(class = "badge bg-secondary", "Customised configuration")
    }
  ),

  navset_card_tab(
    nav_panel(
      "Overview",
      h4(textOutput("project_title", inline = TRUE)),
      textOutput("project_objective"),
      hr(),
      layout_columns(
        fill = FALSE,
        value_box("Trials", textOutput("n_trials"), theme = "primary"),
        value_box("Treatments", textOutput("n_treatments")),
        value_box("Plots", textOutput("n_plots")),
        value_box("Analyses", textOutput("n_analyses"))
      ),
      h5("Per trial"),
      tableOutput("profile_table")
    ),

    nav_panel(
      "Checks",
      p(class = "lead", textOutput("validation_summary", inline = TRUE)),
      p(class = "text-muted small",
        "Problems mean the plan contradicts itself. Checks are probably wrong but",
        "may be deliberate. Notes are design features worth a conversation."),
      tableOutput("issues_table")
    ),

    nav_panel(
      "Plan",
      p(class = "text-muted",
        "The guided section-by-section workflow goes here. For now, this shows the",
        "underlying tables so the data model can be inspected."),
      uiOutput("raw_tables")
    ),

    nav_panel(
      "Export preview",
      p(class = "text-muted small",
        "Exactly what the downloaded CSV contains: one row per recorded fact."),
      tableOutput("long_preview")
    )
  )
)

server <- function(input, output, session) {

  plan <- reactiveVal(ftp_new_state())

  observeEvent(input$load_example, plan(ftp_example_state()))
  observeEvent(input$clear, plan(ftp_new_state()))

  observeEvent(input$upload, {
    req(input$upload)
    tryCatch({
      plan(ftp_read_csv(input$upload$datapath))
      showNotification("Plan loaded.", type = "message")
    }, error = function(e) {
      showNotification(paste("Could not read that file:", conditionMessage(e)),
                       type = "error", duration = NULL)
    })
  })

  # Question sentences are regenerated from the selections rather than stored,
  # so editing an effect type updates the wording everywhere it appears.
  current <- reactive(ftp_fill_question_text(plan(), config))
  profile <- reactive(ftp_profile(current(), config))
  issues  <- reactive(ftp_validate(current(), config))

  output$project_title <- renderText({
    title <- current()$project$project_title[1]
    if (is.na(title) || !nzchar(title)) "Untitled project" else title
  })

  output$project_objective <- renderText({
    obj <- current()$project$project_objective[1]
    if (is.na(obj)) "No objective recorded yet." else obj
  })

  output$n_trials     <- renderText(nrow(current()$trials))
  output$n_analyses   <- renderText(nrow(current()$responses))
  output$n_treatments <- renderText({
    n <- vapply(profile(), function(p) p$n_treatments %||% NA_integer_, integer(1))
    if (length(n) == 0 || all(is.na(n))) "—" else format(sum(n, na.rm = TRUE))
  })
  output$n_plots <- renderText({
    n <- vapply(profile(), function(p) p$total_plots %||% NA_integer_, integer(1))
    if (length(n) == 0 || all(is.na(n))) "—" else format(sum(n, na.rm = TRUE))
  })

  output$profile_table <- renderTable({
    p <- profile()
    if (length(p) == 0) return(NULL)
    do.call(rbind, lapply(p, function(x) data.frame(
      Trial       = x$trial_id,
      Name        = x$trial_name,
      Factors     = x$n_factors,
      Treatments  = x$n_treatments,
      Responses   = x$n_responses,
      Questions   = x$n_questions,
      Sites       = x$n_sites,
      Plots       = x$total_plots,
      `Residual df` = paste(unique(x$error_df_range), collapse = "–"),
      Profile     = x$band,
      check.names = FALSE, stringsAsFactors = FALSE
    )))
  })

  output$validation_summary <- renderText(ftp_validation_summary(issues()))

  output$issues_table <- renderTable({
    i <- issues()
    if (nrow(i) == 0) return(NULL)
    data.frame(
      Severity = as.character(i$severity),
      Where    = ifelse(is.na(i$entity_id), i$section,
                        paste0(i$section, " · ", i$entity_id)),
      Issue    = i$message,
      stringsAsFactors = FALSE
    )
  })

  output$raw_tables <- renderUI({
    state <- current()
    lapply(names(state), function(key) {
      tbl <- state[[key]]
      tagList(
        h5(ftp_schema()[[key]]$title, span(class = "text-muted small",
                                           paste0(" (", nrow(tbl), " row(s))"))),
        renderTable(tbl)()
      )
    })
  })

  output$long_preview <- renderTable(ftp_state_to_long(current()))

  output$download_csv <- downloadHandler(
    filename = function() ftp_export_filename(current(), config, "csv"),
    content  = function(file) ftp_write_csv(current(), file)
  )

  output$download_html <- downloadHandler(
    filename = function() ftp_export_filename(current(), config, "html"),
    content  = function(file) ftp_render_summary(current(), config, file)
  )
}

shinyApp(ui, server)
