# Rendered summary -------------------------------------------------------------
#
# The CSV is the machine-readable artefact; this is the one a person reads. Both
# come from the same state, so they cannot drift apart.
#
# Output is a self-contained HTML file with a print stylesheet. That is a
# deliberate choice over a LaTeX PDF: it needs no TeX installation, opens
# anywhere, and the browser's own "Save as PDF" produces a perfectly good PDF
# from it when a PDF is what someone wants.

ftp_report_dir <- function() {
  root <- ftp_project_root()
  if (!is.null(root)) return(file.path(root, "inst", "report"))
  system.file("report", package = "FieldTrialPlanner")
}

#' Make sure rmarkdown can find a pandoc
#'
#' Shiny Server and shinyapps.io provide pandoc on the path, but a local Rscript
#' session usually does not: RStudio and Quarto each bundle a copy that only
#' their own processes know about. Rather than requiring a separate pandoc
#' install for development, borrow whichever bundled copy is present.
#'
#' @return TRUE if pandoc is usable, FALSE otherwise.
ftp_ensure_pandoc <- function() {
  if (rmarkdown::pandoc_available()) return(TRUE)

  candidates <- c(
    Sys.getenv("RSTUDIO_PANDOC", ""),
    file.path(Sys.getenv("LOCALAPPDATA", ""), "Programs", "Quarto", "bin", "tools"),
    "C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools",
    "C:/Program Files/Quarto/bin/tools",
    "/usr/lib/rstudio-server/bin/quarto/bin/tools",
    "/Applications/quarto/bin/tools"
  )
  candidates <- candidates[nzchar(candidates) & dir.exists(candidates)]

  for (dir in candidates) {
    if (any(file.exists(file.path(dir, c("pandoc", "pandoc.exe"))))) {
      Sys.setenv(RSTUDIO_PANDOC = dir)
      rmarkdown::find_pandoc(dir = dir, cache = FALSE)
      if (rmarkdown::pandoc_available()) return(TRUE)
    }
  }
  FALSE
}

#' Render the trial plan summary
#'
#' @param state The plan to render.
#' @param config Loaded configuration.
#' @param output_file Where to write the HTML. Defaults to a temporary file.
#' @return The path written.
ftp_render_summary <- function(state, config, output_file = tempfile(fileext = ".html")) {
  if (!ftp_ensure_pandoc()) {
    stop("pandoc is required to render the summary, and none was found. ",
         "Install Quarto or pandoc, or download the CSV instead.", call. = FALSE)
  }
  state <- ftp_fill_question_text(state, config)

  # Render in a copy of the template directory so a Shiny app serving several
  # sessions never has two renders writing intermediates to the same place.
  staging <- file.path(tempdir(), paste0("ftp-report-", as.integer(runif(1, 1e6, 1e7))))
  dir.create(staging, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(staging, recursive = TRUE), add = TRUE)

  file.copy(list.files(ftp_report_dir(), full.names = TRUE), staging, overwrite = TRUE)

  rmarkdown::render(
    input       = file.path(staging, "plan_summary.Rmd"),
    output_file = basename(output_file),
    output_dir  = dirname(normalizePath(output_file, winslash = "/", mustWork = FALSE)),
    params = list(
      state   = state,
      config  = config,
      issues  = ftp_validate(state, config),
      profile = ftp_profile(state, config)
    ),
    envir = new.env(parent = globalenv()),
    quiet = TRUE
  )

  output_file
}

#' Filename stem for downloads, derived from the project title
#'
#' Falls back to the configured basename when the title is blank, and strips
#' anything that would be awkward in a filename.
ftp_export_filename <- function(state, config, ext) {
  title <- state$project$project_title[1]
  stem <- if (is.na(title) || !nzchar(trimws(title))) {
    config$app$export_basename
  } else {
    slug <- tolower(trimws(title))
    slug <- gsub("[^a-z0-9]+", "-", slug)
    slug <- gsub("^-+|-+$", "", slug)
    substr(slug, 1, 60)
  }
  paste0(stem, "-", format(Sys.Date(), "%Y%m%d"), ".", ext)
}
