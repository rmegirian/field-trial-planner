# Shared UI pieces -------------------------------------------------------------
#
# The components every section reuses. Keeping them here means the teaching
# furniture - why a step matters, what a good answer looks like - is consistent
# rather than reinvented per section.

#' Heading for a step
#'
#' @param step A step definition from `ftp_steps()`.
#' @param lede One or two sentences setting up what the step is asking for.
ftp_step_header <- function(step, lede = NULL) {
  htmltools::tagList(
    htmltools::h2(class = "ftp-step-title", step$label),
    if (!is.null(lede) && nzchar(lede)) htmltools::p(class = "ftp-lede", lede)
  )
}

#' The "why this matters" callout
#'
#' Shown open by default rather than hidden behind a link. The reasoning is the
#' point of the tool, not a footnote to it.
ftp_why_panel <- function(text, title = "Why this matters") {
  if (!nzchar(text)) return(NULL)
  htmltools::div(
    class = "ftp-why",
    htmltools::div(class = "ftp-why-title", title),
    htmltools::p(text)
  )
}

#' Collapsible guidance, for the longer explanations
#'
#' @param items A named list of heading -> paragraph(s).
ftp_guidance_accordion <- function(id, items) {
  items <- items[vapply(items, function(x) nzchar(paste(x, collapse = "")), logical(1))]
  if (length(items) == 0) return(NULL)

  bslib::accordion(
    id = id,
    open = FALSE,
    class = "ftp-guidance",
    !!!lapply(names(items), function(heading) {
      bslib::accordion_panel(
        title = heading,
        lapply(as.character(items[[heading]]), htmltools::p)
      )
    })
  )
}

#' A worked-example aside
#'
#' The workbook showed a worked example in a column beside every field, with a
#' control to hide it. The same idea: visible when wanted, never in the way.
ftp_example_box <- function(...) {
  htmltools::div(
    class = "ftp-example",
    htmltools::div(class = "ftp-example-title", "Example"),
    ...
  )
}

#' Back and Next buttons for a step
#'
#' Navigation is free rather than gated - nothing here blocks moving on with a
#' step incomplete, because a plan is often built out of order.
ftp_step_nav <- function(ns, id) {
  prev_step <- ftp_step_neighbour(id, -1)
  next_step <- ftp_step_neighbour(id, +1)

  htmltools::div(
    class = "ftp-step-nav",
    if (!is.null(prev_step)) {
      shiny::actionButton(ns("back"), paste("←", prev_step$label),
                          class = "btn-link ftp-back")
    } else htmltools::div(),
    if (!is.null(next_step)) {
      shiny::actionButton(ns("next"), paste(next_step$label, "→"),
                          class = "btn-primary")
    }
  )
}

#' Label with an inline hint underneath
#'
#' Field guidance sat beside the field name in the workbook. Putting it directly
#' under the label keeps it attached to the thing it explains.
ftp_labelled <- function(label, hint = NULL) {
  htmltools::tagList(
    htmltools::span(class = "ftp-label", label),
    if (!is.null(hint) && nzchar(hint)) htmltools::span(class = "ftp-hint", hint)
  )
}

#' Status dot for the step list
ftp_status_dot <- function(status) {
  htmltools::span(class = paste0("ftp-dot ftp-dot-", status), title = status)
}

#' Render guidance text as one or more paragraphs
#'
#' Guidance in the YAML may be a single string or a list of them. Either way it
#' arrives here joined by blank lines, so splitting on those gives the paragraphs
#' the author intended rather than one undifferentiated block.
ftp_paragraphs <- function(text, class = NULL) {
  if (length(text) == 0 || !nzchar(paste(text, collapse = ""))) return(NULL)
  parts <- unlist(strsplit(paste(text, collapse = "\n\n"), "\n\n+"))
  parts <- trimws(parts)
  parts <- parts[nzchar(parts)]
  lapply(parts, function(p) htmltools::p(class = class, p))
}
