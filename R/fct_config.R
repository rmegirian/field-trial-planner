# Configuration --------------------------------------------------------------
#
# Wording, vocabularies and thresholds live in YAML rather than in R, so they can
# be revised without touching code. Shipped defaults sit in inst/config/; files
# of the same name in inst/config/local/ are merged over the top, restating only
# the keys they change.

FTP_CONFIG_FILES <- c("app", "lists", "guidance", "complexity")

#' Locate the project root
#'
#' Walks up from the working directory looking for inst/config. Needed because
#' the app runs from the project root but testthat runs from tests/testthat.
ftp_project_root <- function(start = getwd()) {
  path <- normalizePath(start, winslash = "/", mustWork = FALSE)
  repeat {
    if (dir.exists(file.path(path, "inst", "config"))) return(path)
    parent <- dirname(path)
    if (identical(parent, path)) return(NULL)
    path <- parent
  }
}

ftp_config_dir <- function() {
  # An explicit option wins, so a deployment can point at a config directory
  # outside the app bundle.
  explicit <- getOption("FieldTrialPlanner.config_dir")
  if (!is.null(explicit)) return(explicit)

  root <- ftp_project_root()
  if (!is.null(root)) return(file.path(root, "inst", "config"))

  # Installed as a package.
  system.file("config", package = "FieldTrialPlanner")
}

#' Recursively merge `over` on top of `base`
#'
#' Named lists are merged key by key; anything else replaces wholesale. This
#' lets an overlay change a single guidance paragraph or a single complexity
#' weight without restating the file.
ftp_merge <- function(base, over) {
  if (!is.list(base) || !is.list(over) || is.null(names(over))) return(over)
  for (key in names(over)) {
    base[[key]] <- if (key %in% names(base)) ftp_merge(base[[key]], over[[key]]) else over[[key]]
  }
  base
}

#' Load one config file, applying the local overlay if present
ftp_load_config_file <- function(name, dir = ftp_config_dir()) {
  shipped <- file.path(dir, paste0(name, ".yml"))
  if (!file.exists(shipped)) {
    stop("Missing shipped config: ", shipped, call. = FALSE)
  }
  cfg <- yaml::read_yaml(shipped)

  overlay <- file.path(dir, "local", paste0(name, ".yml"))
  if (file.exists(overlay)) {
    cfg <- ftp_merge(cfg, yaml::read_yaml(overlay))
  }
  cfg
}

#' Load the whole configuration
#'
#' @return A named list with one element per file in `FTP_CONFIG_FILES`, plus
#'   `is_customised`, which reports whether any local overlay was applied. The
#'   app uses that flag only to label the build, never to change behaviour.
ftp_load_config <- function(dir = ftp_config_dir()) {
  cfg <- stats::setNames(
    lapply(FTP_CONFIG_FILES, ftp_load_config_file, dir = dir),
    FTP_CONFIG_FILES
  )
  overlays <- file.path(dir, "local", paste0(FTP_CONFIG_FILES, ".yml"))
  cfg$is_customised <- any(file.exists(overlays))
  cfg
}

#' Look up a vocabulary from lists.yml
#'
#' @param name A `choices` value from the schema, e.g. "data_type".
ftp_choices <- function(config, name) {
  if (is.null(name)) return(NULL)
  values <- config$lists[[name]]
  if (is.null(values)) {
    warning("No vocabulary named '", name, "' in lists.yml", call. = FALSE)
    return(character(0))
  }
  as.character(unlist(values, use.names = FALSE))
}

#' Look up guidance text by dotted path, e.g. "factors.what_is_a_factor"
#'
#' Returns "" rather than erroring on a missing key, so a partially translated
#' or trimmed overlay degrades to a blank panel instead of a broken app.
ftp_guidance <- function(config, path) {
  node <- config$guidance
  for (part in strsplit(path, ".", fixed = TRUE)[[1]]) {
    if (!is.list(node) || is.null(node[[part]])) return("")
    node <- node[[part]]
  }
  if (is.list(node)) return(node)
  paste(as.character(node), collapse = "\n\n")
}
