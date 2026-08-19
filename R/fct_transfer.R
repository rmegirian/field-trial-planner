# Export and import ------------------------------------------------------------
#
# The plan leaves as one long-format CSV: one row per recorded fact. That shape
# suits a ragged hierarchy (a trial has many factors, a factor has many levels)
# without a forest of empty columns, stays readable to a person opening it in
# Excel, and is trivially pivoted back into wide tables for analysis.
#
# The same file is also the save format. Importing it restores the state exactly,
# so "download your plan" and "save your progress" are one action rather than two.

FTP_LONG_COLS <- c("section", "entity_id", "parent_id", "field", "value")

#' Convert a state to long format
#'
#' @param state A trial plan state, as built by `ftp_new_state()`.
#' @param schema The schema the state conforms to.
#' @return A tibble with columns section, entity_id, parent_id, field, value.
ftp_state_to_long <- function(state, schema = ftp_schema()) {
  meta <- tibble::tibble(
    section   = "meta",
    entity_id = NA_character_,
    parent_id = NA_character_,
    field     = c("schema_version", "exported_at"),
    value     = c(FTP_SCHEMA_VERSION, format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"))
  )

  rows <- lapply(names(schema), function(key) {
    section <- schema[[key]]
    tbl <- state[[key]]
    if (is.null(tbl) || nrow(tbl) == 0) return(NULL)

    fields <- ftp_field_names(section)
    id_col <- section$id_field
    parent_col <- section$parent_field

    # Identity and parentage are carried in their own columns, so they are not
    # repeated as ordinary fields.
    value_fields <- setdiff(fields, c(id_col, parent_col))

    per_row <- lapply(seq_len(nrow(tbl)), function(i) {
      tibble::tibble(
        section   = key,
        entity_id = if (is.null(id_col)) NA_character_ else as.character(tbl[[id_col]][i]),
        parent_id = if (is.null(parent_col)) NA_character_ else as.character(tbl[[parent_col]][i]),
        field     = value_fields,
        value     = vapply(value_fields, function(f) {
          v <- tbl[[f]][i]
          if (is.na(v)) NA_character_ else as.character(v)
        }, character(1), USE.NAMES = FALSE)
      )
    })
    do.call(rbind, per_row)
  })

  out <- do.call(rbind, c(list(meta), rows))
  # Facts never recorded are omitted rather than exported as blanks, which keeps
  # the file to what the user actually said.
  out[!is.na(out$value), , drop = FALSE]
}

#' Write a plan to CSV
ftp_write_csv <- function(state, path, schema = ftp_schema()) {
  readr::write_csv(ftp_state_to_long(state, schema), path, na = "")
  invisible(path)
}

#' Rebuild a state from long format
#'
#' Unknown sections and unknown fields are dropped with a warning rather than
#' erroring: a file exported by a later version should still load what it can.
ftp_long_to_state <- function(long, schema = ftp_schema()) {
  missing_cols <- setdiff(FTP_LONG_COLS, names(long))
  if (length(missing_cols) > 0) {
    stop("Not a Field Trial Planner file: missing column(s) ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  long <- long[, FTP_LONG_COLS, drop = FALSE]
  long[] <- lapply(long, as.character)

  unknown <- setdiff(unique(long$section), c("meta", names(schema)))
  if (length(unknown) > 0) {
    warning("Ignoring unrecognised section(s): ", paste(unknown, collapse = ", "),
            call. = FALSE)
  }

  state <- ftp_new_state(schema)

  for (key in names(schema)) {
    section <- schema[[key]]
    part <- long[long$section == key & !is.na(long$section), , drop = FALSE]
    if (nrow(part) == 0) next

    id_col <- section$id_field
    parent_col <- section$parent_field
    fields <- ftp_field_names(section)

    known <- part$field %in% fields
    if (any(!known)) {
      warning("Ignoring unrecognised field(s) in section '", key, "': ",
              paste(unique(part$field[!known]), collapse = ", "), call. = FALSE)
      part <- part[known, , drop = FALSE]
    }
    if (nrow(part) == 0) next

    # Group by entity, preserving the order the entities first appear so that
    # trial and factor ordering survives the round trip.
    entity_key <- ifelse(is.na(part$entity_id), "", part$entity_id)
    order_of <- unique(entity_key)

    rows <- lapply(order_of, function(eid) {
      rows_for <- part[entity_key == eid, , drop = FALSE]
      row <- ftp_blank_row(section)
      for (i in seq_len(nrow(rows_for))) {
        f <- rows_for$field[i]
        row[[f]] <- ftp_coerce(rows_for$value[i], ftp_field_types(section)[[f]])
      }
      if (!is.null(id_col)) row[[id_col]] <- ftp_coerce(eid, ftp_field_types(section)[[id_col]])
      if (!is.null(parent_col)) {
        row[[parent_col]] <- ftp_coerce(rows_for$parent_id[1],
                                        ftp_field_types(section)[[parent_col]])
      }
      row
    })

    tbl <- do.call(rbind, rows)
    # The project section holds exactly one record even if the file carries none.
    state[[key]] <- if (section$single) tbl[1, , drop = FALSE] else tbl
  }

  state
}

#' Read a plan from CSV
ftp_read_csv <- function(path, schema = ftp_schema()) {
  long <- readr::read_csv(path, col_types = readr::cols(.default = readr::col_character()),
                          progress = FALSE)
  ftp_long_to_state(long, schema)
}
