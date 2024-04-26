construct_sql_query <- function(sql_script, ..., select_id = -1) {
  script <- file.path(system.file("sql", package = "curbcut"), sql_script)
  script <- paste0(script, ".sql")

  query <- readLines(script, warn = FALSE)
  query <- paste(query, collapse = " ")

  sprintf(query, ..., select_id)
}

#' @export
explore_text_postgres_val <- function(var, scale, parent_string, var_schema,
                                      parent_string_schema, select_id = NA, ...) {
  UseMethod("explore_text_postgres_val", var)
}

#' @export
explore_text_postgres_val.pct <- function(var, scale, parent_string, var_schema,
                                          parent_string_schema, select_id = NA, ...) {
  id <- if (is.na(select_id)) -1 else select_id
  inst_prefix <- get_from_globalenv("inst_prefix")

  sql_query <- construct_sql_query(sql_script = "value_pct",
                        var_schema, inst_prefix, scale, var, parent_string_schema,
                        parent_string, select_id = select_id)

  db_get_helper(sql_query)
}

#' @export
explore_text_postgres_val.count <- function(var, scale, parent_string, var_schema,
                                            parent_string_schema, select_id = NA, ...) {
  id <- if (is.na(select_id)) -1 else select_id
  inst_prefix <- get_from_globalenv("inst_prefix")

  sql_query <- construct_sql_query(sql_script = "value_count",
                        var_schema, inst_prefix, scale, var, var_schema,
                        select_id = select_id)

  db_get_helper(sql_query)
}

#' @export
explore_text_postgres_val.ind <- function(var, scale, parent_string, var_schema,
                                          parent_string_schema, select_id = NA, ...) {
  if (is.na(select_id)) stop("explore_text_postgres_val.ind can't be used when select_id is NA")

}

#' @export
explore_text_postgres_val.default <- function(var, scale, parent_string, var_schema,
                                              parent_string_schema, select_id = NA, ...) {
  if (is.na(select_id)) stop("explore_text_postgres_val.default can't be used when select_id is NA")
}
