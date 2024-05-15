#' Perform a Database Operation
#'
#' This function abstracts the execution of database operations by checking out a
#' connection from a global connection pool, performing the specified operation,
#' and then returning the connection to the pool.
#'
#' @param what <`function`> The database operation to perform. This should be
#' a function capable of accepting a database connection and additional arguments.
#' Usually a DBI function, like `DBI::dbGetQuery`.
#' @param args <`list`> Additional arguments to pass to the `what` function along
#' with the connection.
#'
#' @return Returns the output of the database operation.
#' @export
db_operation <- function(what, args) {
  # Extract a connection from the pool
  db_pool <- get_from_globalenv("db_pool")
  conn <- pool::poolCheckout(db_pool)

  # Apply the query
  out <- tryCatch(do.call(what, c(conn, args)),
                  error = function(e) {
                    pool::poolReturn(conn)
                    stop(e$message)
                  })

  # Return the connection to the pool of connections
  pool::poolReturn(conn)

  # Return the output of the query
  return(out)
}

#' Execute a SQL Query Helper Function
#'
#' A helper function that executes a SQL query using a generalized database
#' operation function. It wraps the \code{db_operation} function for specific use
#' with \code{DBI::dbGetQuery}.
#'
#' @param call <\code{character}> The SQL query string to be executed.
#'
#' @return Returns the result set as a data frame.
#' @export
db_get_helper <- function(call) {
  db_operation(DBI::dbGetQuery, args = call)
}

#' Retrieve Data from Database
#'
#' Constructs and executes a SELECT SQL query to retrieve data from a specified
#' database table. Supports filtering through a WHERE clause.
#'
#' @param select <`character vector`> The names of columns to select.
#' @param from <`character`> The name of the table to select from.
#' @param where <`list`> Optional. A named list where names are column names
#' and values are the values those columns should match. Supports multiple values
#' for a single column.
#' @param schema <`character`> The database schema to use. Defaults to a
#' global environment variable \code{inst_prefix}.
#'
#' @return Returns a data frame containing the requested data.
#' @export
db_get <- function(select, from, where = NULL, schema = get_from_globalenv("inst_prefix")) {

  # Convert select vector into a comma-separated string of quoted column names
  cols <- if (length(select) == 1 && select[1] == "*") {
    "*"
  } else {
    paste0('"', select, '"', collapse = ", ")
  }

  # Start building the SQL query
  call <- sprintf('SELECT %s FROM %s."%s"', cols, schema, from)

  # Process the where list to construct the WHERE clause
  if (!is.null(where) && length(where) > 0) {
    where_clauses <- sapply(names(where), function(field) {
      if (length(where[[field]]) > 1) {
        # For multiple values, construct an IN clause
        values <- paste0("'", where[[field]], "'", collapse = ", ")
        return(sprintf("\"%s\" IN (%s)", field, values))
      } else {
        # For a single value, construct a simple equality clause
        return(sprintf("\"%s\" = '%s'", field, where[[field]]))
      }
    }, USE.NAMES = FALSE)

    where_clause <- paste(where_clauses, collapse = " AND ")
    call <- sprintf("%s WHERE %s", call, where_clause)
  }

  # Execute the query with the constructed SQL
  out <- db_get_helper(call)

  # Switch JSON columns back to list
  data_types <- sapply(out, class)
  cols_json <- which(data_types == "pq_jsonb")
  for (col in cols_json) {
    out[[col]] <- sapply(out[[col]], \(x) list(jsonlite::fromJSON(x)), USE.NAMES = FALSE)
  }

  out

}

