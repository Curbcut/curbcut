#' Generate an AWS RDS Authentication Token
#'
#' This function generates an authentication token for AWS RDS using the AWS CLI.
#' It requires certain environment variables to be set for the function to succeed.
#' These include AWS credentials and RDS instance details.
#'
#' @return A character string containing the generated authentication token.
aws_generate_rds_auth_token <- function() {
  # Ensure necessary environment variables are set
  required_vars <- c("CURBCUT_PROD_DB_HOST", "CURBCUT_PROD_DB_PORT",
                     "CURBCUT_PROD_DB_USER", "CURBCUT_PROD_DB_REGION")
  missing_vars <- required_vars[!nzchar(Sys.getenv(required_vars))]

  if (length(missing_vars) > 0) {
    stop("Missing environment variables: ", paste(missing_vars, collapse = ", "), call. = FALSE)
  }

  # Ensure necessary system variables are set
  required_vars <- c("AWS_ACCESS_KEY_ID", "AWS_DEFAULT_REGION", "AWS_SECRET_ACCESS_KEY")
  missing_vars <- required_vars[!nzchar(Sys.getenv(required_vars))]

  if (length(missing_vars) > 0) {
    stop(paste0("Missing SYSTEM environment variables (Not .Renviron!), which ",
                "are necessary to produce a valid token: "),
         paste(missing_vars, collapse = ", "), call. = FALSE)
  }

  # Command to generate the token
  cmd <- paste("aws rds generate-db-auth-token",
               "--hostname", Sys.getenv("CURBCUT_PROD_DB_HOST"),
               "--port", Sys.getenv("CURBCUT_PROD_DB_PORT"),
               "--username", Sys.getenv("CURBCUT_PROD_DB_USER"),
               "--region", Sys.getenv("CURBCUT_PROD_DB_REGION"))

  # Execute the command and retrieve the token
  token <- tryCatch({
    system(cmd, intern = TRUE)
  }, error = function(e) {
    stop("Failed to generate RDS auth token: ", e$message, call. = FALSE)
  })

  if (!nzchar(token)) {
    stop("Generated token is empty.", call. = FALSE)
  }

  return(token)
}

#' Retrieve or Download the SSL Root Certificate
#'
#' Depending on the execution environment (production or development),
#' this function retrieves the path to an existing SSL root certificate
#' or downloads it from a specified AWS S3 bucket.
#'
#' @return A character string with the path to the SSL root certificate.
aws_get_rootcert <- function() {

  # Are we in production?
  prod <- in_prod()

  # When in production, the certificate must be in this Docker directory
  if (prod) {

    root_cert_path <- if (Sys.info()["sysname"] == "Windows") {
      "etc/ssl/certs/global-bundle.pem" # Adjusted for Windows
    } else {
      "/etc/ssl/certs/global-bundle.pem" # Unix-like path. The first
      # / is necessary for the docker image, as it's the absolute path from the root
      # of the filesystem, where the bundle has been copied.
    }

    if (!file.exists(root_cert_path)) {
      stop("SSL root certificate not found at ", root_cert_path, call. = FALSE)
    }
    return(root_cert_path)
  }

  # Ensure necessary environment variables are set (when developping locally)
  required_vars <- c("CURBCUT_BUCKET_DEFAULT_REGION", "CURBCUT_BUCKET_ACCESS_ID",
                     "CURBCUT_BUCKET_ACCESS_KEY")
  missing_vars <- required_vars[!nzchar(Sys.getenv(required_vars))]
  if (length(missing_vars) > 0) {
    stop("Missing environment variables for downloading the SSL root certificate: ",
         paste(missing_vars, collapse = ", "), call. = FALSE)
  }

  bundle_path <- tempfile(fileext = ".pem")
  bundle <- tryCatch({
    aws.s3::save_object(region = Sys.getenv("CURBCUT_BUCKET_DEFAULT_REGION"),
                        key = Sys.getenv("CURBCUT_BUCKET_ACCESS_ID"),
                        secret = Sys.getenv("CURBCUT_BUCKET_ACCESS_KEY"),
                        object = "global-bundle.pem",
                        bucket = "curbcut.misc",
                        file = bundle_path,
                        overwrite = TRUE)
  }, error = function(e) {
    stop("Failed to download SSL root certificate: ", e$message, call. = FALSE)
  })

  return(bundle)
}

#' Establish a Connection to AWS RDS with SSL
#'
#' Uses the `DBI` package to establish a connection to an AWS RDS instance.
#' It automatically generates an RDS authentication token and determines
#' the SSL root certificate path for the connection.
#'
#' @return A `DBIConnection` object representing the connection to the RDS instance.
aws_connect <- function() {
  conn <- tryCatch({
    DBI::dbConnect(RPostgres::Postgres(),
                   host = Sys.getenv("CURBCUT_PROD_DB_HOST"),
                   port = Sys.getenv("CURBCUT_PROD_DB_PORT"),
                   dbname = Sys.getenv("CURBCUT_PROD_DB_NAME"),
                   user = Sys.getenv("CURBCUT_PROD_DB_USER"),
                   password = aws_generate_rds_auth_token(),
                   sslmode = 'verify-full',
                   sslrootcert = aws_get_rootcert())
  }, error = function(e) {
    stop("Failed to connect to the database: ", e$message, call. = FALSE)
  })

  return(conn)
}

#' Database Connection Pool with Short-Lived Token Management
#'
#' A reimplementation of \code{\link[pool]{dbPool}} designed to handle database
#' connections using short-lived authentication tokens. It includes mechanisms
#' for token validation, regeneration, and secure storage within the function's scope.
#'
#' @param drv The database driver.
#' @param ... Additional parameters passed to the database driver's connect method.
#' @param token_default_duration Duration in seconds before the token expires.
#' Defaults to 14 * 60 (14 minutes).
#' @param minSize Minimum number of connections in the pool.
#' @param maxSize Maximum number of connections in the pool. Set to \code{Inf}
#' for no limit.
#' @param idleTimeout Time in seconds a connection can remain idle before being
#' closed, to free resources.
#' @param validationInterval Time in seconds to wait between validation checks
#' on idle connections.
#'
#' @param validateQuery SQL query used to validate a connection is still active.
#' @return An object representing the database connection pool, with enhanced
#' token management capabilities.
cc_dbPool <- function(drv,
                      ...,
                      token_default_duration = 14 * 60,
                      minSize = 1,
                      maxSize = Inf,
                      idleTimeout = 60,
                      validationInterval = 60,
                      validateQuery = NULL) {

  # Initialize token cache within the function
  token_cache <- new.env(parent = emptyenv())

  # Function to check if the cached token is still valid
  is_token_valid <- function() {
    if (!exists("expiration", envir = token_cache)) {
      return(FALSE)
    }
    Sys.time() < token_cache$expiration
  }

  # Generate a new token and update cache with new token and its expiration time
  generate_token <- function() {
    new_token <- aws_generate_rds_auth_token()
    token_cache$token <- new_token
    token_cache$expiration <- Sys.time() + token_default_duration
    return(new_token)
  }

  # Function to generate or retrieve a valid token
  get_valid_token <- function() {
    if (is_token_valid()) {
      return(token_cache$token)
    } else {
      generate_token()
    }
  }

  is_authentication_error <- function(error_message) {
    # Patterns or keywords indicative of authentication errors
    grepl("authentication failed", error_message, ignore.case = TRUE) ||
      grepl("token expired", error_message, ignore.case = TRUE) ||
      grepl("access denied", error_message, ignore.case = TRUE)
  }

  # The actual factory function used by the pool
  factory <- function() {
    tryCatch(DBI::dbConnect(drv, password = get_valid_token(), ...),
             # If there's an error to connect, generate & save a new token
             error = \(e) DBI::dbConnect(drv, password = generate_token(), ...))
  }


  state <- new.env(parent = emptyenv())
  state$validateQuery <- validateQuery

  pool::poolCreate(
    factory = factory,
    state = state,
    minSize = minSize,
    maxSize = maxSize,
    idleTimeout = idleTimeout,
    validationInterval = validationInterval
  )
}

#' AWS RDS Database Connection Pool Initialization
#'
#' Initializes a connection pool for an AWS RDS database using \code{cc_dbPool}.
#' It configures the pool based on environment variables and specific AWS settings,
# including SSL mode and root certificate for secure connections.
#'
#' @return A pooled database connections object on success, or stops with an error
#' message if unable to connect.
aws_pool <- function() {
  conn <- tryCatch({
    # Using cc_dbPool, a repurpose
    cc_dbPool(
      drv = RPostgres::Postgres(),
      host = Sys.getenv("CURBCUT_PROD_DB_HOST"),
      port = Sys.getenv("CURBCUT_PROD_DB_PORT"),
      dbname = Sys.getenv("CURBCUT_PROD_DB_NAME"),
      user = Sys.getenv("CURBCUT_PROD_DB_USER"),
      sslmode = 'verify-full',
      sslrootcert = aws_get_rootcert(),
      minSize = if (in_prod()) 5 else 1,
      maxSize = Inf,
      idleTimeout = 60*10, # When created, the pool will initialize minSize
      # connections, and keeps them around until they’re requested. If all the
      # idle connections are taken up when another request for a connection
      # comes up, the pool will create a new connection. It’ll keep doing
      # this as needed until it gets to maxSize connections at which point
      # it will error. Any connection that is created when we’re over minSize
      # will have a timer attached to it: from the moment it is returned back
      # to the pool, a countdown of idleTimeout seconds will start
      validationInterval = 60*4,
      validateQuery = "SELECT 1"
    )
  }, error = function(e) {
    stop("Failed to connect to the database: ", e$message, call. = FALSE)
  })

  return(conn)
}
