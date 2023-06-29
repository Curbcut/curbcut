### CURBCUT PACKAGE TEST SETUP #################################################

test_setup()

# All qs and qsm files
data_files <- list.files("resources", full.names = TRUE)
invisible(lapply(data_files[grepl("qsm$", data_files)],
  qs::qload,
  env = .GlobalEnv
))
invisible(lapply(
  data_files[grepl("qs$", data_files)],
  \(x) {
    object_name <- gsub("(resources/)|(\\.qs)", "", x)
    assign(object_name, qs::qread(x), envir = .GlobalEnv)
  }
))

# All sqlite files
dbs <- list.files("resources", full.names = TRUE)
dbs <- subset(dbs, grepl(".sqlite$", dbs))

lapply(dbs, \(x) {
  connection_name <- paste0(s_extract("(?<=/).*?(?=\\.)", x), "_conn")
  assign(connection_name, DBI::dbConnect(RSQLite::SQLite(), x), envir = .GlobalEnv)
})

conn <- DBI::dbConnect(RSQLite::SQLite(), "resources/building.sqlite")
tbs <- DBI::dbListTables(conn)
tbs <- tbs[!grepl("city_", tbs)]
lapply(tbs, \(x) DBI::dbRemoveTable(conn, x))
suppressWarnings(DBI::dbGetQuery(conn, "VACUUM"))

