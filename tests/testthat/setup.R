### CURBCUT PACKAGE TEST SETUP #################################################

# Variables present in the .GlobalEnv
assign("all_choropleths",
  value = c("CSD", "CT", "DA", "building", "grid50", "grid100", "grid250", "cmhczone"),
  envir = .GlobalEnv
)

# Default random address
assign("default_random_address",
  value = "845 Sherbrooke",
  envir = .GlobalEnv
)

# All qs and qsm files
data_files <- list.files("resources", full.names = TRUE)
invisible(lapply(data_files[grepl("qsm$", data_files)],
                 qs::qload, env = .GlobalEnv))
invisible(lapply(data_files[grepl("qs$", data_files)],
                 \(x) {
                   object_name <- gsub("(resources/)|(\\.qs)", "", x)
                   assign(object_name, qs::qread(x), envir = .GlobalEnv)
                 }))

