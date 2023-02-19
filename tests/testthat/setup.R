### CURBCUT PACKAGE TEST SETUP #################################################

# Get a minimalist variables table in the global env
assign("variables",
  value = qs::qread("resources/variables.qs"),
  envir = .GlobalEnv
)

# Assign the colours list in the global environment
assign("colours_dfs",
  value = qs::qread("resources/colours_dfs.qs"),
  envir = .GlobalEnv
)

# Variables present in the .GlobalEnv
assign("all_choropleths",
  value = c("CSD", "CT", "DA", "building", "grid"),
  envir = .GlobalEnv
)

# Translation dataframe present in the .GlobalEnv
assign("translation_df",
  value = qs::qread("resources/translation_df.qs"),
  envir = .GlobalEnv
)

# Connection to the sqlite db present in the .GlobalEnv
assign("city_CSD_conn",
  value = DBI::dbConnect(RSQLite::SQLite(), "resources/city_CSD.sqlite"),
  envir = .GlobalEnv
)
