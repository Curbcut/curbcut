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
assign("city_CT_conn",
  value = DBI::dbConnect(RSQLite::SQLite(), "resources/city_CT.sqlite"),
  envir = .GlobalEnv
)
assign("city_DA_conn",
  value = DBI::dbConnect(RSQLite::SQLite(), "resources/city_DA.sqlite"),
  envir = .GlobalEnv
)

# Map zoom levels
qs::qload("resources/map_zoom_levels.qsm", env = .GlobalEnv)

# Scales dictionary
assign("scales_dictionary",
  value = qs::qread("resources/scales_dictionary.qs"),
  envir = .GlobalEnv
)

# Regions dictionary
assign("regions_dictionary",
  value = qs::qread("resources/regions_dictionary.qs"),
  envir = .GlobalEnv
)

# Modules
assign("modules",
  value = qs::qread("resources/modules.qs"),
  envir = .GlobalEnv
)

# Default random address
assign("default_random_address",
  value = "845 Sherbrooke",
  envir = .GlobalEnv
)

# Postal codes
assign("postal_codes",
  value = qs::qread("resources/postal_codes.qs"),
  envir = .GlobalEnv
)

# Few regions
qs::qload("resources/city.qsm", env = .GlobalEnv)
qs::qload("resources/island.qsm", env = .GlobalEnv)

# Stories for poi test
qs::qload("resources/stories.qsm", env = .GlobalEnv)
