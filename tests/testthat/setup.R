### CURBCUT PACKAGE TEST SETUP #################################################

# Get a minimalist variables table in the global env
assign("variables",
  value = qs::qread("resources/variables.qs"),
  envir = .GlobalEnv
)

# Assign the colours list in the global environment
assign("colours",
  value = qs::qread("resources/colours.qs"),
  envir = .GlobalEnv
)

# Variables present in the .GlobalEnv
assign("all_choropleths",
  value = c("CSD", "CT", "DA", "building", "grid"),
  envir = .GlobalEnv
)
