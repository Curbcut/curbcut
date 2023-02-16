### CURBCUT PACKAGE TEST SETUP #################################################

# Create a minimalist variables table
assign("variables", value = qs::qread("resources/variables.qs"),
       envir = .GlobalEnv)

# Variables present in the .GlobalEnv
assign("all_choropleths", value = c("CSD", "CT", "DA", "building"),
       envir = .GlobalEnv)

