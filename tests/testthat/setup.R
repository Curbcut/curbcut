### CURBCUT PACKAGE TEST SETUP #################################################

# Create a minimalist variables table
assign("variables", value = qs::qread("resources/variables.qs"),
       envir = .GlobalEnv)

# variables <- get0("variables", .GlobalEnv)
# if (is.null(variables))
#   stop(paste0("Load the `variables` table in the global environment before ",
#               "runing tests to mimic a Curbcut instance. variables <- qs::qre",
#               "ad('tests/testthat/resources/variables.qs')"))
