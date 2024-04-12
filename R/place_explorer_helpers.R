#' Create and concatenate Place Explorer HTML files
#'
#' This function creates a place explorer HTML file by concatenating a header
#' file with a specific place explorer file, based on the provided df, select_ID
#' and language. It also generates a temporary file to store the concatenated
#' output.
#'
#' @param temp_folder <`character`> The temporary folder of the app. By default
#' will grab the `temp_folder` object as it's already supposed to have been assigned
#' in the `global.R` file
#' @param region <`character`> Region under study, e.g. `CMA`.
#' @param scale <`character`> Scale under study
#' @param select_id <`character`> The ID of the selected polygon.
#' @param lang <`character`> String indicating the language of the place explorer
#' file (either "en" for English or "fr" for French). Default is NULL, which
#' will use English if not specified.
#'
#' @return A list containing the following elements:
#' * src: The relative path to the temporary file, to be used when grabbing
#' the static file for the web server.
#' * file: The absolute path to the temporary file.
place_explorer_html_links <- function(temp_folder, region, scale, select_id, lang = NULL) {
  shiny::withProgress(
    message = cc_t("Generating report", lang = lang),
    {
      # Show there is a progress happening
      shiny::incProgress(0.2)

      # Add the head to the place explorer file
      lang <- if (is.null(lang) || lang == "en") "en" else "fr"

      # The link to the place explorer HTML file
      pe_file <- sprintf("www/place_explorer/%s_%s_%s_%s.html", region, scale, select_id, lang)

      # Is place explorer already pre-processed?
      pe_docs <- get_from_globalenv("pe_docs")

      # Make a temporary file
      tmpfile <- tempfile(
        pattern = "placeex_tmp",
        tmpdir = temp_folder,
        fileext = ".html"
      )

      shiny::incProgress(0.3)

      if (pe_file %in% pe_docs) {
        # Get the full paths of the style (header of HTML) and the place explorer HTML file
        head <- normalizePath("www/place_explorer/header.html")
        pef <- normalizePath(pe_file)

        # Concatenate both in the temporary file
        if (Sys.info()[["sysname"]] == "Windows") {
          # the gsub() function is used to replace forward slashes with backslashes,
          # which is specific to Windows
          head <- gsub("/", "\\\\", head)
          pef <- gsub("/", "\\\\", pef)
          shell(sprintf("type %s %s > %s", head, pef, tmpfile))
        } else {
          system(sprintf("cat %s %s > %s", head, pef, tmpfile))
        }
      } else {
        # # If the place explorer is not already pre-processed, process it on the spot.
        # place_explorer_create_html(
        #   tmpfile = tmpfile, region = region, df = df,
        #   select_id = select_id, lang = lang
        # )
      }

      shiny::incProgress(0.3)

      shiny::incProgress(0.2)
    }
  )

  # Return the `src` for when we want to grab the static file for the web
  # server. `file` is the absolute path to the temporary file
  return(list(
    src = tmpfile,
    file = tmpfile
  ))
}
