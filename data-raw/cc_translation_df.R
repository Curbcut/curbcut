## CURBCUT TRANSLATION DATAFRAME ###############################################
library(tibble)


cc_translation_df <-
  # General UI --------------------------------------------------------------

  tibble(
    en = character(),
    fr = character()
  ) |>
  add_row(
    en = "----",
    fr = "----"
  ) |>
  add_row(
    en = "Select a year",
    fr = "Sélectionnez une année"
  ) |>
  add_row(
    en = "Select two years",
    fr = "Sélectionnez deux années"
  ) |>
  add_row(
    en = "Compare dates",
    fr = "Comparer deux dates"
  ) |>
  add_row(
    en = "Compare",
    fr = "Comparez"
  ) |>
  add_row(
    en = "[LEARN MORE]",
    fr = "[EN SAVOIR PLUS]"
  ) |>
  add_row(
    en = "Did you know?",
    fr = "Le saviez-vous?"
  ) |>
  add_row(
    en = "Explore",
    fr = "Explorez"
  ) |>
  add_row(
    en = "Auto-scale",
    fr = "Échelle automatique"
  ) |>
  add_row(
    en = "No postal code found for `{address}`",
    fr = "Pas de code postal trouvé pour `{address}`"
  ) |>
  add_row(
    en = "Curbcut does not currently support mobile phones. Please visit from a computer.",
    fr = "Curbcut ne supporte pas actuellement les téléphones mobiles. Veuillez visiter le site à partir d'un ordinateur."
  ) |>
  add_row(
    en = "Clear selection",
    fr = "Effacer la sélection"
  )
  # Legend ------------------------------------------------------------------

  add_row(
    en = "Low",
    fr = "Bas"
  ) |>
  add_row(
    en = "High",
    fr = "Haut"
  ) |>
  add_row(
    en = "Legend",
    fr = "Légende"
  ) |>
  add_row(
    en = "Both low",
    fr = "Les deux faibles"
  ) |>
  add_row(
    en = "high only",
    fr = "élevé seulement"
  ) |>
  add_row(
    en = "Both high",
    fr = "Les deux élevés"
  ) |>
  # Tutorial ----------------------------------------------------------------

  add_row(
    en = paste0(
      "Curbcut is designed as a series of pages that explore ",
      "a given theme. Here you will find information about the ",
      "theme and the data used on the page."
    ),
    fr = paste0(
      "Curbcut est conçu comme une série de pages qui explorent ",
      "un thème donné. Vous trouverez ici des informations sur ",
      "le thème et les données utilisées sur la page."
    )
  ) |>
  add_row(
    en = paste0(
      "All the maps within Curbcut are interactive and let users ",
      "scroll, zoom in and out, and click into areas for more ",
      "information."
    ),
    fr = paste0(
      "Toutes les cartes de Curbcut sont interactives et ",
      "permettent aux utilisateurs de les faire glisser, de ",
      "les agrandir ou de les réduire, et de cliquer sur des ",
      "zones pour obtenir plus d'informations."
    )
  ) |>
  add_row(
    en = paste0(
      "The interactive nature of our maps means that you can ",
      "choose which variables you wish to explore through the ",
      "widgets located here."
    ),
    fr = paste0(
      "La nature interactive de nos pages signifie que vous ",
      "pouvez choisir les variables que vous souhaitez explorer ",
      "grâce aux éléments (widgets) situés ici."
    )
  ) |>
  add_row(
    en = paste0(
      "The legend displays how the selected variable(s) is ",
      "being visually represented on the map with different ",
      "colours."
    ),
    fr = paste0(
      "La légende montre comment la ou les variables ",
      "sélectionnées sont représentées visuellement sur la ",
      "carte à l'aide de différentes couleurs."
    )
  ) |>
  add_row(
    en = paste0(
      "The level of zoom determines the spatial scale of what ",
      "you see or you can click off \u2018auto-scale\u2019 and manually ",
      "choose the spatial scale with the slider."
    ),
    fr = paste0(
      "Le niveau de zoom détermine l'échelle spatiale de ce que ",
      "vous voyez. Vous pouvez également désactiver l'option 'échelle ",
      "automatique' et choisir manuellement l'échelle spatiale à ",
      "l'aide de la barre de défilement."
    )
  ) |>
  add_row(
    en = paste0(
      "This function allows you to select a variable to compare ",
      "with the one selected on the left-hand panel. We will show ",
      "you the potential relationship between the variables of ",
      "your choice."
    ),
    fr = paste0(
      "Cette fonctionnalité vous permet de sélectionner une variable ",
      "à comparer avec celle sélectionnée dans le panneau de gauche. ",
      "Nous vous montrerons la relation potentielle entre les ",
      "variables choisies"
    )
  ) |>
  add_row(
    en = paste0(
      "You will see meaningful information here about the variables ",
      "selected and any potential relationships between them."
    ),
    fr = paste0(
      "Vous obtiendrez ici des informations significatives sur ",
      "les variables sélectionnées et sur les relations potentielles ",
      "entre elles."
    )
  ) |>
  add_row(
    en = paste0(
      "These buttons allow you to switch between map view and ",
      "data view. Both show the same information, either ",
      "spatialized or in table form."
    ),
    fr = ""
  ) |>
  add_row(
    en = paste0(
      "Congratulations on completing the tutorial! If you wouldd like ",
      "to revisit any part of it, or run through the entire ",
      "tutorial again, simply click on this button."
    ),
    fr = paste0(
      "Félicitations pour avoir complété le tutoriel ! Si vous ",
      "souhaitez revoir une partie ou l'ensemble du tutoriel, ",
      "il vous suffit de cliquer sur ce bouton."
    )
  ) |>
  add_row(
    en = "Title text",
    fr = "Intitulé"
  ) |>
  add_row(
    en = "Left-hand widgets",
    fr = "Éléments (widgets)"
  ) |>
  add_row(
    en = "Zoom",
    fr = "Zoom"
  ) |>
  add_row(
    en = "Compare menu",
    fr = "Menu de comparaison"
  ) |>
  add_row(
    en = "View switch",
    fr = "Changer de vue"
  ) |>
  add_row(
    en = "Tutorial",
    fr = "Tutoriel"
  ) |>
  add_row(
    en = "Next",
    fr = "Suivant"
  ) |>
  add_row(
    en = "Back",
    fr = "Retour"
  ) |>
  add_row(
    en = "Done",
    fr = "Terminé"
  ) |>
  # Misc --------------------------------------------------------------------

  add_row(
    en = "Population",
    fr = "Population"
  ) |>
  add_row(
    en = "Households",
    fr = "Ménages"
  ) |>
  add_row(
    en = "Visit the place explorer",
    fr = "Visiter l'explorateur de lieux"
  ) |>
  add_row(
    en = "The spatial organization of the data is the %s scale.",
    fr = "L'organisation spatiale des données est l'échelle '%s'."
  ) |>
  add_row(
    en = "Overview",
    fr = "Vue d'ensemble"
  ) |>
  add_row(
    en = "Exporting data",
    fr = "Exporter des données"
  ) |>
  add_row(
    en = "Map",
    fr = "Carte"
  ) |>
  add_row(
    en = "Table",
    fr = "Tableau"
  ) |>
  add_row(
    en = "Portrait",
    fr = "Portrait"
  ) |>
  add_row(
    en = "Download '.csv'",
    fr = "Télécharger '.csv'"
  ) |>
  add_row(
    en = "Download '.shp'",
    fr = "Télécharger '.shp'"
  ) |>
  add_row(
    en = "No postal code found for `{input$address_searched}`",
    fr = "Aucun code postal n'a été trouvé pour `{input$address_searched}`"
  ) |>
  add_row(
    en = "Enter postal code or click on the map",
    fr = "Entrez un code postal ou cliquez sur la carte"
  ) |>
  add_row(
    en = "Back to the place explorer",
    fr = "Retour à l'explorateur de lieux"
  ) |>
  add_row(
    en = "Download regional portrait",
    fr = "Télécharger le portrait régional"
  ) |>
  add_row(
    en = "Change default region",
    fr = "Modifier la région par défaut"
  ) |>
  add_row(
    en = paste0(
      "Enter and save a default location (postal ",
      "code or address)"
    ),
    fr = "Saisir et enregistrer un emplacement par défaut (code postal ou adresse)"
  ) |>
  add_row(
    en = paste0(
      "Default location will be saved until ",
      "manually cleared from advanced options"
    ),
    fr = "L'emplacement par défaut sera sauvegardé jusqu'à ce qu'il soit effacé manuellement dans les options avancées."
  ) |>
  add_row(
    en = "Clear default location",
    fr = "Effacer l'emplacement par défaut"
  ) |>
  add_row(
    en = "Postal code `{postal_c}` isn't within an available region.",
    fr = "Code postal `{postal_c}` ne se trouve pas dans une région disponible."
  ) |>
  add_row(
    en = "Postal code `{postal_c}` saved as default.",
    fr = "Code postal `{postal_c}` sauvegardé par défaut."
  ) |>
  add_row(
    en = "Search `{address}` wasn't found within an available region.",
    fr = "La recherche `{adresse}` n'a pas été trouvée dans une région disponible."
  ) |>
  add_row(
    en = "Address `{address}` isn't within an available region.",
    fr = "L'adresse `{adresse}` ne se trouve pas dans une région disponible."
  ) |>
  add_row(
    en = "No zone has been found in a 1km radius of the provided address.",
    fr = "Aucune zone n'a été trouvée dans un rayon de 1 km autour de l'adresse fournie."
  ) |>
  add_row(
    en = "Address `{address}` saved as default.",
    fr = "L'adresse `{adresse}` est sauvegardée par défaut."
  ) |>
  add_row(
    en = "Advanced options",
    fr = "Options avancées"
  ) |>
  add_row(
    en = "Dismiss",
    fr = "Fermer"
  ) |>
  add_row(
    en = "Default location successfully cleared",
    fr = "L'emplacement par défaut a été supprimé avec succès"
  ) |>
  add_row(
    en = "Learn more",
    fr = "En savoir plus"
  ) |>
  add_row(
    en = "Visit the {stories_page} page",
    fr = "Visiter la page {stories_page}"
  ) |>
  add_row(
    en = "Comparison requires two different dates.",
    fr = "La comparaison nécessite deux dates différentes."
  ) |>
  add_row(
    en = paste0(
      "Displayed data for <b>{var_left_title}</b> is for the ",
      "closest available year <b>({left_year})</b>."
    ),
    fr = paste0(
      "Les données affichées pour <b>{var_left_title}</b> ",
      "correspondent à l'année la plus proche <b>({left_year})</b>."
    )
  ) |>
  add_row(
    en = paste0(
      "Displayed data for <b>{var_right_title}</b> is for the ",
      "closest available year <b>({right_year})</b>."
    ),
    fr = paste0(
      "Les données affichées pour <b>{var_right_title}</b> ",
      "correspondent à l'année la plus proche <b>({right_year})</b>."
    )
  ) |>
  # Explore panel -----------------------------------------------------------

  # Context and q5
add_row(
  en = "This value",
  fr = "Cette valeur"
) |>
  add_row(
    en = "This is %s for %s",
    fr = "C'est %s pour %s"
  ) |>
  add_row(
    en = "%s %s is higher than in %s of other %s %s",
    fr = "%s %s est plus élevé que dans %s des autres %s %s"
  ) |>
  add_row(
    en = "%s <i>(Data from %s.)</i>",
    fr = "%s <i>(Données de %s.)</i>"
  ) |>
  add_row(
    en = "around %s",
    fr = "autour du %s"
  ) |>
  add_row(
    en = "Dissemination area around %s",
    fr = "Aire de diffusion autour du %s"
  ) |>
  add_row(
    en = "in {p_start}",
    fr = "dans {p_start}"
  ) |>
  add_row(
    en = "in {name}",
    fr = "dans {name}"
  ) |>
  add_row(
    en = "{name_2} of {name}",
    fr = "{name_2} {name}"
  ) |>
  add_row(
    en = "we currently don't have information regarding %s",
    fr = "nous n'avons présentement pas d'informations concernant %s"
  ) |>
  add_row(
    en = "`%s` to `%s`",
    fr = "`%s` à `%s`"
  ) |>
  add_row(
    en = "a higher-than-average",
    fr = "un niveau plus élevé que la moyenne en matière de"
  ) |>
  add_row(
    en = "%s is %s",
    fr = "%s est %s"
  ) |>
  # bivar -------------------------------------------------------------------

add_row(
  en = "Pearson's r: %s",
  fr = "r de Pearson : %s"
) |>
  add_row(
    en = "The first value",
    fr = "La première valeur"
  ) |>
  add_row(
    en = "The second value",
    fr = "La deuxième valeur"
  ) |>
  add_row(
    en = "Spearman's rho: %s",
    fr = "rho de Spearman : %s"
  ) |>
  add_row(
    en = "%s and %s",
    fr = "%s et %s"
  ) |>
  add_row(
    en = "positive",
    fr = "positive"
  ) |>
  add_row(
    en = "negative",
    fr = "négative"
  ) |>
  add_row(
    en = "%s %s correlation",
    fr = "corrélation %s et %s"
  ) |>
  add_row(
    en = "almost always have",
    fr = "ont presque toujours"
  ) |>
  add_row(
    en = "tend to have",
    fr = "tendent à avoir"
  ) |>
  add_row(
    en = "often have _X_, although with many exceptions",
    fr = "ont souvent _X_, bien qu'à de nombreuses exceptions près"
  ) |>
  add_row(
    en = "strong",
    fr = "forte"
  ) |>
  add_row(
    en = "moderate",
    fr = "modérée"
  ) |>
  add_row(
    en = "weak",
    fr = "faible"
  ) |>
  add_row(
    en = "effectively no relationship",
    fr = "aucune relation effective"
  ) |>
  add_row(
    en = "effectively no correlation",
    fr = "pas de corrélation effective"
  ) |>
  add_row(
    en = "higher",
    fr = "un niveau plus élevé de"
  ) |>
  add_row(
    en = "lower",
    fr = "un niveau plus bas de"
  ) |>
  add_row(
    en = "a higher",
    fr = "un niveau plus élevé de"
  ) |>
  add_row(
    en = "a lower",
    fr = "un niveau plus bas de"
  ) |>
  add_row(
    en = "%s, %s with %s %s %s.",
    fr = "%s, les %s avec %s %s %s."
  ) |>
  add_row(
    en = "%s, %s with %s %s %s %s %s.",
    fr = "%s, les %s avec %s %s %s %s %s."
  ) |>
  add_row(
    en = "There is a %s (%s) between these two variables.",
    fr = "Il existe une %s (%s) entre ces deux variables."
  ) |>
  add_row(
    en = "<p><b>STRONG CORRELATION</b>%s",
    fr = "<p><b>FORTE CORRÉLATION</b>%s"
  ) |>
  add_row(
    en = "%s<p>%s, %s and %s.",
    fr = "%s<p>%s, %s et %s."
  ) |>
  add_row(
    en = "%s is higher than in %s of other %s",
    fr = "%s est plus élevé que dans %s des autres %s"
  ) |>
  add_row(
    en = "which is %s for %s",
    fr = "ce qui est %s pour %s"
  ) |>
  add_row(
    en = "By contrast",
    fr = "En revanche"
  ) |>
  add_row(
    en = "Similarly",
    fr = "De même"
  ) |>
  add_row(
    en = "%s, there is %s (%s) between %s and %s in %s.",
    fr = "%s, il existe une %s (%s) entre %s et %s en %s."
  ) |>
  # Delta -------------------------------------------------------------------


add_row(
  en = "the value",
  fr = "la valeur"
) |>
  add_row(
    en = "%s percentage points (%sx)",
    fr = "%s points de pourcentage (%sx)"
  ) |>
  add_row(
    en = "This number",
    fr = "Ce nombre"
  ) |>
  add_row(
    en = "increased",
    fr = "augmenté"
  ) |>
  add_row(
    en = "decreased",
    fr = "diminué"
  ) |>
  add_row(
    en = "%s, %s changed from %s in %s to %s in %s.",
    fr = "%s, %s a changé de %s en %s à %s en %s."
  ) |>
  add_row(
    en = "%s has %s by %s between these years.",
    fr = "%s a %s de %s entre ces deux années."
  ) |>
  add_row(
    en = "%s, %s has remained %s between %s and %s.",
    fr = "%s, %s est resté de %s entre %s et %s."
  ) |>
  add_row(
    en = "the percentage of %s that %s",
    fr = "le pourcentage de %s qui %s"
  ) |>
  add_row(
    en = "increase",
    fr = "augmentation"
  ) |>
  add_row(
    en = "decrease",
    fr = "diminution"
  ) |>
  add_row(
    en = "exceptionally small",
    fr = "exceptionnellement faible"
  ) |>
  add_row(
    en = "unusually small",
    fr = "très faible"
  ) |>
  add_row(
    en = "just about average",
    fr = "à peu près dans la moyenne"
  ) |>
  add_row(
    en = "unusually large",
    fr = "très forte"
  ) |>
  add_row(
    en = "exceptionnally large",
    fr = "exceptionnellement forte"
  ) |>
  add_row(
    en = "The slight",
    fr = "La légère"
  ) |>
  add_row(
    en = "This",
    fr = "Cette"
  ) |>
  add_row(
    en = "%s %s is %s for %s.",
    fr = "%s %s est %s pour %s."
  ) |>
  add_row(
    en = "The change in %s %s between %s and %s is larger than in %s of other %s between the same years.",
    fr = "La variation de la/du %s entre %s et %s est plus élevé que dans %s des autres %s entre les mêmes années."
  ) |>
  # Delta bivar -------------------------------------------------------------

add_row(
  en = "the first value",
  fr = "la première valeur"
) |>
  add_row(
    en = "the second value",
    fr = "la deuxième valeur"
  ) |>
  add_row(
    en = "%s from %s to %s, there is %s (%s) between the change in %s and the change in %s in %s.",
    fr = "%s de %s à %s, il n'existe %s (%s) entre la variation dans %s et la variation dans %s dans %s."
  ) |>
  add_row(
    en = " change</b>",
    fr = " variation</b>"
  ) |>
  add_row(
    en = "%s in %s",
    fr = "%s dans %s"
  ) |>
  add_row(
    en = "%s from %s to %s, %s with %s in %s %s.",
    fr = "%s de %s à %s, %s avec %s dans %s %s."
  ) |>
  add_row(
    en = "%s from %s to %s, %s with %s in %s %s have had %s in %s.",
    fr = "%s de %s à %s, %s avec %s dans %s %s %s dams %s."
  ) |>
  add_row(
    en = "There is a %s (%s) between the change in these two variables between these years.",
    fr = "Il existe une %s (%s) entre l'évolution de ces deux variables entre ces années."
  ) |>
  add_row(
    en = "%s<p>%s, %s changed from %s in %s to %s in %s.",
    fr = "%s<p>%s, %s est passé de %s en %s à %s en %s."
  ) |>
  add_row(
    en = "%s %s changed from %s in %s to %s in %s.",
    fr = "%s %s est passé de %s en %s à %s en %s."
  ) |>
  add_row(
    en = "an exceptionally small change",
    fr = "un changement exceptionnellement faible"
  ) |>
  add_row(
    en = "an unusually small change",
    fr = "un changement très faible"
  ) |>
  add_row(
    en = "a just about average change",
    fr = "un changement à peu près dans la moyenne"
  ) |>
  add_row(
    en = "an unusually large change",
    fr = "un changement très fort"
  ) |>
  add_row(
    en = "an exceptionnally large change",
    fr = "un changement exceptionnellement fort"
  ) |>
  add_row(
    en = "The change in %s %s from %s to %s is larger than %s other %s, which is %s for %s.",
    fr = "Le changement dans %s %s de %s à %s est plus élevé que dans %s des autres %s, ce qui est %s pour %s."
  ) |>
  add_row(
    en = "%s, the change in %s between the same years is larger than %s of other %s, which is %s for %s.",
    fr = "%s, le changement dans %s entre ces deux mêmes années est plus élevés que %s des autres %s, ce qui est %s pour %s."
  ) |>
  add_row(
    en = "larger change",
    fr = "une variation plus élevée"
  ) |>
  add_row(
    en = "smaller change",
    fr = "une variation plus faible"
  ) |>
  add_row(
    en = "a larger change",
    fr = "une variation plus élevée"
  ) |>
  add_row(
    en = "a smaller change",
    fr = "une variation plus faible"
  ) |>
  # Panel view --------------------------------------------------------------

  add_row(
    en = "the change in %s between %s and %s",
    fr = "le changement dans %s entre %s et %s"
  ) |>
  add_row(
    en = paste0(
      "<p>The minimum and maximum values for %s are respectively %s and %s. ",
      "The data points have an average value (mean) of %s. Additionally, ",
      "the standard deviation, which measures the dispersion or spread ",
      "around this mean, is %s. (Approximately two thirds of data points ",
      "lie within one standard deviation of the mean.)</p>"
    ),
    fr = paste0(
      "<p>Les valeurs minimale et maximale pour %s sont respectivement %s et ",
      "%s. Les points de données ont une valeur moyenne de %s. De plus, l'éca",
      "rt type, qui mesure la dispersion ou l'étendue autour de cette moyenne",
      ", est de %s. (Environ deux tiers des points de données se situent à mo",
      "ins d'un écart-type de la moyenne).</p>"
    )
  ) |>
  add_row(
    en = paste0(
      "The data comes from the %s Canadian census and has ",
      "been retrieved from <a href = 'https://censusma",
      "pper.ca/', target = '_blank'>censusmapper.ca</a> ",
      "using the R <a href = 'https://cran.r-project.org",
      "/web/packages/cancensus/', target = '_blank'>canc",
      "ensus</a> package."
    ),
    fr = "Les données proviennent du recensement canadien de %s et ont été extraites de <a href = 'https://censusmapper.ca/', target = '_blank'>censusmapper.ca</a> à l'aide de la librairie R <a href = 'https://cran.r-project.org/web/packages/cancensus/', target = '_blank'>cancensus</a>."
  ) |>
  add_row(
    en = "vectors and their",
    fr = "vecteurs"
  ) |>
  add_row(
    en = "vector and its",
    fr = "le vecteur"
  ) |>
  add_row(
    en = "parent vectors",
    fr = "vecteurs parents"
  ) |>
  add_row(
    en = "parent vector",
    fr = "vecteur parent"
  ) |>
  add_row(
    en = paste0(
      "To calculate %s, we extract the %s corresponding ",
      "%s. Here, the term 'parent vector' refers to ",
      "the data source that represents %s, which we use ",
      "as a basis to compute %s."
    ),
    fr = "Pour calculer %s, nous extrayons %s et son/ses correspondant %s. Ici, le terme `vecteur parent` fait référence à la source de données qui représente %s, que nous utilisons comme base pour calculer %s."
  ) |>
  add_row(
    en = "The source of the data is `%s`.",
    fr = "La donnée provient de `%s`."
  ) |>
  add_row(
    en = "%s has been spatially interpolated from %s.",
    fr = "%s a été spatialement interpolé à partir des %s."
  ) |>
  add_row(
    en = "{source_vec} {v}",
    fr = "{v} {source_vec}"
  ) |>


  # Place explorer ----------------------------------------------------------

add_row(
  en = "Its value is higher than the WHO's guideline value of 53. ",
  fr = "Sa valeur est supérieure à la valeur recommandée par l'OMS, qui est de 53. "
) |>
  add_row(
    en = "%s (count)",
    fr = "%s (compte)"
  ) |>
  add_row(
    en = "No data.",
    fr = "Aucune donnée."
  ) |>
  add_row(en = "Air pollution",
          fr = "Pollution de l'air") |>
  add_row(en = "Vegetation",
          fr = "Végétation") |>
  add_row(en = "Sustainable transport",
          fr = "Mobilité durable") |>
  add_row(en = "Housing",
          fr = "Logement") |>
  add_row(en = "Active living",
          fr = "Vie active") |>
  add_row(en = "Nitrogen dioxide (ppb)",
          fr = "Dioxyde d'azote (ppb)") |>
  add_row(en = "Normalized Difference Vegetation Index",
          fr = "Indice de végétation par différence normalisée") |>
  add_row(en = "Walk, cycle or transit to work (%)",
          fr = "Se rendent au travail à pied, à vélo ou en transport en commun (%)") |>
  add_row(en = "Single-detached housing (%)",
          fr = "Maison individuelle non attenante (%)") |>
  add_row(en = "Active living potential index",
          fr = "Indice de potentiel de vie active") |>
  add_row(en = "{data_rank} in terms of level of <a href = 'https://www.canuedata.ca/tmp/CANUE_METADATA_NO2LUR_A_YY.pdf'>NO2</a> pollution. {higher_than_threshold}(NO2 = {pretty_data_var}, data from {data_date})",
          fr = "{data_rank} en termes de niveau de pollution de <a href = 'https://www.canuedata.ca/tmp/CANUE_METADATA_NO2LUR_A_YY.pdf'>NO2</a>. {higher_than_threshold}(NO2 = {pretty_data_var}, données de {data_date})") |>
  add_row(en = "{data_rank} in terms of vegetation (<a href='https://canue.ca/wp-content/uploads/2018/11/CANUE-Metadata-NDVI-Landsat-Annual.pdf' target='_blank'>NDVI</a> = {pretty_data_var}, data from {data_date})",
          fr = "{data_rank} en termes de végétation (<a href='https://canue.ca/wp-content/uploads/2018/11/CANUE-Metadata-NDVI-Landsat-Annual.pdf' target='_blank'>NDVI</a> = {pretty_data_var}, données de {data_date})") |>
  add_row(en = "{pretty_data_var} of residents use public transit, walk or bicycle to get to work. {data_rank}. (Data from {data_date})",
          fr = "{pretty_data_var} des résidents utilisent les transports en commun, la marche ou le vélo pour se rendre au travail. {data_rank}. (Données de {data_date})") |>
  add_row(en = "{pretty_data_var} of occupied dwellings are single-detached houses. {data_rank}. (Data from {data_date})",
          fr = "{pretty_data_var} des logements occupés sont des maisons individuelles non attenantes. {data_rank}. (Données de {data_date})") |>
  add_row(en = "{data_rank} in terms of active living potential. (Data from {data_date})",
          fr = "{data_rank} en termes de potentiel vie active. (Données de {data_date})")

# lapply(string_to_translate, \(x) {
#   paste0('add_row(en = "', x, '",\n fr = "', .t(x), '") |>\n')
# })->z
# Reduce(paste0, z) |> writeLines()

# Check test
en_length <- length(unique(cc_translation_df$en))
if (en_length != nrow(cc_translation_df)) {
  cts <- table(cc_translation_df$en)
  dplc <- names(cts)[cts > 1]
  stop(paste0("`", dplc, "`is an english dupplicate in `cc_translation_df`"))
}

# Save
usethis::use_data(cc_translation_df, overwrite = TRUE)
