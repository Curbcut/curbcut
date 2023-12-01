##### CURBCUT TRANSLATION DATAFRAME ###############################################
library(tibble)


cc_translation_df <-
  # General UI --------------------------------------------------------------

  tibble::tibble(
    en = character(),
    fr = character()
  ) |>
  tibble::add_row(
    en = "----",
    fr = "----"
  ) |>
  tibble::add_row(
    en = "Select a year",
    fr = "S\u00e9lectionnez une ann\u00e9e"
  ) |>
  tibble::add_row(
    en = "Select two years",
    fr = "S\u00e9lectionnez deux ann\u00e9es"
  ) |>
  tibble::add_row(
    en = "Compare dates",
    fr = "Comparer deux dates"
  ) |>
  tibble::add_row(
    en = "Compare",
    fr = "Comparez"
  ) |>
  tibble::add_row(
    en = "Comparison is unavailable at the <b>%s</b> scale.",
    fr = "La comparaison n'est pas possible à l'échelle <b>%s</b>."
  ) |>
  tibble::add_row(
    en = "[LEARN MORE]",
    fr = "[EN SAVOIR PLUS]"
  ) |>
  tibble::add_row(
    en = "Did you know?",
    fr = "Le saviez-vous?"
  ) |>
  tibble::add_row(
    en = "Explore",
    fr = "Explorez"
  ) |>
  tibble::add_row(
    en = "Auto-scale",
    fr = "\u00c9chelle automatique"
  ) |>
  tibble::add_row(
    en = "Select a scale",
    fr = "S\u00e9lectionnez une \u00e9chelle"
  ) |>
  tibble::add_row(
    en = "No postal code found for `{address}`",
    fr = "Pas de code postal trouv\u00e9 pour `{address}`"
  ) |>
  tibble::add_row(
    en = "Curbcut does not currently support mobile phones. Please visit from a computer.",
    fr = "Curbcut ne supporte pas actuellement les t\u00e9l\u00e9phones mobiles. Veuillez visiter le site \u00e0 partir d'un ordinateur."
  ) |>
  tibble::add_row(
    en = "Clear selection",
    fr = "Effacer la s\u00e9lection"
  ) |>
  tibble::add_row(
    en = "Hide",
    fr = "En voir moins"
  ) |>
  tibble::add_row(
    en = "Show",
    fr = "Afficher"
  ) |>
  tibble::add_row(
    en = "Comparison",
    fr = "Comparaison"
  ) |>
  tibble::add_row(
    en = "Advanced controls",
    fr = "Contr\u00f4les avanc\u00e9s"
  ) |>
  tibble::add_row(
    en = "Time",
    fr = "Temps"
  ) |>
  tibble::add_row(
    en = "Themes",
    fr = "Th\u00e8mes"
  ) |>
  # Geography widget --------------------------------------------------------

  tibble::add_row(
    en = "Geography",
    fr = "G\u00e9ographie"
  ) |>
  tibble::add_row(
    en = "Region",
    fr = "R\u00e9gion"
  ) |>
  tibble::add_row(
    en = "Main scale",
    fr = "\u00C9chelle principale"
  ) |>
  tibble::add_row(
    en = paste0(
      "Changing the region to <b>%s</b> required changing the ",
      "main scale to <b>%s</b>."
    ),
    fr = paste0(
      "Le passage de la r\u00e9gion \u00E0 <b>%s</b> a n\u00e9cessit\u00e9 ",
      "le passage de l'\u00e9chelle principale \u00E0 <b>%s</b>."
    )
  ) |>
  tibble::add_row(
    en = paste0(
      "Changing the main scale to <b>%s</b> required changing the ",
      "region to <b>%s</b>."
    ),
    fr = paste0(
      "Le passage de l'\u00e9chelle principale \u00E0 <b>%s</b> a ",
      "n\u00e9cessit\u00e9 le passage de la r\u00e9gion \u00E0 <b>%s</b>."
    )
  ) |>
  # Legend ------------------------------------------------------------------

  tibble::add_row(
    en = "Low",
    fr = "Bas"
  ) |>
  tibble::add_row(
    en = "High",
    fr = "Haut"
  ) |>
  tibble::add_row(
    en = "Legend",
    fr = "L\u00e9gende"
  ) |>
  tibble::add_row(
    en = "Both low",
    fr = "Les deux faibles"
  ) |>
  tibble::add_row(
    en = "high only",
    fr = "\u00e9lev\u00e9 seulement"
  ) |>
  tibble::add_row(
    en = "Both high",
    fr = "Les deux \u00e9lev\u00e9s"
  ) |>
  # Tutorial ----------------------------------------------------------------

  tibble::add_row(
    en = paste0(
      "A Curbcut page explores a specific theme. Here you'll find information",
      " about the theme and the data used on the page."
    ),
    fr = paste0(
      "Chaque page Curbcut explore un th\u00e8me sp\u00e9cifique. Vous trouverez ici des ",
      "informations sur le th\u00e8me et les donn\u00e9es qui y figurent."
    )
  ) |>
  tibble::add_row(
    en = paste0(
      "Curbcut maps are interactive: you can scroll, zoom in and out, and ",
      "click on areas for more information."
    ),
    fr = paste0(
      "Les cartes Curbcut sont interactives : vous pouvez les naviguer, zoomer ",
      "et cliquer sur les zones pour obtenir plus d'informations."
    )
  ) |>
  tibble::add_row(
    en = paste0(
      "Choose the variables and time frame you want, and the map will update."
    ),
    fr = paste0(
      "Choisissez les variables et la p\u00e9riode que vous souhaitez, et la carte s'actualisera."
    )
  ) |>
  tibble::add_row(
    en = paste0(
      "The legend shows how the selected variables are displayed on the map."
    ),
    fr = paste0(
      "La l\u00e9gende montre comment les variables s\u00e9lectionn\u00e9es sont affich\u00e9es sur la carte."
    )
  ) |>
  tibble::add_row(
    en = paste0(
      "Curbcut maps automatically update the scale as you zoom in and out. ",
      "If you want to control the scale manually, click off ",
      "\u2018Auto-scale\u2019 and move the slider yourself."
    ),
    fr = paste0(
      "Les cartes Curbcut mettent automatiquement \u00e0 jour l'\u00e9chelle lorsque vous ",
      "effectuez un zoom avant ou arri\u00e8re. Si vous souhaitez contr\u00f4ler l'\u00e9chelle ",
      "manuellement, d\u00e9sactivez l'option \u2018\u00c9chelle automatique\u2019 et ",
      "d\u00e9placez vous-m\u00eame le curseur."
    )
  ) |>
  tibble::add_row(
    en = paste0(
      "If you want to compare your main variable with a second variable, ",
      "choose one here and the map will update to show the relationship ",
      "between the two."
    ),
    fr = paste0(
      "Si vous souhaitez comparer votre variable principale avec une deuxi\u00e8me ",
      "variable, choisissez-en une ici et la carte sera actualis\u00e9e pour montrer ",
      "la relation entre les deux."
    )
  ) |>
  tibble::add_row(
    en = paste0(
      "Explore patterns and relationships in the variables you've selected. ",
      "The text and graph here automatically update as you navigate the page."
    ),
    fr = paste0(
      "Explorez les tendances et les relations entre les variables que vous ",
      "avez s\u00e9lectionn\u00e9es. Le texte et le graphique se mettent automatiquement ",
      "\u00e0 jour au fur et \u00e0 mesure que vous naviguez sur la page."
    )
  ) |>
  tibble::add_row(
    en = paste0(
      "Use these buttons to switch between map view and table view. ",
      "Both show the same information!"
    ),
    fr = "Ces boutons permettent de passer de l'affichage de la carte \u00e0 l'affichage du tableau. Les deux affichent les m\u00eames informations !"
  ) |>
  tibble::add_row(
    en = paste0(
      "Congratulations, you've completed the tutorial! You can revisit ",
      "it by clicking this button."
    ),
    fr = paste0(
      "F\u00e9licitations pour avoir compl\u00e9t\u00e9 le tutoriel ! Vous ",
      "pouvez revoir le tutoriel en cliquant sur ce bouton."
    )
  ) |>
  tibble::add_row(
    en = "Title text",
    fr = "Intitul\u00e9"
  ) |>
  tibble::add_row(
    en = "Variable selection",
    fr = "S\u00e9lection de variable"
  ) |>
  tibble::add_row(
    en = "Scale",
    fr = "\u00c9chelle"
  ) |>
  tibble::add_row(
    en = "Switch views",
    fr = "Changer de vue"
  ) |>
  tibble::add_row(
    en = "Tutorial",
    fr = "Tutoriel"
  ) |>
  tibble::add_row(
    en = "Next",
    fr = "Suivant"
  ) |>
  tibble::add_row(
    en = "Back",
    fr = "Retour"
  ) |>
  tibble::add_row(
    en = "Done",
    fr = "Termin\u00e9"
  ) |>
  tibble::add_row(
    en = paste0(
      "Congratulations, you\u2019ve completed the tutorial! You can revisit ",
      "it by clicking this button."
    ),
    fr = paste0(
      "F\u00e9licitations, vous avez termin\u00e9 le tutoriel ! Vous pouvez y ",
      "revenir en cliquant sur ce bouton."
    )
  ) |>
  # Misc --------------------------------------------------------------------

  tibble::add_row(
    en = "Population",
    fr = "Population"
  ) |>
  tibble::add_row(
    en = "Households",
    fr = "M\u00e9nages"
  ) |>
  tibble::add_row(
    en = "households",
    fr = "m\u00e9nages"
  ) |>
  tibble::add_row(
    en = "Visit the place explorer",
    fr = "Visiter l'explorateur de lieux"
  ) |>
  tibble::add_row(
    en = "The spatial organization of the data is the %s scale.",
    fr = "L'organisation spatiale des donn\u00e9es est l'\u00e9chelle '%s'."
  ) |>
  tibble::add_row(
    en = "Overview",
    fr = "Vue d'ensemble"
  ) |>
  tibble::add_row(
    en = "Exporting data",
    fr = "Exporter des donn\u00e9es"
  ) |>
  tibble::add_row(
    en = "Map",
    fr = "Carte"
  ) |>
  tibble::add_row(
    en = "Table",
    fr = "Tableau"
  ) |>
  tibble::add_row(
    en = "Portrait",
    fr = "Portrait"
  ) |>
  tibble::add_row(
    en = "Download '.csv'",
    fr = "T\u00e9l\u00e9charger '.csv'"
  ) |>
  tibble::add_row(
    en = "Download '.shp'",
    fr = "T\u00e9l\u00e9charger '.shp'"
  ) |>
  tibble::add_row(
    en = "No postal code found for `{input$address_searched}`",
    fr = "Aucun code postal n'a \u00e9t\u00e9 trouv\u00e9 pour `{input$address_searched}`"
  ) |>
  tibble::add_row(
    en = "Enter postal code or click on the map",
    fr = "Entrez un code postal ou cliquez sur la carte"
  ) |>
  tibble::add_row(
    en = "Back to the place explorer",
    fr = "Retour \u00e0 l'explorateur de lieux"
  ) |>
  tibble::add_row(
    en = "Download regional portrait",
    fr = "T\u00e9l\u00e9charger le portrait r\u00e9gional"
  ) |>
  tibble::add_row(
    en = "Change default region",
    fr = "Modifier la r\u00e9gion par d\u00e9faut"
  ) |>
  tibble::add_row(
    en = paste0(
      "Enter and save a default location (postal ",
      "code or address)"
    ),
    fr = "Saisir et enregistrer un emplacement par d\u00e9faut (code postal ou adresse)"
  ) |>
  tibble::add_row(
    en = paste0(
      "Default location will be saved until ",
      "manually cleared from advanced options"
    ),
    fr = "L'emplacement par d\u00e9faut sera sauvegard\u00e9 jusqu'\u00e0 ce qu'il soit effac\u00e9 manuellement dans les options avanc\u00e9es."
  ) |>
  tibble::add_row(
    en = "Clear default location",
    fr = "Effacer l'emplacement par d\u00e9faut"
  ) |>
  tibble::add_row(
    en = "Postal code `{postal_c}` isn't within an available region.",
    fr = "Code postal `{postal_c}` ne se trouve pas dans une r\u00e9gion disponible."
  ) |>
  tibble::add_row(
    en = "Postal code `{postal_c}` saved as default.",
    fr = "Code postal `{postal_c}` sauvegard\u00e9 par d\u00e9faut."
  ) |>
  tibble::add_row(
    en = "Search `{address}` wasn't found within an available region.",
    fr = "La recherche `{address}` n'a pas \u00e9t\u00e9 trouv\u00e9e dans une r\u00e9gion disponible."
  ) |>
  tibble::add_row(
    en = "Address `{address}` isn't within an available region.",
    fr = "L'adresse `{address}` ne se trouve pas dans une r\u00e9gion disponible."
  ) |>
  tibble::add_row(
    en = "No zone has been found in a 1km radius of the provided address.",
    fr = "Aucune zone n'a \u00e9t\u00e9 trouv\u00e9e dans un rayon de 1 km autour de l'adresse fournie."
  ) |>
  tibble::add_row(
    en = "Address `{address}` saved as default.",
    fr = "L'adresse `{address}` est sauvegard\u00e9e par d\u00e9faut."
  ) |>
  tibble::add_row(
    en = "Advanced options",
    fr = "Options avanc\u00e9es"
  ) |>
  tibble::add_row(
    en = "Change language",
    fr = "Modifier la langue"
  ) |>
  tibble::add_row(
    en = "Close",
    fr = "Fermer"
  ) |>
  tibble::add_row(
    en = "Default location successfully cleared",
    fr = "L'emplacement par d\u00e9faut a \u00e9t\u00e9 supprim\u00e9 avec succ\u00e8s"
  ) |>
  tibble::add_row(
    en = "Learn more",
    fr = "En savoir plus"
  ) |>
  tibble::add_row(
    en = "Visit the {stories_page} page",
    fr = "Visiter la page {stories_page}"
  ) |>
  tibble::add_row(
    en = "Comparison requires two different dates.",
    fr = "La comparaison n\u00e9cessite deux dates diff\u00e9rentes."
  ) |>
  tibble::add_row(
    en = paste0(
      "Displayed data for <b>{var_left_title}</b> is for the ",
      "closest available year <b>({left_year})</b>."
    ),
    fr = paste0(
      "Les donn\u00e9es affich\u00e9es pour <b>{var_left_title}</b> ",
      "correspondent \u00e0 l'ann\u00e9e la plus proche <b>({left_year})</b>."
    )
  ) |>
  tibble::add_row(
    en = paste0(
      "Displayed data for <b>{var_right_title}</b> is for the ",
      "closest available year <b>({right_year})</b>."
    ),
    fr = paste0(
      "Les donn\u00e9es affich\u00e9es pour <b>{var_right_title}</b> ",
      "correspondent \u00e0 l'ann\u00e9e la plus proche <b>({right_year})</b>."
    )
  ) |>
  # Explore panel -----------------------------------------------------------

  # Context and q5
  tibble::add_row(
    en = "individuals",
    fr = "individus"
  ) |>
  tibble::add_row(
    en = "This value",
    fr = "Cette valeur"
  ) |>
  tibble::add_row(
    en = "This is %s for %s",
    fr = "C'est %s pour %s"
  ) |>
  tibble::add_row(
    en = "%s %s is higher than in %s of other %s %s",
    fr = "%s %s est plus \u00e9lev\u00e9 que dans %s des autres %s %s"
  ) |>
  tibble::add_row(
    en = "%s <i>(Data from %s.)</i>",
    fr = "%s <i>(Donn\u00e9es de %s.)</i>"
  ) |>
  tibble::add_row(
    en = "around %s",
    fr = "autour du %s"
  ) |>
  tibble::add_row(
    en = "Dissemination area %s",
    fr = "Aire de diffusion %s"
  ) |>
  tibble::add_row(
    en = "in {p_start}",
    fr = "dans {p_start}"
  ) |>
  tibble::add_row(
    en = "in {name}",
    fr = "dans {name}"
  ) |>
  tibble::add_row(
    en = "{name_2} of {name}",
    fr = "{name_2} {name}"
  ) |>
  tibble::add_row(
    en = "we currently don't have information regarding %s",
    fr = "nous n'avons pr\u00e9sentement pas d'informations concernant %s"
  ) |>
  tibble::add_row(
    en = "'%s' to '%s'",
    fr = "'%s' \u00e0 '%s'"
  ) |>
  tibble::add_row(
    en = "a higher-than-average",
    fr = "un niveau plus \u00e9lev\u00e9 que la moyenne en mati\u00e8re de"
  ) |>
  tibble::add_row(
    en = "%s is %s",
    fr = "%s est %s"
  ) |>
  # bivar -------------------------------------------------------------------

  tibble::add_row(
    en = "Pearson's r: %s",
    fr = "r de Pearson : %s"
  ) |>
  tibble::add_row(
    en = "The first value",
    fr = "La premi\u00e8re valeur"
  ) |>
  tibble::add_row(
    en = "The second value",
    fr = "La deuxi\u00e8me valeur"
  ) |>
  tibble::add_row(
    en = "Spearman's rho: %s",
    fr = "rho de Spearman : %s"
  ) |>
  tibble::add_row(
    en = "%s and %s",
    fr = "%s et %s"
  ) |>
  tibble::add_row(
    en = "positive",
    fr = "positive"
  ) |>
  tibble::add_row(
    en = "negative",
    fr = "n\u00e9gative"
  ) |>
  tibble::add_row(
    en = "%s %s correlation",
    fr = "corr\u00e9lation %s et %s"
  ) |>
  tibble::add_row(
    en = "almost always have",
    fr = "ont presque toujours"
  ) |>
  tibble::add_row(
    en = "tend to have",
    fr = "tendent \u00e0 avoir"
  ) |>
  tibble::add_row(
    en = "often have _X_, although with many exceptions",
    fr = "ont souvent _X_, bien qu'\u00e0 de nombreuses exceptions pr\u00e8s"
  ) |>
  tibble::add_row(
    en = "strong",
    fr = "forte"
  ) |>
  tibble::add_row(
    en = "moderate",
    fr = "mod\u00e9r\u00e9e"
  ) |>
  tibble::add_row(
    en = "weak",
    fr = "faible"
  ) |>
  tibble::add_row(
    en = "effectively no relationship",
    fr = "aucune relation effective"
  ) |>
  tibble::add_row(
    en = "effectively no correlation",
    fr = "pas de corr\u00e9lation effective"
  ) |>
  tibble::add_row(
    en = "higher",
    fr = "un niveau plus \u00e9lev\u00e9 de"
  ) |>
  tibble::add_row(
    en = "lower",
    fr = "un niveau plus bas de"
  ) |>
  tibble::add_row(
    en = "a higher",
    fr = "un niveau plus \u00e9lev\u00e9 de"
  ) |>
  tibble::add_row(
    en = "a lower",
    fr = "un niveau plus bas de"
  ) |>
  tibble::add_row(
    en = "%s, %s with %s %s %s.",
    fr = "%s, les %s avec %s %s %s."
  ) |>
  tibble::add_row(
    en = "%s, %s with %s %s %s %s %s.",
    fr = "%s, les %s avec %s %s %s %s %s."
  ) |>
  tibble::add_row(
    en = "There is a %s (%s) between these two variables.",
    fr = "Il existe une %s (%s) entre ces deux variables."
  ) |>
  tibble::add_row(
    en = "<p><b>STRONG CORRELATION</b>%s",
    fr = "<p><b>FORTE CORR\u00c9LATION</b>%s"
  ) |>
  tibble::add_row(
    en = "%s<p>%s, %s and %s.",
    fr = "%s<p>%s, %s et %s."
  ) |>
  tibble::add_row(
    en = "%s is higher than in %s of other %s",
    fr = "%s est plus \u00e9lev\u00e9 que dans %s des autres %s"
  ) |>
  tibble::add_row(
    en = "which is %s for %s",
    fr = "ce qui est %s pour %s"
  ) |>
  tibble::add_row(
    en = "By contrast",
    fr = "En revanche"
  ) |>
  tibble::add_row(
    en = "Similarly",
    fr = "De m\u00eame"
  ) |>
  tibble::add_row(
    en = "%s, there is %s (%s) between %s and %s in %s.",
    fr = "%s, il n'existe %s (%s) entre %s et %s \u00e0 l'\u00e9chelle des %s."
  ) |>
  # Delta -------------------------------------------------------------------


  tibble::add_row(
    en = "the value",
    fr = "la valeur"
  ) |>
  tibble::add_row(
    en = "%s percentage points (%sx)",
    fr = "%s points de pourcentage (%sx)"
  ) |>
  tibble::add_row(
    en = "This number",
    fr = "Ce nombre"
  ) |>
  tibble::add_row(
    en = "increased",
    fr = "augment\u00e9"
  ) |>
  tibble::add_row(
    en = "decreased",
    fr = "diminu\u00e9"
  ) |>
  tibble::add_row(
    en = "Increase",
    fr = "Augment\u00e9"
  ) |>
  tibble::add_row(
    en = "Decrease",
    fr = "Diminu\u00e9"
  ) |>
  tibble::add_row(
    en = "No change",
    fr = "Aucun changement"
  ) |>
  tibble::add_row(
    en = "NA",
    fr = "NA"
  ) |>
  tibble::add_row(
    en = "%s, %s changed from %s in %s to %s in %s.",
    fr = "%s, %s a chang\u00e9 de %s en %s \u00e0 %s en %s."
  ) |>
  tibble::add_row(
    en = "%s has %s by %s between these years.",
    fr = "%s a %s de %s entre ces deux ann\u00e9es."
  ) |>
  tibble::add_row(
    en = "%s, %s has remained %s between %s and %s.",
    fr = "%s, %s est rest\u00e9 de %s entre %s et %s."
  ) |>
  tibble::add_row(
    en = "the percentage of %s that %s",
    fr = "le pourcentage de %s qui %s"
  ) |>
  tibble::add_row(
    en = "increase",
    fr = "augmentation"
  ) |>
  tibble::add_row(
    en = "decrease",
    fr = "diminution"
  ) |>
  tibble::add_row(
    en = "exceptionally small",
    fr = "exceptionnellement faible"
  ) |>
  tibble::add_row(
    en = "unusually small",
    fr = "tr\u00e8s faible"
  ) |>
  tibble::add_row(
    en = "just about average",
    fr = "\u00e0 peu pr\u00e8s dans la moyenne"
  ) |>
  tibble::add_row(
    en = "unusually large",
    fr = "tr\u00e8s forte"
  ) |>
  tibble::add_row(
    en = "exceptionally large",
    fr = "exceptionnellement forte"
  ) |>
  tibble::add_row(
    en = "The slight",
    fr = "La l\u00e9g\u00e8re"
  ) |>
  tibble::add_row(
    en = "This",
    fr = "Cette"
  ) |>
  tibble::add_row(
    en = "%s %s is %s for %s.",
    fr = "%s %s est %s pour %s."
  ) |>
  tibble::add_row(
    en = "The change in %s %s between %s and %s is larger than in %s of other %s between the same years.",
    fr = "La variation de la/du %s %s entre %s et %s est plus \u00e9lev\u00e9 que dans %s des autres %s entre les m\u00eames ann\u00e9es."
  ) |>
  # Delta bivar -------------------------------------------------------------

  tibble::add_row(
    en = "the first value",
    fr = "la premi\u00e8re valeur"
  ) |>
  tibble::add_row(
    en = "the second value",
    fr = "la deuxi\u00e8me valeur"
  ) |>
  tibble::add_row(
    en = "%s from %s to %s, there is %s (%s) between the change in %s and the change in %s in %s.",
    fr = "%s de %s \u00e0 %s, il n'existe %s (%s) entre la variation dans %s et la variation dans %s dans %s."
  ) |>
  tibble::add_row(
    en = " change</b>",
    fr = " variation</b>"
  ) |>
  tibble::add_row(
    en = "%s in %s",
    fr = "%s dans %s"
  ) |>
  tibble::add_row(
    en = "%s from %s to %s, %s with %s in %s %s.",
    fr = "%s de %s \u00e0 %s, %s avec %s dans %s %s."
  ) |>
  tibble::add_row(
    en = "%s from %s to %s, %s with %s in %s %s have had %s in %s.",
    fr = "%s de %s \u00e0 %s, les %s avec %s dans %s %s %s dans %s."
  ) |>
  tibble::add_row(
    en = "There is a %s (%s) between the change in these two variables between these years.",
    fr = "Il existe une %s (%s) entre l'\u00e9volution de ces deux variables entre ces ann\u00e9es."
  ) |>
  tibble::add_row(
    en = "%s<p>%s, %s changed from %s in %s to %s in %s.",
    fr = "%s<p>%s, %s est pass\u00e9 de %s en %s \u00e0 %s en %s."
  ) |>
  tibble::add_row(
    en = "%s %s changed from %s in %s to %s in %s.",
    fr = "%s %s est pass\u00e9 de %s en %s \u00e0 %s en %s."
  ) |>
  tibble::add_row(
    en = "an exceptionally small change",
    fr = "un changement exceptionnellement faible"
  ) |>
  tibble::add_row(
    en = "an unusually small change",
    fr = "un changement tr\u00e8s faible"
  ) |>
  tibble::add_row(
    en = "a just about average change",
    fr = "un changement \u00e0 peu pr\u00e8s dans la moyenne"
  ) |>
  tibble::add_row(
    en = "an unusually large change",
    fr = "un changement tr\u00e8s fort"
  ) |>
  tibble::add_row(
    en = "an exceptionally large change",
    fr = "un changement exceptionnellement fort"
  ) |>
  tibble::add_row(
    en = "The change in %s %s from %s to %s is larger than %s other %s, which is %s for %s.",
    fr = "Le changement dans %s %s de %s \u00e0 %s est plus \u00e9lev\u00e9 que dans %s des autres %s, ce qui est %s pour %s."
  ) |>
  tibble::add_row(
    en = "%s, the change in %s between the same years is larger than %s of other %s, which is %s for %s.",
    fr = "%s, le changement dans %s entre ces deux m\u00eames ann\u00e9es est plus \u00e9lev\u00e9s que %s des autres %s, ce qui est %s pour %s."
  ) |>
  tibble::add_row(
    en = "larger change",
    fr = "une variation plus \u00e9lev\u00e9e"
  ) |>
  tibble::add_row(
    en = "smaller change",
    fr = "une variation plus faible"
  ) |>
  tibble::add_row(
    en = "a larger change",
    fr = "une variation plus \u00e9lev\u00e9e"
  ) |>
  tibble::add_row(
    en = "a smaller change",
    fr = "une variation plus faible"
  ) |>
  # Panel view --------------------------------------------------------------

  tibble::add_row(
    en = "Name",
    fr = "Nom"
  ) |>
  tibble::add_row(
    en = "the change in %s between %s and %s",
    fr = "le changement dans %s entre %s et %s"
  ) |>
  tibble::add_row(
    en = paste0(
      "<p>The minimum and maximum values for %s are respectively %s and %s. ",
      "The data points have an average value (mean) of %s. Additionally, ",
      "the standard deviation, which measures the dispersion or spread ",
      "around this mean, is %s. (Approximately two thirds of data points ",
      "lie within one standard deviation of the mean.)</p>"
    ),
    fr = paste0(
      "<p>Les valeurs minimale et maximale pour %s sont respectivement %s et ",
      "%s. Les points de donn\u00e9es ont une valeur moyenne de %s. De plus, l'\u00e9ca",
      "rt type, qui mesure la dispersion ou l'\u00e9tendue autour de cette moyenne",
      ", est de %s. (Environ deux tiers des points de donn\u00e9es se situent \u00e0 mo",
      "ins d'un \u00e9cart-type de la moyenne).</p>"
    )
  ) |>
  tibble::add_row(
    en = paste0(
      "The data comes from the %s Canadian census and has ",
      "been retrieved from <a href = 'https://censusma",
      "pper.ca/', target = '_blank'>censusmapper.ca</a> ",
      "using the R <a href = 'https://cran.r-project.org",
      "/web/packages/cancensus/', target = '_blank'>canc",
      "ensus</a> package."
    ),
    fr = "Les donn\u00e9es proviennent du recensement canadien de %s et ont \u00e9t\u00e9 extraites de <a href = 'https://censusmapper.ca/', target = '_blank'>censusmapper.ca</a> \u00e0 l'aide de la librairie R <a href = 'https://cran.r-project.org/web/packages/cancensus/', target = '_blank'>cancensus</a>."
  ) |>
  tibble::add_row(
    en = "vectors and their",
    fr = "vecteurs"
  ) |>
  tibble::add_row(
    en = "vector and its",
    fr = "le vecteur"
  ) |>
  tibble::add_row(
    en = "parent vectors",
    fr = "vecteurs parents"
  ) |>
  tibble::add_row(
    en = "parent vector",
    fr = "vecteur parent"
  ) |>
  tibble::add_row(
    en = paste0(
      "To calculate %s, we extract the %s corresponding ",
      "%s. Here, the term 'parent vector' refers to ",
      "the data source that represents %s, which we use ",
      "as a basis to compute %s."
    ),
    fr = "Pour calculer %s, nous extrayons %s et son/ses correspondant %s. Ici, le terme `vecteur parent` fait r\u00e9f\u00e9rence \u00e0 la source de donn\u00e9es qui repr\u00e9sente %s, que nous utilisons comme base pour calculer %s."
  ) |>
  tibble::add_row(
    en = "The source of the data is '%s'.",
    fr = "La donn\u00e9e provient de '%s'."
  ) |>
  tibble::add_row(
    en = "%s has been spatially interpolated from %s.",
    fr = "%s a \u00e9t\u00e9 spatialement interpol\u00e9 \u00e0 partir des %s."
  ) |>
  tibble::add_row(
    en = "{source_vec} {v}",
    fr = "{v} {source_vec}"
  ) |>
  # Place explorer ----------------------------------------------------------

  tibble::add_row(
    en = "Generating report",
    fr = "Cr\u00e9ation du rapport"
  ) |>
  tibble::add_row(
    en = "Its value is higher than the WHO's guideline value of 53. ",
    fr = "Sa valeur est sup\u00e9rieure \u00e0 la valeur recommand\u00e9e par l'OMS, qui est de 53. "
  ) |>
  tibble::add_row(
    en = "%s (count)",
    fr = "%s (compte)"
  ) |>
  tibble::add_row(
    en = "No data.",
    fr = "Aucune donn\u00e9e."
  ) |>
  tibble::add_row(
    en = "Air pollution",
    fr = "Pollution de l'air"
  ) |>
  tibble::add_row(
    en = "Vegetation",
    fr = "V\u00e9g\u00e9tation"
  ) |>
  tibble::add_row(
    en = "Sustainable transport",
    fr = "Mobilit\u00e9 durable"
  ) |>
  tibble::add_row(
    en = "Active living",
    fr = "Vie active"
  ) |>
  tibble::add_row(
    en = "Nitrogen dioxide (ppb)",
    fr = "Dioxyde d'azote (ppb)"
  ) |>
  tibble::add_row(
    en = "Normalized Difference Vegetation Index",
    fr = "Indice de v\u00e9g\u00e9tation par diff\u00e9rence normalis\u00e9e"
  ) |>
  tibble::add_row(
    en = "Walk, cycle or transit to work (%)",
    fr = "Se rendent au travail \u00e0 pied, \u00e0 v\u00e9lo ou en transport en commun (%)"
  ) |>
  tibble::add_row(
    en = "Single-detached housing (%)",
    fr = "Maison individuelle non attenante (%)"
  ) |>
  tibble::add_row(
    en = "Active living potential index",
    fr = "Indice de potentiel de vie active"
  ) |>
  tibble::add_row(
    en = "{data_rank} in terms of level of <a href = 'https://www.canuedata.ca/tmp/CANUE_METADATA_NO2LUR_A_YY.pdf'>NO2</a> pollution. {higher_than_threshold}(NO2 = {pretty_data_var}, data from {data_date})",
    fr = "{data_rank} en termes de niveau de pollution de <a href = 'https://www.canuedata.ca/tmp/CANUE_METADATA_NO2LUR_A_YY.pdf'>NO2</a>. {higher_than_threshold}(NO2 = {pretty_data_var}, donn\u00e9es de {data_date})"
  ) |>
  tibble::add_row(
    en = "{data_rank} in terms of vegetation (<a href='https://canue.ca/wp-content/uploads/2018/11/CANUE-Metadata-NDVI-Landsat-Annual.pdf' target='_blank'>NDVI</a> = {pretty_data_var}, data from {data_date})",
    fr = "{data_rank} en termes de v\u00e9g\u00e9tation (<a href='https://canue.ca/wp-content/uploads/2018/11/CANUE-Metadata-NDVI-Landsat-Annual.pdf' target='_blank'>NDVI</a> = {pretty_data_var}, donn\u00e9es de {data_date})"
  ) |>
  tibble::add_row(
    en = "{pretty_data_var} of residents use public transit, walk or bicycle to get to work. {data_rank}. (Data from {data_date})",
    fr = "{pretty_data_var} des r\u00e9sidents utilisent les transports en commun, la marche ou le v\u00e9lo pour se rendre au travail. {data_rank}. (Donn\u00e9es de {data_date})"
  ) |>
  tibble::add_row(
    en = "{pretty_data_var} of occupied dwellings are single-detached houses. {data_rank}. (Data from {data_date})",
    fr = "{pretty_data_var} des logements occup\u00e9s sont des maisons individuelles non attenantes. {data_rank}. (Donn\u00e9es de {data_date})"
  ) |>
  tibble::add_row(
    en = "{data_rank} in terms of active living potential. (Data from {data_date})",
    fr = "{data_rank} en termes de potentiel vie active. (Donn\u00e9es de {data_date})"
  ) |>
  # Possible themes ---------------------------------------------------------
  tibble::add_row(
    en = "Climate",
    fr = "Climat"
  ) |>
  tibble::add_row(
    en = "Ecology",
    fr = "\u00c9cologie"
  ) |>
  tibble::add_row(
    en = "Health",
    fr = "Sant\u00e9"
  ) |>
  tibble::add_row(
    en = "Housing",
    fr = "Logement"
  ) |>
  tibble::add_row(
    en = "Transport",
    fr = "Mobilit\u00e9"
  ) |>
  tibble::add_row(
    en = "Urban life",
    fr = "Vie urbaine"
  ) |>
  tibble::add_row(
    en = "Explorer",
    fr = "Explorer"
  ) |>
  tibble::add_row(
    en = "Demopgrahics",
    fr = "D\u00e9mographie"
  ) |>
  tibble::add_row(
    en = "Economy",
    fr = "\u00c9conomie"
  ) |>
  tibble::add_row(
    en = "Resources",
    fr = "Ressources"
  ) |>
  tibble::add_row(
    en = "Land use",
    fr = "Utilisation des sols"
  ) |>
  # Stories -----------------------------------------------------------------
  tibble::add_row(
    en = "Image gallery",
    fr = "Galerie d'images"
  ) |>
  tibble::add_row(
    en = "Choose themes:",
    fr = "Choisissez des th\u00e8mes :"
  )




# lapply(string_to_translate, \(x) {
#   paste0('tibble::add_row(en = "', x, '",\n fr = "', .t(x), '") |>\n')
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
