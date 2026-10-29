# Application Shiny - Suivi de la collecte et contrôle de cohérence
# Enquête complémentaire et de productivité des ruches d'abeilles (ANSD)
# Les fonctions sont dans le dossier R/ (chargées automatiquement par Shiny) :
#   lecture.R (exports), controles.R (R01 à R39), suivi.R (tableaux), export_excel.R, interface.R (charte ANSD)

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(bsicons)
  library(dplyr)
  library(tidyr)
  library(echarts4r)
  library(reactable)
  library(shinyWidgets)
  library(waiter)
})
options(shiny.maxRequestSize = 500 * 1024^2)

DOSSIER_APP <- normalizePath(getwd(), winslash = "/")
candidats <- c(file.path(DOSSIER_APP, "donnees"), DOSSIER_APP, dirname(DOSSIER_APP))
avec_exports <- candidats[vapply(candidats, function(d) length(trouver_exports(d, extraire = FALSE)) > 0, logical(1))]
DOSSIER_DEFAUT <- if (length(avec_exports)) avec_exports[1] else file.path(DOSSIER_APP, "donnees")

selecteur <- function(id, label, texte_vide) {
  pickerInput(id, label, choices = character(), multiple = TRUE,
              options = pickerOptions(actionsBox = TRUE, liveSearch = TRUE, selectAllText = "Tout",
                                      deselectAllText = "Aucun", noneSelectedText = texte_vide,
                                      selectedTextFormat = "count > 2", countSelectedText = "{0} sélectionnés",
                                      liveSearchPlaceholder = "Rechercher…", size = 10))
}
seuil <- function(id, label, step = 1) numericInput(id, label, SEUILS_DEFAUT[[id]], step = step, width = "100%")
boite <- function(titre, id_valeur, icone, fond, encre, id_sous = NULL, showcase = bs_icon(icone)) {
  value_box(title = titre, value = textOutput(id_valeur, inline = TRUE), showcase = showcase,
            theme = value_box_theme(bg = fond, fg = encre),
            if (!is.null(id_sous)) p(class = "small mb-0", textOutput(id_sous, inline = TRUE)))
}

ecran_attente <- tagList(
  tags$img(src = "logo_ansd.png", height = "110px", style = "background:#fff;border-radius:12px;padding:8px 14px;"),
  tags$h4(style = "color:#fff;margin-top:1.2rem;font-weight:600", "Suivi de la collecte - Ruches d'abeilles"),
  tags$p(style = "color:#E8C84E", "Lecture des exports Survey Solutions et contrôle de cohérence…"),
  spin_flower()
)

# ==========================================================================================
# Interface
# ==========================================================================================
ui <- page_navbar(
  title = tagList(tags$img(src = "logo_ansd.png", alt = "ANSD"),
                  span("Suivi de la collecte", tags$small(class = "opacity-75 fw-normal", " · Ruches d'abeilles"))),
  window_title = "Suivi collecte - Ruches d'abeilles (ANSD)",
  theme = theme_ansd(),
  navbar_options = navbar_options(bg = ANSD$marine, theme = "dark"),
  fillable = FALSE,
  header = tagList(useWaiter(), waiter_show_on_load(html = ecran_attente, color = ANSD$marine)),
  footer = div(class = "pied", "ANSD · Recensement complémentaire et étude de la productivité des ruches d'abeilles · ",
               "Source : exports Survey Solutions"),

  sidebar = sidebar(
    width = 320, open = "desktop",
    accordion(
      open = c("Données", "Filtres"), multiple = TRUE,
      accordion_panel(
        "Données", icon = bs_icon("database"),
        textInput("dossier", "Dossier contenant les exports Stata", value = DOSSIER_DEFAUT),
        fileInput("zips", "… ou téléverser des exports .zip", multiple = TRUE, accept = ".zip",
                  buttonLabel = "Parcourir", placeholder = "PROD_ECPRA_BMD_<n>_STATA_All.zip"),
        input_task_button("charger", "Charger / actualiser", icon = bs_icon("arrow-repeat"),
                          label_busy = "Chargement…", class = "w-100"),
        uiOutput("info_sources")
      ),
      accordion_panel(
        "Filtres", icon = bs_icon("funnel"),
        selecteur("f_version", "Version du questionnaire", "Toutes les versions"),
        selecteur("f_region", "Région", "Toutes les régions"),
        selecteur("f_enq", "Enquêteur", "Tous les enquêteurs"),
        dateRangeInput("f_dates", "Période (date de début de l'entretien)", language = "fr", separator = "au",
                       format = "dd/mm/yyyy", weekstart = 1),
        actionLink("reinit_filtres", "Effacer les filtres", icon = bs_icon("x-circle"))
      ),
      accordion_panel(
        "Seuils des contrôles", icon = bs_icon("sliders"),
        seuil("duree_min", "Durée minimale (min) - rejet"),
        seuil("duree_max", "Durée maximale (min) - à vérifier", 10),
        seuil("nonrep_rejet", "Questions non renseignées entraînant le rejet"),
        seuil("rdt_miel_max", "Rendement maximal (L de miel / ruche)"),
        seuil("rdt_cire_max", "Cire maximale (kg / ruche)"),
        seuil("prix_miel_rejet_min", "Prix du miel : minimum (rejet, FCFA/L)", 100),
        seuil("prix_miel_rejet_max", "Prix du miel : maximum (rejet, FCFA/L)", 1000),
        seuil("prix_miel_verif_min", "Prix du miel : minimum (à vérifier)", 100),
        seuil("prix_miel_verif_max", "Prix du miel : maximum (à vérifier)", 1000),
        seuil("ecart_mensuel_rejet", "Écart mois / année entraînant le rejet (facteur)", 0.5),
        seuil("variation_ruches", "Variation des ruches 2024-2025 (facteur)", 0.5),
        actionLink("reinit", "Rétablir les valeurs par défaut", icon = bs_icon("arrow-counterclockwise"))
      ),
      accordion_panel(
        "Téléchargements", icon = bs_icon("download"),
        downloadButton("dl_suivi", "Classeur de suivi (.xlsx)", class = "btn-primary w-100 mb-2"),
        downloadButton("dl_incoh", "Classeur des incohérences (.xlsx)", class = "btn-primary w-100 mb-2"),
        downloadButton("dl_csv", "Liste de rejet pour l'API (.csv)", class = "btn-outline-primary w-100"),
        div(class = "note mt-2", "Les fichiers tiennent compte des filtres et des seuils choisis.")
      )
    )
  ),

  # ---------------------------------------------------------------- Tableau de bord
  nav_panel(
    "Tableau de bord", icon = bs_icon("speedometer2"),
    layout_column_wrap(
      width = 1 / 4, fill = FALSE,
      boite("Entretiens réalisés", "v_total", "clipboard-data", ANSD$marine, "#FFFFFF", "s_total",
            showcase = echarts4rOutput("spark", height = "70px")),
      boite("Complétés ou approuvés", "v_complet", "check2-circle", ANSD$marine_pale, ANSD$marine, "s_complet"),
      boite("Rejetés ou en cours", "v_cours", "hourglass-split", ANSD$or_pale, "#6B4708", "s_cours"),
      boite("À rejeter maintenant", "v_rejeter", "exclamation-octagon", ANSD$rouge_pale, ANSD$rouge, "s_rejeter")
    ),
    layout_columns(
      col_widths = c(8, 4),
      carte("Progression de la collecte", sous_titre = "entretiens cumulés", attente(echarts4rOutput("g_cumul", height = "330px"))),
      carte("Taux de complétion", attente(echarts4rOutput("g_jauge", height = "170px")),
            attente(echarts4rOutput("g_donut", height = "170px")))
    ),
    layout_columns(
      col_widths = c(6, 6),
      carte("Avancement par région", attente(echarts4rOutput("g_region_bord", height = "360px"))),
      carte("Contrôles les plus fréquents", sous_titre = "entretiens concernés",
            attente(echarts4rOutput("g_controles", height = "360px")))
    )
  ),

  # ---------------------------------------------------------------- Évolution
  nav_panel(
    "Évolution", icon = bs_icon("graph-up-arrow"),
    layout_columns(
      col_widths = c(7, 5),
      carte("Activité quotidienne", sous_titre = "entretiens démarrés, envoyés et rejetés",
            attente(echarts4rOutput("g_jour", height = "340px"))),
      carte("Durée des entretiens envoyés", attente(echarts4rOutput("g_durees", height = "340px")))
    ),
    carte("Évolution journalière", outils = outils_table("t_jour", "evolution_journaliere"),
          attente(reactableOutput("t_jour")))
  ),

  # ---------------------------------------------------------------- Territoires
  nav_panel(
    "Territoires", icon = bs_icon("geo-alt"),
    layout_columns(
      col_widths = c(5, 7),
      carte("Entretiens par région et statut", attente(echarts4rOutput("g_region", height = "440px"))),
      carte("Avancement par région", outils = outils_table("t_region", "avancement_regions"),
            attente(reactableOutput("t_region")))
    ),
    carte("Avancement par département", sous_titre = "regroupé par région",
          outils = outils_table("t_dep", "avancement_departements"), attente(reactableOutput("t_dep")))
  ),

  # ---------------------------------------------------------------- Enquêteurs
  nav_panel(
    "Enquêteurs", icon = bs_icon("people"),
    carte("Performance des enquêteurs", outils = outils_table("t_enq", "performance_enqueteurs"),
          div(class = "note", "Taux de rejet = entretiens rejetés au moins une fois / entretiens envoyés."),
          attente(reactableOutput("t_enq"))),
    layout_columns(
      col_widths = c(7, 5),
      carte("Entretiens démarrés par enquêteur et par jour", attente(echarts4rOutput("g_matrice", height = "620px"))),
      carte("Durée et qualité", sous_titre = "chaque point est un enquêteur (taille = nombre d'entretiens)",
            div(class = "note", "En haut à gauche : entretiens courts et souvent rejetés, à cibler en priorité."),
            attente(echarts4rOutput("g_nuage", height = "560px")))
    )
  ),

  # ---------------------------------------------------------------- Production
  nav_panel(
    "Production", icon = bs_icon("droplet-half"),
    div(class = "note", bs_icon("info-circle"), " Premiers résultats bruts 2025 : entretiens complétés ou approuvés, ",
        "lignes en incohérence bloquante (R16 à R22) exclues ; rendement et prix = médiane par type de ruche."),
    layout_columns(
      col_widths = c(6, 6),
      carte("Miel produit en 2025 par région", sous_titre = "litres", attente(echarts4rOutput("g_miel", height = "380px"))),
      carte("Prix médian du miel par région", sous_titre = "FCFA par litre", attente(echarts4rOutput("g_prix", height = "380px")))
    ),
    carte("Production déclarée par région", outils = outils_table("t_prod", "production_2025"),
          attente(reactableOutput("t_prod")))
  ),

  # ---------------------------------------------------------------- Incohérences
  nav_panel(
    "Incohérences", icon = bs_icon("shield-exclamation"),
    layout_column_wrap(
      width = 1 / 3, fill = FALSE,
      boite("À rejeter", "v_ar", "x-octagon", ANSD$rouge_pale, ANSD$rouge, "s_ar"),
      boite("À vérifier", "v_av", "search", ANSD$or_pale, "#6B4708", "s_av"),
      boite("Chez l'enquêteur", "v_ce", "person-gear", ANSD$marine_pale, ANSD$marine, "s_ce")
    ),
    navset_card_tab(
      full_screen = TRUE,
      nav_panel("À rejeter", icon = bs_icon("x-octagon"),
                div(class = "note", "Dans Survey Solutions > Entretiens : rechercher la clé (interview__key), ouvrir l'entretien, ",
                    "cliquer sur « Rejeter » et coller le commentaire (bouton « Copier »). Cliquer sur ▸ pour voir le détail."),
                outils_table("t_rejeter", "entretiens_a_rejeter"), attente(reactableOutput("t_rejeter"))),
      nav_panel("À vérifier", icon = bs_icon("search"),
                div(class = "note", "Aucune incohérence bloquante mais des valeurs atypiques : décision du superviseur."),
                outils_table("t_verifier", "entretiens_a_verifier"), attente(reactableOutput("t_verifier"))),
      nav_panel("Chez l'enquêteur", icon = bs_icon("person-gear"),
                div(class = "note", "Entretiens non encore envoyés ou déjà rejetés : à signaler aux enquêteurs."),
                outils_table("t_chez", "chez_enqueteur"), attente(reactableOutput("t_chez"))),
      nav_panel("Détail", icon = bs_icon("list-ul"),
                outils_table("t_detail", "detail_incoherences"), attente(reactableOutput("t_detail"))),
      nav_panel("Par contrôle", icon = bs_icon("bar-chart"), attente(reactableOutput("t_controles"))),
      nav_panel("Par enquêteur", icon = bs_icon("person-lines-fill"),
                outils_table("t_incoh_enq", "incoherences_par_enqueteur"), attente(reactableOutput("t_incoh_enq")))
    )
  ),

  # ---------------------------------------------------------------- Méthodologie
  nav_panel(
    "Méthodologie", icon = bs_icon("book"),
    layout_columns(
      col_widths = c(4, 8),
      carte("Mode d'emploi",
            tags$ol(class = "small",
                    tags$li("Exporter chaque version du questionnaire depuis Survey Solutions au format Stata, « Tous les statuts »."),
                    tags$li("Déposer les exports (dossiers ou .zip, sans les renommer) dans le dossier indiqué, puis « Charger / actualiser »."),
                    tags$li("Onglet Incohérences > À rejeter : rejeter chaque entretien dans Survey Solutions en collant le commentaire."),
                    tags$li("Onglet À vérifier : le superviseur confirme avec l'enquêteur puis rejette ou valide."),
                    tags$li("Ajuster les seuils si besoin (panneau de gauche) ; les listes se recalculent immédiatement.")),
            tags$hr(),
            tags$p(class = "small", tags$b("Rejet"), " : incohérence bloquante, l'entretien doit être corrigé."),
            tags$p(class = "small", tags$b("À vérifier"), " : valeur atypique à confirmer, pas forcément une erreur."),
            tags$p(class = "small", "Les contrôles sont calculés sur l'ensemble des versions (détection des doublons entre versions), ",
                   "puis filtrés selon la sélection.")),
      carte("Catalogue des contrôles", sous_titre = "seuils en vigueur", attente(reactableOutput("t_catalogue")))
    )
  ),

  nav_spacer(),
  nav_item(uiOutput("arret"))
)

# ==========================================================================================
# Serveur
# ==========================================================================================
server <- function(input, output, session) {
  donnees <- reactiveVal(NULL)
  message_sources <- reactiveVal(NULL)

  charger_donnees <- function() {
    dossiers <- c(trouver_exports(input$dossier), extraire_televerses(input$zips))
    res <- tryCatch(charger(dossiers), error = function(e) e)
    waiter_hide()
    if (inherits(res, "error")) {
      message_sources(div(class = "text-danger small mt-2", bs_icon("exclamation-triangle"), " ", conditionMessage(res)))
      showNotification(conditionMessage(res), type = "error", duration = 10)
      return()
    }
    donnees(res)
    message_sources(div(class = "note mt-2", bs_icon("check-circle-fill", class = "text-success"),
                        sprintf(" %d entretiens lus", nrow(res$m)), tags$br(), paste(res$sources, collapse = ", ")))
    updatePickerInput(session, "f_version", choices = sort(unique(res$m$version)))
    updatePickerInput(session, "f_region", choices = sort(unique(res$m$region[res$m$region != ""])))
    updatePickerInput(session, "f_enq", choices = sort(unique(res$m$enqueteur)))
    dd <- as.Date(res$m$date_debut)
    updateDateRangeInput(session, "f_dates", start = min(dd, na.rm = TRUE), end = max(dd, na.rm = TRUE),
                         min = min(dd, na.rm = TRUE), max = max(dd, na.rm = TRUE))
  }
  observeEvent(input$charger, charger_donnees(), ignoreNULL = FALSE)
  observeEvent(input$zips, charger_donnees())
  output$info_sources <- renderUI(message_sources())

  observeEvent(input$reinit, {
    for (n in c("duree_min", "duree_max", "nonrep_rejet", "rdt_miel_max", "rdt_cire_max", "prix_miel_rejet_min",
                "prix_miel_rejet_max", "prix_miel_verif_min", "prix_miel_verif_max", "ecart_mensuel_rejet",
                "variation_ruches")) updateNumericInput(session, n, value = SEUILS_DEFAUT[[n]])
  })
  observeEvent(input$reinit_filtres, {
    for (n in c("f_version", "f_region", "f_enq")) updatePickerInput(session, n, selected = character())
    dd <- as.Date(req(donnees())$m$date_debut)
    updateDateRangeInput(session, "f_dates", start = min(dd, na.rm = TRUE), end = max(dd, na.rm = TRUE))
  })

  seuils <- reactive({
    s <- SEUILS_DEFAUT
    for (n in names(s)) if (!is.null(input[[n]]) && !is.na(input[[n]])) s[[n]] <- input[[n]]
    s
  }) |> debounce(800)

  # contrôles calculés sur toutes les données (doublons entre versions), puis filtrés
  controles <- reactive(controler(req(donnees())$t, donnees()$m, seuils()))

  m_f <- reactive({
    m <- req(donnees())$m
    if (length(input$f_version)) m <- m[m$version %in% input$f_version, ]
    if (length(input$f_region)) m <- m[m$region %in% input$f_region, ]
    if (length(input$f_enq)) m <- m[m$enqueteur %in% input$f_enq, ]
    # période : ignorée tant qu'elle ne recoupe pas les données (valeur initiale du calendrier)
    dd <- as.Date(m$date_debut); p <- input$f_dates
    if (length(p) == 2 && !any(is.na(p)) && any(!is.na(dd)) &&
        p[1] <= max(dd, na.rm = TRUE) && p[2] >= min(dd, na.rm = TRUE)) {
      m <- m[is.na(dd) | (dd >= p[1] & dd <= p[2]), ]
    }
    validate(need(nrow(m) > 0, "Aucun entretien pour ces filtres."))
    m
  })
  det_f <- reactive(controles()$det[controles()$det$interview__id %in% m_f()$interview__id, ])
  tab <- reactive(tableaux_suivi(m_f(), donnees()$a, det_f(), controles()$pr, controles()$fiables))
  listes <- reactive(listes_rejet(det_f()))
  kpi <- reactive({ k <- indicateurs(m_f(), det_f(), tab()$jour); stats::setNames(k$Valeur, k$Indicateur) })

  output$arret <- renderUI({
    d <- req(donnees())
    span(class = "navbar-text small text-white-50", bs_icon("clock-history"),
         sprintf(" Données au %s", format(max(d$m$derniere_action, na.rm = TRUE), "%d/%m/%Y %H:%M")))
  })

  # ------------------------------------------------------------ Tableau de bord
  output$v_total <- renderText(fmt_n(kpi()[[1]]))
  output$s_total <- renderText(sprintf("%s enquêteurs · %s régions · %s jours", kpi()[[7]], kpi()[[11]], kpi()[[8]]))
  output$v_complet <- renderText(fmt_n(kpi()[[2]]))
  output$s_complet <- renderText(sprintf("%s des entretiens", fmt_pct(kpi()[[3]])))
  output$v_cours <- renderText(sprintf("%s · %s", fmt_n(kpi()[[4]]), fmt_n(kpi()[[5]])))
  output$s_cours <- renderText("rejetés en correction · non encore envoyés")
  output$v_rejeter <- renderText(fmt_n(kpi()[[6]]))
  output$s_rejeter <- renderText(sprintf("durée médiane d'un entretien : %s min", fmt_n(kpi()[[10]])))

  output$spark <- renderEcharts4r({
    j <- tab()$jour
    data.frame(jour = format(j$Date, "%d/%m"), n = j$`Entretiens démarrés`) |>
      e_charts(jour) |>
      e_area(n, symbol = "none", smooth = TRUE, lineStyle = list(color = ANSD$or, width = 2),
             areaStyle = list(color = "rgba(232,200,78,.25)")) |>
      e_color(ANSD$or) |> e_x_axis(show = FALSE) |> e_y_axis(show = FALSE) |> e_legend(show = FALSE) |>
      e_grid(left = 0, right = 0, top = 5, bottom = 0) |>
      e_tooltip(trigger = "axis", formatter = htmlwidgets::JS("function(p){return p[0].name + ' : ' + p[0].value[1] + ' démarrés';}"))
  })

  output$g_cumul <- renderEcharts4r({
    j <- tab()$jour
    data.frame(jour = format(j$Date, "%d/%m"), dem = j$`Cumul démarrés`, env = j$`Cumul envoyés`) |>
      e_charts(jour) |>
      e_line(dem, name = "Entretiens démarrés", symbol = "circle", symbolSize = 8, lineStyle = list(width = 2),
             areaStyle = list(opacity = 0.10)) |>
      e_line(env, name = "Envoyés au superviseur", symbol = "circle", symbolSize = 8, lineStyle = list(width = 2)) |>
      ec_style() |> e_tooltip(trigger = "axis")
  })

  output$g_jauge <- renderEcharts4r({
    e_charts() |>
      e_gauge(round(100 * kpi()[[3]], 1), "complétés ou approuvés", startAngle = 210, endAngle = -30, min = 0, max = 100,
              radius = "92%", center = c("50%", "58%"),
              progress = list(show = TRUE, width = 14, roundCap = TRUE, itemStyle = list(color = ANSD$marine)),
              axisLine = list(roundCap = TRUE, lineStyle = list(width = 14, color = list(list(1, ANSD$ligne)))),
              pointer = list(show = FALSE), axisTick = list(show = FALSE), splitLine = list(show = FALSE),
              axisLabel = list(show = FALSE), title = list(offsetCenter = c(0, "62%"), color = ANSD$gris, fontSize = 12),
              detail = list(formatter = htmlwidgets::JS("function(v){return v.toFixed(1).replace('.', ',') + ' %';}"),
                            offsetCenter = c(0, "0%"), color = ANSD$marine, fontSize = 26, fontWeight = 700)) |>
      e_text_style(fontFamily = POLICE)
  })

  output$g_donut <- renderEcharts4r({
    m <- m_f()
    d <- data.frame(groupe = GROUPES, n = vapply(GROUPES, function(g) sum(m$groupe == g), numeric(1)))
    d |> e_charts(groupe) |>
      e_pie(n, radius = c("50%", "80%"), center = c("28%", "50%"), label = list(show = FALSE),
            itemStyle = list(borderColor = "#FFFFFF", borderWidth = 2), name = "Statut") |>
      e_color(SERIES) |> e_text_style(fontFamily = POLICE) |>
      e_legend(orient = "vertical", right = 0, top = "middle", itemWidth = 10, itemHeight = 10, itemGap = 12,
               textStyle = list(fontSize = 11, color = ANSD$encre),
               formatter = htmlwidgets::JS(sprintf("function(n){var v = %s; return n.replace(' / ', '/\\n').replace(' (', '\\n(').replace(' chez', '\\nchez') + '  ' + v[n];}",
                                                   jsonlite::toJSON(as.list(stats::setNames(d$n, d$groupe)), auto_unbox = TRUE)))) |>
      e_tooltip(trigger = "item", formatter = "{b} : {c} ({d} %)", backgroundColor = "#FFFFFF", borderColor = ANSD$ligne)
  })

  graphe_regions <- function(r) {
    r <- r[order(r$Entretiens), ]
    data.frame(region = r$Région, a = r[[GROUPES[1]]], b = r[[GROUPES[2]]], c = r[[GROUPES[3]]]) |>
      e_charts(region) |>
      e_bar(a, name = GROUPES[1], stack = "s", barMaxWidth = 22, itemStyle = barres_style(TRUE)) |>
      e_bar(b, name = GROUPES[2], stack = "s", barMaxWidth = 22, itemStyle = barres_style(TRUE)) |>
      e_bar(c, name = GROUPES[3], stack = "s", barMaxWidth = 22, itemStyle = barres_style(TRUE)) |>
      e_flip_coords() |> ec_style() |> e_tooltip(trigger = "axis")
  }
  output$g_region_bord <- renderEcharts4r(graphe_regions(tab()$region))
  output$g_region <- renderEcharts4r(graphe_regions(tab()$region))

  output$g_controles <- renderEcharts4r({
    sc <- synthese_controles(det_f(), seuils())
    sc <- sc[sc$`Entretiens concernés` > 0, ]
    if (!nrow(sc)) return(graphe_vide("Aucune incohérence"))
    sc <- utils::head(sc[order(-sc$`Entretiens concernés`), ], 12)
    sc <- sc[rev(seq_len(nrow(sc))), ]
    lib <- paste0(sc$Code, " · ", ifelse(nchar(sc$Contrôle) > 42, paste0(substr(sc$Contrôle, 1, 40), "…"), sc$Contrôle))
    data.frame(lib = lib, rejet = ifelse(sc$Niveau == "Rejet", sc$`Entretiens concernés`, NA),
               verif = ifelse(sc$Niveau == "À vérifier", sc$`Entretiens concernés`, NA)) |>
      e_charts(lib) |>
      e_bar(rejet, name = "Rejet", stack = "s", barMaxWidth = 18, itemStyle = barres_style(TRUE)) |>
      e_bar(verif, name = "À vérifier", stack = "s", barMaxWidth = 18, itemStyle = barres_style(TRUE)) |>
      e_flip_coords() |> ec_style(couleurs = c(ANSD$rouge, ANSD$ocre)) |>
      e_tooltip(trigger = "axis")
  })

  # ------------------------------------------------------------ Évolution
  output$g_jour <- renderEcharts4r({
    j <- tab()$jour
    data.frame(jour = format(j$Date, "%d/%m"), dem = j$`Entretiens démarrés`, env = j$`Premiers envois au superviseur`,
               rej = j$Rejets) |>
      e_charts(jour) |>
      e_bar(dem, name = "Démarrés", barMaxWidth = 22, itemStyle = barres_style()) |>
      e_bar(env, name = "Premiers envois", barMaxWidth = 22, itemStyle = barres_style()) |>
      e_bar(rej, name = "Rejets", barMaxWidth = 22, itemStyle = barres_style()) |>
      ec_style()
  })
  output$g_durees <- renderEcharts4r({
    du <- tab()$durees
    data.frame(tranche = as.character(du$Durée), n = du$Entretiens,
               color = ifelse(du$Durée == "< 10 min", ANSD$rouge, ANSD$marine)) |>
      e_charts(tranche) |>
      e_bar(n, name = "Entretiens", barMaxWidth = 34, itemStyle = barres_style()) |>
      e_add_nested("itemStyle", color) |>
      ec_style(legende = FALSE) |> e_x_axis(axisLabel = list(interval = 0, color = ANSD$gris))
  })
  output$t_jour <- renderReactable({
    j <- tab()$jour
    tableau(j, id = "t_jour", recherche = FALSE, page = 31,
            colonnes = list(Date = colDef(format = colFormat(date = TRUE, locales = "fr-FR"), minWidth = 100),
                            Rejets = colDef(style = function(v) if (!is.na(v) && v > 0) list(color = ANSD$ocre, fontWeight = 600))))
  })

  # ------------------------------------------------------------ Territoires
  output$t_region <- renderReactable({
    tableau(tab()$region, id = "t_region", recherche = FALSE, page = 20,
            colonnes = list(Région = colDef(minWidth = 130, style = list(fontWeight = 600)),
                            `% complétés` = col_barre("% complétés"),
                            `Entretiens avec incohérence bloquante` = colDef(name = "Incohérences bloquantes",
                                                                             style = list(color = ANSD$rouge))))
  })
  output$t_dep <- renderReactable({
    d <- tab()$departement
    tableau(d, id = "t_dep", groupBy = "Région", page = 50,
            colonnes = list(Région = colDef(minWidth = 140),
                            Entretiens = colDef(aggregate = "sum"),
                            `Complété / approuvé` = colDef(aggregate = "sum"),
                            `Rejeté (en correction)` = colDef(aggregate = "sum"),
                            `En cours chez l'enquêteur` = colDef(aggregate = "sum"),
                            Enquêteurs = colDef(),
                            `% complétés` = utils::modifyList(col_barre("% complétés"), colDef(
                              aggregate = htmlwidgets::JS("function(values, rows) { var e = 0, c = 0;
                                rows.forEach(function(r) { e += r['Entretiens']; c += r['Complété / approuvé']; });
                                return e > 0 ? c / e : null; }"),
                              format = list(aggregated = colFormat(percent = TRUE, digits = 1, locales = "fr-FR"))))))
  })

  # ------------------------------------------------------------ Enquêteurs
  output$t_enq <- renderReactable({
    e <- tab()$enqueteurs
    tableau(e, id = "t_enq", page = 40, colonnes = list(
      Enquêteur = colDef(minWidth = 150, style = list(fontWeight = 600), sticky = "left"),
      `Région principale` = colDef(minWidth = 125),
      `% complétés` = col_barre("% complétés"),
      `Taux de rejet` = col_barre("Taux de rejet", ANSD$ocre),
      `Durée médiane (min)` = colDef(format = colFormat(digits = 0), style = function(v) {
        if (!is.na(v) && v < SEUILS_DEFAUT$duree_min * 2) list(color = ANSD$rouge, fontWeight = 600)
      }),
      `Entretiens par jour actif` = colDef(format = colFormat(digits = 1, locales = "fr-FR")),
      `Première activité` = colDef(format = colFormat(date = TRUE, locales = "fr-FR")),
      `Dernière activité` = colDef(format = colFormat(date = TRUE, locales = "fr-FR")),
      `Incohérences bloquantes` = colDef(style = function(v) if (!is.na(v) && v > 0) list(color = ANSD$rouge, fontWeight = 600))
    ))
  })
  output$g_matrice <- renderEcharts4r({
    mt <- tab()$matrice
    if (!nrow(mt)) return(graphe_vide())
    jours <- setdiff(names(mt), c("Enquêteur", "Total"))
    long <- tidyr::pivot_longer(mt[, c("Enquêteur", jours)], -Enquêteur, names_to = "jour", values_to = "n")
    long$jour <- factor(long$jour, levels = jours)
    long$Enquêteur <- factor(long$Enquêteur, levels = rev(mt$Enquêteur))
    long <- long[order(long$jour, long$Enquêteur), ]
    long$jour <- as.character(long$jour); long$Enquêteur <- as.character(long$Enquêteur)
    long |> e_charts(jour) |>
      e_heatmap(Enquêteur, n, itemStyle = list(borderColor = "#FFFFFF", borderWidth = 2, borderRadius = 3),
                label = list(show = TRUE, color = ANSD$encre, fontSize = 10,
                             formatter = htmlwidgets::JS("function(p){return p.value[2] > 0 ? p.value[2] : '';}"))) |>
      e_visual_map(n, inRange = list(color = c("#F4F6FA", ANSD$ciel, ANSD$bleu, ANSD$marine)), orient = "horizontal",
                   left = "center", bottom = 0, itemHeight = 160, calculable = FALSE, textStyle = list(color = ANSD$gris)) |>
      e_y_axis(type = "category", data = rev(mt$Enquêteur), axisLabel = list(fontSize = 10, color = ANSD$encre),
               axisLine = list(show = FALSE), axisTick = list(show = FALSE)) |>
      e_x_axis(position = "top", axisLine = list(show = FALSE), axisTick = list(show = FALSE)) |>
      e_grid(left = 10, right = 10, top = 30, bottom = 50, containLabel = TRUE) |>
      e_tooltip(formatter = htmlwidgets::JS("function(p){return p.value[1] + '<br>' + p.value[0] + ' : <b>' + p.value[2] + '</b> entretien(s)';}")) |>
      e_text_style(fontFamily = POLICE)
  })
  output$g_nuage <- renderEcharts4r({
    e <- tab()$enqueteurs
    e <- e[!is.na(e$`Durée médiane (min)`) & !is.na(e$`Taux de rejet`), ]
    if (!nrow(e)) return(graphe_vide())
    data.frame(duree = round(e$`Durée médiane (min)`), taux = round(100 * e$`Taux de rejet`, 1),
               n = e$Entretiens, nom = paste0(e$Enquêteur, " (", e$Entretiens, " entretiens)")) |>
      e_charts(duree) |>
      e_scatter(taux, n, bind = nom, name = "Enquêteurs", symbol_size = 2,
                itemStyle = list(color = "rgba(34,57,132,.65)", borderColor = "#FFFFFF", borderWidth = 1)) |>
      e_mark_line(data = list(xAxis = SEUILS_DEFAUT$duree_min * 2), symbol = "none", title = "",
                  lineStyle = list(color = ANSD$ocre, type = "dashed")) |>
      ec_style(legende = FALSE, declencheur = "item") |>
      e_x_axis(name = "Durée médiane (min)", nameLocation = "middle", nameGap = 28, min = 0) |>
      e_y_axis(name = "Taux de rejet (%)", nameLocation = "middle", nameGap = 40, min = 0) |>
      e_tooltip(trigger = "item", formatter = htmlwidgets::JS(
        "function(p){return '<b>' + p.name + '</b><br>Durée médiane : ' + p.value[0] + ' min<br>Taux de rejet : ' + p.value[1] + ' %';}"))
  })

  # ------------------------------------------------------------ Production
  output$g_miel <- renderEcharts4r({
    p <- tab()$production
    if (!nrow(p)) return(graphe_vide())
    p <- p[order(p$`Miel produit (L)`), ]
    data.frame(region = p$Région, miel = p$`Miel produit (L)`) |>
      e_charts(region) |> e_bar(miel, name = "Miel produit (L)", barMaxWidth = 20, itemStyle = barres_style(TRUE)) |>
      e_flip_coords() |> ec_style(legende = FALSE)
  })
  output$g_prix <- renderEcharts4r({
    p <- tab()$production
    p <- p[!is.na(p$`Prix médian du miel (FCFA/L)`), ]
    if (!nrow(p)) return(graphe_vide())
    p <- p[order(p$`Prix médian du miel (FCFA/L)`), ]
    data.frame(region = p$Région, prix = round(p$`Prix médian du miel (FCFA/L)`)) |>
      e_charts(region) |> e_bar(prix, name = "Prix médian (FCFA/L)", barMaxWidth = 20, itemStyle = barres_style(TRUE)) |>
      e_flip_coords() |> ec_style(couleurs = ANSD$ocre, legende = FALSE)
  })
  output$t_prod <- renderReactable({
    tableau(tab()$production, id = "t_prod", recherche = FALSE, page = 20,
            colonnes = list(Région = colDef(minWidth = 130, style = list(fontWeight = 600)),
                            `Prix médian du miel (FCFA/L)` = colDef(format = colFormat(digits = 0, separators = TRUE, locales = "fr-FR"))))
  })

  # ------------------------------------------------------------ Incohérences
  output$v_ar <- renderText(fmt_n(nrow(listes()$a_rejeter)))
  output$s_ar <- renderText("complétés / approuvés avec incohérence bloquante")
  output$v_av <- renderText(fmt_n(nrow(listes()$a_verifier)))
  output$s_av <- renderText("valeurs atypiques à confirmer")
  output$v_ce <- renderText(fmt_n(nrow(listes()$chez_enqueteur)))
  output$s_ce <- renderText("non encore envoyés ou déjà rejetés")

  table_liste <- function(df, id) {
    det <- det_f()
    com <- "Commentaire de rejet (à coller)"
    df$Copier <- df[[com]]
    df <- dplyr::relocate(df, Copier, .after = interview__key)
    cols <- list(
      interview__key = colDef(name = "Clé", minWidth = 110, sticky = "left", style = list(fontWeight = 700, color = ANSD$marine)),
      Copier = col_copier(),
      interview__id = colDef(show = FALSE),
      Version = colDef(minWidth = 70),
      `Statut actuel` = col_badge("Statut", 160),
      `Action dans Survey Solutions` = col_badge("Action", 175),
      Enquêteur = colDef(minWidth = 150),
      Région = colDef(minWidth = 120),
      Département = colDef(show = FALSE),
      Commune = colDef(minWidth = 130),
      `Date de début` = colDef(format = colFormat(date = TRUE, locales = "fr-FR"), minWidth = 100),
      `Nb incohérences bloquantes` = colDef(name = "Bloquantes", minWidth = 90, align = "center",
                                            style = function(v) if (!is.na(v) && v > 0) list(color = ANSD$rouge, fontWeight = 700)),
      `Nb points à vérifier` = colDef(name = "À vérifier", minWidth = 85, align = "center"),
      `Contrôles déclenchés` = colDef(name = "Contrôles", minWidth = 170),
      `Commentaire de rejet (à coller)` = colDef(show = FALSE),
      `Décision superviseur` = colDef(show = FALSE)
    )
    cols <- cols[names(cols) %in% names(df)]
    tableau(df, id = id, colonnes = cols, page = 15, wrap = FALSE,
            details = function(i) detail_entretien(det[det$interview__id == df$interview__id[i], ], df[[com]][i]))
  }
  output$t_rejeter <- renderReactable(table_liste(listes()$a_rejeter, "t_rejeter"))
  output$t_verifier <- renderReactable(table_liste(listes()$a_verifier, "t_verifier"))
  output$t_chez <- renderReactable(table_liste(listes()$chez_enqueteur, "t_chez"))
  output$t_detail <- renderReactable({
    tableau(detail_affiche(det_f()), id = "t_detail", filtres = TRUE, page = 25, colonnes = list(
      interview__key = colDef(name = "Clé", minWidth = 105, style = list(fontWeight = 700, color = ANSD$marine)),
      interview__id = colDef(show = FALSE), Statut = col_badge(NULL, 150), Niveau = col_badge(NULL, 100),
      Code = colDef(minWidth = 60), Contrôle = colDef(minWidth = 220), Message = colDef(minWidth = 320),
      Valeur = colDef(align = "right")))
  })
  output$t_controles <- renderReactable({
    sc <- synthese_controles(det_f(), seuils())
    mx <- max(c(1, sc$`Entretiens concernés`))
    tableau(sc, recherche = TRUE, page = 40, colonnes = list(
      Code = colDef(minWidth = 60, style = list(fontWeight = 700)), Niveau = col_badge(NULL, 100),
      Contrôle = colDef(minWidth = 280), Seuil = colDef(minWidth = 200),
      `Entretiens concernés` = colDef(minWidth = 160, cell = function(v, i) {
        div(class = "barre", div(class = "barre-fond", div(class = "barre-val", style = sprintf(
          "width:%.0f%%;background:%s", 100 * v / mx, if (sc$Niveau[i] == "Rejet") ANSD$rouge else ANSD$ocre))),
          span(class = "barre-txt", fmt_n(v)))
      })))
  })
  output$t_incoh_enq <- renderReactable({
    pe <- incoherences_par_enqueteur(det_f(), m_f())
    codes <- grep("^R\\d+$", names(pe), value = TRUE)
    cols <- c(list(Enquêteur = colDef(minWidth = 150, sticky = "left", style = list(fontWeight = 600)),
                   `Total bloquantes` = colDef(style = list(color = ANSD$rouge, fontWeight = 700))),
              stats::setNames(lapply(codes, function(k) colDef(minWidth = 52, align = "center", style = function(v) {
                if (!is.na(v) && v > 0) list(background = ANSD$marine_pale, color = ANSD$marine, fontWeight = 600)
                else list(color = "#C9D1DE")
              })), codes))
    tableau(pe, id = "t_incoh_enq", colonnes = cols, page = 40)
  })

  # ------------------------------------------------------------ Méthodologie
  output$t_catalogue <- renderReactable({
    cat <- catalogue_regles(seuils())
    names(cat) <- c("Code", "Niveau", "Section", "Contrôle", "Seuil")
    tableau(cat, page = 40, groupBy = "Section", colonnes = list(
      Section = colDef(minWidth = 110), Code = colDef(minWidth = 60, style = list(fontWeight = 700)),
      Niveau = col_badge(NULL, 100), Contrôle = colDef(minWidth = 300), Seuil = colDef(minWidth = 220)),
      defaultExpanded = TRUE)
  })

  # ------------------------------------------------------------ Téléchargements
  jour <- function() format(Sys.Date(), "%Y-%m-%d")
  output$dl_suivi <- downloadHandler(
    filename = function() paste0("Suivi_collecte_", jour(), ".xlsx"),
    content = function(f) ecrire_suivi_xlsx(f, tab(), m_f(), det_f(), donnees()$sources))
  output$dl_incoh <- downloadHandler(
    filename = function() paste0("Incoherences_a_rejeter_", jour(), ".xlsx"),
    content = function(f) ecrire_incoherences_xlsx(f, listes(), det_f(), m_f(), seuils()))
  output$dl_csv <- downloadHandler(
    filename = function() paste0("Liste_rejet_API_", jour(), ".csv"),
    content = function(f) ecrire_csv_api(f, listes()$a_rejeter))
}

shinyApp(ui, server)
