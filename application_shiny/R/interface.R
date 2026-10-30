# Charte graphique ANSD et composants d'interface (thème, graphiques echarts4r, tableaux reactable)

# Couleurs relevées sur le logo officiel et le site www.ansd.sn
ANSD <- list(
  marine = "#223984", royal = "#003399", nuit = "#1D4474", bleu = "#7CAED1", ciel = "#ABD9E8",
  or = "#E8C84E", ocre = "#CA8514", encre = "#1D2433", gris = "#5B6475", fond = "#F4F6FA",
  ligne = "#E3E8F0", rouge = "#B42318", rouge_pale = "#FBE3E1", or_pale = "#FBF0C9", marine_pale = "#E6EBF7"
)
# ordre fixe des séries : marine, ocre, bleu moyen
SERIES <- c(ANSD$marine, ANSD$ocre, ANSD$bleu)
POLICE <- "'Segoe UI', Roboto, 'Helvetica Neue', Arial, sans-serif"

CSS_ANSD <- sprintf("
body { background: %1$s; }
.navbar { border-bottom: 4px solid %2$s; box-shadow: 0 2px 8px rgba(34,57,132,.18); }
.navbar .navbar-brand { display: flex; align-items: center; gap: .6rem; font-weight: 600; }
.navbar .navbar-brand img { height: 38px; background: #fff; border-radius: 6px; padding: 2px 5px; }
.navbar .nav-link { font-weight: 500; }
.navbar .nav-link.active { box-shadow: inset 0 -3px 0 %2$s; }
.bslib-sidebar-layout > .sidebar { background: #fff; border-right: 1px solid %3$s; }
.sidebar .accordion-button { font-weight: 600; color: %4$s; }
.sidebar .accordion-button:not(.collapsed) { background: %5$s; box-shadow: inset 3px 0 0 %2$s; }
.card { border: 1px solid %3$s; border-radius: .7rem; box-shadow: 0 1px 3px rgba(29,36,51,.06); }
.card-header { background: #fff; border-bottom: 1px solid %3$s; font-weight: 600; color: %4$s;
               display: flex; align-items: center; justify-content: space-between; gap: .5rem; }
.card-header .sous-titre { font-weight: 400; color: %6$s; font-size: .8rem; }
.bslib-value-box { border: none; border-radius: .7rem; }
.bslib-value-box .value-box-title { font-weight: 500; opacity: .9; }
.bslib-value-box .value-box-value { font-weight: 700; }
.note { color: %6$s; font-size: .82rem; margin-bottom: .5rem; }
.badge-ansd { display: inline-block; padding: .15rem .55rem; border-radius: 999px; font-size: .75rem;
              font-weight: 600; white-space: nowrap; }
.barre { display: flex; align-items: center; gap: .45rem; }
.barre-fond { flex: 1; background: %3$s; border-radius: 4px; height: 8px; min-width: 50px; }
.barre-val { height: 8px; border-radius: 4px; }
.barre-txt { font-variant-numeric: tabular-nums; min-width: 3.2rem; text-align: right; font-size: .8rem; }
.commentaire { white-space: normal; line-height: 1.35; font-size: .8rem; }
.btn-copier { margin-top: .35rem; padding: .1rem .5rem; font-size: .75rem; }
.detail-incoh { padding: .5rem 1rem 1rem 2.5rem; background: %1$s; }
.detail-incoh table { width: 100%%; font-size: .8rem; }
.detail-incoh td { padding: .25rem .5rem; border-bottom: 1px solid %3$s; vertical-align: top; }
.outils-table { display: flex; justify-content: flex-end; gap: .4rem; margin-bottom: .4rem; }
.pied { color: %6$s; font-size: .75rem; text-align: center; padding: .75rem 0 .25rem; }
.rt-tbody .rt-td { font-variant-numeric: tabular-nums; }
.bootstrap-select > .dropdown-toggle, .bootstrap-select > .dropdown-toggle.bs-placeholder {
  background: #fff !important; border: 1px solid %3$s !important; color: %7$s !important; }
.bootstrap-select > .dropdown-toggle:focus { box-shadow: 0 0 0 .2rem rgba(34,57,132,.15) !important; }
.bootstrap-select .dropdown-menu li.selected a, .bootstrap-select .dropdown-item.active { background: %5$s; color: %4$s; }
", ANSD$fond, ANSD$or, ANSD$ligne, ANSD$marine, ANSD$marine_pale, ANSD$gris, ANSD$encre)

theme_ansd <- function() {
  bslib::bs_theme(
    version = 5, primary = ANSD$marine, secondary = ANSD$bleu, info = "#4AB6D5", warning = ANSD$or,
    danger = ANSD$rouge, success = "#1E7B4F", bg = "#FFFFFF", fg = ANSD$encre,
    base_font = bslib::font_collection("Segoe UI", "Roboto", "Helvetica Neue", "Arial", "sans-serif"),
    heading_font = bslib::font_collection("Segoe UI", "Roboto", "Helvetica Neue", "Arial", "sans-serif"),
    font_scale = 0.9, "border-radius" = "0.55rem", "link-color" = ANSD$royal
  ) |> bslib::bs_add_rules(CSS_ANSD)
}

fmt_n <- function(x, d = 0) ifelse(is.na(x), "–", formatC(x, format = "f", digits = d, big.mark = " ", decimal.mark = ","))
fmt_pct <- function(x, d = 1) ifelse(is.na(x), "–", paste0(fmt_n(100 * x, d), " %"))

# ------------------------------------------------------------------------------------------
# Graphiques echarts4r
# ------------------------------------------------------------------------------------------
# séparateur de milliers à la française sur les axes numériques (les libellés texte sont inchangés)
FORMAT_AXE <- htmlwidgets::JS("function(v){ return (typeof v === 'number') ? v.toLocaleString('fr-FR') : v; }")

ec_style <- function(e, couleurs = SERIES, legende = TRUE, declencheur = "axis") {
  e <- e |>
    echarts4r::e_color(couleurs) |>
    echarts4r::e_tooltip(trigger = declencheur, backgroundColor = "#FFFFFF", borderColor = ANSD$ligne,
                         textStyle = list(color = ANSD$encre, fontFamily = POLICE),
                         axisPointer = list(type = "shadow", shadowStyle = list(color = "rgba(124,174,209,.15)"))) |>
    echarts4r::e_grid(left = 10, right = 20, top = 25, bottom = if (legende) 45 else 15, containLabel = TRUE) |>
    echarts4r::e_text_style(fontFamily = POLICE, color = ANSD$gris) |>
    echarts4r::e_y_axis(splitLine = list(lineStyle = list(color = "#EEF1F6")), axisLine = list(show = FALSE),
                        axisTick = list(show = FALSE), axisLabel = list(color = ANSD$gris, formatter = FORMAT_AXE)) |>
    echarts4r::e_x_axis(axisLine = list(lineStyle = list(color = "#C9D1DE")), axisTick = list(show = FALSE),
                        axisLabel = list(color = ANSD$gris, formatter = FORMAT_AXE)) |>
    echarts4r::e_toolbox_feature(feature = "saveAsImage", title = "Enregistrer l'image") |>
    echarts4r::e_toolbox(right = 5, top = -5, iconStyle = list(borderColor = ANSD$gris))
  if (legende) echarts4r::e_legend(e, bottom = 0, itemWidth = 14, itemHeight = 10,
                                   textStyle = list(color = ANSD$encre))
  else echarts4r::e_legend(e, show = FALSE)
}

barres_style <- function(horizontal = FALSE) {
  list(borderRadius = if (horizontal) c(0, 4, 4, 0) else c(4, 4, 0, 0), borderColor = "#FFFFFF", borderWidth = 1)
}

graphe_vide <- function(texte = "Aucune donnée") {
  echarts4r::e_charts() |> echarts4r::e_title(texte, left = "center", top = "middle",
                                              textStyle = list(color = ANSD$gris, fontSize = 13, fontWeight = 400))
}

# ------------------------------------------------------------------------------------------
# Tableaux reactable
# ------------------------------------------------------------------------------------------
THEME_TABLE <- reactable::reactableTheme(
  color = ANSD$encre, borderColor = ANSD$ligne, stripedColor = "#F8FAFD", highlightColor = "#EEF3FA",
  cellPadding = "7px 9px", style = list(fontFamily = POLICE, fontSize = "0.84rem"),
  headerStyle = list(background = ANSD$marine_pale, color = ANSD$marine, fontWeight = 600, fontSize = "0.8rem",
                     borderBottom = paste0("2px solid ", ANSD$marine)),
  groupHeaderStyle = list(color = ANSD$marine, fontWeight = 700),
  searchInputStyle = list(width = "280px", borderColor = ANSD$ligne),
  paginationStyle = list(color = ANSD$gris), rowSelectedStyle = list(background = ANSD$marine_pale)
)
LANGUE_TABLE <- reactable::reactableLang(
  searchPlaceholder = "Rechercher…", noData = "Aucune donnée", filterPlaceholder = "Filtrer…",
  pageInfo = "{rowStart}–{rowEnd} sur {rows} lignes", pagePrevious = "‹ Précédent", pageNext = "Suivant ›",
  pagePreviousLabel = "Page précédente", pageNextLabel = "Page suivante", pageSizeOptions = "Afficher {rows}",
  detailsExpandLabel = "Afficher le détail"
)

tableau <- function(df, colonnes = list(), page = 15, recherche = TRUE, filtres = FALSE, id = NULL, wrap = TRUE, ...) {
  dbl <- vapply(df, function(x) is.double(x) && !inherits(x, c("Date", "POSIXt")), logical(1))
  defauts <- lapply(names(df)[dbl], function(n) {
    entier <- all(df[[n]] == round(df[[n]]), na.rm = TRUE)
    reactable::colDef(format = reactable::colFormat(separators = TRUE, locales = "fr-FR", digits = if (entier) 0 else 1),
                      align = "right")
  })
  names(defauts) <- names(df)[dbl]
  colonnes <- utils::modifyList(defauts, colonnes)
  reactable::reactable(
    df, columns = colonnes, searchable = recherche, filterable = filtres, striped = TRUE, highlight = TRUE,
    compact = TRUE, wrap = wrap, resizable = TRUE, defaultPageSize = page, showPageSizeOptions = nrow(df) > page,
    pageSizeOptions = c(10, 15, 25, 50, 100), theme = THEME_TABLE, language = LANGUE_TABLE,
    # dans Shiny, l'identifiant du tableau (pour l'export CSV) est celui de la sortie
    elementId = if (is.null(shiny::getDefaultReactiveDomain())) id else NULL,
    defaultColDef = reactable::colDef(minWidth = 90, headerVAlign = "bottom"), ...)
}

outils_table <- function(id, fichier) {
  shiny::div(class = "outils-table",
             shiny::tags$button(class = "btn btn-sm btn-outline-primary",
                                onclick = sprintf("Reactable.downloadDataCSV('%s', '%s.csv', {sep: ';', dec: ','})", id, fichier),
                                bsicons::bs_icon("download"), " CSV"))
}

col_barre <- function(nom, couleur = ANSD$marine, largeur = 150) {
  reactable::colDef(name = nom, minWidth = largeur, align = "left", cell = function(v) {
    if (is.na(v)) return("–")
    shiny::div(class = "barre",
               shiny::div(class = "barre-fond", shiny::div(class = "barre-val",
                                                           style = sprintf("width:%.0f%%;background:%s", 100 * min(max(v, 0), 1), couleur))),
               shiny::span(class = "barre-txt", fmt_pct(v)))
  })
}

COULEURS_STATUT <- list(
  "Complété" = c(ANSD$marine_pale, ANSD$marine), "Approuvé superviseur" = c("#DDF1E6", "#1E7B4F"),
  "Approuvé siège" = c("#DDF1E6", "#1E7B4F"), "Rejeté par le superviseur" = c(ANSD$or_pale, "#8A5A0B"),
  "Rejeté par le siège" = c(ANSD$or_pale, "#8A5A0B"), "Chez l'enquêteur" = c("#EEF1F6", ANSD$gris),
  "Rejet" = c(ANSD$rouge_pale, ANSD$rouge), "À vérifier" = c(ANSD$or_pale, "#8A5A0B"),
  "Rejeter (superviseur)" = c(ANSD$rouge_pale, ANSD$rouge), "Rejeter (siège)" = c(ANSD$rouge_pale, ANSD$rouge),
  "Désapprouver puis rejeter (siège)" = c(ANSD$rouge_pale, ANSD$rouge)
)

badge <- function(v) {
  if (is.na(v) || !nzchar(v)) return("")
  cl <- COULEURS_STATUT[[v]]
  if (is.null(cl)) cl <- c("#EEF1F6", ANSD$gris)
  shiny::span(class = "badge-ansd", style = sprintf("background:%s;color:%s", cl[1], cl[2]), v)
}
col_badge <- function(nom = NULL, largeur = 140) reactable::colDef(name = nom, cell = badge, minWidth = largeur)

bouton_copier <- function(texte, libelle = "Copier") {
  shiny::tags$button(class = "btn btn-sm btn-outline-primary btn-copier", title = "Copier le commentaire de rejet",
                     onclick = sprintf("event.stopPropagation(); navigator.clipboard.writeText(%s); this.innerHTML='Copié ✓';",
                                       jsonlite::toJSON(texte, auto_unbox = TRUE)),
                     paste0("⧉ ", libelle))
}
col_copier <- function() {
  reactable::colDef(name = "Commentaire", minWidth = 110, sortable = FALSE, filterable = FALSE,
                    cell = function(v) bouton_copier(v))
}

# Détail des incohérences d'un entretien (ligne dépliable)
detail_entretien <- function(det_id, commentaire = NULL) {
  shiny::div(class = "detail-incoh",
             if (!is.null(commentaire) && nzchar(commentaire))
               shiny::div(class = "mb-2 p-2 bg-white border rounded",
                          shiny::div(class = "note mb-1", shiny::tags$b("Commentaire de rejet à coller dans Survey Solutions")),
                          shiny::div(class = "commentaire", commentaire), bouton_copier(commentaire)),
             shiny::tags$table(lapply(seq_len(nrow(det_id)), function(i) {
               r <- det_id[i, ]
               shiny::tags$tr(shiny::tags$td(style = "width:60px", shiny::tags$b(r$code)),
                              shiny::tags$td(style = "width:110px", badge(r$niveau)),
                              shiny::tags$td(style = "width:30%", r$controle),
                              shiny::tags$td(r$message))
             })),
             shiny::div(class = "note mt-2", "interview__id : ", shiny::tags$code(det_id$interview__id[1])))
}

carte <- function(titre, ..., sous_titre = NULL, hauteur = NULL, outils = NULL) {
  bslib::card(full_screen = TRUE, height = hauteur,
              bslib::card_header(shiny::span(titre, if (!is.null(sous_titre)) shiny::span(class = "sous-titre", paste0(" · ", sous_titre))),
                                 outils),
              bslib::card_body(...))
}

attente <- function(x) shinycssloaders::withSpinner(x, type = 8, color = ANSD$marine, size = 0.6)
