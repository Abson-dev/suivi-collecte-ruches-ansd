# Version de diffusion : fichier HTML autonome destiné à des personnes extérieures
#   - agrégats uniquement (aucune clé d'entretien, commune, nom, commentaire ni donnée individuelle) ;
#   - secret statistique : cellules de 1 à (seuil - 1) unités masquées (« s »), ainsi que les valeurs
#     calculées sur moins de « seuil » unités (pourcentages, durées, production, prix) ;
#   - enquêteurs anonymisés (Enquêteur 01, 02…, sans région).
# Le masquage est primaire : un total peut permettre de déduire une cellule masquée par différence.

SYMBOLE_SECRET <- "s"

masquer_effectifs <- function(df, colonnes, seuil) {
  for (k in intersect(colonnes, names(df))) {
    x <- df[[k]]
    x[!is.na(x) & x > 0 & x < seuil] <- NA
    df[[k]] <- x
  }
  df
}

masquer_si_petit <- function(df, base, colonnes, seuil) {
  petit <- is.na(df[[base]]) | df[[base]] < seuil
  for (k in intersect(colonnes, names(df))) df[[k]][petit] <- NA
  df
}

# ------------------------------------------------------------------------------------------
# 1. Agrégats (seuls objets intégrés au fichier)
# ------------------------------------------------------------------------------------------
donnees_diffusion <- function(tab, m, det, seuils, seuil = 3) {
  k <- indicateurs(m, det, tab$jour)
  kpi <- stats::setNames(k$Valeur, k$Indicateur)

  jour <- data.frame(Date = tab$jour$Date, `Entretiens démarrés` = tab$jour$`Entretiens démarrés`,
                     `Premiers envois` = tab$jour$`Premiers envois au superviseur`, Rejets = tab$jour$Rejets,
                     `Cumul démarrés` = tab$jour$`Cumul démarrés`, `Cumul envoyés` = tab$jour$`Cumul envoyés`,
                     check.names = FALSE)

  comptes <- c("Entretiens", GROUPES)
  region <- tab$region[, c("Région", comptes, "% complétés", "Durée médiane (min)")]
  region <- masquer_si_petit(region, "Entretiens", c("% complétés", "Durée médiane (min)"), seuil)
  region <- masquer_effectifs(region, comptes, seuil)

  dep <- tab$departement[, c("Région", "Département", comptes, "% complétés")]
  dep <- masquer_si_petit(dep, "Entretiens", "% complétés", seuil)
  dep <- masquer_effectifs(dep, comptes, seuil)

  statuts <- masquer_effectifs(tab$statuts, setdiff(names(tab$statuts), "Statut"), seuil)

  valeurs_prod <- setdiff(names(tab$production), "Région")
  prod <- masquer_si_petit(tab$production, "Exploitations productrices", valeurs_prod, seuil)

  e <- tab$enqueteurs
  e <- e[order(-e$Entretiens, e$Enquêteur), ]
  enq <- data.frame(Enquêteur = sprintf("Enquêteur %02d", seq_len(nrow(e))), Entretiens = e$Entretiens,
                    `Complété / approuvé` = e$`Complété / approuvé`, `% complétés` = e$`% complétés`,
                    `Taux de rejet` = e$`Taux de rejet`, `Durée médiane (min)` = e$`Durée médiane (min)`,
                    `Jours actifs` = e$`Jours actifs`, `Entretiens par jour actif` = e$`Entretiens par jour actif`,
                    `Incohérences bloquantes par entretien` = e$`Incohérences bloquantes` / e$Entretiens,
                    check.names = FALSE)
  enq <- masquer_si_petit(enq, "Entretiens", c("% complétés", "Taux de rejet", "Durée médiane (min)",
                                               "Entretiens par jour actif", "Incohérences bloquantes par entretien"), seuil)
  enq <- masquer_effectifs(enq, c("Entretiens", "Complété / approuvé"), seuil)

  sc <- synthese_controles(det, seuils)
  ctrl <- data.frame(Code = sc$Code, Niveau = sc$Niveau, Contrôle = sc$Contrôle,
                     `Entretiens concernés` = sc$`Entretiens concernés`, check.names = FALSE)
  ctrl <- masquer_effectifs(ctrl[ctrl$`Entretiens concernés` > 0, ], "Entretiens concernés", seuil)

  list(kpi = kpi, jour = jour, durees = tab$durees, region = region, departement = dep, statuts = statuts,
       production = prod, enqueteurs = enq, controles = ctrl, seuil = seuil)
}

# ------------------------------------------------------------------------------------------
# 2. Mise en page
# ------------------------------------------------------------------------------------------
col_secret <- function(...) reactable::colDef(na = SYMBOLE_SECRET, ...)

tableau_diffusion <- function(df, pct = character(), ...) {
  colonnes <- lapply(names(df), function(n) {
    if (n %in% pct) return(reactable::colDef(na = SYMBOLE_SECRET, align = "right",
                                            format = reactable::colFormat(percent = TRUE, digits = 1, locales = "fr-FR")))
    if (is.numeric(df[[n]])) return(col_secret(align = "right"))
    reactable::colDef()
  })
  names(colonnes) <- names(df)
  tableau(df, colonnes = utils::modifyList(colonnes, list(...)), page = 50)
}

page_diffusion <- function(dd, meta) {
  k <- dd$kpi
  logo <- base64enc_logo()
  hauteur <- "330px"

  j <- data.frame(jour = format(dd$jour$Date, "%d/%m"), dem = dd$jour$`Cumul démarrés`, env = dd$jour$`Cumul envoyés`,
                  d = dd$jour$`Entretiens démarrés`, p = dd$jour$`Premiers envois`, r = dd$jour$Rejets)
  g_cumul <- j |> echarts4r::e_charts(jour, height = hauteur) |>
    echarts4r::e_line(dem, name = "Entretiens démarrés", symbolSize = 8, lineStyle = list(width = 2), areaStyle = list(opacity = 0.10)) |>
    echarts4r::e_line(env, name = "Envoyés au superviseur", symbolSize = 8, lineStyle = list(width = 2)) |> ec_style()
  g_jour <- j |> echarts4r::e_charts(jour, height = hauteur) |>
    echarts4r::e_bar(d, name = "Démarrés", barMaxWidth = 22, itemStyle = barres_style()) |>
    echarts4r::e_bar(p, name = "Premiers envois", barMaxWidth = 22, itemStyle = barres_style()) |>
    echarts4r::e_bar(r, name = "Rejets", barMaxWidth = 22, itemStyle = barres_style()) |> ec_style()

  r <- dd$region[!is.na(dd$region$Entretiens), ]
  r <- r[order(r$Entretiens), ]
  # les régions dont l'effectif est masqué n'apparaissent pas dans le graphique (voir le tableau)
  g_region <- data.frame(region = r$Région, a = r[[GROUPES[1]]], b = r[[GROUPES[2]]], c = r[[GROUPES[3]]]) |>
    echarts4r::e_charts(region, height = "420px") |>
    echarts4r::e_bar(a, name = GROUPES[1], stack = "s", barMaxWidth = 22, itemStyle = barres_style(TRUE)) |>
    echarts4r::e_bar(b, name = GROUPES[2], stack = "s", barMaxWidth = 22, itemStyle = barres_style(TRUE)) |>
    echarts4r::e_bar(c, name = GROUPES[3], stack = "s", barMaxWidth = 22, itemStyle = barres_style(TRUE)) |>
    echarts4r::e_flip_coords() |> ec_style()

  du <- dd$durees
  g_durees <- data.frame(tranche = as.character(du$Durée), n = du$Entretiens) |>
    echarts4r::e_charts(tranche, height = hauteur) |>
    echarts4r::e_bar(n, name = "Entretiens", barMaxWidth = 34, itemStyle = barres_style()) |>
    ec_style(legende = FALSE) |> echarts4r::e_x_axis(axisLabel = list(interval = 0, color = ANSD$gris))

  p <- dd$production[!is.na(dd$production$`Miel produit (L)`), ]
  p <- p[order(p$`Miel produit (L)`), ]
  g_miel <- if (nrow(p)) {
    data.frame(region = p$Région, miel = p$`Miel produit (L)`) |>
      echarts4r::e_charts(region, height = hauteur) |>
      echarts4r::e_bar(miel, name = "Miel produit (L)", barMaxWidth = 20, itemStyle = barres_style(TRUE)) |>
      echarts4r::e_flip_coords() |> ec_style(legende = FALSE)
  } else shiny::p(class = "note", "Toutes les valeurs sont soumises au secret statistique.")

  sc <- dd$controles[!is.na(dd$controles$`Entretiens concernés`), ]
  sc <- utils::head(sc[order(-sc$`Entretiens concernés`), ], 12)
  sc <- sc[rev(seq_len(nrow(sc))), ]
  g_ctrl <- if (nrow(sc)) {
    lib <- paste0(sc$Code, " · ", ifelse(nchar(sc$Contrôle) > 42, paste0(substr(sc$Contrôle, 1, 40), "…"), sc$Contrôle))
    data.frame(lib = lib, rejet = ifelse(sc$Niveau == "Rejet", sc$`Entretiens concernés`, NA),
               verif = ifelse(sc$Niveau == "À vérifier", sc$`Entretiens concernés`, NA)) |>
      echarts4r::e_charts(lib, height = "380px") |>
      echarts4r::e_bar(rejet, name = "Rejet", stack = "s", barMaxWidth = 18, itemStyle = barres_style(TRUE)) |>
      echarts4r::e_bar(verif, name = "À vérifier", stack = "s", barMaxWidth = 18, itemStyle = barres_style(TRUE)) |>
      echarts4r::e_flip_coords() |> ec_style(couleurs = c(ANSD$rouge, ANSD$ocre))
  } else shiny::p(class = "note", "Aucune incohérence.")

  e <- dd$enqueteurs[!is.na(dd$enqueteurs$`Taux de rejet`) & !is.na(dd$enqueteurs$`Durée médiane (min)`), ]
  g_nuage <- if (nrow(e)) {
    data.frame(duree = round(e$`Durée médiane (min)`), taux = round(100 * e$`Taux de rejet`, 1), n = e$Entretiens,
               nom = e$Enquêteur) |>
      echarts4r::e_charts(duree, height = "380px") |>
      echarts4r::e_scatter(taux, n, bind = nom, name = "Enquêteurs", symbol_size = 2,
                           itemStyle = list(color = "rgba(34,57,132,.65)", borderColor = "#FFFFFF", borderWidth = 1)) |>
      ec_style(legende = FALSE, declencheur = "item") |>
      echarts4r::e_x_axis(name = "Durée médiane (min)", nameLocation = "middle", nameGap = 28, min = 0) |>
      echarts4r::e_y_axis(name = "Taux de rejet (%)", nameLocation = "middle", nameGap = 40, min = 0) |>
      echarts4r::e_tooltip(trigger = "item", formatter = htmlwidgets::JS(
        "function(p){return '<b>' + p.name + '</b><br>Durée médiane : ' + p.value[0] + ' min<br>Taux de rejet : ' + p.value[1] + ' %';}"))
  } else shiny::p(class = "note", "Données insuffisantes.")

  vb <- function(titre, valeur, sous, icone, fond, encre) {
    bslib::value_box(title = titre, value = valeur, showcase = bsicons::bs_icon(icone),
                     theme = bslib::value_box_theme(bg = fond, fg = encre), shiny::p(class = "small mb-0", sous))
  }
  section <- function(titre, ...) shiny::tagList(shiny::h4(class = "section-diffusion", titre), ...)

  bslib::page_fluid(
    theme = theme_ansd(), title = "Suivi de la collecte - version de diffusion (ANSD)",
    shiny::tags$style(shiny::HTML(sprintf("
      body { padding-bottom: 2rem; }
      .entete-diffusion { background: %1$s; color: #fff; border-bottom: 4px solid %2$s; margin: 0 -12px 1.2rem;
                          padding: .9rem 1.5rem; display: flex; align-items: center; gap: 1rem; }
      .entete-diffusion img { height: 52px; background: #fff; border-radius: 8px; padding: 3px 6px; }
      .entete-diffusion h1 { font-size: 1.35rem; margin: 0; font-weight: 600; }
      .entete-diffusion .sous { color: %2$s; font-size: .85rem; }
      .bandeau { background: %3$s; border-left: 4px solid %2$s; padding: .6rem 1rem; border-radius: .4rem;
                 font-size: .82rem; color: %4$s; margin-bottom: 1rem; }
      .section-diffusion { color: %1$s; font-weight: 700; font-size: 1.05rem; margin: 1.6rem 0 .7rem;
                           padding-bottom: .3rem; border-bottom: 2px solid %5$s; }
      @media print { .entete-diffusion { -webkit-print-color-adjust: exact; print-color-adjust: exact; }
                     .card { break-inside: avoid; } }",
      ANSD$marine, ANSD$or, ANSD$or_pale, ANSD$encre, ANSD$ligne))),

    shiny::div(class = "entete-diffusion",
               shiny::tags$img(src = logo, alt = "ANSD"),
               shiny::div(shiny::h1("Suivi de la collecte - Ruches d'abeilles"),
                          shiny::div(class = "sous", "Recensement complémentaire et étude de la productivité des ruches d'abeilles · version de diffusion"))),
    shiny::div(class = "bandeau",
               shiny::tags$b(sprintf("Données arrêtées au %s", meta$arret)), " · ", meta$filtres, " · généré le ", meta$genere, shiny::tags$br(),
               "Document de diffusion : agrégats uniquement, aucune donnée individuelle. ",
               sprintf("Secret statistique : « %s » signale une valeur masquée (moins de %d unités). ", SYMBOLE_SECRET, dd$seuil),
               "Les enquêteurs sont anonymisés."),

    bslib::layout_column_wrap(
      width = 1 / 4, fill = FALSE,
      vb("Entretiens réalisés", fmt_n(k[[1]]), sprintf("%s enquêteurs · %s régions · %s jours", k[[7]], k[[11]], k[[8]]),
         "clipboard-data", ANSD$marine, "#FFFFFF"),
      vb("Complétés ou approuvés", fmt_n(k[[2]]), sprintf("%s des entretiens", fmt_pct(k[[3]])), "check2-circle",
         ANSD$marine_pale, ANSD$marine),
      vb("Rejetés ou en cours", sprintf("%s · %s", fmt_n(k[[4]]), fmt_n(k[[5]])), "rejetés en correction · non encore envoyés",
         "hourglass-split", ANSD$or_pale, "#6B4708"),
      vb("Durée médiane", paste(fmt_n(k[[10]]), "min"), "par entretien envoyé", "stopwatch", "#EEF1F6", ANSD$encre)
    ),

    section("Progression de la collecte",
            bslib::layout_columns(col_widths = c(6, 6),
                                  carte("Entretiens cumulés", g_cumul), carte("Activité quotidienne", g_jour))),
    section("Avancement territorial",
            bslib::layout_columns(col_widths = c(5, 7),
                                  carte("Entretiens par région et statut", g_region),
                                  carte("Avancement par région", tableau_diffusion(dd$region, pct = "% complétés"))),
            carte("Avancement par département", tableau_diffusion(dd$departement, pct = "% complétés"))),
    section("Qualité de la collecte",
            bslib::layout_columns(col_widths = c(6, 6),
                                  carte("Durée des entretiens envoyés", g_durees),
                                  carte("Contrôles de cohérence les plus fréquents", sous_titre = "entretiens concernés", g_ctrl)),
            bslib::layout_columns(col_widths = c(4, 8),
                                  carte("Statut des entretiens par version", tableau_diffusion(dd$statuts)),
                                  carte("Contrôles de cohérence", tableau_diffusion(dd$controles,
                                        Niveau = col_badge(NULL, 100), Contrôle = reactable::colDef(minWidth = 300))))),
    section("Enquêteurs (anonymisés)",
            bslib::layout_columns(col_widths = c(7, 5),
                                  carte("Indicateurs par enquêteur", tableau_diffusion(
                                    dd$enqueteurs, pct = c("% complétés", "Taux de rejet"),
                                    Enquêteur = reactable::colDef(minWidth = 115, style = list(fontWeight = 600)),
                                    `Durée médiane (min)` = col_secret(align = "right", format = reactable::colFormat(digits = 0)),
                                    `Entretiens par jour actif` = col_secret(align = "right", format = reactable::colFormat(digits = 1, locales = "fr-FR")),
                                    `Incohérences bloquantes par entretien` = col_secret(align = "right", format = reactable::colFormat(digits = 2, locales = "fr-FR")))),
                                  carte("Durée et qualité", sous_titre = "un point par enquêteur", g_nuage))),
    section("Production déclarée en 2025 (premiers résultats bruts)",
            shiny::p(class = "note", "Entretiens complétés ou approuvés ; lignes en incohérence bloquante exclues ; rendement et prix = médiane par type de ruche."),
            bslib::layout_columns(col_widths = c(5, 7),
                                  carte("Miel produit par région", sous_titre = "litres", g_miel),
                                  carte("Production par région", tableau_diffusion(dd$production,
                                        `Prix médian du miel (FCFA/L)` = col_secret(align = "right", format = reactable::colFormat(digits = 0, separators = TRUE, locales = "fr-FR")))))),
    shiny::div(class = "pied", "ANSD · Source : exports Survey Solutions, traitement par l'application de suivi de la collecte · ",
               "Document généré automatiquement - premiers résultats non apurés")
  )
}

base64enc_logo <- function() {
  f <- file.path("www", "logo_ansd.png")
  if (!file.exists(f)) return("")
  paste0("data:image/png;base64,", jsonlite::base64_enc(readBin(f, "raw", file.size(f))))
}

# ------------------------------------------------------------------------------------------
# 3. Fichier HTML autonome (scripts et styles intégrés : s'ouvre hors connexion)
# ------------------------------------------------------------------------------------------
ecrire_html_autonome <- function(page, fichier, titre = "Version de diffusion") {
  rt <- htmltools::renderTags(page)
  deps <- htmltools::resolveDependencies(rt$dependencies)
  lire <- function(chemin) paste(readLines(chemin, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  morceaux <- unlist(lapply(deps, function(d) {
    dossier <- d$src$file
    if (is.null(dossier)) return(character())
    if (!is.null(d$package)) dossier <- system.file(dossier, package = d$package)
    css <- vapply(d$stylesheet, function(s) {
      ch <- file.path(dossier, if (is.list(s)) s$href else s)
      if (file.exists(ch)) paste0("<style>", lire(ch), "</style>") else ""
    }, "")
    js <- vapply(d$script, function(s) {
      ch <- file.path(dossier, if (is.list(s)) s$src else s)
      if (file.exists(ch)) paste0("<script>", gsub("</script", "<\\/script", lire(ch), fixed = TRUE), "</script>") else ""
    }, "")
    c(as.character(d$head), css, js)
  }))
  html <- paste0("<!DOCTYPE html>\n<html lang=\"fr\">\n<head>\n<meta charset=\"utf-8\">\n",
                 "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n",
                 "<title>", htmltools::htmlEscape(titre), "</title>\n",
                 paste(morceaux, collapse = "\n"), "\n", rt$head, "\n</head>\n<body>\n", rt$html, "\n</body>\n</html>\n")
  con <- file(fichier, open = "wb")
  on.exit(close(con))
  writeBin(charToRaw(enc2utf8(html)), con)
  invisible(fichier)
}
