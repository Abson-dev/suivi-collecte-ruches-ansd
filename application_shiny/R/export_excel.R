# Export des classeurs Excel (même structure que les sorties du programme Python)

STYLE_TITRE <- openxlsx::createStyle(fontName = "Arial", fontSize = 14, textDecoration = "bold", fontColour = "#0D366B")
STYLE_SOUS <- openxlsx::createStyle(fontName = "Arial", fontSize = 9, textDecoration = "italic", fontColour = "#52514E")
STYLE_ENTETE <- openxlsx::createStyle(fontName = "Arial", fontSize = 10, textDecoration = "bold", fontColour = "#FFFFFF",
                                      fgFill = "#1C5CAB", wrapText = TRUE, valign = "center")
STYLE_CORPS <- openxlsx::createStyle(fontName = "Arial", fontSize = 10, border = "bottom", borderColour = "#D9D9D9")
STYLE_PCT <- openxlsx::createStyle(numFmt = "0.0%")
STYLE_DEC <- openxlsx::createStyle(numFmt = "#,##0.0")
STYLE_ENT <- openxlsx::createStyle(numFmt = "#,##0")
STYLE_DATE <- openxlsx::createStyle(numFmt = "dd/mm/yyyy")
STYLE_REJET <- openxlsx::createStyle(fgFill = "#F8D3D3")
STYLE_VERIF <- openxlsx::createStyle(fgFill = "#FDEBC4")
STYLE_SAISIE <- openxlsx::createStyle(fgFill = "#FFFF00")
STYLE_RETOUR <- openxlsx::createStyle(wrapText = TRUE, valign = "top")

feuille <- function(wb, nom, df, titre, sous_titre = "", larges = character(), ligne = 4, filtre = TRUE,
                    pct = character(), ent = character(), dec = character()) {
  openxlsx::addWorksheet(wb, nom, gridLines = FALSE)
  openxlsx::writeData(wb, nom, titre, startRow = 1)
  openxlsx::addStyle(wb, nom, STYLE_TITRE, rows = 1, cols = 1)
  if (nzchar(sous_titre)) {
    openxlsx::writeData(wb, nom, sous_titre, startRow = 2)
    openxlsx::addStyle(wb, nom, STYLE_SOUS, rows = 2, cols = 1)
  }
  ajouter_table(wb, nom, df, ligne, filtre, larges, pct, ent, dec)
  if (filtre) openxlsx::freezePane(wb, nom, firstActiveRow = ligne + 1)
}

ajouter_table <- function(wb, nom, df, ligne, filtre = FALSE, larges = character(), pct = character(),
                          ent = character(), dec = character()) {
  openxlsx::writeData(wb, nom, df, startRow = ligne, headerStyle = STYLE_ENTETE, withFilter = filtre && nrow(df) > 0)
  n <- nrow(df)
  if (n) {
    lignes <- (ligne + 1):(ligne + n)
    openxlsx::addStyle(wb, nom, STYLE_CORPS, rows = lignes, cols = seq_along(df), gridExpand = TRUE)
    for (k in seq_along(df)) {
      nm <- names(df)[k]; x <- df[[k]]
      st <- if (nm %in% pct) STYLE_PCT else if (nm %in% ent) STYLE_ENT else if (nm %in% dec) STYLE_DEC else
        if (inherits(x, c("Date", "POSIXct"))) STYLE_DATE else if (is.numeric(x) && all(x == round(x), na.rm = TRUE)) STYLE_ENT else
          if (is.numeric(x)) STYLE_DEC else NULL
      if (!is.null(st)) openxlsx::addStyle(wb, nom, st, rows = lignes, cols = k, stack = TRUE)
    }
  }
  larg <- vapply(seq_along(df), function(k) {
    nm <- names(df)[k]
    if (nm %in% names(larges)) return(larges[[nm]])
    v <- c(nchar(nm) * 0.8, nchar(format(utils::head(df[[k]], 200))))
    min(max(10, max(v, na.rm = TRUE) + 2), 45)
  }, numeric(1))
  openxlsx::setColWidths(wb, nom, cols = seq_along(df), widths = larg)
  openxlsx::setRowHeights(wb, nom, rows = ligne, heights = 30)
}

colorer_niveau <- function(wb, nom, niveaux, col, ligne = 4) {
  r <- which(niveaux == "Rejet"); v <- which(niveaux == "À vérifier")
  if (length(r)) openxlsx::addStyle(wb, nom, STYLE_REJET, rows = ligne + r, cols = col, stack = TRUE)
  if (length(v)) openxlsx::addStyle(wb, nom, STYLE_VERIF, rows = ligne + v, cols = col, stack = TRUE)
}

ecrire_suivi_xlsx <- function(fichier, tab, m, det, sources) {
  wb <- openxlsx::createWorkbook()
  kpi <- indicateurs(m, det, tab$jour)
  arret <- max(m$derniere_action, na.rm = TRUE)
  feuille(wb, "Synthèse", kpi[, 1:2], "Suivi de la collecte - Enquête complémentaire et productivité des ruches d'abeilles",
          sprintf("Données arrêtées au %s (dernière synchronisation) - exports : %s - généré le %s",
                  format(arret, "%d/%m/%Y %H:%M"), paste(sources, collapse = ", "), format(Sys.time(), "%d/%m/%Y %H:%M")),
          larges = c(Indicateur = 62, Valeur = 14), filtre = FALSE)
  openxlsx::addStyle(wb, "Synthèse", STYLE_PCT, rows = 4 + which(kpi$format == "0.0%"), cols = 2, stack = TRUE)
  openxlsx::addStyle(wb, "Synthèse", STYLE_DEC, rows = 4 + which(kpi$format == "0.0"), cols = 2, stack = TRUE)
  r0 <- 4 + nrow(kpi) + 3
  openxlsx::writeData(wb, "Synthèse", "Statut des entretiens par version du questionnaire", startRow = r0 - 1)
  ajouter_table(wb, "Synthèse", tab$statuts, r0)
  r1 <- r0 + nrow(tab$statuts) + 3
  openxlsx::writeData(wb, "Synthèse", "Répartition des durées d'entretien (entretiens envoyés)", startRow = r1 - 1)
  du <- tab$durees; du$Durée <- as.character(du$Durée)
  ajouter_table(wb, "Synthèse", du, r1, pct = "%")
  openxlsx::setColWidths(wb, "Synthèse", cols = 1:2, widths = c(62, 14))

  feuille(wb, "Évolution journalière", tab$jour, "Évolution journalière de la collecte",
          "Dates issues des paradonnées du serveur (début = première réponse saisie ; envoi = statut Complété)")
  feuille(wb, "Par région", tab$region, "Avancement par région", "Région déclarée en A.1",
          pct = "% complétés", ent = "Durée médiane (min)")
  feuille(wb, "Par département", tab$departement, "Avancement par département", pct = "% complétés")
  feuille(wb, "Production déclarée", tab$production,
          "Production apicole déclarée en 2025 (premiers résultats bruts, non apurés)",
          "Entretiens complétés ou approuvés ; lignes en incohérence bloquante (R16 à R22) exclues ; rendement et prix = médiane par type de ruche",
          ent = c("Prix médian du miel (FCFA/L)", "Miel produit (L)", "Cire produite (kg)", "Miel vendu (L)", "Ventes de miel (FCFA)"))
  feuille(wb, "Par enquêteur", tab$enqueteurs, "Performance des enquêteurs",
          "Taux de rejet = entretiens rejetés au moins une fois / entretiens envoyés",
          pct = c("% complétés", "Taux de rejet"), ent = "Durée médiane (min)", dec = "Entretiens par jour actif")
  feuille(wb, "Enquêteur x jour", tab$matrice, "Entretiens démarrés par enquêteur et par jour",
          larges = stats::setNames(rep(7, ncol(tab$matrice) - 1), names(tab$matrice)[-1]))
  if (nrow(tab$matrice)) {
    openxlsx::conditionalFormatting(wb, "Enquêteur x jour", cols = 2:(ncol(tab$matrice) - 1),
                                    rows = 5:(4 + nrow(tab$matrice)), style = c("#FFFFFF", "#5598E7"), type = "colourScale")
  }
  openxlsx::saveWorkbook(wb, fichier, overwrite = TRUE)
}

detail_affiche <- function(det) {
  d <- det[, c("interview__key", "interview__id", "version", "statut", "enqueteur", "region", "departement",
               "commune", "code", "niveau", "section", "controle", "variable", "ligne", "valeur", "message")]
  names(d) <- c("interview__key", "interview__id", "Version", "Statut", "Enquêteur", "Région", "Département",
                "Commune", "Code", "Niveau", "Section", "Contrôle", "Variable", "Ligne / rubrique", "Valeur", "Message")
  d
}

synthese_controles <- function(det, s) {
  cat <- catalogue_regles(s)
  n_ent <- tapply(det$interview__id, det$code, function(x) length(unique(x)))
  n_cas <- table(det$code)
  n_soum <- tapply(det$statut_code %in% STATUTS_SOUMIS, det$code, sum)
  data.frame(Code = cat$code, Niveau = cat$niveau, Section = cat$section, Contrôle = cat$controle, Seuil = cat$seuil,
             `Entretiens concernés` = dplyr::coalesce(as.numeric(n_ent[cat$code]), 0),
             `Cas détectés` = dplyr::coalesce(as.numeric(n_cas[cat$code]), 0),
             `dont entretiens complétés/approuvés (cas)` = dplyr::coalesce(as.numeric(n_soum[cat$code]), 0),
             check.names = FALSE)
}

incoherences_par_enqueteur <- function(det, m) {
  if (!nrow(det)) return(data.frame(Enquêteur = character()))
  pv <- as.data.frame.matrix(table(det$enqueteur, det$code))
  enq <- rownames(pv)
  out <- data.frame(Enquêteur = enq,
                    `Total bloquantes` = as.numeric(table(factor(det$enqueteur[det$niveau == "Rejet"], levels = enq))),
                    `Total à vérifier` = as.numeric(table(factor(det$enqueteur[det$niveau == "À vérifier"], levels = enq))),
                    Entretiens = as.numeric(table(factor(m$enqueteur, levels = enq))), pv, check.names = FALSE)
  rownames(out) <- NULL
  out[order(-out$`Total bloquantes`), ]
}

ecrire_incoherences_xlsx <- function(fichier, listes, det, m, s) {
  wb <- openxlsx::createWorkbook()
  larges <- c(`Commentaire de rejet (à coller)` = 90, interview__id = 34, `Contrôles déclenchés` = 18)
  ar <- listes$a_rejeter; av <- listes$a_verifier; ce <- listes$chez_enqueteur
  feuille(wb, "À rejeter", ar,
          sprintf("Entretiens à rejeter : %d (complétés/approuvés avec au moins une incohérence bloquante)", nrow(ar)),
          "Dans Survey Solutions > Entretiens : rechercher la clé (interview__key), ouvrir, « Rejeter » et coller le commentaire.",
          larges = larges)
  if (nrow(ar)) openxlsx::addStyle(wb, "À rejeter", STYLE_RETOUR, rows = 5:(4 + nrow(ar)), cols = ncol(ar), stack = TRUE)
  feuille(wb, "À vérifier", av,
          sprintf("Entretiens à vérifier : %d (aucune incohérence bloquante, mais des valeurs atypiques)", nrow(av)),
          "Le superviseur décide dans la colonne jaune : « Rejeter » ou « Valider ».",
          larges = c(larges, `Décision superviseur` = 16))
  if (nrow(av)) {
    openxlsx::dataValidation(wb, "À vérifier", cols = 6, rows = 5:(4 + nrow(av)), type = "list", value = "'Listes'!$A$1:$A$2")
    openxlsx::addStyle(wb, "À vérifier", STYLE_SAISIE, rows = 5:(4 + nrow(av)), cols = 6, stack = TRUE)
    openxlsx::addStyle(wb, "À vérifier", STYLE_RETOUR, rows = 5:(4 + nrow(av)), cols = ncol(av), stack = TRUE)
  }
  feuille(wb, "Chez l'enquêteur", ce,
          sprintf("Entretiens non encore envoyés ou déjà rejetés, présentant des incohérences : %d", nrow(ce)),
          "Pour information : à signaler aux enquêteurs avant leur prochain envoi.", larges = larges)
  d <- detail_affiche(det)
  feuille(wb, "Détail des incohérences", d,
          sprintf("Détail : %d incohérences sur %d entretiens (tous statuts)", nrow(d), length(unique(det$interview__id))),
          larges = c(Message = 80, Contrôle = 45, interview__id = 34))
  colorer_niveau(wb, "Détail des incohérences", d$Niveau, 10)
  sc <- synthese_controles(det, s)
  feuille(wb, "Synthèse par contrôle", sc, "Nombre d'entretiens concernés par contrôle", larges = c(Contrôle = 60, Seuil = 45))
  colorer_niveau(wb, "Synthèse par contrôle", sc$Niveau, 2)
  pe <- incoherences_par_enqueteur(det, m)
  feuille(wb, "Par enquêteur", pe, "Incohérences par enquêteur et par contrôle (tous statuts)")
  openxlsx::addWorksheet(wb, "Listes", visible = FALSE)
  openxlsx::writeData(wb, "Listes", c("Rejeter", "Valider"), colNames = FALSE)
  openxlsx::saveWorkbook(wb, fichier, overwrite = TRUE)
}

ecrire_csv_api <- function(fichier, a_rejeter) {
  x <- a_rejeter[, c("interview__id", "interview__key", "Enquêteur", "Commentaire de rejet (à coller)")]
  names(x)[4] <- "commentaire"
  # BOM UTF-8 pour qu'Excel affiche correctement les accents
  con <- file(fichier, open = "wb")
  writeBin(as.raw(c(0xEF, 0xBB, 0xBF)), con)
  close(con)
  suppressWarnings(utils::write.table(x, fichier, sep = ";", row.names = FALSE, qmethod = "double",
                                      fileEncoding = "UTF-8", append = TRUE))
}
