# Tableaux de suivi de la collecte

compte_par <- function(x, niveaux) as.numeric(table(factor(x, levels = niveaux)))

tableaux_suivi <- function(m, a, det, pr, fiables) {
  envoyes <- m$statut_code %in% c(STATUTS_SOUMIS, STATUTS_REJETES)
  a <- a[a$interview__id %in% m$interview__id, ]
  version <- stats::setNames(m$version, m$interview__id)
  premier <- function(d) { s <- d |> dplyr::group_by(interview__id) |> dplyr::summarise(jour = min(jour), .groups = "drop"); s }

  # Évolution journalière
  debut <- premier(a[a$action == 2, ])
  envois <- a[a$action == 3, ]
  premier_envoi <- premier(envois)
  jours <- if (nrow(a)) seq(min(a$jour, na.rm = TRUE), max(a$jour, na.rm = TRUE), by = "day") else as.Date(character())
  nv <- as.character(jours)
  j <- data.frame(Date = jours, `Entretiens démarrés` = compte_par(as.character(debut$jour), nv), check.names = FALSE)
  for (v in sort(unique(m$version))) {
    j[[paste("dont", v)]] <- compte_par(as.character(debut$jour[version[debut$interview__id] == v]), nv)
  }
  j$`Premiers envois au superviseur` <- compte_par(as.character(premier_envoi$jour), nv)
  j$`Renvois après correction` <- compte_par(as.character(envois$jour), nv) - j$`Premiers envois au superviseur`
  j$Rejets <- compte_par(as.character(a$jour[a$action %in% c(7, 8)]), nv)
  j$Approbations <- compte_par(as.character(a$jour[a$action %in% c(5, 6)]), nv)
  ea <- a[a$role == 1, ] |> dplyr::distinct(jour, originator)
  j$`Enquêteurs actifs` <- compte_par(as.character(ea$jour), nv)
  j$`Cumul démarrés` <- cumsum(j$`Entretiens démarrés`)
  j$`Cumul envoyés` <- cumsum(j$`Premiers envois au superviseur`)

  # Région / département
  m$region2 <- ifelse(m$region == "", "(non renseignée)", m$region)
  m$departement2 <- ifelse(m$departement == "", "(non renseigné)", m$departement)
  bloq <- det[det$niveau == "Rejet", ]
  bloq$region2 <- ifelse(is.na(bloq$region) | bloq$region == "", "(non renseignée)", bloq$region)
  reg <- m |> dplyr::group_by(Région = region2) |> dplyr::summarise(
    Entretiens = dplyr::n(),
    `Complété / approuvé` = sum(groupe == GROUPES[1]), `Rejeté (en correction)` = sum(groupe == GROUPES[2]),
    `En cours chez l'enquêteur` = sum(groupe == GROUPES[3]),
    `% complétés` = `Complété / approuvé` / Entretiens, Enquêteurs = dplyr::n_distinct(enqueteur),
    `Durée médiane (min)` = stats::median(duree_min[statut_code %in% c(STATUTS_SOUMIS, STATUTS_REJETES)], na.rm = TRUE),
    .groups = "drop")
  nb_bloq <- tapply(bloq$interview__id, bloq$region2, function(x) length(unique(x)))
  reg$`Entretiens avec incohérence bloquante` <- dplyr::coalesce(as.numeric(nb_bloq[reg$Région]), 0)
  reg <- as.data.frame(reg[order(-reg$Entretiens), ], check.names = FALSE)
  dep <- m |> dplyr::group_by(Région = region2, Département = departement2) |> dplyr::summarise(
    Entretiens = dplyr::n(),
    `Complété / approuvé` = sum(groupe == GROUPES[1]), `Rejeté (en correction)` = sum(groupe == GROUPES[2]),
    `En cours chez l'enquêteur` = sum(groupe == GROUPES[3]), Enquêteurs = dplyr::n_distinct(enqueteur),
    `% complétés` = `Complété / approuvé` / Entretiens, .groups = "drop") |>
    dplyr::arrange(Région, dplyr::desc(Entretiens)) |> as.data.frame(check.names = FALSE)

  # Production déclarée 2025 (complétés/approuvés, hors lignes en incohérence bloquante)
  dout <- det[det$code %in% c("R16", "R17", "R18", "R19", "R21", "R22"), ]
  cle_douteuse <- paste(dout$interview__id, dout$ligne)
  valides <- m[m$statut_code %in% STATUTS_SOUMIS, c("interview__id", "region2")]
  p25 <- dplyr::inner_join(pr[pr$annee == "2025", ], valides, by = "interview__id")
  p25 <- p25[!paste(p25$interview__id, p25$lib) %in% cle_douteuse, ]
  med <- function(x) if (all(is.na(x))) NA_real_ else stats::median(x, na.rm = TRUE)
  pp <- p25 |> dplyr::group_by(Région = region2) |> dplyr::summarise(
    `Exploitations productrices` = dplyr::n_distinct(interview__id),
    `Ruches détenues` = sum(C_1_1, na.rm = TRUE), `Ruches utilisées` = sum(C_1_2, na.rm = TRUE),
    `Miel produit (L)` = sum(C_1_3, na.rm = TRUE), `Cire produite (kg)` = sum(C_1_4, na.rm = TRUE),
    `Miel vendu (L)` = sum(C_1_6, na.rm = TRUE), `Ventes de miel (FCFA)` = sum(C_1_6bis, na.rm = TRUE),
    `Rendement médian (L/ruche utilisée)` = med(rdt), `Prix médian du miel (FCFA/L)` = med(prix_miel),
    .groups = "drop") |> dplyr::arrange(dplyr::desc(`Ruches détenues`)) |> as.data.frame(check.names = FALSE)

  # Enquêteurs
  act <- a[a$role == 1, ] |> dplyr::group_by(enqueteur = originator) |>
    dplyr::summarise(jours = dplyr::n_distinct(jour), premiere = min(jour), derniere = max(jour), .groups = "drop")
  reg_lab <- stats::setNames(m$region, m$A_1)
  e <- m |> dplyr::group_by(Enquêteur = enqueteur) |> dplyr::summarise(
    Entretiens = dplyr::n(),
    `Complété / approuvé` = sum(groupe == GROUPES[1]), `Rejeté (en correction)` = sum(groupe == GROUPES[2]),
    `En cours chez l'enquêteur` = sum(groupe == GROUPES[3]),
    `% complétés` = `Complété / approuvé` / Entretiens,
    `Entretiens rejetés au moins une fois` = sum(nb_rejets > 0),
    `Taux de rejet` = ifelse(sum(!is.na(date_1er_envoi)) > 0, sum(nb_rejets > 0) / sum(!is.na(date_1er_envoi)), NA),
    `Durée médiane (min)` = stats::median(duree_min[statut_code %in% c(STATUTS_SOUMIS, STATUTS_REJETES)], na.rm = TRUE),
    .groups = "drop")
  k <- match(e$Enquêteur, act$enqueteur)
  e$`Jours actifs` <- act$jours[k]
  e$`Entretiens par jour actif` <- e$Entretiens / e$`Jours actifs`
  e$`Première activité` <- act$premiere[k]
  e$`Dernière activité` <- act$derniere[k]
  e$`Incohérences bloquantes` <- as.numeric(table(factor(det$enqueteur[det$niveau == "Rejet"], levels = e$Enquêteur)))
  e$`Incohérences à vérifier` <- as.numeric(table(factor(det$enqueteur[det$niveau == "À vérifier"], levels = e$Enquêteur)))
  rp <- unname(reg_lab[as.character(fiables[e$Enquêteur])])
  e$`Région principale` <- ifelse(is.na(rp), "", rp)
  e <- dplyr::relocate(e, `Région principale`, .after = 1)
  e <- as.data.frame(e[order(-e$Entretiens), ], check.names = FALSE)

  # Matrice enquêteur x jour (entretiens démarrés)
  debut$enqueteur <- m$enqueteur[match(debut$interview__id, m$interview__id)]
  mat <- as.data.frame.matrix(table(debut$enqueteur, factor(format(debut$jour, "%d/%m"), levels = format(jours, "%d/%m"))))
  mat$Total <- rowSums(mat)
  mat <- cbind(Enquêteur = rownames(mat), mat)
  mat <- mat[order(-mat$Total), ]
  rownames(mat) <- NULL

  # Durées
  noms <- c("< 10 min", "10-20 min", "20-30 min", "30-45 min", "45-60 min", "60-90 min", "> 90 min")
  tr <- cut(m$duree_min[envoyes], c(0, 10, 20, 30, 45, 60, 90, Inf), labels = noms, right = FALSE)
  du <- data.frame(Durée = factor(noms, levels = noms), Entretiens = as.numeric(table(factor(tr, levels = noms))))
  du$`%` <- if (sum(du$Entretiens)) du$Entretiens / sum(du$Entretiens) else 0

  # Statuts par version
  sv <- as.data.frame.matrix(table(m$statut, m$version))
  sv$Total <- rowSums(sv)
  sv <- rbind(sv, Total = colSums(sv))
  sv <- cbind(Statut = rownames(sv), sv)
  rownames(sv) <- NULL

  list(jour = j, region = reg, departement = dep, production = pp, enqueteurs = e, matrice = mat,
       durees = du, statuts = sv)
}

indicateurs <- function(m, det, jour) {
  n <- nrow(m)
  soumis <- sum(m$statut_code %in% STATUTS_SOUMIS)
  bloq <- length(unique(det$interview__id[det$niveau == "Rejet" & det$statut_code %in% STATUTS_SOUMIS]))
  data.frame(
    Indicateur = c("Entretiens au total (toutes versions)", "Complétés ou approuvés", "Taux de complétion",
                   "Rejetés, en cours de correction", "En cours chez l'enquêteur (non envoyés)",
                   "Complétés/approuvés avec incohérence bloquante (à rejeter)", "Enquêteurs ayant travaillé",
                   "Jours de collecte", "Entretiens démarrés par jour (moyenne des 7 derniers jours)",
                   "Durée médiane d'un entretien envoyé (min)", "Régions couvertes"),
    Valeur = c(n, soumis, if (n) soumis / n else 0, sum(m$statut_code %in% STATUTS_REJETES),
               sum(!m$statut_code %in% c(STATUTS_SOUMIS, STATUTS_REJETES)), bloq,
               dplyr::n_distinct(m$enqueteur), nrow(jour),
               if (nrow(jour)) mean(utils::tail(jour$`Entretiens démarrés`, 7)) else 0,
               stats::median(m$duree_min[m$statut_code %in% c(STATUTS_SOUMIS, STATUTS_REJETES)], na.rm = TRUE),
               dplyr::n_distinct(m$region[m$region != ""])),
    format = c("0", "0", "0.0%", "0", "0", "0", "0", "0", "0.0", "0", "0"))
}
