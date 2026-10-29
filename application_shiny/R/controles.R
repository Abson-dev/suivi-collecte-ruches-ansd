# Contrôles de cohérence (R01 à R39) et listes de rejet

nombre <- function(x) {
  out <- ifelse(abs(x - round(x)) < 1e-9, formatC(x, format = "f", digits = 0, big.mark = " "),
                formatC(x, format = "f", digits = 1, big.mark = " "))
  out[is.na(x)] <- "manquant"
  out
}

somme <- function(x) if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)

normaliser <- function(x) {
  x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT", sub = "")
  gsub("[^A-Z0-9]", "", toupper(ifelse(is.na(x), "", x)))
}

prep_productivite <- function(r, an) {
  names(r) <- sub(paste0("_", an, "bis$"), "bis", names(r))
  names(r) <- sub(paste0("_", an, "$"), "", names(r))
  names(r)[names(r) == paste0("rstProductivite", an, "__id")] <- "type"
  r$annee <- paste0("20", an)
  lib <- unname(TYPES_RUCHE[as.character(r$type)])
  r$lib <- paste(ifelse(is.na(lib), "?", lib), r$annee)
  r
}

controler <- function(t, m, s) {
  journal <- list()
  sig <- function(code, d, message, variable = "", valeur = NULL, ligne = NULL) {
    n <- NROW(d)
    if (n == 0) return(invisible(NULL))
    journal[[length(journal) + 1]] <<- data.frame(
      interview__id = d$interview__id, code = code, variable = rep_len(as.character(variable), n),
      ligne = if (is.null(ligne)) "" else as.character(ligne),
      valeur = if (is.null(valeur)) "" else nombre(valeur),
      message = rep_len(as.character(message), n))
  }
  envoyes <- m[m$statut_code %in% c(STATUTS_SOUMIS, STATUTS_REJETES), ]

  # R01 / R37 - erreurs de validation encore présentes
  err <- t$interview__errors
  if (!is.null(err) && nrow(err)) {
    err$message <- trimws(gsub("\\s+", " ", gsub("<[^>]+>", " ", err$message)))
    ro <- ifelse(is.na(err$roster), "", err$roster)
    err$ligne <- ifelse(ro != "", paste(ro, "ligne", ifelse(is.na(err$id1), 0, err$id1)), "")
    err <- err[!duplicated(err[, c("interview__id", "variable", "ligne", "message")]), ]
    forme <- err$variable %in% VALIDATIONS_NON_BLOQUANTES
    for (k in c("R01", "R37")) {
      d <- err[if (k == "R01") !forme else forme, ]
      sig(k, d, paste(d$variable, ":", d$message), d$variable, ligne = d$ligne)
    }
  }

  # R02 / R03 - questions non renseignées
  nr <- envoyes$n_questions_unanswered
  d <- envoyes[which(nr >= s$nonrep_rejet), ]
  sig("R02", d, sprintf("%.0f questions non renseignées", d$n_questions_unanswered), valeur = d$n_questions_unanswered)
  d <- envoyes[which(nr > 0 & nr < s$nonrep_rejet), ]
  sig("R03", d, sprintf("%.0f question(s) non renseignée(s)", d$n_questions_unanswered), valeur = d$n_questions_unanswered)

  # R04 / R05 - durée
  d <- envoyes[which(envoyes$duree_min < s$duree_min), ]
  sig("R04", d, sprintf("Entretien réalisé en %.0f min", d$duree_min), valeur = d$duree_min)
  d <- envoyes[which(envoyes$duree_min > s$duree_max), ]
  sig("R05", d, sprintf("Entretien de %.0f min", d$duree_min), valeur = d$duree_min)

  # R06 - localisation
  for (v in list(c("A_1", "Région"), c("A_2", "Département"), c("A_3", "Commune"), c("A_5", "Milieu"))) {
    d <- envoyes[is.na(envoyes[[v[1]]]), ]
    sig("R06", d, sprintf("%s (%s) non renseigné(e)", v[2], v[1]), v[1])
  }

  # R07 - âges
  for (v in list(c("A_8", "répondant"), c("A_12_2", "propriétaire"))) {
    x <- m[[v[1]]]
    d <- m[which(x < 15), ]
    sig("R07", d, sprintf("Âge du %s = %.0f ans", v[2], d[[v[1]]]), v[1], d[[v[1]]])
    d <- m[which(x == 99), ]
    sig("R07", d, sprintf("Âge du %s = 99 (code « ne sait pas » ou âge réel ?)", v[2]), v[1], d[[v[1]]])
  }

  # R08 / R09 - année de création
  annee <- as.integer(format(dplyr::coalesce(m$date_tablette, m$date_debut), "%Y"))
  annee[is.na(annee)] <- as.integer(format(Sys.Date(), "%Y"))
  d <- m[which(!is.na(m$B_1) & (m$B_1 < 1900 | m$B_1 > annee)), ]
  sig("R08", d, sprintf("Année de création = %.0f", d$B_1), "B_1", d$B_1)
  d <- m[which(m$B_1 >= 1900 & m$B_1 < s$annee_creation_min), ]
  sig("R09", d, sprintf("Exploitation créée en %.0f", d$B_1), "B_1", d$B_1)

  # R10 à R13 - personnel
  rs <- t$RostSexe
  rs$sx <- ifelse(rs$RostSexe__id == 1, "H", "F")
  sexe <- tidyr::pivot_wider(rs[, c("interview__id", "sx", "B_3_2_1", "B_3_2_2")], names_from = sx,
                             values_from = c(B_3_2_1, B_3_2_2), values_fn = dplyr::first)
  for (cc in c("B_3_2_1_H", "B_3_2_1_F", "B_3_2_2_H", "B_3_2_2_F")) if (!cc %in% names(sexe)) sexe[[cc]] <- NA_real_
  p <- dplyr::inner_join(m[, c("interview__id", "B_3")], sexe, by = "interview__id")
  connus <- p[which(p$B_3_2_1_H != 9999 & p$B_3_2_1_F != 9999 & !is.na(p$B_3)), ]
  d <- connus[which(connus$B_3_2_1_H + connus$B_3_2_1_F != connus$B_3), ]
  sig("R10", d, sprintf("Effectif total %.0f ≠ %.0f hommes + %.0f femmes", d$B_3, d$B_3_2_1_H, d$B_3_2_1_F), "B_3", d$B_3)
  for (v in list(c("B_3_2_1_H", "effectif hommes"), c("B_3_2_1_F", "effectif femmes"),
                 c("B_3_2_2_H", "salaires hommes"), c("B_3_2_2_F", "salaires femmes"))) {
    d <- p[which(p[[v[1]]] == 9999), ]
    sig("R11", d, paste("Code 9999 (ne sait pas) pour", v[2]), sub("_[HF]$", "", v[1]))
  }
  d <- m[which(m$B_3 > s$effectif_max), ]
  sig("R12", d, sprintf("Personnel de %.0f personnes", d$B_3), "B_3", d$B_3)
  for (sx in c(H = "hommes", F = "femmes") |> names()) {
    n <- p[[paste0("B_3_2_1_", sx)]]; w <- p[[paste0("B_3_2_2_", sx)]]
    ok <- which(n > 0 & n != 9999 & !is.na(w) & w != 9999)
    q <- p[ok, ]; q$sal <- w[ok] / n[ok]
    d <- q[which(q$sal < s$salaire_min | q$sal > s$salaire_max), ]
    sig("R13", d, paste0("Masse salariale annuelle par employé (", c(H = "hommes", F = "femmes")[sx], ") = ",
                         nombre(d$sal), " FCFA"), "B_3_2_2", d$sal)
  }

  # Productivité par type de ruche
  prod <- list("24" = prep_productivite(t$rstProductivite24, "24"),
               "25" = prep_productivite(t$rstProductivite25, "25"))
  pr <- dplyr::bind_rows(prod)

  # R14 - pas de ruche mais exploitation
  sans <- m$interview__id[which(m$B_4Ter == 1)]
  d <- pr[which(pr$interview__id %in% sans & pr$C_1_1 > 0), ]
  sig("R14", d, sprintf("Déclare n'avoir aucune ruche (B.4) mais %.0f ruches détenues (%s)", d$C_1_1, d$lib),
      "B_4Ter", d$C_1_1, d$lib)

  # R15 - type exploité non possédé
  b4 <- tidyr::pivot_longer(m[, c("interview__id", grep("^B_4__\\d+$", names(m), value = TRUE))],
                            -interview__id, names_to = "b4", values_to = "possede")
  b4$b4 <- as.numeric(sub("B_4__", "", b4$b4))
  q <- pr; q$b4 <- unname(RUCHE_C_VERS_B[as.character(q$type)])
  q <- dplyr::inner_join(q, b4, by = c("interview__id", "b4"))
  d <- q[which(q$possede == 0 & q$C_1_1 > 0 & q$annee == "2025"), ]
  sig("R15", d, sprintf("Ruches %s exploitées mais ce type n'est pas coché en B.4", d$lib), "B_4", d$C_1_1, d$lib)

  # R16 à R24, R38, R39 - quantités et prix
  d <- pr[which(pr$C_1_2 > pr$C_1_1), ]
  sig("R16", d, sprintf("%s : %.0f ruches utilisées > %.0f détenues", d$lib, d$C_1_2, d$C_1_1), "C_1_2", d$C_1_2, d$lib)
  d <- pr[which(pr$C_1_6 > pr$C_1_3), ]
  sig("R17", d, sprintf("%s : miel vendu (%s) > miel produit (%s)", d$lib, nombre(d$C_1_6), nombre(d$C_1_3)),
      "C_1_6", d$C_1_6, d$lib)
  d <- pr[which(pr$C_1_7 > pr$C_1_4), ]
  sig("R17", d, sprintf("%s : cire vendue (%s kg) > cire produite (%s kg)", d$lib, nombre(d$C_1_7), nombre(d$C_1_4)),
      "C_1_7", d$C_1_7, d$lib)
  for (v in list(c("C_1_6", "C_1_6bis", "miel"), c("C_1_7", "C_1_8", "cire"))) {
    qq <- pr[[v[1]]]; mt <- pr[[v[2]]]
    d <- pr[which((qq > 0 & mt == 0) | (qq == 0 & mt > 0)), ]
    sig("R18", d, sprintf("%s : %s vendu = %s, montant = %s FCFA", d$lib, v[3], nombre(d[[v[1]]]), nombre(d[[v[2]]])),
        v[2], d[[v[2]]], d$lib)
  }
  d <- pr[which(pr$C_1_3 > 0 & pr$C_1_2 == 0), ]
  sig("R19", d, sprintf("%s : %s L de miel avec 0 ruche utilisée", d$lib, nombre(d$C_1_3)), "C_1_3", d$C_1_3, d$lib)
  prod_tot <- rowSums(pr[, c("C_1_3", "C_1_4", "C_1_5")], na.rm = TRUE)
  d <- pr[which(pr$C_1_2 > 0 & prod_tot == 0 & !is.na(pr$C_1_3)), ]
  sig("R20", d, sprintf("%s : %.0f ruches utilisées mais aucune production", d$lib, d$C_1_2), "C_1_3", d$C_1_2, d$lib)
  pr$rdt <- ifelse(pr$C_1_2 > 0, pr$C_1_3 / pr$C_1_2, NA)
  d <- pr[which(pr$rdt > s$rdt_miel_max), ]
  sig("R21", d, sprintf("%s : %s L de miel par ruche utilisée", d$lib, nombre(d$rdt)), "C_1_3", d$rdt, d$lib)
  pr$rdt_cire <- ifelse(pr$C_1_2 > 0, pr$C_1_4 / pr$C_1_2, NA)
  d <- pr[which(pr$rdt_cire > s$rdt_cire_max | (pr$C_1_4 > pr$C_1_3 & pr$C_1_4 > 10)), ]
  sig("R38", d, sprintf("%s : %s kg de cire pour %s L de miel et %s ruches utilisées", d$lib, nombre(d$C_1_4),
                        nombre(d$C_1_3), nombre(d$C_1_2)), "C_1_4", d$C_1_4, d$lib)
  for (v in list(c("C_1_1", "ruches détenues"), c("C_1_2", "ruches utilisées"), c("C_1_3", "miel produit"),
                 c("C_1_4", "cire produite"), c("C_1_5", "gâteau"), c("C_1_6", "miel vendu"), c("C_1_7", "cire vendue"))) {
    d <- pr[which(pr[[v[1]]] %in% c(99, 999, 9999)), ]
    sig("R39", d, sprintf("%s : %s = %s (code « ne sait pas » ?)", d$lib, v[2], nombre(d[[v[1]]])), v[1], d[[v[1]]], d$lib)
  }
  pr$prix_miel <- ifelse(pr$C_1_6 > 0, pr$C_1_6bis / pr$C_1_6, NA)
  pr$prix_cire <- ifelse(pr$C_1_7 > 0, pr$C_1_8 / pr$C_1_7, NA)
  pm <- pr[which(pr$C_1_6bis > 0), ]
  hors_rejet <- pm$prix_miel < s$prix_miel_rejet_min | pm$prix_miel > s$prix_miel_rejet_max
  hors_verif <- pm$prix_miel < s$prix_miel_verif_min | pm$prix_miel > s$prix_miel_verif_max
  txt <- function(d) sprintf("%s : %s L vendus pour %s FCFA, soit %s FCFA/L", d$lib, nombre(d$C_1_6),
                             nombre(d$C_1_6bis), nombre(d$prix_miel))
  d <- pm[which(hors_rejet), ]
  sig("R22", d, txt(d), "C_1_6bis", d$prix_miel, d$lib)
  d <- pm[which(hors_verif & !hors_rejet), ]
  sig("R23", d, txt(d), "C_1_6bis", d$prix_miel, d$lib)
  pc <- pr[which(pr$C_1_8 > 0), ]
  d <- pc[which(pc$prix_cire < s$prix_cire_verif_min | pc$prix_cire > s$prix_cire_verif_max), ]
  sig("R24", d, sprintf("%s : %s kg de cire vendus pour %s FCFA, soit %s FCFA/kg", d$lib, nombre(d$C_1_7),
                        nombre(d$C_1_8), nombre(d$prix_cire)), "C_1_8", d$prix_cire, d$lib)

  # R25 / R36 - quantités mensuelles
  qt <- t$QtitPeriode
  for (an in c("24", "25")) {
    annuel <- prod[[an]] |> dplyr::group_by(interview__id) |> dplyr::summarise(annuel = somme(C_1_3), .groups = "drop")
    mensuel <- qt |> dplyr::group_by(interview__id) |>
      dplyr::summarise(mensuel = somme(.data[[paste0("C2bis", an)]]), .groups = "drop")
    cmp <- dplyr::inner_join(annuel, mensuel, by = "interview__id")
    cmp <- cmp[!is.na(cmp$annuel) & !is.na(cmp$mensuel), ]
    ecart <- abs(cmp$mensuel - cmp$annuel)
    hors <- cmp[which(ecart > s$ecart_mensuel_min & ecart > s$ecart_mensuel * pmax(cmp$annuel, cmp$mensuel)), ]
    mn <- pmin(hors$annuel, hors$mensuel)
    rapport <- ifelse(mn > 0, pmax(hors$annuel, hors$mensuel) / mn, NA)
    fort <- is.na(rapport) | rapport > s$ecart_mensuel_rejet
    for (k in c("R25", "R36")) {
      d <- hors[if (k == "R25") fort else !fort, ]
      sig(k, d, sprintf("20%s : somme des quantités mensuelles = %s L ≠ production annuelle déclarée = %s L",
                        an, nombre(d$mensuel), nombre(d$annuel)), paste0("C2bis", an), d$mensuel, rep(paste0("20", an), nrow(d)))
    }
  }
  tot <- pr |> dplyr::group_by(interview__id) |> dplyr::summarise(tot = somme(C_1_3), .groups = "drop")
  mois <- rowSums(m[, grep("^C_2__\\d+$", names(m)), drop = FALSE], na.rm = TRUE)
  d <- m[which(mois > 0 & tot$tot[match(m$interview__id, tot$interview__id)] == 0), ]
  sig("R26", d, "Mois de production cochés en C.2 mais aucune production de miel en 2024 et 2025", "C_2")

  # R27 - variation du nombre de ruches
  tr <- function(r) r |> dplyr::group_by(interview__id) |> dplyr::summarise(n = somme(C_1_1), .groups = "drop")
  v <- dplyr::inner_join(tr(prod[["24"]]), tr(prod[["25"]]), by = "interview__id", suffix = c("24", "25"))
  v <- v[which(v$n24 > 0 & v$n25 > 0 & pmax(v$n24, v$n25) >= 10), ]
  d <- v[which(v$n25 / v$n24 > s$variation_ruches | v$n24 / v$n25 > s$variation_ruches), ]
  sig("R27", d, sprintf("Ruches détenues : %.0f en 2024 contre %.0f en 2025", d$n24, d$n25), "C_1_1", d$n25)

  # R28 - ruches en équipement vs ruches détenues
  eq <- t$rstEquipement
  eq1 <- eq[eq$rstEquipement__id == 1, ]
  t25 <- tr(prod[["25"]])
  w <- m[, c("interview__id", "D_2_0__1")]
  w$n_eq <- eq1$D_2_1[match(w$interview__id, eq1$interview__id)]
  w$c1 <- t25$n[match(w$interview__id, t25$interview__id)]
  d <- w[which(w$D_2_0__1 == 0 & w$c1 > 0), ]
  sig("R28", d, sprintf("%.0f ruches détenues en 2025 mais « Ruches » non coché en D.2.0", d$c1), "D_2_0", d$c1)
  w <- w[which(w$n_eq > 0 & w$c1 > 0), ]
  d <- w[which(w$n_eq / w$c1 > 2 | w$n_eq / w$c1 < 0.5), ]
  sig("R28", d, sprintf("%.0f ruches en équipement (D.2.1) contre %.0f ruches détenues en 2025 (C.1.1)", d$n_eq, d$c1),
      "D_2_1", d$n_eq)

  # R29 à R31 - infrastructures et équipements
  blocs <- list(
    list(nom = "rstInfrastructures", pre = "D_1_", suf = "", cats = INFRAS, txt = NULL, prix = 1000, age = s$age_infra_max),
    list(nom = "rstAutresInfrastructures", pre = "D_1_", suf = "Autres", cats = NULL, txt = "D_1_0_pre", prix = 1000, age = s$age_infra_max),
    list(nom = "rstEquipement", pre = "D_2_", suf = "", cats = EQUIPEMENTS, txt = NULL, prix = 500, age = s$age_equip_max),
    list(nom = "rstEquipementAutres", pre = "D_2_", suf = "Autres", cats = NULL, txt = "D_2_0_pre", prix = 500, age = s$age_equip_max))
  for (b in blocs) {
    r <- t[[b$nom]]
    if (is.null(r) || !nrow(r)) next
    if (!is.null(b$cats)) {
      r$lib <- unname(b$cats[as.character(r[[paste0(b$nom, "__id")]])]); r$lib[is.na(r$lib)] <- "?"
    } else {
      r$lib <- ifelse(is.na(r[[b$txt]]) | r[[b$txt]] == "", "autre", r[[b$txt]])
    }
    col <- function(i) paste0(b$pre, i, b$suf)
    n <- r[[col(1)]]; ind <- r[[col(2)]]; cl <- r[[col(3)]]; age <- r[[col(4)]]; val <- r[[col(6)]]
    d <- r[which(ind + cl != n), ]
    sig("R29", d, sprintf("%s : %s individuel(s) + %s collectif(s) ≠ %s", d$lib, nombre(d[[col(2)]]),
                          nombre(d[[col(3)]]), nombre(d[[col(1)]])), col(1), d[[col(1)]], d$lib)
    d <- r[which(val < b$prix), ]
    sig("R30", d, sprintf("%s : prix d'achat = %s FCFA", d$lib, nombre(d[[col(6)]])), col(6), d[[col(6)]], d$lib)
    d <- r[which(age > b$age), ]
    sig("R31", d, sprintf("%s : âge = %s ans", d$lib, nombre(d[[col(4)]])), col(4), d[[col(4)]], d$lib)
  }

  # R32 - organisations
  d <- m[which(m$A_14 %in% 2:4 & m$E_1 < 2), ]
  sig("R32", d, sprintf("Organisation déclarant %.0f membre(s)", d$E_1), "E_1", d$E_1)

  # R33 - dates
  ecart_j <- abs(as.numeric(difftime(m$date_tablette, m$date_debut, units = "days")))
  d <- m[which(ecart_j > 1), ]
  sig("R33", d, sprintf("Date saisie sur la tablette %s ≠ date du serveur %s", format(d$date_tablette, "%d/%m/%Y"),
                        format(d$date_debut, "%d/%m/%Y")), "DateEntretien")
  d <- m[which((is.na(m$DateEntretien) | m$DateEntretien == "") & m$statut_code %in% c(STATUTS_SOUMIS, STATUTS_REJETES)), ]
  sig("R33", d, "Date de l'entretien non renseignée", "DateEntretien")
  d <- m[which(date_heure(m$DateFinEntretien) < date_heure(m$DebutEntretien)), ]
  sig("R33", d, "Date de fin de l'entretien antérieure à la date de début", "DateFinEntretien")

  # R34 - doublons potentiels
  vides <- c("", "NEANT", "NA", "NAN", "AUCUN", "AUCUNE", "RAS", "NON", "PAS", "NEAN", "NONE", "0")
  u <- m[m$statut_code >= 0, ]
  u$nom <- normaliser(u$A_15)
  criteres <- list(list(cols = c("A_3", "nom"), lib = "même commune et même dénomination"),
                   list(cols = c("A_3", "A_8", "A_10", "B_1", "B_3"), lib = "même commune, âge, sexe, année de création et effectif"))
  for (cr in criteres) {
    g <- u[stats::complete.cases(u[, cr$cols]), ]
    if ("nom" %in% cr$cols) g <- g[!g$nom %in% vides & nchar(g$nom) > 2, ]
    g <- g |> dplyr::group_by(dplyr::across(dplyr::all_of(cr$cols))) |> dplyr::filter(dplyr::n() > 1) |>
      dplyr::mutate(autres = vapply(seq_len(dplyr::n()), function(i) paste(interview__key[-i], collapse = ", "), "")) |>
      dplyr::ungroup()
    sig("R34", g, sprintf("Doublon potentiel (%s) avec : %s", cr$lib, g$autres), "A_3")
  }

  # R35 - région hors zone habituelle de l'enquêteur
  z <- m[!is.na(m$A_1), ]
  zr <- z |> dplyr::count(enqueteur, A_1) |> dplyr::group_by(enqueteur) |>
    dplyr::mutate(total = sum(n), part = n / total) |> dplyr::arrange(dplyr::desc(n), A_1, .by_group = TRUE) |>
    dplyr::slice(1) |> dplyr::ungroup()
  zr <- zr[zr$total >= 3 & zr$part >= 0.6, ]
  fiables <- stats::setNames(zr$A_1, zr$enqueteur)
  z <- z[z$enqueteur %in% names(fiables), ]
  d <- z[which(z$A_1 != fiables[z$enqueteur]), ]
  reg_lab <- stats::setNames(m$region, m$A_1)
  sig("R35", d, sprintf("Entretien en région %s alors que l'enquêteur travaille surtout en région %s",
                        d$region, reg_lab[as.character(fiables[d$enqueteur])]), "A_1")

  det <- if (length(journal)) dplyr::bind_rows(journal) else
    data.frame(interview__id = character(), code = character(), variable = character(), ligne = character(),
               valeur = character(), message = character())
  info <- m[, c("interview__id", "interview__key", "version", "statut_code", "statut", "enqueteur",
                "region", "departement", "commune", "date_debut")]
  det <- dplyr::left_join(det, info, by = "interview__id")
  cat <- catalogue_regles(s)
  det$niveau <- cat$niveau[match(det$code, cat$code)]
  det$section <- cat$section[match(det$code, cat$code)]
  det$controle <- cat$controle[match(det$code, cat$code)]
  det <- det[order(det$niveau != "Rejet", det$enqueteur, det$interview__key, det$code), ]
  list(det = det, pr = pr, fiables = fiables)
}

commentaire <- function(msg, niv) {
  b <- msg[niv == "Rejet"]; v <- msg[niv == "À vérifier"]
  parts <- c(if (length(b)) paste0("À CORRIGER : ", paste0(seq_along(b), ") ", b, collapse = " | ")),
             if (length(v)) paste0("À VÉRIFIER : ", paste(v, collapse = " | ")))
  x <- paste(parts, collapse = " // ")
  if (nchar(x) > 1500) x <- paste0(substr(x, 1, 1490), " (...)")
  x
}

par_entretien <- function(d) {
  cols <- c("interview__key", "interview__id", "Version", "Statut actuel", "Action dans Survey Solutions",
            "Enquêteur", "Région", "Département", "Commune", "Date de début", "Nb incohérences bloquantes",
            "Nb points à vérifier", "Contrôles déclenchés", "Commentaire de rejet (à coller)")
  if (!nrow(d)) return(stats::setNames(data.frame(matrix(ncol = length(cols), nrow = 0)), cols))
  out <- d |> dplyr::group_by(interview__id) |> dplyr::summarise(
    interview__key = dplyr::first(interview__key), Version = dplyr::first(version),
    `Statut actuel` = dplyr::first(statut),
    `Action dans Survey Solutions` = unname(ACTION_REJET[as.character(dplyr::first(statut_code))]),
    Enquêteur = dplyr::first(enqueteur), Région = dplyr::first(region), Département = dplyr::first(departement),
    Commune = dplyr::first(commune), `Date de début` = as.Date(dplyr::first(date_debut)),
    `Nb incohérences bloquantes` = sum(niveau == "Rejet"), `Nb points à vérifier` = sum(niveau == "À vérifier"),
    `Contrôles déclenchés` = paste(sort(unique(code)), collapse = ", "),
    `Commentaire de rejet (à coller)` = commentaire(message, niveau), .groups = "drop")
  out$`Action dans Survey Solutions`[is.na(out$`Action dans Survey Solutions`)] <- ""
  out <- as.data.frame(out[, cols], check.names = FALSE)
  out[order(out$Enquêteur, out$interview__key), ]
}

listes_rejet <- function(det) {
  soumis <- det[det$statut_code %in% STATUTS_SOUMIS, ]
  ids_bloq <- unique(soumis$interview__id[soumis$niveau == "Rejet"])
  a_verifier <- par_entretien(soumis[!soumis$interview__id %in% ids_bloq, ])
  a_verifier$`Action dans Survey Solutions` <- rep("", nrow(a_verifier))
  a_verifier$`Décision superviseur` <- rep("", nrow(a_verifier))
  a_verifier <- dplyr::relocate(a_verifier, `Décision superviseur`, .after = 5)
  chez <- par_entretien(det[!det$statut_code %in% STATUTS_SOUMIS, ])
  chez$`Action dans Survey Solutions` <- NULL
  list(a_rejeter = par_entretien(soumis[soumis$interview__id %in% ids_bloq, ]),
       a_verifier = a_verifier, chez_enqueteur = chez)
}
