# Lecture et préparation des exports Stata de Survey Solutions

MOTIF_DOSSIER <- "^PROD_ECPRA_BMD_\\d+_STATA_All$"
MOTIF_ZIP <- "^PROD_ECPRA_BMD_\\d+_STATA_All\\.zip$"

# Liste les exports d'un dossier ; un export encore compressé est décompressé à côté
# (ou dans le dossier temporaire si le dossier n'est pas accessible en écriture)
trouver_exports <- function(dossier, extraire = TRUE) {
  if (is.null(dossier) || !nzchar(dossier) || !dir.exists(dossier)) return(character())
  archives <- list.files(dossier, MOTIF_ZIP, full.names = TRUE)
  dossiers <- list.dirs(dossier, recursive = FALSE)
  dossiers <- dossiers[grepl(MOTIF_DOSSIER, basename(dossiers))]
  if (!extraire) return(c(dossiers, archives))
  for (a in archives) {
    nom <- sub("\\.zip$", "", basename(a))
    if (nom %in% basename(dossiers)) next
    cible <- file.path(dossier, nom)
    ok <- tryCatch({ utils::unzip(a, exdir = cible); TRUE }, error = function(e) FALSE, warning = function(w) FALSE)
    if (!ok) {
      cible <- file.path(tempdir(), "exports", nom)
      utils::unzip(a, exdir = cible)
    }
    dossiers <- c(dossiers, cible)
  }
  dossiers
}

# Décompresse les .zip téléversés dans l'application
extraire_televerses <- function(fichiers) {
  if (is.null(fichiers) || !nrow(fichiers)) return(character())
  out <- character()
  for (i in seq_len(nrow(fichiers))) {
    nom <- sub("\\.zip$", "", fichiers$name[i], ignore.case = TRUE)
    if (!grepl(MOTIF_DOSSIER, nom)) nom <- sprintf("PROD_ECPRA_BMD_%d_STATA_All", 900 + i)
    cible <- file.path(tempdir(), "televerses", nom)
    unlink(cible, recursive = TRUE)
    utils::unzip(fichiers$datapath[i], exdir = cible)
    # certains zips contiennent un sous-dossier
    if (!file.exists(file.path(cible, "PROD_ECPRA_BMD.dta"))) {
      sous <- dirname(list.files(cible, "^PROD_ECPRA_BMD\\.dta$", recursive = TRUE, full.names = TRUE))
      if (length(sous)) cible <- sous[1]
    }
    out <- c(out, stats::setNames(cible, nom))
  }
  out
}

lire_dta <- function(chemin) {
  df <- haven::read_dta(chemin)
  labs <- lapply(df, function(x) attr(x, "labels", exact = TRUE))
  labs <- labs[!vapply(labs, is.null, logical(1))]
  df <- as.data.frame(haven::zap_label(haven::zap_formats(haven::zap_labels(df))))
  chr <- vapply(df, is.character, logical(1))
  df[chr] <- lapply(df[chr], function(x) { x[!is.na(x) & x == "##N/A##"] <- ""; x })
  list(df = df, labels = labs)
}

# dossiers : vecteur de chemins (noms éventuels = nom de l'export)
lire_exports <- function(dossiers) {
  noms <- if (is.null(names(dossiers))) basename(dossiers) else ifelse(nzchar(names(dossiers)), names(dossiers), basename(dossiers))
  version <- as.integer(sub(".*_(\\d+)_STATA_All$", "\\1", noms))
  garder <- !duplicated(version, fromLast = TRUE) & file.exists(file.path(dossiers, "PROD_ECPRA_BMD.dta"))
  dossiers <- dossiers[garder]; noms <- noms[garder]; version <- version[garder]
  if (!length(dossiers)) stop("Aucun export PROD_ECPRA_BMD_<n>_STATA_All valide trouvé.")
  ordre <- order(version)
  tables <- list(); labels <- list()
  for (k in ordre) {
    lisezmoi <- file.path(dossiers[k], "export__readme.txt")
    v_suso <- NA_integer_
    if (file.exists(lisezmoi)) {
      txt <- paste(readLines(lisezmoi, warn = FALSE, encoding = "UTF-8"), collapse = " ")
      if (grepl("\\$(\\d+)", txt)) v_suso <- as.integer(sub(".*?\\$(\\d+).*", "\\1", txt))
    }
    for (f in list.files(dossiers[k], "\\.dta$", full.names = TRUE)) {
      x <- lire_dta(f)
      x$df$version <- paste0("V", version[k])
      x$df$version_suso <- v_suso
      nom <- sub("\\.dta$", "", basename(f))
      tables[[nom]] <- c(tables[[nom]], list(x$df))
      for (v in names(x$labels)) {
        l <- x$labels[[v]]
        labels[[v]] <- c(labels[[v]], stats::setNames(names(l), as.character(unname(l))))
        labels[[v]] <- labels[[v]][!duplicated(names(labels[[v]]), fromLast = TRUE)]
      }
    }
  }
  tables <- lapply(tables, dplyr::bind_rows)
  list(tables = tables, labels = labels, sources = noms[ordre])
}

libelle <- function(x, dico) {
  cle <- ifelse(is.na(x), NA, formatC(x, format = "d", big.mark = ""))
  out <- unname(dico[cle])
  out[is.na(out) & !is.na(x)] <- cle[is.na(out) & !is.na(x)]
  out[is.na(x)] <- ""
  out
}

# Durée Survey Solutions JJ.HH:MM:SS -> minutes
minutes <- function(duree) {
  p <- regmatches(duree, regexec("^(\\d+)\\.(\\d+):(\\d+):(\\d+)$", duree))
  vapply(p, function(z) if (length(z) == 5) {
    z <- as.numeric(z[-1]); z[1] * 1440 + z[2] * 60 + z[3] + z[4] / 60
  } else NA_real_, numeric(1))
}

date_heure <- function(x) as.POSIXct(x, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")

groupe_statut <- function(code) {
  ifelse(code %in% STATUTS_SOUMIS, GROUPES[1], ifelse(code %in% STATUTS_REJETES, GROUPES[2], GROUPES[3]))
}

preparer <- function(t, labels) {
  m <- t$PROD_ECPRA_BMD
  diag <- t$interview__diagnostics[, c("interview__id", "interview__duration", "n_questions_unanswered",
                                       "rejections__sup", "rejections__hq", "responsible")]
  m <- dplyr::left_join(m, diag, by = "interview__id")
  m$statut_code <- as.integer(m$interview__status)
  m$statut <- unname(STATUTS[as.character(m$statut_code)])
  m$statut[is.na(m$statut)] <- as.character(m$statut_code[is.na(m$statut)])
  m$groupe <- groupe_statut(m$statut_code)
  m$duree_min <- minutes(m$interview__duration)
  m$region <- libelle(m$A_1, labels$A_1)
  m$departement <- libelle(m$A_2, labels$A_2)
  m$commune <- libelle(m$A_3, labels$A_3)
  m$nb_rejets <- dplyr::coalesce(m$rejections__sup, 0) + dplyr::coalesce(m$rejections__hq, 0)

  a <- t$interview__actions
  a$ts <- as.POSIXct(paste(a$date, a$time), format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  a$jour <- as.Date(a$ts)
  enq <- a[a$role == 1, ] |> dplyr::arrange(ts) |> dplyr::group_by(interview__id) |>
    dplyr::summarise(enq = dplyr::last(originator), .groups = "drop")
  m$enqueteur <- dplyr::coalesce(enq$enq[match(m$interview__id, enq$interview__id)], m$responsible)
  par_id <- function(d, f) { s <- d |> dplyr::group_by(interview__id) |> dplyr::summarise(v = f(ts), .groups = "drop")
                             s$v[match(m$interview__id, s$interview__id)] }
  m$date_debut <- par_id(a[a$action == 2, ], min)
  m$date_1er_envoi <- par_id(a[a$action == 3, ], min)
  m$derniere_action <- par_id(a, max)
  m$date_tablette <- date_heure(m$DateEntretien)
  list(m = m, a = a)
}

charger <- function(dossiers) {
  x <- lire_exports(dossiers)
  p <- preparer(x$tables, x$labels)
  list(t = x$tables, labels = x$labels, sources = x$sources, m = p$m, a = p$a)
}
