# Installe les packages R nécessaires à l'application (seulement ceux qui manquent).
# Versions avec lesquelles l'application a été testée (R 4.5.2) :
#   shiny 1.14.0, bslib 0.11.0, bsicons 0.1.2, shinyWidgets 0.9.1, shinycssloaders 1.1.0, waiter 0.2.5.1,
#   echarts4r 0.5.0, reactable 0.4.5, haven 2.5.5, dplyr 1.2.1, tidyr 1.3.2, openxlsx 4.2.8.1

paquets <- c("shiny", "bslib", "bsicons", "shinyWidgets", "shinycssloaders", "waiter", "echarts4r", "reactable",
             "haven", "dplyr", "tidyr", "openxlsx", "jsonlite", "htmlwidgets")
depot <- "https://cloud.r-project.org"

manquants <- paquets[!vapply(paquets, requireNamespace, logical(1), quietly = TRUE)]
if (length(manquants)) {
  # si la bibliothèque de R n'est pas modifiable (R installé pour tous les utilisateurs), on utilise
  # la bibliothèque personnelle de l'utilisateur
  lib <- .libPaths()[1]
  if (file.access(lib, 2) != 0) {
    lib <- Sys.getenv("R_LIBS_USER")
    dir.create(lib, recursive = TRUE, showWarnings = FALSE)
    .libPaths(c(lib, .libPaths()))
  }
  # derrière un proxy d'entreprise, la méthode « wininet » (paramètres réseau de Windows) passe souvent
  # là où la méthode par défaut échoue
  if (.Platform$OS.type == "windows") {
    joignable <- suppressWarnings(tryCatch(nrow(utils::available.packages(repos = depot)) > 0,
                                           error = function(e) FALSE))
    if (!joignable) options(download.file.method = "wininet")
  }
  cat("Installation des packages manquants :", paste(manquants, collapse = ", "), "\n")
  suppressWarnings(utils::install.packages(manquants, lib = lib, repos = depot))
  encore <- manquants[!vapply(manquants, requireNamespace, logical(1), quietly = TRUE)]
  if (length(encore)) stop("Installation impossible : ", paste(encore, collapse = ", "))
}
cat("Packages R : OK\n")
