# Paramètres, nomenclatures et catalogue des contrôles
# (mêmes règles que programmes/suivi_collecte.py)

SEUILS_DEFAUT <- list(
  duree_min = 10,             # minutes : en dessous, entretien jugé bâclé (rejet)
  duree_max = 180,            # minutes : au-dessus, à vérifier
  nonrep_rejet = 5,           # nb de questions non renseignées à partir duquel on rejette
  rdt_miel_max = 40,          # litres de miel par ruche utilisée et par an
  rdt_cire_max = 5,           # kg de cire par ruche utilisée et par an
  prix_miel_rejet_min = 500,  prix_miel_rejet_max = 25000,   # FCFA / L (erreur d'unité probable)
  prix_miel_verif_min = 1000, prix_miel_verif_max = 15000,   # FCFA / L (atypique)
  prix_cire_verif_min = 300,  prix_cire_verif_max = 20000,   # FCFA / kg
  ecart_mensuel = 0.20,       # écart relatif toléré entre somme mensuelle (C2bis) et production annuelle
  ecart_mensuel_rejet = 2,    # facteur d'écart à partir duquel on rejette
  ecart_mensuel_min = 5,      # litres : écart absolu en dessous duquel on ne signale pas
  variation_ruches = 3,       # rapport ruches 2025 / 2024 (ou inverse)
  effectif_max = 100,
  salaire_min = 12000, salaire_max = 6000000,                # masse salariale annuelle par employé
  age_equip_max = 50, age_infra_max = 100,
  annee_creation_min = 1960
)

# validations Survey Solutions mal paramétrées ou de pure forme : signalées sans bloquer
VALIDATIONS_NON_BLOQUANTES <- c("A_18", "A_6", "A_12_1", "C_1_6_24bis")

STATUTS <- c("-1" = "Supprimé", "0" = "Restauré", "20" = "Créé", "40" = "Assigné au superviseur",
             "60" = "Chez l'enquêteur", "65" = "Rejeté par le superviseur", "80" = "Prêt",
             "85" = "Envoyé CAPI", "95" = "Redémarré", "100" = "Complété",
             "120" = "Approuvé superviseur", "125" = "Rejeté par le siège", "130" = "Approuvé siège")
STATUTS_SOUMIS <- c(100, 120, 130)
STATUTS_REJETES <- c(65, 125)
ACTION_REJET <- c("100" = "Rejeter (superviseur)", "120" = "Rejeter (siège)",
                  "130" = "Désapprouver puis rejeter (siège)")
GROUPES <- c("Complété / approuvé", "Rejeté (en correction)", "En cours chez l'enquêteur")

TYPES_RUCHE <- c("1" = "Langstroth", "2" = "Dadant", "3" = "Wautier", "4" = "Kényane", "5" = "Paille",
                 "6" = "Tronc d'arbre", "7" = "Mortier", "99" = "Autre")
# codes C_O_2x (types exploités) -> codes B_4 (types possédés)
RUCHE_C_VERS_B <- c("1" = 1, "2" = 3, "3" = 4, "4" = 2, "5" = 5, "6" = 6, "7" = 7, "99" = 99)
EQUIPEMENTS <- c("1" = "Ruches", "2" = "Presse à miel", "3" = "Extracteur", "4" = "Maturateur",
                 "5" = "Bac à désoperculer", "6" = "Matériel roulant", "7" = "Combinaison", "8" = "Enfumoir")
INFRAS <- c("1" = "Unité de traitement de cire", "2" = "Miellerie", "3" = "Bureaux", "4" = "Magasin de stock")

COULEURS <- c(bleu = "#2A78D6", orange = "#EB6834", aqua = "#1BAF7A")

milliers <- function(x) formatC(x, format = "f", digits = 0, big.mark = " ")

catalogue_regles <- function(s) {
  r <- function(code, niveau, section, controle, seuil) {
    data.frame(code = code, niveau = niveau, section = section, controle = controle, seuil = seuil)
  }
  rbind(
    r("R01", "Rejet", "Toutes", "Erreur de validation Survey Solutions non corrigée", "has__errors > 0"),
    r("R02", "Rejet", "Toutes", "Trop de questions non renseignées", paste(">=", s$nonrep_rejet, "questions")),
    r("R03", "À vérifier", "Toutes", "Quelques questions non renseignées", paste("1 à", s$nonrep_rejet - 1, "questions")),
    r("R04", "Rejet", "Paradonnées", "Durée d'entretien trop courte", paste("<", s$duree_min, "min")),
    r("R05", "À vérifier", "Paradonnées", "Durée d'entretien très longue", paste(">", s$duree_max, "min")),
    r("R06", "Rejet", "A", "Localisation incomplète (région, département, commune, milieu)", "valeur manquante"),
    r("R07", "À vérifier", "A", "Âge du répondant ou du propriétaire atypique", "< 15 ans ou code 99"),
    r("R08", "Rejet", "B", "Année de création hors bornes", "hors [1900 ; année de l'entretien]"),
    r("R09", "À vérifier", "B", "Exploitation très ancienne", paste("<", s$annee_creation_min)),
    r("R10", "Rejet", "B", "Effectif total ≠ hommes + femmes", "B_3 ≠ B_3_2_1 (H) + B_3_2_1 (F)"),
    r("R11", "À vérifier", "B", "Code « Ne sait pas » (9999) dans les effectifs ou salaires", "= 9999"),
    r("R12", "À vérifier", "B", "Effectif du personnel élevé", paste(">", s$effectif_max)),
    r("R13", "À vérifier", "B", "Masse salariale annuelle par employé atypique",
      paste0("hors [", milliers(s$salaire_min), " ; ", milliers(s$salaire_max), "] FCFA")),
    r("R14", "Rejet", "B/C", "Déclare ne posséder aucune ruche mais en exploite", "B_4Ter = 1 et C_1_1 > 0"),
    r("R15", "À vérifier", "B/C", "Type de ruche exploité mais non déclaré possédé en B.4", "C_O_2x vs B_4"),
    r("R16", "Rejet", "C", "Ruches utilisées > ruches détenues", "C_1_2 > C_1_1"),
    r("R17", "Rejet", "C", "Quantité vendue > quantité produite (miel ou cire)", "C_1_6 > C_1_3 ou C_1_7 > C_1_4"),
    r("R18", "Rejet", "C", "Vente sans montant ou montant sans vente", "quantité > 0 et montant = 0 (ou l'inverse)"),
    r("R19", "Rejet", "C", "Production de miel sans ruche utilisée", "C_1_3 > 0 et C_1_2 = 0"),
    r("R20", "À vérifier", "C", "Ruches utilisées sans aucune production", "C_1_2 > 0 et miel = cire = gâteau = 0"),
    r("R21", "À vérifier", "C", "Rendement en miel très élevé", paste(">", s$rdt_miel_max, "L / ruche utilisée")),
    r("R22", "Rejet", "C", "Prix du miel aberrant (erreur d'unité probable)",
      paste0("hors [", s$prix_miel_rejet_min, " ; ", s$prix_miel_rejet_max, "] FCFA/L")),
    r("R23", "À vérifier", "C", "Prix du miel atypique",
      paste0("hors [", s$prix_miel_verif_min, " ; ", s$prix_miel_verif_max, "] FCFA/L")),
    r("R24", "À vérifier", "C", "Prix de la cire atypique",
      paste0("hors [", s$prix_cire_verif_min, " ; ", s$prix_cire_verif_max, "] FCFA/kg")),
    r("R25", "Rejet", "C", "Somme des quantités mensuelles très différente de la production annuelle",
      paste0("rapport > ", s$ecart_mensuel_rejet, " et écart > ", s$ecart_mensuel_min, " L")),
    r("R26", "À vérifier", "C", "Mois de production déclarés sans aucune production", "C_2 renseigné, C_1_3 = 0"),
    r("R27", "À vérifier", "C", "Forte variation du nombre de ruches entre 2024 et 2025",
      paste("rapport >", s$variation_ruches)),
    r("R28", "À vérifier", "C/D", "Ruches (équipement D.2) incohérentes avec ruches détenues (C.1)",
      "rapport hors [0,5 ; 2] ou ruches non déclarées en D.2"),
    r("R29", "À vérifier", "D", "Propriété individuelle + collective ≠ nombre total", "D_x_2 + D_x_3 ≠ D_x_1"),
    r("R30", "À vérifier", "D", "Prix d'achat très faible", "< 1 000 FCFA (infrastructure) ou < 500 FCFA (équipement)"),
    r("R31", "À vérifier", "D", "Âge de l'équipement / infrastructure très élevé",
      paste0("> ", s$age_equip_max, " ans (équip.) ou > ", s$age_infra_max, " ans (infra.)")),
    r("R32", "À vérifier", "E", "Organisation (coopérative, GIE, association) avec moins de 2 membres", "E_1 < 2"),
    r("R33", "À vérifier", "Paradonnées", "Dates incohérentes (date tablette, fin avant début)",
      "écart > 1 jour avec la date serveur"),
    r("R34", "À vérifier", "A", "Doublon potentiel", "même commune et même dénomination, ou même profil"),
    r("R35", "À vérifier", "A", "Région différente de la zone habituelle de l'enquêteur", "région ≠ région modale"),
    r("R36", "À vérifier", "C", "Somme des quantités mensuelles ≠ production annuelle",
      paste0("écart > ", round(100 * s$ecart_mensuel), " % (rapport ≤ ", s$ecart_mensuel_rejet, ") et > ",
             s$ecart_mensuel_min, " L")),
    r("R37", "À vérifier", "Toutes", "Erreur de validation non bloquante (règle de forme ou mal paramétrée)",
      paste(sort(VALIDATIONS_NON_BLOQUANTES), collapse = ", ")),
    r("R38", "À vérifier", "C", "Production de cire anormalement élevée",
      paste0("> ", s$rdt_cire_max, " kg / ruche utilisée, ou cire (kg) > miel (L) avec plus de 10 kg")),
    r("R39", "À vérifier", "C", "Quantité égale à 99 / 999 / 9999 (code « ne sait pas » utilisé à tort ?)",
      "C_1_1 à C_1_7 = 99, 999 ou 9999")
  )
}
