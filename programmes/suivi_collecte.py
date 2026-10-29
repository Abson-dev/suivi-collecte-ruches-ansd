# -*- coding: utf-8 -*-
"""
Enquête complémentaire et de productivité des ruches d'abeilles (ANSD) - Suivi de la collecte

Produit deux classeurs Excel dans le dossier « sorties » :
  1. Suivi_collecte_<date>.xlsx            : tendances de la collecte (jour, région, enquêteur, durées)
  2. Incoherences_a_rejeter_<date>.xlsx     : unités incohérentes, liste à rejeter dans Survey Solutions
et un fichier CSV (interview__id + commentaire) utilisable pour un rejet en masse par l'API.

Toutes les versions du questionnaire sont empilées : chaque export Stata de Survey Solutions
doit être décompressé dans un dossier nommé PROD_ECPRA_BMD_<n>_STATA_All à la racine du projet.

Utilisation :  python programmes/suivi_collecte.py
"""
import re
import sys
import unicodedata
import zipfile
from datetime import datetime
from pathlib import Path

import numpy as np
import pandas as pd
import pyreadstat
from openpyxl import Workbook
from openpyxl.chart import BarChart, LineChart, Reference
from openpyxl.formatting.rule import ColorScaleRule
from openpyxl.styles import Alignment, Border, Font, PatternFill, Side
from openpyxl.utils import get_column_letter
from openpyxl.worksheet.datavalidation import DataValidation

RACINE = Path(__file__).resolve().parent.parent
SORTIES = RACINE / "sorties"

# --------------------------------------------------------------------------------------------
# Seuils des contrôles (à ajuster si besoin)
# --------------------------------------------------------------------------------------------
DUREE_MIN = 10            # minutes : en dessous, entretien jugé bâclé (rejet)
DUREE_MAX = 180           # minutes : au-dessus, à vérifier (entretien laissé ouvert ?)
NONREP_REJET = 5          # nb de questions non renseignées à partir duquel on rejette
RDT_MIEL_MAX = 40         # litres de miel par ruche utilisée et par an
RDT_CIRE_MAX = 5          # kg de cire par ruche utilisée et par an
PRIX_MIEL_REJET = (500, 25000)    # FCFA / litre : hors bornes = erreur d'unité probable (rejet)
PRIX_MIEL_VERIF = (1000, 15000)   # FCFA / litre : hors bornes = à vérifier
PRIX_CIRE_VERIF = (300, 20000)    # FCFA / kg
ECART_MENSUEL = 0.20      # écart relatif toléré entre la somme mensuelle (C2bis) et la production annuelle
ECART_MENSUEL_REJET = 2   # facteur d'écart (somme mensuelle / annuelle ou l'inverse) à partir duquel on rejette
ECART_MENSUEL_MIN = 5     # litres : écart absolu en dessous duquel on ne signale pas
# validations Survey Solutions mal paramétrées ou de pure forme : signalées sans bloquer
VALIDATIONS_NON_BLOQUANTES = {"A_18", "A_6", "A_12_1", "C_1_6_24bis"}
VARIATION_RUCHES = 3      # rapport ruches 2025 / 2024 (ou inverse) à partir duquel on vérifie
EFFECTIF_MAX = 100        # personnel de l'exploitation
SALAIRE_MIN, SALAIRE_MAX = 12000, 6000000   # masse salariale annuelle par employé (FCFA)
AGE_EQUIP_MAX, AGE_INFRA_MAX = 50, 100      # années
ANNEE_CREATION_MIN = 1960

STATUTS = {-1: "Supprimé", 0: "Restauré", 20: "Créé", 40: "Assigné au superviseur",
           60: "Chez l'enquêteur", 65: "Rejeté par le superviseur", 80: "Prêt", 85: "Envoyé CAPI",
           95: "Redémarré", 100: "Complété", 120: "Approuvé superviseur",
           125: "Rejeté par le siège", 130: "Approuvé siège"}
STATUTS_SOUMIS = [100, 120, 130]          # entretiens envoyés par l'enquêteur et non rejetés
STATUTS_REJETES = [65, 125]
ACTION_REJET = {100: "Rejeter (superviseur)", 120: "Rejeter (siège)",
                130: "Désapprouver puis rejeter (siège)"}

TYPES_RUCHE = {1: "Langstroth", 2: "Dadant", 3: "Wautier", 4: "Kényane", 5: "Paille",
               6: "Tronc d'arbre", 7: "Mortier", 99: "Autre"}
# codes C_O_2x (types exploités) -> codes B_4 (types possédés)
RUCHE_C_VERS_B = {1: 1, 2: 3, 3: 4, 4: 2, 5: 5, 6: 6, 7: 7, 99: 99}
MOIS = {1: "Janvier", 2: "Février", 3: "Mars", 4: "Avril", 5: "Mai", 6: "Juin", 7: "Juillet",
        8: "Août", 9: "Septembre", 10: "Octobre", 11: "Novembre", 12: "Décembre"}
EQUIPEMENTS = {1: "Ruches", 2: "Presse à miel", 3: "Extracteur", 4: "Maturateur",
               5: "Bac à désoperculer", 6: "Matériel roulant", 7: "Combinaison", 8: "Enfumoir"}
INFRAS = {1: "Unité de traitement de cire", 2: "Miellerie", 3: "Bureaux", 4: "Magasin de stock"}

REGLES = {
    # code: (niveau, section, contrôle, seuil)
    "R01": ("Rejet", "Toutes", "Erreur de validation Survey Solutions non corrigée", "has__errors > 0"),
    "R02": ("Rejet", "Toutes", "Trop de questions non renseignées", f">= {NONREP_REJET} questions"),
    "R03": ("À vérifier", "Toutes", "Quelques questions non renseignées", f"1 à {NONREP_REJET - 1} questions"),
    "R04": ("Rejet", "Paradonnées", "Durée d'entretien trop courte", f"< {DUREE_MIN} min"),
    "R05": ("À vérifier", "Paradonnées", "Durée d'entretien très longue", f"> {DUREE_MAX} min"),
    "R06": ("Rejet", "A", "Localisation incomplète (région, département, commune, milieu)", "valeur manquante"),
    "R07": ("À vérifier", "A", "Âge du répondant ou du propriétaire atypique", "< 15 ans ou code 99"),
    "R08": ("Rejet", "B", "Année de création hors bornes", "hors [1900 ; année de l'entretien]"),
    "R09": ("À vérifier", "B", "Exploitation très ancienne", f"< {ANNEE_CREATION_MIN}"),
    "R10": ("Rejet", "B", "Effectif total ≠ hommes + femmes", "B_3 ≠ B_3_2_1 (H) + B_3_2_1 (F)"),
    "R11": ("À vérifier", "B", "Code « Ne sait pas » (9999) dans les effectifs ou salaires", "= 9999"),
    "R12": ("À vérifier", "B", "Effectif du personnel élevé", f"> {EFFECTIF_MAX}"),
    "R13": ("À vérifier", "B", "Masse salariale annuelle par employé atypique",
            f"hors [{SALAIRE_MIN:,} ; {SALAIRE_MAX:,}] FCFA".replace(",", " ")),
    "R14": ("Rejet", "B/C", "Déclare ne posséder aucune ruche mais en exploite", "B_4Ter = 1 et C_1_1 > 0"),
    "R15": ("À vérifier", "B/C", "Type de ruche exploité mais non déclaré possédé en B.4", "C_O_2x vs B_4"),
    "R16": ("Rejet", "C", "Ruches utilisées > ruches détenues", "C_1_2 > C_1_1"),
    "R17": ("Rejet", "C", "Quantité vendue > quantité produite (miel ou cire)", "C_1_6 > C_1_3 ou C_1_7 > C_1_4"),
    "R18": ("Rejet", "C", "Vente sans montant ou montant sans vente", "quantité > 0 et montant = 0 (ou l'inverse)"),
    "R19": ("Rejet", "C", "Production de miel sans ruche utilisée", "C_1_3 > 0 et C_1_2 = 0"),
    "R20": ("À vérifier", "C", "Ruches utilisées sans aucune production", "C_1_2 > 0 et miel = cire = gâteau = 0"),
    "R21": ("À vérifier", "C", "Rendement en miel très élevé", f"> {RDT_MIEL_MAX} L / ruche utilisée"),
    "R22": ("Rejet", "C", "Prix du miel aberrant (erreur d'unité probable)",
            f"hors [{PRIX_MIEL_REJET[0]} ; {PRIX_MIEL_REJET[1]}] FCFA/L"),
    "R23": ("À vérifier", "C", "Prix du miel atypique",
            f"hors [{PRIX_MIEL_VERIF[0]} ; {PRIX_MIEL_VERIF[1]}] FCFA/L"),
    "R24": ("À vérifier", "C", "Prix de la cire atypique",
            f"hors [{PRIX_CIRE_VERIF[0]} ; {PRIX_CIRE_VERIF[1]}] FCFA/kg"),
    "R25": ("Rejet", "C", "Somme des quantités mensuelles très différente de la production annuelle",
            f"rapport > {ECART_MENSUEL_REJET} et écart > {ECART_MENSUEL_MIN} L"),
    "R26": ("À vérifier", "C", "Mois de production déclarés sans aucune production", "C_2 renseigné, C_1_3 = 0"),
    "R27": ("À vérifier", "C", "Forte variation du nombre de ruches entre 2024 et 2025",
            f"rapport > {VARIATION_RUCHES}"),
    "R28": ("À vérifier", "C/D", "Ruches (équipement D.2) incohérentes avec ruches détenues (C.1)",
            "rapport hors [0,5 ; 2] ou ruches non déclarées en D.2"),
    "R29": ("À vérifier", "D", "Propriété individuelle + collective ≠ nombre total", "D_x_2 + D_x_3 ≠ D_x_1"),
    "R30": ("À vérifier", "D", "Prix d'achat très faible", "< 1 000 FCFA (infrastructure) ou < 500 FCFA (équipement)"),
    "R31": ("À vérifier", "D", "Âge de l'équipement / infrastructure très élevé",
            f"> {AGE_EQUIP_MAX} ans (équip.) ou > {AGE_INFRA_MAX} ans (infra.)"),
    "R32": ("À vérifier", "E", "Organisation (coopérative, GIE, association) avec moins de 2 membres", "E_1 < 2"),
    "R33": ("À vérifier", "Paradonnées", "Dates incohérentes (date tablette, fin avant début)",
            "écart > 1 jour avec la date serveur"),
    "R34": ("À vérifier", "A", "Doublon potentiel", "même commune et même dénomination, ou même profil"),
    "R35": ("À vérifier", "A", "Région différente de la zone habituelle de l'enquêteur", "région ≠ région modale"),
    "R36": ("À vérifier", "C", "Somme des quantités mensuelles ≠ production annuelle",
            f"écart > {ECART_MENSUEL:.0%} (rapport ≤ {ECART_MENSUEL_REJET}) et > {ECART_MENSUEL_MIN} L"),
    "R37": ("À vérifier", "Toutes", "Erreur de validation non bloquante (règle de forme ou mal paramétrée)",
            ", ".join(sorted(VALIDATIONS_NON_BLOQUANTES))),
    "R38": ("À vérifier", "C", "Production de cire anormalement élevée",
            f"> {RDT_CIRE_MAX} kg / ruche utilisée, ou cire (kg) > miel (L) avec plus de 10 kg"),
    "R39": ("À vérifier", "C", "Quantité égale à 99 / 999 / 9999 (code « ne sait pas » utilisé à tort ?)",
            "C_1_1 à C_1_7 = 99, 999 ou 9999"),
}


# --------------------------------------------------------------------------------------------
# Lecture des exports
# --------------------------------------------------------------------------------------------
def lire_exports():
    # un export encore compressé (tel que téléchargé depuis Survey Solutions) est décompressé à côté
    for archive in RACINE.glob("PROD_ECPRA_BMD_*_STATA_All.zip"):
        cible = RACINE / archive.stem
        if not cible.exists():
            with zipfile.ZipFile(archive) as z:
                z.extractall(cible)
    dossiers = sorted(RACINE.glob("PROD_ECPRA_BMD_*_STATA_All"),
                      key=lambda p: int(re.search(r"_(\d+)_STATA", p.name).group(1)))
    if not dossiers:
        sys.exit("Aucun dossier PROD_ECPRA_BMD_<n>_STATA_All trouvé dans " + str(RACINE))
    tables, labels = {}, {}
    for dossier in dossiers:
        version = int(re.search(r"_(\d+)_STATA", dossier.name).group(1))
        lisezmoi = (dossier / "export__readme.txt").read_text(encoding="utf-8", errors="ignore")
        v_suso = re.search(r"\$(\d+)", lisezmoi)
        for fichier in dossier.glob("*.dta"):
            df, meta = pyreadstat.read_dta(str(fichier))
            df = df.copy()
            for c in df.columns[df.dtypes == object]:
                df[c] = df[c].replace("##N/A##", "")
            df["version"] = f"V{version}"
            df["version_suso"] = int(v_suso.group(1)) if v_suso else np.nan
            tables.setdefault(fichier.stem, []).append(df)
            for var, lab in meta.variable_value_labels.items():
                labels.setdefault(var, {}).update({int(k): v for k, v in lab.items()})
    tables = {k: pd.concat(v, ignore_index=True) for k, v in tables.items()}
    return tables, labels, [d.name for d in dossiers]


def libelle(serie, dico):
    return serie.map(lambda x: dico.get(int(x), str(int(x))) if pd.notna(x) else "")


def nombre(x):
    if pd.isna(x):
        return "manquant"
    if float(x).is_integer():
        return f"{x:,.0f}".replace(",", " ")
    return f"{x:,.1f}".replace(",", " ")


def minutes(duree):
    # format Survey Solutions JJ.HH:MM:SS
    d = duree.fillna("").str.replace(r"^(\d+)\.", r"\1 days ", regex=True)
    return pd.to_timedelta(d, errors="coerce").dt.total_seconds() / 60


def normaliser(txt):
    txt = unicodedata.normalize("NFKD", str(txt)).encode("ascii", "ignore").decode()
    return re.sub(r"[^A-Z0-9]", "", txt.upper())


# --------------------------------------------------------------------------------------------
# Préparation de la base entretien
# --------------------------------------------------------------------------------------------
def preparer(t, labels):
    m = t["PROD_ECPRA_BMD"].copy()
    diag = t["interview__diagnostics"][["interview__id", "interview__duration", "n_questions_unanswered",
                                        "rejections__sup", "rejections__hq", "responsible"]]
    m = m.merge(diag, on="interview__id", how="left")
    m["statut_code"] = m["interview__status"].astype(int)
    m["statut"] = m["statut_code"].map(STATUTS).fillna(m["statut_code"].astype(str))
    m["duree_min"] = minutes(m["interview__duration"])
    m["region"] = libelle(m["A_1"], labels.get("A_1", {}))
    m["departement"] = libelle(m["A_2"], labels.get("A_2", {}))
    m["commune"] = libelle(m["A_3"], labels.get("A_3", {}))
    m["nb_rejets"] = m["rejections__sup"].fillna(0) + m["rejections__hq"].fillna(0)

    a = t["interview__actions"].copy()
    a["ts"] = pd.to_datetime(a["date"] + " " + a["time"], errors="coerce")
    a["jour"] = a["ts"].dt.normalize()
    enq = a[a["role"] == 1].sort_values("ts").groupby("interview__id")["originator"].last()
    m["enqueteur"] = m["interview__id"].map(enq).fillna(m["responsible"])
    premiers = {
        "date_debut": a[a["action"] == 2].groupby("interview__id")["ts"].min(),
        "date_1er_envoi": a[a["action"] == 3].groupby("interview__id")["ts"].min(),
        "date_dernier_envoi": a[a["action"] == 3].groupby("interview__id")["ts"].max(),
        "derniere_action": a.groupby("interview__id")["ts"].max(),
    }
    for col, s in premiers.items():
        m[col] = m["interview__id"].map(s)
    m["date_tablette"] = pd.to_datetime(m["DateEntretien"], errors="coerce")
    return m, a


# --------------------------------------------------------------------------------------------
# Contrôles de cohérence
# --------------------------------------------------------------------------------------------
class Controles:
    def __init__(self, m):
        self.m = m
        self.lignes = []

    def signale(self, code, d, message, variable="", valeur=None, ligne=None):
        if len(d) == 0:
            return
        msg = message if isinstance(message, pd.Series) else pd.Series(message, index=d.index)
        self.lignes.append(pd.DataFrame({
            "interview__id": d["interview__id"].values,
            "code": code,
            "variable": variable,
            "ligne": ligne.values if ligne is not None else "",
            "valeur": valeur.map(nombre).values if valeur is not None else "",
            "message": msg.values,
        }))

    def resultat(self):
        if not self.lignes:
            return pd.DataFrame(columns=["interview__id", "code", "variable", "ligne", "valeur", "message"])
        return pd.concat(self.lignes, ignore_index=True)


def controler(t, m):
    c = Controles(m)
    envoyes = m[m["statut_code"].isin(STATUTS_SOUMIS + STATUTS_REJETES)]

    # R01 - erreurs de validation encore présentes
    err = t["interview__errors"].copy()
    err["message"] = (err["message"].str.replace(r"<[^>]+>", " ", regex=True)
                      .str.replace(r"\s+", " ", regex=True).str.strip())
    err["ligne"] = np.where(err["roster"].astype(str).replace("nan", "") != "",
                            err["roster"].astype(str) + " ligne "
                            + pd.to_numeric(err["id1"], errors="coerce").fillna(0).astype(int).astype(str), "")
    err = err.drop_duplicates(["interview__id", "variable", "ligne", "message"])
    forme = err["variable"].isin(VALIDATIONS_NON_BLOQUANTES)
    for code, d in [("R01", err[~forme]), ("R37", err[forme])]:
        c.signale(code, d, d["variable"] + " : " + d["message"], d["variable"].values, ligne=d["ligne"])

    # R02 / R03 - questions non renseignées
    nr = envoyes["n_questions_unanswered"]
    d = envoyes[nr >= NONREP_REJET]
    c.signale("R02", d, d["n_questions_unanswered"].map(lambda n: f"{n:.0f} questions non renseignées"),
              valeur=d["n_questions_unanswered"])
    d = envoyes[(nr > 0) & (nr < NONREP_REJET)]
    c.signale("R03", d, d["n_questions_unanswered"].map(lambda n: f"{n:.0f} question(s) non renseignée(s)"),
              valeur=d["n_questions_unanswered"])

    # R04 / R05 - durée
    d = envoyes[envoyes["duree_min"] < DUREE_MIN]
    c.signale("R04", d, d["duree_min"].map(lambda x: f"Entretien réalisé en {x:.0f} min"), valeur=d["duree_min"])
    d = envoyes[envoyes["duree_min"] > DUREE_MAX]
    c.signale("R05", d, d["duree_min"].map(lambda x: f"Entretien de {x:.0f} min"), valeur=d["duree_min"])

    # R06 - localisation
    for var, nom in [("A_1", "Région"), ("A_2", "Département"), ("A_3", "Commune"), ("A_5", "Milieu")]:
        d = envoyes[envoyes[var].isna()]
        c.signale("R06", d, f"{nom} ({var}) non renseigné(e)", var)

    # R07 - âges
    for var, qui in [("A_8", "répondant"), ("A_12_2", "propriétaire")]:
        d = m[m[var] < 15]
        c.signale("R07", d, d[var].map(lambda x: f"Âge du {qui} = {x:.0f} ans"), var, d[var])
        d = m[m[var] == 99]
        c.signale("R07", d, f"Âge du {qui} = 99 (code « ne sait pas » ou âge réel ?)", var, d[var])

    # R08 / R09 - année de création
    annee = m["date_tablette"].dt.year.fillna(m["date_debut"].dt.year).fillna(datetime.now().year)
    d = m[m["B_1"].notna() & ((m["B_1"] < 1900) | (m["B_1"] > annee))]
    c.signale("R08", d, d["B_1"].map(lambda x: f"Année de création = {x:.0f}"), "B_1", d["B_1"])
    d = m[(m["B_1"] >= 1900) & (m["B_1"] < ANNEE_CREATION_MIN)]
    c.signale("R09", d, d["B_1"].map(lambda x: f"Exploitation créée en {x:.0f}"), "B_1", d["B_1"])

    # R10 à R13 - personnel
    sexe = t["RostSexe"].pivot_table(index="interview__id", columns="RostSexe__id",
                                     values=["B_3_2_1", "B_3_2_2"], aggfunc="first")
    sexe.columns = [f"{v}_{'H' if s == 1 else 'F'}" for v, s in sexe.columns]
    p = m[["interview__id", "B_3"]].merge(sexe, left_on="interview__id", right_index=True, how="inner")
    for col in ["B_3_2_1_H", "B_3_2_1_F", "B_3_2_2_H", "B_3_2_2_F"]:
        if col not in p:
            p[col] = np.nan
    connus = p[p["B_3_2_1_H"].notna() & p["B_3_2_1_F"].notna()
               & (p["B_3_2_1_H"] != 9999) & (p["B_3_2_1_F"] != 9999) & p["B_3"].notna()]
    d = connus[connus["B_3_2_1_H"] + connus["B_3_2_1_F"] != connus["B_3"]]
    c.signale("R10", d, d.apply(lambda r: f"Effectif total {r.B_3:.0f} ≠ {r.B_3_2_1_H:.0f} hommes + "
                                          f"{r.B_3_2_1_F:.0f} femmes", axis=1), "B_3", d["B_3"])
    for col, lib in [("B_3_2_1_H", "effectif hommes"), ("B_3_2_1_F", "effectif femmes"),
                     ("B_3_2_2_H", "salaires hommes"), ("B_3_2_2_F", "salaires femmes")]:
        d = p[p[col] == 9999]
        c.signale("R11", d, f"Code 9999 (ne sait pas) pour {lib}", col.rsplit("_", 1)[0])
    d = m[m["B_3"] > EFFECTIF_MAX]
    c.signale("R12", d, d["B_3"].map(lambda x: f"Personnel de {x:.0f} personnes"), "B_3", d["B_3"])
    for s, lib in [("H", "hommes"), ("F", "femmes")]:
        q = p[(p[f"B_3_2_1_{s}"] > 0) & (p[f"B_3_2_1_{s}"] != 9999)
              & p[f"B_3_2_2_{s}"].notna() & (p[f"B_3_2_2_{s}"] != 9999)].copy()
        q["sal"] = q[f"B_3_2_2_{s}"] / q[f"B_3_2_1_{s}"]
        d = q[(q["sal"] < SALAIRE_MIN) | (q["sal"] > SALAIRE_MAX)]
        c.signale("R13", d, d["sal"].map(lambda x: f"Masse salariale annuelle par employé ({lib}) = "
                                                   f"{nombre(x)} FCFA"), "B_3_2_2", d["sal"])

    # Productivité par type de ruche
    prod = {}
    for an in ["24", "25"]:
        r = t[f"rstProductivite{an}"].copy()
        r.columns = [re.sub(rf"_{an}(bis)?$", lambda x: "_bis" if x.group(1) else "", col)
                     .replace("C_1_6_bis", "C_1_6bis") for col in r.columns]
        r = r.rename(columns={f"rstProductivite{an}__id": "type"})
        r["annee"] = "20" + an
        r["lib"] = r["type"].map(TYPES_RUCHE).fillna("?") + " " + r["annee"]
        prod[an] = r
    pr = pd.concat(prod.values(), ignore_index=True)

    # R14 - pas de ruche mais exploitation
    sans = m.loc[m["B_4Ter"] == 1, "interview__id"]
    d = pr[pr["interview__id"].isin(sans) & (pr["C_1_1"] > 0)]
    c.signale("R14", d, d.apply(lambda r: f"Déclare n'avoir aucune ruche (B.4) mais {r.C_1_1:.0f} ruches "
                                          f"détenues ({r.lib})", axis=1), "B_4Ter", d["C_1_1"], d["lib"])

    # R15 - type exploité non possédé
    pr["b4"] = pr["type"].map(RUCHE_C_VERS_B)
    pos = m.set_index("interview__id")
    possede = pr.apply(lambda r: pos.at[r.interview__id, f"B_4__{int(r.b4)}"]
                       if pd.notna(r.b4) and f"B_4__{int(r.b4)}" in pos.columns else np.nan, axis=1)
    d = pr[(possede == 0) & (pr["C_1_1"] > 0) & (pr["annee"] == "2025")]
    c.signale("R15", d, d["lib"].map(lambda x: f"Ruches {x} exploitées mais ce type n'est pas coché en B.4"),
              "B_4", d["C_1_1"], d["lib"])

    # R16 à R24 - cohérence des quantités et des prix
    d = pr[pr["C_1_2"] > pr["C_1_1"]]
    c.signale("R16", d, d.apply(lambda r: f"{r.lib} : {r.C_1_2:.0f} ruches utilisées > {r.C_1_1:.0f} détenues",
                                axis=1), "C_1_2", d["C_1_2"], d["lib"])
    d = pr[pr["C_1_6"] > pr["C_1_3"]]
    c.signale("R17", d, d.apply(lambda r: f"{r.lib} : miel vendu ({nombre(r.C_1_6)}) > miel produit "
                                          f"({nombre(r.C_1_3)})", axis=1), "C_1_6", d["C_1_6"], d["lib"])
    d = pr[pr["C_1_7"] > pr["C_1_4"]]
    c.signale("R17", d, d.apply(lambda r: f"{r.lib} : cire vendue ({nombre(r.C_1_7)} kg) > cire produite "
                                          f"({nombre(r.C_1_4)} kg)", axis=1), "C_1_7", d["C_1_7"], d["lib"])
    for q, mt, prod_lib in [("C_1_6", "C_1_6bis", "miel"), ("C_1_7", "C_1_8", "cire")]:
        d = pr[((pr[q] > 0) & (pr[mt] == 0)) | ((pr[q] == 0) & (pr[mt] > 0))]
        c.signale("R18", d, d.apply(lambda r: f"{r.lib} : {prod_lib} vendu = {nombre(r[q])}, montant = "
                                              f"{nombre(r[mt])} FCFA", axis=1), mt, d[mt], d["lib"])
    d = pr[(pr["C_1_3"] > 0) & (pr["C_1_2"] == 0)]
    c.signale("R19", d, d.apply(lambda r: f"{r.lib} : {nombre(r.C_1_3)} L de miel avec 0 ruche utilisée", axis=1),
              "C_1_3", d["C_1_3"], d["lib"])
    d = pr[(pr["C_1_2"] > 0) & (pr[["C_1_3", "C_1_4", "C_1_5"]].astype(float).fillna(0).sum(axis=1) == 0)
           & pr["C_1_3"].notna()]
    c.signale("R20", d, d.apply(lambda r: f"{r.lib} : {r.C_1_2:.0f} ruches utilisées mais aucune production",
                                axis=1), "C_1_3", d["C_1_2"], d["lib"])
    pr["rdt"] = pr["C_1_3"] / pr["C_1_2"].where(pr["C_1_2"] > 0)
    d = pr[pr["rdt"] > RDT_MIEL_MAX]
    c.signale("R21", d, d.apply(lambda r: f"{r.lib} : {nombre(r.rdt)} L de miel par ruche utilisée", axis=1),
              "C_1_3", d["rdt"], d["lib"])
    pr["rdt_cire"] = pr["C_1_4"] / pr["C_1_2"].where(pr["C_1_2"] > 0)
    d = pr[(pr["rdt_cire"] > RDT_CIRE_MAX) | ((pr["C_1_4"] > pr["C_1_3"]) & (pr["C_1_4"] > 10))]
    c.signale("R38", d, d.apply(lambda r: f"{r.lib} : {nombre(r.C_1_4)} kg de cire pour {nombre(r.C_1_3)} L de "
                                          f"miel et {nombre(r.C_1_2)} ruches utilisées", axis=1),
              "C_1_4", d["C_1_4"], d["lib"])
    for var, quoi in [("C_1_1", "ruches détenues"), ("C_1_2", "ruches utilisées"), ("C_1_3", "miel produit"),
                      ("C_1_4", "cire produite"), ("C_1_5", "gâteau"), ("C_1_6", "miel vendu"),
                      ("C_1_7", "cire vendue")]:
        d = pr[pr[var].isin([99, 999, 9999])]
        c.signale("R39", d, d.apply(lambda r: f"{r.lib} : {quoi} = {nombre(r[var])} (code « ne sait pas » ?)",
                                    axis=1), var, d[var], d["lib"])
    pr["prix_miel"] = pr["C_1_6bis"] / pr["C_1_6"].where(pr["C_1_6"] > 0)
    pr["prix_cire"] = pr["C_1_8"] / pr["C_1_7"].where(pr["C_1_7"] > 0)
    pm = pr[pr["C_1_6bis"] > 0]
    hors_rejet = (pm["prix_miel"] < PRIX_MIEL_REJET[0]) | (pm["prix_miel"] > PRIX_MIEL_REJET[1])
    hors_verif = (pm["prix_miel"] < PRIX_MIEL_VERIF[0]) | (pm["prix_miel"] > PRIX_MIEL_VERIF[1])
    texte = lambda r: (f"{r.lib} : {nombre(r.C_1_6)} L vendus pour {nombre(r.C_1_6bis)} FCFA, "
                       f"soit {nombre(r.prix_miel)} FCFA/L")
    d = pm[hors_rejet]
    c.signale("R22", d, d.apply(texte, axis=1), "C_1_6bis", d["prix_miel"], d["lib"])
    d = pm[hors_verif & ~hors_rejet]
    c.signale("R23", d, d.apply(texte, axis=1), "C_1_6bis", d["prix_miel"], d["lib"])
    pc = pr[pr["C_1_8"] > 0]
    d = pc[(pc["prix_cire"] < PRIX_CIRE_VERIF[0]) | (pc["prix_cire"] > PRIX_CIRE_VERIF[1])]
    c.signale("R24", d, d.apply(lambda r: f"{r.lib} : {nombre(r.C_1_7)} kg de cire vendus pour "
                                          f"{nombre(r.C_1_8)} FCFA, soit {nombre(r.prix_cire)} FCFA/kg", axis=1),
              "C_1_8", d["prix_cire"], d["lib"])

    # R25 / R26 - quantités mensuelles
    qt = t["QtitPeriode"]
    for an in ["24", "25"]:
        annuel = prod[an].groupby("interview__id")["C_1_3"].sum(min_count=1)
        mensuel = qt.groupby("interview__id")[f"C2bis{an}"].sum(min_count=1)
        cmp_ = pd.concat([annuel.rename("annuel"), mensuel.rename("mensuel")], axis=1).dropna().astype(float)
        ecart = (cmp_["mensuel"] - cmp_["annuel"]).abs()
        hors = cmp_[(ecart > ECART_MENSUEL_MIN) & (ecart > ECART_MENSUEL * cmp_[["annuel", "mensuel"]].max(axis=1))]
        rapport = hors[["annuel", "mensuel"]].max(axis=1) / hors[["annuel", "mensuel"]].min(axis=1).replace(0, np.nan)
        fort = rapport.isna() | (rapport > ECART_MENSUEL_REJET)
        for code, d in [("R25", hors[fort]), ("R36", hors[~fort])]:
            d = d.reset_index()
            c.signale(code, d, d.apply(lambda r: f"20{an} : somme des quantités mensuelles = {nombre(r.mensuel)} L "
                                                 f"≠ production annuelle déclarée = {nombre(r.annuel)} L", axis=1),
                      f"C2bis{an}", d["mensuel"], pd.Series(f"20{an}", index=d.index))
    tot = pr.groupby("interview__id")["C_1_3"].sum(min_count=1)
    mois = m.filter(regex=r"^C_2__\d+$").sum(axis=1)
    d = m[(mois > 0) & (m["interview__id"].map(tot) == 0)]
    c.signale("R26", d, "Mois de production cochés en C.2 mais aucune production de miel en 2024 et 2025", "C_2")

    # R27 - variation du nombre de ruches
    t24 = prod["24"].groupby("interview__id")["C_1_1"].sum(min_count=1)
    t25 = prod["25"].groupby("interview__id")["C_1_1"].sum(min_count=1)
    v = pd.concat([t24.rename("r24"), t25.rename("r25")], axis=1).dropna()
    v = v[(v["r24"] > 0) & (v["r25"] > 0) & (v[["r24", "r25"]].max(axis=1) >= 10)]
    d = v[(v["r25"] / v["r24"] > VARIATION_RUCHES) | (v["r24"] / v["r25"] > VARIATION_RUCHES)].reset_index()
    c.signale("R27", d, d.apply(lambda r: f"Ruches détenues : {r.r24:.0f} en 2024 contre {r.r25:.0f} en 2025",
                                axis=1), "C_1_1", d["r25"])

    # R28 - ruches en équipement vs ruches détenues
    eq = t["rstEquipement"]
    ruches_eq = eq[eq["rstEquipement__id"] == 1].set_index("interview__id")["D_2_1"]
    w = m[["interview__id", "D_2_0__1"]].copy()
    w["n_eq"] = w["interview__id"].map(ruches_eq)
    w["c1"] = w["interview__id"].map(t25)
    d = w[(w["D_2_0__1"] == 0) & (w["c1"] > 0)]
    c.signale("R28", d, d["c1"].map(lambda x: f"{x:.0f} ruches détenues en 2025 mais « Ruches » non coché en D.2.0"),
              "D_2_0", d["c1"])
    w = w[(w["n_eq"] > 0) & (w["c1"] > 0)]
    d = w[(w["n_eq"] / w["c1"] > 2) | (w["n_eq"] / w["c1"] < 0.5)]
    c.signale("R28", d, d.apply(lambda r: f"{r.n_eq:.0f} ruches en équipement (D.2.1) contre {r.c1:.0f} ruches "
                                          f"détenues en 2025 (C.1.1)", axis=1), "D_2_1", d["n_eq"])

    # R29 à R31 - infrastructures et équipements
    blocs = [("rstInfrastructures", "D_1_", "", INFRAS, 1000, AGE_INFRA_MAX),
             ("rstAutresInfrastructures", "D_1_", "Autres", None, 1000, AGE_INFRA_MAX),
             ("rstEquipement", "D_2_", "", EQUIPEMENTS, 500, AGE_EQUIP_MAX),
             ("rstEquipementAutres", "D_2_", "Autres", None, 500, AGE_EQUIP_MAX)]
    for nom, pre, suf, cats, prix_min, age_max in blocs:
        r = t[nom].copy()
        if cats:
            r["lib"] = r[f"{nom}__id"].map(cats).fillna("?")
        else:
            col_txt = "D_1_0_pre" if pre == "D_1_" else "D_2_0_pre"
            r["lib"] = r[col_txt].fillna("autre").astype(str)
        n, ind, col, age, val = (r[f"{pre}{i}{suf}"] for i in (1, 2, 3, 4, 6))
        d = r[n.notna() & ind.notna() & col.notna() & (ind + col != n)]
        c.signale("R29", d, d.apply(lambda x: f"{x.lib} : {nombre(x[f'{pre}2{suf}'])} individuel(s) + "
                                              f"{nombre(x[f'{pre}3{suf}'])} collectif(s) ≠ {nombre(x[f'{pre}1{suf}'])}",
                                    axis=1), f"{pre}1{suf}", d[f"{pre}1{suf}"], d["lib"])
        d = r[val < prix_min]
        c.signale("R30", d, d.apply(lambda x: f"{x.lib} : prix d'achat = {nombre(x[f'{pre}6{suf}'])} FCFA", axis=1),
                  f"{pre}6{suf}", d[f"{pre}6{suf}"], d["lib"])
        d = r[age > age_max]
        c.signale("R31", d, d.apply(lambda x: f"{x.lib} : âge = {nombre(x[f'{pre}4{suf}'])} ans", axis=1),
                  f"{pre}4{suf}", d[f"{pre}4{suf}"], d["lib"])

    # R32 - organisations
    d = m[m["A_14"].isin([2, 3, 4]) & (m["E_1"] < 2)]
    c.signale("R32", d, d["E_1"].map(lambda x: f"Organisation déclarant {x:.0f} membre(s)"), "E_1", d["E_1"])

    # R33 - dates
    ecart = (m["date_tablette"] - m["date_debut"]).abs().dt.total_seconds() / 86400
    d = m[ecart > 1]
    c.signale("R33", d, d.apply(lambda r: f"Date saisie sur la tablette {r.date_tablette:%d/%m/%Y} ≠ date du "
                                          f"serveur {r.date_debut:%d/%m/%Y}", axis=1), "DateEntretien")
    d = m[m["DateEntretien"].fillna("") == ""]
    d = d[d["statut_code"].isin(STATUTS_SOUMIS + STATUTS_REJETES)]
    c.signale("R33", d, "Date de l'entretien non renseignée", "DateEntretien")
    fin = pd.to_datetime(m["DateFinEntretien"], errors="coerce")
    deb = pd.to_datetime(m["DebutEntretien"], errors="coerce")
    d = m[fin < deb]
    c.signale("R33", d, "Date de fin de l'entretien antérieure à la date de début", "DateFinEntretien")

    # R34 - doublons potentiels
    vides = {"", "NEANT", "NA", "NAN", "AUCUN", "AUCUNE", "RAS", "NON", "PAS", "NEAN", "NONE", "0"}
    u = m[m["statut_code"] >= 0].copy()
    u["nom"] = u["A_15"].fillna("").map(normaliser)
    criteres = [(["A_3", "nom"], "même commune et même dénomination"),
                (["A_3", "A_8", "A_10", "B_1", "B_3"], "même commune, âge, sexe, année de création et effectif")]
    for cols, lib in criteres:
        g = u.dropna(subset=cols)
        if "nom" in cols:
            g = g[~g["nom"].isin(vides) & (g["nom"].str.len() > 2)]
        g = g[g.duplicated(cols, keep=False)]
        for _, grp in g.groupby(cols):
            for i, r in grp.iterrows():
                autres = ", ".join(grp.loc[grp.index != i, "interview__key"])
                c.signale("R34", grp.loc[[i]], f"Doublon potentiel ({lib}) avec : {autres}", "A_3")

    # R35 - région hors zone habituelle de l'enquêteur
    z = m[m["A_1"].notna()]
    modale = z.groupby("enqueteur")["A_1"].agg(lambda s: s.mode().iloc[0])
    part = z.groupby("enqueteur")["A_1"].agg(lambda s: (s == s.mode().iloc[0]).mean())
    nb = z.groupby("enqueteur").size()
    fiables = modale[(nb >= 3) & (part >= 0.6)]
    z = z[z["enqueteur"].isin(fiables.index)]
    d = z[z["A_1"] != z["enqueteur"].map(fiables)]
    reg_lab = dict(zip(m["A_1"], m["region"]))
    c.signale("R35", d, d.apply(lambda r: f"Entretien en région {r.region} alors que l'enquêteur travaille "
                                          f"surtout en région {reg_lab.get(fiables[r.enqueteur], '?')}", axis=1), "A_1")

    det = c.resultat()
    info = m.set_index("interview__id")[["interview__key", "version", "statut_code", "statut", "enqueteur",
                                         "region", "departement", "commune", "date_debut"]]
    det = det.join(info, on="interview__id")
    det["niveau"] = det["code"].map(lambda k: REGLES[k][0])
    det["section"] = det["code"].map(lambda k: REGLES[k][1])
    det["controle"] = det["code"].map(lambda k: REGLES[k][2])
    det = det.sort_values(["niveau", "enqueteur", "interview__key", "code"],
                          key=lambda s: s.map({"Rejet": 0, "À vérifier": 1}) if s.name == "niveau" else s)
    return det, pr, fiables


# --------------------------------------------------------------------------------------------
# Tableaux de suivi
# --------------------------------------------------------------------------------------------
def groupe_statut(code):
    if code in STATUTS_SOUMIS:
        return "Complété / approuvé"
    if code in STATUTS_REJETES:
        return "Rejeté (en correction)"
    return "En cours chez l'enquêteur"


def tableaux_suivi(m, a, det, pr, labels, fiables):
    m = m.copy()
    m["groupe"] = m["statut_code"].map(groupe_statut)
    groupes = ["Complété / approuvé", "Rejeté (en correction)", "En cours chez l'enquêteur"]
    a = a[a["interview__id"].isin(m["interview__id"])]
    version = m.set_index("interview__id")["version"]

    # Évolution journalière
    debut = a[a["action"] == 2].groupby("interview__id")["jour"].min()
    envois = a[a["action"] == 3]
    premier_envoi = envois.groupby("interview__id")["jour"].min()
    jours = pd.date_range(a["jour"].min(), a["jour"].max(), freq="D")
    j = pd.DataFrame(index=jours)
    j["Entretiens démarrés"] = debut.value_counts()
    for v in sorted(m["version"].unique(), key=lambda x: int(x[1:])):
        j[f"dont {v}"] = debut[debut.index.map(version) == v].value_counts()
    j["Premiers envois au superviseur"] = premier_envoi.value_counts()
    j["Renvois après correction"] = envois.groupby("jour").size() - j["Premiers envois au superviseur"].fillna(0)
    j["Rejets"] = a[a["action"].isin([7, 8])].groupby("jour").size()
    j["Approbations"] = a[a["action"].isin([5, 6])].groupby("jour").size()
    j["Enquêteurs actifs"] = a[(a["role"] == 1)].groupby("jour")["originator"].nunique()
    j = j.fillna(0)
    j["Cumul démarrés"] = j["Entretiens démarrés"].cumsum()
    j["Cumul envoyés"] = j["Premiers envois au superviseur"].cumsum()
    j.insert(0, "Date", j.index)
    j = j.reset_index(drop=True)

    # Région
    def agrege(cle, libelle_cle):
        g = m.groupby(cle)
        r = pd.DataFrame({"Entretiens": g.size()})
        for gr in groupes:
            r[gr] = m[m["groupe"] == gr].groupby(cle).size()
        r = r.fillna(0)
        r["% complétés"] = r["Complété / approuvé"] / r["Entretiens"]
        r["Enquêteurs"] = g["enqueteur"].nunique()
        r["Durée médiane (min)"] = m[m["statut_code"].isin(STATUTS_SOUMIS + STATUTS_REJETES)].groupby(cle)["duree_min"].median()
        rej = det[det["niveau"] == "Rejet"].groupby(det[cle] if cle in det else det["region"])["interview__id"].nunique()
        r["Entretiens avec incohérence bloquante"] = rej
        r = r.fillna({"Entretiens avec incohérence bloquante": 0})
        return r.reset_index().rename(columns={cle: libelle_cle})

    m["region"] = m["region"].replace("", "(non renseignée)")
    m["departement"] = m["departement"].replace("", "(non renseigné)")
    det = det.copy()
    det["region"] = det["region"].replace("", "(non renseignée)")
    det["departement"] = det["departement"].replace("", "(non renseigné)")
    reg = agrege("region", "Région").sort_values("Entretiens", ascending=False)
    dep = (m.groupby(["region", "departement"]).agg(
        Entretiens=("interview__id", "size"),
        Completes=("groupe", lambda s: (s == groupes[0]).sum()),
        Rejetes=("groupe", lambda s: (s == groupes[1]).sum()),
        En_cours=("groupe", lambda s: (s == groupes[2]).sum()),
        Enqueteurs=("enqueteur", "nunique")).reset_index())
    dep.columns = ["Région", "Département", "Entretiens", groupes[0], groupes[1], groupes[2], "Enquêteurs"]
    dep["% complétés"] = dep[groupes[0]] / dep["Entretiens"]
    dep = dep.sort_values(["Région", "Entretiens"], ascending=[True, False])

    # Production déclarée (entretiens complétés/approuvés, hors lignes en incohérence bloquante)
    valides = m.loc[m["statut_code"].isin(STATUTS_SOUMIS), ["interview__id", "region"]]
    douteuses = det[det["code"].isin(["R16", "R17", "R18", "R19", "R21", "R22"])]
    douteuses = set(zip(douteuses["interview__id"], douteuses["ligne"]))
    p25 = pr[pr["annee"] == "2025"].merge(valides, on="interview__id")
    p25 = p25[[(i, l) not in douteuses for i, l in zip(p25["interview__id"], p25["lib"])]]
    pp = p25.groupby("region").agg(
        Exploitations=("interview__id", "nunique"), Ruches_detenues=("C_1_1", "sum"),
        Ruches_utilisees=("C_1_2", "sum"), Miel_L=("C_1_3", "sum"), Cire_kg=("C_1_4", "sum"),
        Miel_vendu_L=("C_1_6", "sum"), Ventes_miel=("C_1_6bis", "sum"),
        Rdt=("rdt", "median"), Prix=("prix_miel", "median")).reset_index()
    pp.columns = ["Région", "Exploitations productrices", "Ruches détenues", "Ruches utilisées",
                  "Miel produit (L)", "Cire produite (kg)", "Miel vendu (L)", "Ventes de miel (FCFA)",
                  "Rendement médian (L/ruche utilisée)", "Prix médian du miel (FCFA/L)"]
    pp = pp.sort_values("Ruches détenues", ascending=False)

    # Enquêteurs
    g = m.groupby("enqueteur")
    e = pd.DataFrame({"Entretiens": g.size()})
    for gr in groupes:
        e[gr] = m[m["groupe"] == gr].groupby("enqueteur").size()
    e = e.fillna(0)
    e["% complétés"] = e[groupes[0]] / e["Entretiens"]
    e["Entretiens rejetés au moins une fois"] = g["nb_rejets"].apply(lambda s: (s > 0).sum())
    envoyes_ = m[m["date_1er_envoi"].notna()].groupby("enqueteur").size()
    e["Taux de rejet"] = e["Entretiens rejetés au moins une fois"] / envoyes_.reindex(e.index).replace(0, np.nan)
    e["Durée médiane (min)"] = m[m["statut_code"].isin(STATUTS_SOUMIS + STATUTS_REJETES)].groupby("enqueteur")["duree_min"].median()
    act = a[a["role"] == 1].groupby("originator")
    e["Jours actifs"] = act["jour"].nunique()
    e["Entretiens par jour actif"] = e["Entretiens"] / e["Jours actifs"]
    e["Première activité"] = act["jour"].min()
    e["Dernière activité"] = act["jour"].max()
    e["Région principale"] = e.index.map(lambda x: dict(zip(m["A_1"], m["region"])).get(fiables.get(x), ""))
    e["Incohérences bloquantes"] = det[det["niveau"] == "Rejet"].groupby("enqueteur").size()
    e["Incohérences à vérifier"] = det[det["niveau"] == "À vérifier"].groupby("enqueteur").size()
    e = e.fillna({"Incohérences bloquantes": 0, "Incohérences à vérifier": 0})
    e = e.reset_index().rename(columns={"enqueteur": "Enquêteur"}).sort_values("Entretiens", ascending=False)
    e.insert(1, "Région principale", e.pop("Région principale"))

    # Matrice enquêteur x jour (entretiens démarrés)
    dj = debut.rename("jour").reset_index()
    dj["enqueteur"] = dj["interview__id"].map(m.set_index("interview__id")["enqueteur"])
    mat = dj.pivot_table(index="enqueteur", columns="jour", values="interview__id", aggfunc="count", fill_value=0)
    mat.columns = [f"{c:%d/%m}" for c in mat.columns]
    mat["Total"] = mat.sum(axis=1)
    mat = mat.sort_values("Total", ascending=False).reset_index().rename(columns={"enqueteur": "Enquêteur"})

    # Durées
    tranches = [0, 10, 20, 30, 45, 60, 90, 1e9]
    noms = ["< 10 min", "10-20 min", "20-30 min", "30-45 min", "45-60 min", "60-90 min", "> 90 min"]
    dd = m[m["statut_code"].isin(STATUTS_SOUMIS + STATUTS_REJETES)]
    du = pd.cut(dd["duree_min"], tranches, labels=noms, right=False).value_counts().reindex(noms).fillna(0)
    du = du.rename("Entretiens").reset_index().rename(columns={"duree_min": "Durée", "index": "Durée"})
    du.columns = ["Durée", "Entretiens"]
    du["%"] = du["Entretiens"] / du["Entretiens"].sum()

    # Statuts par version
    sv = pd.crosstab(m["statut"], m["version"], margins=True, margins_name="Total").reset_index()
    sv = sv.rename(columns={"statut": "Statut"})

    return dict(jour=j, region=reg, departement=dep, production=pp, enqueteurs=e, matrice=mat,
                durees=du, statuts=sv)


# --------------------------------------------------------------------------------------------
# Mise en forme Excel
# --------------------------------------------------------------------------------------------
POLICE = "Arial"
BLEU, ORANGE, AQUA = "2A78D6", "EB6834", "1BAF7A"
F_TITRE = Font(name=POLICE, size=14, bold=True, color="0D366B")
F_SOUS = Font(name=POLICE, size=9, italic=True, color="52514E")
F_ENTETE = Font(name=POLICE, size=10, bold=True, color="FFFFFF")
F_NORMAL = Font(name=POLICE, size=10)
F_GRAS = Font(name=POLICE, size=10, bold=True)
FOND_ENTETE = PatternFill("solid", fgColor="1C5CAB")
FOND_REJET = PatternFill("solid", fgColor="F8D3D3")
FOND_VERIF = PatternFill("solid", fgColor="FDEBC4")
FOND_SAISIE = PatternFill("solid", fgColor="FFFF00")
TRAIT = Border(bottom=Side(style="thin", color="D9D9D9"))


def valeur_excel(v):
    if v is None or (isinstance(v, float) and np.isnan(v)) or v is pd.NaT:
        return None
    if isinstance(v, pd.Timestamp):
        return v.to_pydatetime()
    if isinstance(v, np.generic):
        return v.item()
    return v


def titre(ws, texte, sous_titre=""):
    ws["A1"] = texte
    ws["A1"].font = F_TITRE
    if sous_titre:
        ws["A2"] = sous_titre
        ws["A2"].font = F_SOUS


def ecrire_table(ws, df, ligne=4, col=1, formats=None, filtre=True, figer=True, largeurs=None):
    formats = formats or {}
    for j, nom in enumerate(df.columns):
        c = ws.cell(row=ligne, column=col + j, value=str(nom))
        c.font, c.fill = F_ENTETE, FOND_ENTETE
        c.alignment = Alignment(wrap_text=True, vertical="center")
    for i, row in enumerate(df.itertuples(index=False), start=1):
        for j, v in enumerate(row):
            c = ws.cell(row=ligne + i, column=col + j, value=valeur_excel(v))
            c.font, c.border = F_NORMAL, TRAIT
            nom = df.columns[j]
            if nom in formats:
                c.number_format = formats[nom]
            elif isinstance(c.value, datetime):
                c.number_format = "dd/mm/yyyy"
            elif isinstance(c.value, float):
                c.number_format = "#,##0.0"
            elif isinstance(c.value, int):
                c.number_format = "#,##0"
    fin = ligne + len(df)
    ws.row_dimensions[ligne].height = 30
    if filtre and len(df):
        ws.auto_filter.ref = f"{get_column_letter(col)}{ligne}:{get_column_letter(col + len(df.columns) - 1)}{fin}"
    if figer:
        ws.freeze_panes = ws.cell(row=ligne + 1, column=col)
    for j, nom in enumerate(df.columns):
        lettre = get_column_letter(col + j)
        if largeurs and nom in largeurs:
            w = largeurs[nom]
        else:
            vals = [len(str(nom)) * 0.8] + [len(str(x)) for x in df.iloc[:200, j] if pd.notna(x)]
            w = min(max(10, max(vals) + 2), 45)
        ws.column_dimensions[lettre].width = max(ws.column_dimensions[lettre].width or 0, w)
    return ligne, fin


def colorer_niveau(ws, ligne_entete, fin, col_niveau, nb_col):
    for r in range(ligne_entete + 1, fin + 1):
        v = ws.cell(row=r, column=col_niveau).value
        fond = FOND_REJET if v == "Rejet" else FOND_VERIF if v == "À vérifier" else None
        if fond:
            ws.cell(row=r, column=col_niveau).fill = fond


def serie_style(s, couleur, ligne=False):
    if ligne:
        s.graphicalProperties.line.solidFill = couleur
        s.graphicalProperties.line.width = 22000
        s.smooth = False
        s.marker.symbol = "none"
    else:
        s.graphicalProperties.solidFill = couleur
        s.graphicalProperties.line.solidFill = "FFFFFF"


def classeur_suivi(tab, m, det, dossiers, chemin):
    wb = Workbook()
    ws = wb.active
    ws.title = "Synthèse"
    arret = m["derniere_action"].max()
    titre(ws, "Suivi de la collecte - Enquête complémentaire et productivité des ruches d'abeilles",
          f"Données arrêtées au {arret:%d/%m/%Y %H:%M} (dernière synchronisation) - exports : "
          f"{', '.join(dossiers)} - généré le {datetime.now():%d/%m/%Y %H:%M}")
    n = len(m)
    soumis = m["statut_code"].isin(STATUTS_SOUMIS).sum()
    rej_auj = (m["statut_code"].isin(STATUTS_REJETES)).sum()
    bloq = det[(det["niveau"] == "Rejet") & det["statut_code"].isin(STATUTS_SOUMIS)]["interview__id"].nunique()
    jours = tab["jour"]
    der7 = jours.tail(7)
    kpi = pd.DataFrame([
        ("Entretiens au total (toutes versions)", n, "#,##0"),
        ("Complétés ou approuvés", soumis, "#,##0"),
        ("Taux de complétion", soumis / n if n else 0, "0.0%"),
        ("Rejetés, en cours de correction", rej_auj, "#,##0"),
        ("En cours chez l'enquêteur (non envoyés)", (~m["statut_code"].isin(STATUTS_SOUMIS + STATUTS_REJETES)).sum(), "#,##0"),
        ("Complétés/approuvés avec incohérence bloquante (à rejeter)", bloq, "#,##0"),
        ("Enquêteurs ayant travaillé", m["enqueteur"].nunique(), "#,##0"),
        ("Jours de collecte", len(jours), "#,##0"),
        ("Entretiens démarrés par jour (moyenne des 7 derniers jours)", der7["Entretiens démarrés"].mean(), "#,##0.0"),
        ("Durée médiane d'un entretien envoyé (min)",
         m.loc[m["statut_code"].isin(STATUTS_SOUMIS + STATUTS_REJETES), "duree_min"].median(), "#,##0"),
        ("Régions couvertes", m.loc[m["region"] != "", "region"].nunique(), "#,##0"),
    ], columns=["Indicateur", "Valeur", "fmt"])
    ecrire_table(ws, kpi[["Indicateur", "Valeur"]], ligne=4, filtre=False, figer=False,
                 largeurs={"Indicateur": 62, "Valeur": 14})
    for i, f in enumerate(kpi["fmt"], start=5):
        ws.cell(row=i, column=2).number_format = f
        ws.cell(row=i, column=2).font = F_GRAS
    r0 = 5 + len(kpi) + 2
    ws.cell(row=r0 - 1, column=1, value="Statut des entretiens par version du questionnaire").font = F_GRAS
    ecrire_table(ws, tab["statuts"], ligne=r0, filtre=False, figer=False)
    r1 = r0 + len(tab["statuts"]) + 3
    ws.cell(row=r1 - 1, column=1, value="Répartition des durées d'entretien (entretiens envoyés)").font = F_GRAS
    ecrire_table(ws, tab["durees"], ligne=r1, filtre=False, figer=False, formats={"%": "0.0%"})
    ch = BarChart()
    ch.type, ch.title, ch.legend = "col", "Durée des entretiens envoyés", None
    ch.y_axis.title = "Entretiens"
    ch.add_data(Reference(ws, min_col=2, min_row=r1, max_row=r1 + len(tab["durees"])), titles_from_data=True)
    ch.set_categories(Reference(ws, min_col=1, min_row=r1 + 1, max_row=r1 + len(tab["durees"])))
    serie_style(ch.series[0], BLEU)
    ch.gapWidth, ch.height, ch.width = 60, 7, 14
    ws.add_chart(ch, f"D{r1}")

    # Évolution journalière
    ws = wb.create_sheet("Évolution journalière")
    titre(ws, "Évolution journalière de la collecte",
          "Dates issues des paradonnées du serveur (début = première réponse saisie ; envoi = statut Complété)")
    j = tab["jour"]
    _, fin = ecrire_table(ws, j, formats={"Date": "ddd dd/mm/yyyy"})
    c_cum1 = list(j.columns).index("Cumul démarrés") + 1
    c_cum2 = list(j.columns).index("Cumul envoyés") + 1
    lc = LineChart()
    lc.title, lc.height, lc.width = "Entretiens cumulés", 8, 18
    lc.y_axis.title = "Entretiens"
    for col, coul in [(c_cum1, BLEU), (c_cum2, ORANGE)]:
        lc.add_data(Reference(ws, min_col=col, min_row=4, max_row=fin), titles_from_data=True)
        serie_style(lc.series[-1], coul, ligne=True)
    lc.set_categories(Reference(ws, min_col=1, min_row=5, max_row=fin))
    lc.x_axis.number_format = "dd/mm"
    lc.legend.position = "b"
    ws.add_chart(lc, f"B{fin + 3}")
    bc = BarChart()
    bc.type, bc.title, bc.height, bc.width = "col", "Entretiens démarrés par jour", 8, 18
    bc.legend = None
    bc.add_data(Reference(ws, min_col=2, min_row=4, max_row=fin), titles_from_data=True)
    bc.set_categories(Reference(ws, min_col=1, min_row=5, max_row=fin))
    bc.x_axis.number_format = "dd/mm"
    serie_style(bc.series[0], BLEU)
    bc.gapWidth = 50
    ws.add_chart(bc, f"I{fin + 3}")

    # Région / département
    ws = wb.create_sheet("Par région")
    titre(ws, "Avancement par région", "Région déclarée en A.1")
    reg = tab["region"]
    _, fin = ecrire_table(ws, reg, formats={"% complétés": "0.0%", "Durée médiane (min)": "#,##0"})
    sc = BarChart()
    sc.type, sc.grouping, sc.overlap = "bar", "stacked", 100
    sc.title, sc.height, sc.width = "Entretiens par région et statut", 9, 18
    for k, coul in zip(range(3, 6), [BLEU, ORANGE, AQUA]):
        sc.add_data(Reference(ws, min_col=k, min_row=4, max_row=fin), titles_from_data=True)
        serie_style(sc.series[-1], coul)
    sc.set_categories(Reference(ws, min_col=1, min_row=5, max_row=fin))
    sc.x_axis.scaling.orientation = "maxMin"
    sc.legend.position = "b"
    sc.gapWidth = 40
    ws.add_chart(sc, f"B{fin + 3}")

    ws = wb.create_sheet("Par département")
    titre(ws, "Avancement par département")
    ecrire_table(ws, tab["departement"], formats={"% complétés": "0.0%"})

    ws = wb.create_sheet("Production déclarée")
    titre(ws, "Production apicole déclarée en 2025 (premiers résultats bruts, non apurés)",
          "Entretiens complétés ou approuvés ; lignes en incohérence bloquante (R16 à R22) exclues ; "
          "rendement et prix = médiane par type de ruche")
    ecrire_table(ws, tab["production"], formats={"Rendement médian (L/ruche utilisée)": "#,##0.0",
                                                 "Prix médian du miel (FCFA/L)": "#,##0",
                                                 "Ventes de miel (FCFA)": "#,##0", "Miel produit (L)": "#,##0",
                                                 "Cire produite (kg)": "#,##0", "Miel vendu (L)": "#,##0"})

    ws = wb.create_sheet("Par enquêteur")
    titre(ws, "Performance des enquêteurs",
          "Taux de rejet = entretiens rejetés au moins une fois / entretiens envoyés ; "
          "incohérences = résultat des contrôles du classeur Incohérences")
    e = tab["enqueteurs"]
    _, fin = ecrire_table(ws, e, formats={"% complétés": "0.0%", "Taux de rejet": "0.0%",
                                          "Durée médiane (min)": "#,##0", "Entretiens par jour actif": "#,##0.0"})
    for nom, regle in [("Taux de rejet", ColorScaleRule(start_type="num", start_value=0, start_color="FFFFFF",
                                                         end_type="max", end_color="F4A3A3")),
                       ("% complétés", ColorScaleRule(start_type="num", start_value=0, start_color="FFFFFF",
                                                      end_type="num", end_value=1, end_color="9EC5F4"))]:
        lettre = get_column_letter(list(e.columns).index(nom) + 1)
        ws.conditional_formatting.add(f"{lettre}5:{lettre}{fin}", regle)

    ws = wb.create_sheet("Enquêteur x jour")
    titre(ws, "Entretiens démarrés par enquêteur et par jour")
    mat = tab["matrice"]
    _, fin = ecrire_table(ws, mat, largeurs={c: 7 for c in mat.columns if c not in ("Enquêteur",)})
    ws.freeze_panes = "B5"
    ws.conditional_formatting.add(
        f"B5:{get_column_letter(len(mat.columns) - 1)}{fin}",
        ColorScaleRule(start_type="num", start_value=0, start_color="FFFFFF", end_type="max", end_color="5598E7"))

    for w in wb.worksheets:
        w.sheet_view.showGridLines = False
    wb.save(chemin)


def classeur_incoherences(det, m, chemin, chemin_csv):
    det = det.copy()
    det["date_debut"] = det["date_debut"].dt.normalize()
    soumis = det[det["statut_code"].isin(STATUTS_SOUMIS)]

    def par_entretien(d):
        rows = []
        for iid, g in d.groupby("interview__id", sort=False):
            r0 = g.iloc[0]
            bloq = g[g["niveau"] == "Rejet"]
            verif = g[g["niveau"] == "À vérifier"]
            parts = []
            if len(bloq):
                parts.append("À CORRIGER : " + " | ".join(f"{i}) {x}" for i, x in enumerate(bloq["message"], 1)))
            if len(verif):
                parts.append("À VÉRIFIER : " + " | ".join(verif["message"]))
            texte = " // ".join(parts)
            if len(texte) > 1500:
                texte = texte[:1490] + " (...)"
            rows.append({
                "interview__key": r0["interview__key"], "interview__id": iid, "Version": r0["version"],
                "Statut actuel": r0["statut"], "Action dans Survey Solutions": ACTION_REJET.get(r0["statut_code"], ""),
                "Enquêteur": r0["enqueteur"], "Région": r0["region"], "Département": r0["departement"],
                "Commune": r0["commune"], "Date de début": r0["date_debut"],
                "Nb incohérences bloquantes": len(bloq), "Nb points à vérifier": len(verif),
                "Contrôles déclenchés": ", ".join(sorted(g["code"].unique())),
                "Commentaire de rejet (à coller)": texte})
        cols = ["interview__key", "interview__id", "Version", "Statut actuel", "Action dans Survey Solutions",
                "Enquêteur", "Région", "Département", "Commune", "Date de début", "Nb incohérences bloquantes",
                "Nb points à vérifier", "Contrôles déclenchés", "Commentaire de rejet (à coller)"]
        out = pd.DataFrame(rows, columns=cols)
        return out.sort_values(["Enquêteur", "interview__key"]) if len(out) else out

    ids_bloq = soumis.loc[soumis["niveau"] == "Rejet", "interview__id"].unique()
    a_rejeter = par_entretien(soumis[soumis["interview__id"].isin(ids_bloq)])
    a_verifier = par_entretien(soumis[~soumis["interview__id"].isin(ids_bloq)])
    if len(a_verifier):
        a_verifier.insert(5, "Décision superviseur", "")
        a_verifier["Action dans Survey Solutions"] = ""
    chez_enq = par_entretien(det[~det["statut_code"].isin(STATUTS_SOUMIS)])
    if len(chez_enq):
        chez_enq = chez_enq.drop(columns=["Action dans Survey Solutions"])

    wb = Workbook()
    ws = wb.active
    ws.title = "À rejeter"
    titre(ws, f"Entretiens à rejeter : {len(a_rejeter)} (complétés/approuvés avec au moins une incohérence bloquante)",
          "Dans Survey Solutions > Entretiens : rechercher la clé (interview__key), ouvrir, « Rejeter » "
          "et coller le commentaire. Le fichier CSV joint permet un rejet en masse par l'API.")
    largeurs = {"Commentaire de rejet (à coller)": 90, "interview__id": 34, "Contrôles déclenchés": 18}
    _, fin = ecrire_table(ws, a_rejeter, largeurs=largeurs)
    col_com = len(a_rejeter.columns)
    for r in range(5, fin + 1):
        ws.cell(row=r, column=col_com).alignment = Alignment(wrap_text=True, vertical="top")
        ws.cell(row=r, column=1).font = F_GRAS
        ws.cell(row=r, column=11).fill = FOND_REJET

    ws = wb.create_sheet("À vérifier")
    titre(ws, f"Entretiens à vérifier : {len(a_verifier)} (aucune incohérence bloquante, mais des valeurs atypiques)",
          "Le superviseur décide dans la colonne jaune : « Rejeter » ou « Valider » (après appel à l'enquêteur si besoin).")
    _, fin = ecrire_table(ws, a_verifier, largeurs={**largeurs, "Décision superviseur": 16})
    if len(a_verifier):
        dv = DataValidation(type="list", formula1='"Rejeter,Valider"', allow_blank=True)
        ws.add_data_validation(dv)
        dv.add(f"F5:F{fin}")
        for r in range(5, fin + 1):
            ws.cell(row=r, column=6).fill = FOND_SAISIE
            ws.cell(row=r, column=len(a_verifier.columns)).alignment = Alignment(wrap_text=True, vertical="top")

    ws = wb.create_sheet("Chez l'enquêteur")
    titre(ws, f"Entretiens non encore envoyés ou déjà rejetés, présentant des incohérences : {len(chez_enq)}",
          "Pour information : à signaler aux enquêteurs avant leur prochain envoi (aucune action de rejet possible).")
    _, fin = ecrire_table(ws, chez_enq, largeurs=largeurs)
    for r in range(5, fin + 1):
        ws.cell(row=r, column=len(chez_enq.columns)).alignment = Alignment(wrap_text=True, vertical="top")

    ws = wb.create_sheet("Détail des incohérences")
    titre(ws, f"Détail : {len(det)} incohérences sur {det['interview__id'].nunique()} entretiens (tous statuts)")
    d = det[["interview__key", "interview__id", "version", "statut", "enqueteur", "region", "departement",
             "commune", "code", "niveau", "section", "controle", "variable", "ligne", "valeur", "message"]]
    d.columns = ["interview__key", "interview__id", "Version", "Statut", "Enquêteur", "Région", "Département",
                 "Commune", "Code", "Niveau", "Section", "Contrôle", "Variable", "Ligne / rubrique", "Valeur",
                 "Message"]
    _, fin = ecrire_table(ws, d, largeurs={"Message": 80, "Contrôle": 45, "interview__id": 34})
    colorer_niveau(ws, 4, fin, 10, len(d.columns))

    ws = wb.create_sheet("Synthèse par contrôle")
    titre(ws, "Nombre d'entretiens concernés par contrôle")
    s = det.groupby("code").agg(entretiens=("interview__id", "nunique"), cas=("interview__id", "size"),
                                soumis=("statut_code", lambda x: x.isin(STATUTS_SOUMIS).sum()))
    regles = pd.DataFrame([(k, *v) for k, v in REGLES.items()], columns=["Code", "Niveau", "Section", "Contrôle", "Seuil"])
    regles["Entretiens concernés"] = regles["Code"].map(s["entretiens"]).fillna(0).astype(int)
    regles["Cas détectés"] = regles["Code"].map(s["cas"]).fillna(0).astype(int)
    regles["dont entretiens complétés/approuvés (cas)"] = regles["Code"].map(s["soumis"]).fillna(0).astype(int)
    _, fin = ecrire_table(ws, regles, largeurs={"Contrôle": 60, "Seuil": 45})
    colorer_niveau(ws, 4, fin, 2, len(regles.columns))
    ws.cell(row=fin + 2, column=1, value="Les seuils sont des paramètres modifiables en tête du programme "
                                          "programmes/suivi_collecte.py. « Rejet » = incohérence bloquante ; "
                                          "« À vérifier » = valeur atypique à confirmer.").font = F_SOUS

    ws = wb.create_sheet("Par enquêteur")
    titre(ws, "Incohérences par enquêteur et par contrôle (tous statuts)")
    pv = det.pivot_table(index="enqueteur", columns="code", values="interview__id", aggfunc="count", fill_value=0)
    pv.insert(0, "Total bloquantes", det[det["niveau"] == "Rejet"].groupby("enqueteur").size().reindex(pv.index).fillna(0))
    pv.insert(1, "Total à vérifier", det[det["niveau"] == "À vérifier"].groupby("enqueteur").size().reindex(pv.index).fillna(0))
    pv.insert(2, "Entretiens", m.groupby("enqueteur").size().reindex(pv.index).fillna(0))
    pv = pv.sort_values("Total bloquantes", ascending=False).reset_index().rename(columns={"enqueteur": "Enquêteur"})
    _, fin = ecrire_table(ws, pv, largeurs={c: 7 for c in pv.columns if str(c).startswith("R")})
    ws.conditional_formatting.add(f"B5:B{fin}", ColorScaleRule(start_type="num", start_value=0, start_color="FFFFFF",
                                                              end_type="max", end_color="F4A3A3"))

    for w in wb.worksheets:
        w.sheet_view.showGridLines = False
    wb.save(chemin)

    a_rejeter[["interview__id", "interview__key", "Enquêteur", "Commentaire de rejet (à coller)"]].rename(
        columns={"Commentaire de rejet (à coller)": "commentaire"}).to_csv(chemin_csv, index=False, sep=";",
                                                                         encoding="utf-8-sig")
    return len(a_rejeter), len(a_verifier), len(chez_enq)


def main():
    tables, labels, dossiers = lire_exports()
    m, actions = preparer(tables, labels)
    det, pr, fiables = controler(tables, m)
    tab = tableaux_suivi(m, actions, det, pr, labels, fiables)
    SORTIES.mkdir(exist_ok=True)
    jour = f"{datetime.now():%Y-%m-%d}"
    f1 = SORTIES / f"Suivi_collecte_{jour}.xlsx"
    f2 = SORTIES / f"Incoherences_a_rejeter_{jour}.xlsx"
    f3 = SORTIES / f"Liste_rejet_API_{jour}.csv"
    try:
        classeur_suivi(tab, m, det, dossiers, f1)
        n_rej, n_ver, n_enq = classeur_incoherences(det, m, f2, f3)
    except PermissionError as err:
        sys.exit(f"Impossible d'écrire {err.filename} : fermez le fichier dans Excel puis relancez.")
    print(f"Exports lus : {', '.join(dossiers)} ({len(m)} entretiens)")
    print(f"Incohérences détectées : {len(det)} sur {det['interview__id'].nunique()} entretiens")
    print(f"  -> à rejeter : {n_rej} | à vérifier : {n_ver} | chez l'enquêteur : {n_enq}")
    print(f"Fichiers produits :\n  {f1}\n  {f2}\n  {f3}")


if __name__ == "__main__":
    main()
