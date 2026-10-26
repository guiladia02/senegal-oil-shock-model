
# ============================================================
# PROJET : MODELISATION MACROECONOMIQUE DU SENEGAL
# Sujet : Exposition aux produits petroliers et simulation
#         d'un choc de prix a partir des MCS 2014-2022
# ============================================================
#
# OBJECTIFS
# 1. Importer et nettoyer les MCS annuelles.
# 2. Mesurer l'exposition des activites au produit I7.
# 3. Simuler des chocs de prix de +10 % et +25 %.
# 4. Etudier une propagation indicative des couts.
# 5. Produire des tableaux et graphiques exportables.
#
# ATTENTION :
# Les effets indirects reposent sur un modele lineaire
# simplifie. Ils ne sont pas des variations de prix observees.
# ============================================================


# ------------------------------------------------------------
# 0. PREPARATION
# ------------------------------------------------------------

# Installer les packages une seule fois si necessaire :
# install.packages(c("readxl", "ggplot2"))

library(readxl)
library(ggplot2)

# Le fichier Excel doit etre dans le repertoire de travail.
# Verifier le dossier courant avec getwd().
fichier <- "MCS_Senegal_2014-2022_vf.xlsx"

if (!file.exists(fichier)) {
  stop(
    "Fichier introuvable. Placez le fichier Excel dans ",
    "le repertoire de travail ou adaptez le chemin."
  )
}

annees <- 2014:2022

# Dossier pour sauvegarder les resultats
dossier_sortie <- "resultats_projet"
dir.create(dossier_sortie, showWarnings = FALSE)


# ------------------------------------------------------------
# 1. IMPORTATION DES MATRICES
# ------------------------------------------------------------

# Les codes des comptes sont situes :
# - sur la ligne 6, a partir de la colonne C ;
# - dans la colonne B, a partir de la ligne 7.
#
# La fonction recupere les codes et les valeurs numeriques,
# puis construit une matrice identifiee par ses comptes.

lire_mcs <- function(fichier, annee) {
  
  brut <- read_excel(
    path = fichier,
    sheet = as.character(annee),
    col_names = FALSE
  )
  
  codes_colonnes <- as.character(
    unlist(brut[6, 3:ncol(brut)], use.names = FALSE)
  )
  
  codes_lignes <- as.character(
    unlist(brut[7:nrow(brut), 2], use.names = FALSE)
  )
  
  lignes_valides <- which(
    !is.na(codes_lignes) & trimws(codes_lignes) != ""
  )
  
  colonnes_valides <- which(
    !is.na(codes_colonnes) & trimws(codes_colonnes) != ""
  )
  
  codes_lignes <- trimws(codes_lignes[lignes_valides])
  codes_colonnes <- trimws(codes_colonnes[colonnes_valides])
  
  # Correspondance entre les indices du tableau importe
  # et les positions dans la feuille Excel.
  indices_lignes_excel <- 6 + lignes_valides
  indices_colonnes_excel <- 2 + colonnes_valides
  
  valeurs <- brut[
    indices_lignes_excel,
    indices_colonnes_excel,
    drop = FALSE
  ]
  
  # Conversion des nombres : virgules decimales, espaces,
  # espaces inseparables et cellules vides.
  convertir_nombre <- function(x) {
    
    x <- trimws(as.character(x))
    x[x == ""] <- NA_character_
    
    x <- gsub("\u00A0", "", x, fixed = TRUE)
    x <- gsub(" ", "", x, fixed = TRUE)
    x <- gsub(",", ".", x, fixed = TRUE)
    
    suppressWarnings(as.numeric(x))
  }
  
  valeurs <- as.data.frame(
    lapply(valeurs, convertir_nombre),
    check.names = FALSE
  )
  
  if (nrow(valeurs) != length(codes_lignes)) {
    stop("Incoherence dans les lignes de la MCS ", annee)
  }
  
  if (ncol(valeurs) != length(codes_colonnes)) {
    stop("Incoherence dans les colonnes de la MCS ", annee)
  }
  
  if (anyDuplicated(codes_lignes)) {
    stop("Codes de lignes dupliques en ", annee)
  }
  
  if (anyDuplicated(codes_colonnes)) {
    stop("Codes de colonnes dupliques en ", annee)
  }
  
  rownames(valeurs) <- codes_lignes
  colnames(valeurs) <- codes_colonnes
  
  valeurs
}


# Importer toutes les annees
mcs_liste <- setNames(
  lapply(annees, function(a) lire_mcs(fichier, a)),
  as.character(annees)
)

# Remplacer les cellules numeriques manquantes par zero.
# Hypothese a verifier : une cellule vide signifie bien
# une absence de flux et non une donnee indisponible.
mcs_liste_propre <- lapply(mcs_liste, function(m) {
  m[] <- lapply(m, function(x) {
    x[is.na(x)] <- 0
    x
  })
  m
})

cat("\nIMPORTATION TERMINEE\n")
cat("Annees :", paste(annees, collapse = ", "), "\n")
cat("Dimensions de la MCS 2022 :\n")
print(dim(mcs_liste_propre[["2022"]]))


# ------------------------------------------------------------
# 2. ANALYSE DE LA MCS 2022
# ------------------------------------------------------------

# I1-I28 : comptes de produits
# J1-J28 : comptes d'activites
#
# On extrait les consommations intermediaires :
# lignes = produits ; colonnes = activites.

m <- as.matrix(mcs_liste_propre[["2022"]])
storage.mode(m) <- "numeric"

produits <- paste0("I", 1:28)
activites <- paste0("J", 1:28)

if (!all(produits %in% rownames(m))) {
  stop("Certains comptes I1-I28 sont absents de la MCS 2022.")
}

if (!all(activites %in% colnames(m))) {
  stop("Certains comptes J1-J28 sont absents de la MCS 2022.")
}

# Matrice des consommations intermediaires
Z <- m[produits, activites, drop = FALSE]

# Production des activites : somme des flux de chaque compte
# d'activite sur ses colonnes, hors colonne TOTAL.
#
# Cette convention reprend celle utilisee dans nos calculs
# precedents ; elle doit etre confirmee avec la documentation
# comptable de la MCS.

colonnes_hors_total <- setdiff(colnames(m), "TOTAL")

if (!all(activites %in% rownames(m))) {
  stop("Comptes d'activites absents des lignes de la MCS.")
}

production <- rowSums(
  m[activites, colonnes_hors_total, drop = FALSE],
  na.rm = TRUE
)

names(production) <- activites
production <- production[activites]

# Achats intermediaires de I7 par activite
achats_I7 <- as.numeric(Z["I7", ])
names(achats_I7) <- activites

# Indicateur d'exposition : achats I7 / production
exposition_2022 <- data.frame(
  Activite = activites,
  Production = as.numeric(production),
  Achats_I7 = as.numeric(achats_I7),
  Exposition_pct = ifelse(
    production > 0,
    100 * achats_I7 / production,
    NA_real_
  )
)

exposition_2022 <- exposition_2022[
  order(exposition_2022$Exposition_pct, decreasing = TRUE),
]

cat("\nTOP 5 DES ACTIVITES EXPOSEES EN 2022\n")
print(head(exposition_2022, 5), row.names = FALSE)


# ------------------------------------------------------------
# 3. EVOLUTION DE L'EXPOSITION, 2014-2022
# ------------------------------------------------------------

# Calcul identique pour chaque MCS annuelle.

calculer_exposition <- function(matrice) {
  
  activites <- paste0("J", 1:28)
  produits <- paste0("I", 1:28)
  
  if (!all(activites %in% rownames(matrice)) ||
      !all(activites %in% colnames(matrice)) ||
      !all(produits %in% rownames(matrice))) {
    stop("Comptes necessaires absents d'une MCS.")
  }
  
  Z <- matrice[produits, activites, drop = FALSE]
  
  colonnes <- setdiff(colnames(matrice), "TOTAL")
  
  production <- rowSums(
    matrice[activites, colonnes, drop = FALSE],
    na.rm = TRUE
  )
  
  names(production) <- activites
  production <- production[activites]
  
  achats <- as.numeric(Z["I7", ])
  names(achats) <- activites
  
  data.frame(
    Activite = activites,
    Production = as.numeric(production),
    Achats_I7 = as.numeric(achats),
    Exposition_pct = ifelse(
      production > 0,
      100 * achats / production,
      NA_real_
    )
  )
}

exposition_annuelle <- do.call(
  rbind,
  lapply(annees, function(a) {
    
    donnees <- calculer_exposition(
      as.matrix(mcs_liste_propre[[as.character(a)]])
    )
    
    donnees$Annee <- a
    donnees[, c(
      "Annee", "Activite", "Production",
      "Achats_I7", "Exposition_pct"
    )]
  })
)

rownames(exposition_annuelle) <- NULL

# Activites selectionnees pour le graphique
secteurs <- c("J11", "J15", "J4", "J9", "J13")

libelles <- c(
  J11 = "Electricite, gaz et energie",
  J15 = "Transport",
  J4 = "Peche",
  J9 = "Ciment et materiaux",
  J13 = "Construction"
)

donnees_secteurs <- subset(
  exposition_annuelle,
  Activite %in% secteurs
)

donnees_secteurs$Secteur <- factor(
  unname(libelles[donnees_secteurs$Activite]),
  levels = unname(libelles[secteurs])
)

# Graphique 1 : evolution de l'exposition sectorielle
graph_exposition <- ggplot(
  donnees_secteurs,
  aes(
    x = Annee,
    y = Exposition_pct,
    color = Secteur,
    group = Secteur
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = annees) +
  labs(
    title = "Evolution de l'exposition aux produits petroliers",
    subtitle = "Senegal, MCS 2014-2022",
    x = "Annee",
    y = "Achats intermediaires de I7 / production (%)",
    color = "Activite"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

print(graph_exposition)

ggsave(
  file.path(dossier_sortie, "01_exposition_sectorielle.png"),
  graph_exposition, width = 11, height = 7, dpi = 300
)


# ------------------------------------------------------------
# 4. SIMULATION DU SURCOUT DIRECT, 2014-2022
# ------------------------------------------------------------

# Hypothese : les quantites achetees restent fixes et le prix
# de I7 augmente de 10 % ou de 25 %.
#
# Surcout direct = achats de I7 par toutes les activites
# multiplies par le taux de choc.
#
# Ce calcul ne represente pas une hausse du PIB ni de
# l'inflation observee.

calculer_surcout <- function(matrice, annee) {
  
  Z <- matrice[
    paste0("I", 1:28),
    paste0("J", 1:28),
    drop = FALSE
  ]
  
  achats_totaux_I7 <- sum(Z["I7", ], na.rm = TRUE)
  
  data.frame(
    Annee = annee,
    Surcout_10 = achats_totaux_I7 * 0.10,
    Surcout_25 = achats_totaux_I7 * 0.25
  )
}

resume_chocs <- do.call(
  rbind,
  lapply(annees, function(a) {
    calculer_surcout(
      as.matrix(mcs_liste_propre[[as.character(a)]]),
      a
    )
  })
)

rownames(resume_chocs) <- NULL

cat("\nSURCOUTS DIRECTS SIMULES\n")
print(resume_chocs, row.names = FALSE)

# Graphique 2 : evolution du surcout direct
evolution_long <- rbind(
  data.frame(
    Annee = resume_chocs$Annee,
    Scenario = "Choc de +10 %",
    Surcout = resume_chocs$Surcout_10
  ),
  data.frame(
    Annee = resume_chocs$Annee,
    Scenario = "Choc de +25 %",
    Surcout = resume_chocs$Surcout_25
  )
)

graph_surcout <- ggplot(
  evolution_long,
  aes(x = Annee, y = Surcout, color = Scenario)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  scale_x_continuous(breaks = annees) +
  labs(
    title = "Evolution du surcout direct d'un choc petrolier",
    subtitle = "Senegal, MCS 2014-2022",
    x = "Annee",
    y = "Surcout theorique (unites monetaires de la MCS)",
    color = "Scenario"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

print(graph_surcout)

ggsave(
  file.path(dossier_sortie, "02_evolution_surcout_direct.png"),
  graph_surcout, width = 10, height = 6, dpi = 300
)


# ------------------------------------------------------------
# 5. COEFFICIENTS TECHNIQUES DE LA MCS 2022
# ------------------------------------------------------------

# A[i,j] = achats du produit i par l'activite j / production j.
# La division se fait colonne par colonne.

A_tech <- sweep(Z, 2, production, "/")

# Eviter les divisions par zero
A_tech[, production == 0] <- 0

cat("\nCOEFFICIENTS TECHNIQUES : EXTRAIT\n")
print(
  round(
    A_tech[
      c("I7", "I11", "I15"),
      c("J11", "J15", "J4", "J9", "J13"),
      drop = FALSE
    ],
    4
  )
)

# Controle de stabilite mathematique
valeurs_propres <- eigen(
  A_tech,
  only.values = TRUE
)$values

rayon_spectral <- max(Mod(valeurs_propres))

M <- diag(ncol(A_tech)) - t(A_tech)

cat("\nCONTROLES MATHEMATIQUES\n")
cat("Rayon spectral :", round(rayon_spectral, 4), "\n")
cat(
  "Test numerique du determinant non nul :",
  abs(det(M)) > 1e-8, "\n"
)
cat("Conditionnement :", round(kappa(M), 2), "\n")

# Un rayon spectral inferieur a 1 est un critere utile pour
# l'inversion de la serie de propagation. Il ne suffit pas,
# a lui seul, a valider l'interpretation economique du modele.


# ------------------------------------------------------------
# 6. SIMULATION INDICATIVE DES EFFETS DIRECTS ET INDIRECTS
# ------------------------------------------------------------

# Hypothese de travail :
# - les produits I1-I28 et activites J1-J28 sont apparies
#   dans le meme ordre ;
# - les coefficients techniques sont fixes ;
# - le choc de prix porte sur le produit I7 ;
# - la propagation suit un modele lineaire simplifie.
#
# Formulation utilisee dans nos simulations precedentes :
# variation_totale = solve(I - t(A), effet_direct)
#
# Les resultats doivent etre interpretes comme des variations
# theoriques conditionnelles au modele, et non comme des prix
# effectivement observes.

simuler_choc_prix <- function(taux_choc, A) {
  
  effet_direct <- as.numeric(A["I7", ]) * taux_choc
  names(effet_direct) <- colnames(A)
  
  M <- diag(ncol(A)) - t(A)
  
  variation_totale <- solve(M, effet_direct)
  
  effet_indirect <- variation_totale - effet_direct
  
  data.frame(
    Activite = colnames(A),
    Effet_direct_pct = 100 * effet_direct,
    Effet_indirect_pct = 100 * effet_indirect,
    Effet_total_pct = 100 * variation_totale,
    row.names = NULL
  )
}

resultats_10 <- simuler_choc_prix(0.10, A_tech)
resultats_25 <- simuler_choc_prix(0.25, A_tech)

resultats_10 <- resultats_10[
  order(resultats_10$Effet_total_pct, decreasing = TRUE),
]

resultats_25 <- resultats_25[
  order(resultats_25$Effet_total_pct, decreasing = TRUE),
]

cat("\nEFFETS SIMULES - CHOC DE +10 %\n")
print(head(resultats_10, 10), row.names = FALSE)

cat("\nEFFETS SIMULES - CHOC DE +25 %\n")
print(head(resultats_25, 10), row.names = FALSE)

# Graphique 3 : decomposition direct / indirect pour les
# dix activites ayant les effets totaux les plus eleves.
graph_data <- head(resultats_10, 10)

graph_data$Activite <- factor(
  graph_data$Activite,
  levels = graph_data$Activite[
    order(graph_data$Effet_total_pct)
  ]
)

graph_long <- rbind(
  data.frame(
    Activite = graph_data$Activite,
    Type = "Effet direct",
    Effet = graph_data$Effet_direct_pct
  ),
  data.frame(
    Activite = graph_data$Activite,
    Type = "Effet indirect",
    Effet = graph_data$Effet_indirect_pct
  )
)

graph_propagation <- ggplot(
  graph_long,
  aes(x = Activite, y = Effet, fill = Type)
) +
  geom_col(width = 0.7) +
  coord_flip() +
  labs(
    title = "Propagation theorique d'un choc petrolier",
    subtitle = "Choc de +10 %, MCS du Senegal, 2022",
    x = "Activite economique",
    y = "Variation theorique des couts (%)",
    fill = "Composante"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

print(graph_propagation)

ggsave(
  file.path(dossier_sortie, "03_propagation_choc_2022.png"),
  graph_propagation, width = 10, height = 7, dpi = 300
)


# ------------------------------------------------------------
# 7. INDICATEURS DE SYNTHESE
# ------------------------------------------------------------

# Evolution du surcout direct de 2014 a 2022
surcout_2014 <- resume_chocs$Surcout_10[
  resume_chocs$Annee == 2014
]

surcout_2022 <- resume_chocs$Surcout_10[
  resume_chocs$Annee == 2022
]

evolution_surcout_pct <- (
  surcout_2022 / surcout_2014 - 1
) * 100

cat("\n================ SYNTHESE ================\n")

cat(
  "Surcout direct simule en 2014, choc +10 % :",
  round(surcout_2014, 1), "\n"
)

cat(
  "Surcout direct simule en 2022, choc +10 % :",
  round(surcout_2022, 1), "\n"
)

cat(
  "Evolution du surcout simule 2014-2022 :",
  round(evolution_surcout_pct, 1), "%\n"
)

# Evolution de l'exposition de J11
j11 <- subset(exposition_annuelle, Activite == "J11")

exp_2014 <- j11$Exposition_pct[j11$Annee == 2014]
exp_2022 <- j11$Exposition_pct[j11$Annee == 2022]

cat(
  "Exposition J11 en 2014 :",
  round(exp_2014, 2), "%\n"
)

cat(
  "Exposition J11 en 2022 :",
  round(exp_2022, 2), "%\n"
)

cat(
  "Variation de J11 :",
  round(exp_2022 - exp_2014, 2),
  "points de pourcentage\n"
)

cat("\nTop 5 des activites exposees en 2022 :\n")
print(
  head(
    exposition_2022[
      , c("Activite", "Exposition_pct")
    ],
    5
  ),
  row.names = FALSE
)

cat("\nTop 5 des effets totaux simules, choc +10 % :\n")
print(
  head(
    resultats_10[
      , c(
        "Activite", "Effet_direct_pct",
        "Effet_indirect_pct", "Effet_total_pct"
      )
    ],
    5
  ),
  row.names = FALSE
)


# ------------------------------------------------------------
# 8. EXPORT DES TABLEAUX
# ------------------------------------------------------------

write.csv(
  exposition_annuelle,
  file.path(dossier_sortie, "exposition_annuelle.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

write.csv(
  resume_chocs,
  file.path(dossier_sortie, "surcouts_directs.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

write.csv(
  resultats_10,
  file.path(dossier_sortie, "simulation_choc_10.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

write.csv(
  resultats_25,
  file.path(dossier_sortie, "simulation_choc_25.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)



# ============================================================
# PREPARATION DE LA MCS 2022 POUR GAMS
# ============================================================

m <- as.matrix(mcs_liste_propre[["2022"]])
storage.mode(m) <- "numeric"

# Total des flux en ligne et en colonne
totaux_lignes <- rowSums(m, na.rm = TRUE)
totaux_colonnes <- colSums(m, na.rm = TRUE)

# Comparaison uniquement pour les comptes présents
# dans les deux dimensions.
comptes_communs <- intersect(rownames(m), colnames(m))

controle_mcs <- data.frame(
  Compte = comptes_communs,
  Total_ligne = totaux_lignes[comptes_communs],
  Total_colonne = totaux_colonnes[comptes_communs]
)

controle_mcs$Ecart <- with(
  controle_mcs,
  Total_ligne - Total_colonne
)

controle_mcs$Ecart_relatif_pct <- with(
  controle_mcs,
  ifelse(
    pmax(abs(Total_ligne), abs(Total_colonne)) > 0,
    100 * Ecart /
      pmax(abs(Total_ligne), abs(Total_colonne)),
    NA_real_
  )
)

cat("Comptes les moins equilibres :\n")

print(
  controle_mcs[
    order(abs(controle_mcs$Ecart), decreasing = TRUE),
  ][1:min(15, nrow(controle_mcs)), ],
  row.names = FALSE
)

# Export du controle pour documentation
write.csv(
  controle_mcs,
  "controle_equilibre_MCS_2022.csv",
  row.names = FALSE
)





# Contrôle complet de l'équilibre de la MCS 2022

m <- as.matrix(mcs_liste_propre[["2022"]])
storage.mode(m) <- "numeric"

# Comptes présents dans les lignes et les colonnes
comptes_lignes <- rownames(m)
comptes_colonnes <- colnames(m)
comptes_communs <- intersect(comptes_lignes, comptes_colonnes)

# Comparaison des totaux
total_ligne <- rowSums(m, na.rm = TRUE)
total_colonne <- colSums(m, na.rm = TRUE)

controle <- data.frame(
  Compte = comptes_communs,
  Total_ligne = total_ligne[comptes_communs],
  Total_colonne = total_colonne[comptes_communs]
)

controle$Ecart <- controle$Total_ligne - controle$Total_colonne

# Comptes dont les totaux ne sont pas égaux
ecarts_non_nuls <- subset(controle, abs(Ecart) > 1e-6)

cat("\nNombre de comptes communs :", length(comptes_communs), "\n")
cat("Nombre de comptes avec un écart :", nrow(ecarts_non_nuls), "\n")
print(ecarts_non_nuls, row.names = FALSE)

cat("\nComptes présents uniquement dans les lignes :\n")
print(setdiff(comptes_lignes, comptes_colonnes))

cat("\nComptes présents uniquement dans les colonnes :\n")
print(setdiff(comptes_colonnes, comptes_lignes))







# 1. Exporter la MCS 2022 pour préparation dans GAMS
mcs_2022 <- mcs_liste_propre[["2022"]]

mcs_export <- data.frame(
  Compte = rownames(mcs_2022),
  mcs_2022,
  check.names = FALSE
)

write.csv(
  mcs_export,
  "MCS_Senegal_2022.csv",
  row.names = FALSE,
  na = "0",
  fileEncoding = "UTF-8"
)

# 2. Vérifier les comptes atypiques
cat("Derniers comptes en lignes :\n")
print(tail(rownames(mcs_2022), 15))

cat("\nDerniers comptes en colonnes :\n")
print(tail(colnames(mcs_2022), 15))

# 3. Vérifier les flux associés aux comptes concernés
if ("Accu" %in% rownames(mcs_2022)) {
  cat("\nFlux de la ligne Accu :\n")
  print(mcs_2022["Accu", ])
}

if ("Epargne" %in% colnames(mcs_2022)) {
  cat("\nFlux de la colonne Epargne :\n")
  print(mcs_2022[, "Epargne"])
}

# 4. Afficher les codes et libellés de la nomenclature
code <- readxl::read_excel(
  "MCS_Senegal_2014-2022_vf.xlsx",
  sheet = "Code"
)

print(
  code[
    apply(code, 1, function(x) {
      any(trimws(as.character(x)) %in% c("Accu", "Epargne"))
    }),
  ],
  n = Inf
)




# Comparer les comptes des lignes et des colonnes
cat("Dimensions de la MCS :", dim(mcs_2022), "\n")

cat("\nComptes différents selon la dimension :\n")
print(data.frame(
  Position = seq_len(nrow(mcs_2022)),
  Compte_ligne = rownames(mcs_2022),
  Compte_colonne = colnames(mcs_2022)
)[rownames(mcs_2022) != colnames(mcs_2022), ],
row.names = FALSE)





# Copie de travail de la MCS 2022
mcs_gams <- mcs_liste_propre[["2022"]]

# Harmonisation provisoire du nom du compte
colnames(mcs_gams)[colnames(mcs_gams) == "Epargne"] <- "Accu"

# Vérifier l'alignement des comptes
stopifnot(identical(rownames(mcs_gams), colnames(mcs_gams)))

# Vérifier l'équilibre de chaque compte
ecarts <- rowSums(mcs_gams) - colSums(mcs_gams)

cat("Nombre de comptes :", nrow(mcs_gams), "\n")
cat("Écart maximal absolu :", max(abs(ecarts)), "\n")

# Export pour GAMS
write.csv(
  data.frame(Compte = rownames(mcs_gams), mcs_gams, check.names = FALSE),
  "MCS_Senegal_2022_GAMS.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

cat("Fichier exporté : MCS_Senegal_2022_GAMS.csv\n")
cat("\nPROJET TERMINE\n")
cat(
  "Les tableaux CSV et graphiques PNG sont dans le dossier :",
  dossier_sortie, "\n"
)
