
# AUDIT QUALITE DES DONNEES - PORTEFEUILLE DE CREDITS AUTO EUROPE

# install.packages("tidyverse")
# install.packages("knitr")

library(tidyverse)
library(knitr)

options(scipen = 999)   # Permet d'éviter l'ecriture scientifique (1e+05) dans les sorties


## 0. PARAMETRES DE L'ETUDE (issus du dictionnaire de donnees) :


fichier    <- "credit_auto_retail_europe.csv"
date_obs   <- as.Date("2024-12-31")   # date d'arrete des donnees
debut_prod <- as.Date("2020-01-01")   # debut de la periode d'octroi
fin_prod   <- as.Date("2022-12-31")   # fin de la periode d'octroi

pays_autorises                 <- c("France", "Allemagne", "Italie", "Espagne", "Belgique",
                                    "Pays-Bas", "Portugal", "Pologne", "Autriche", "Irlande")
genres_autorises               <- c("M", "F")
statuts_maritaux_autorises     <- c("Célibataire", "Marié(e)", "Divorcé(e)", "Veuf(ve)")
statuts_emploi_autorises       <- c("Salarié CDI", "Salarié CDD", "Indépendant",
                                    "Fonctionnaire", "Retraité", "Sans emploi")
statuts_logement_autorises     <- c("Propriétaire", "Locataire", "Hébergé")
types_vehicule_autorises       <- c("Véhicule particulier", "Véhicule utilitaire")
conditions_vehicule_autorisees <- c("Neuf", "Occasion")
durees_autorisees              <- c(12, 24, 36, 48, 60, 72, 84)

# Marques "utilitaires" (pour vérifier la coherence type de véhicule / marque)
marques_utilitaires <- c("Volkswagen Utilitaires", "Opel Professional",
                         "Mercedes-Benz Vans", "Ford Transit", "Citroën Business",
                         "Peugeot Pro", "Fiat Professional", "Iveco", "Renault Pro+")




## I. IMPORT ET APERCU DE LA BASE :


# fileEncoding = "UTF-8" pour bien lire les accents (Célibataire, Citroën...)
d <- read.csv(fichier, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
dim_originale <- dim(d)


cat("APERCU DE LA BASE \n")
cat("Lignes :", nrow(d), "| Colonnes :", ncol(d), "\n\n")

# Apercu des variables 
str(d)

# Apercu statistique des variables
print(summary(d))




## II. COMPLETUDE :


cat("\n VALEURS MANQUANTES PAR COLONNE \n")
print(colSums(is.na(d)))

cat("\n Chaines vides / espaces par colonne :\n")
print(sapply(d, function(x) sum(x == "" | x == " ", na.rm = TRUE)))
# Pas de NA mais probablement des valeurs sentinelles qui remplacent chaque valeur manquante



# ETUDE DES VALEURS SENTINELLES ET FAUX NA :

# Extraction sous forme de vecteur de toutes les colonnes quantitatives
cols_num <- names(d)[sapply(d, is.numeric)]

# 1. Recherche spécifique de la valeur sentinelle 9999 sur les colonnes numériques
valeur_sentinelle <- 9999

scan_sentinelle <- map_dfr(cols_num, function(col) {
  d %>%
    filter(.data[[col]] == valeur_sentinelle) %>%
    count(valeur = .data[[col]], name = "nb_lignes") %>%
    mutate(
      colonne  = col,
      pct_base = round(100 * nb_lignes / nrow(d), 3)
    )
})

# Affichage des sentinelles numériques :
print(as_tibble(scan_sentinelle), n = Inf)

# On a ici que plusieurs variables ont des valeurs sentilles ce qui confirme notre théorie


# 2. Colonnes texte : modalités "vides" déguisées, et liste complète des modalités
mots_vides <- c("", "NA", "N/A", "NULL", "?", "-", "INCONNU", "UNKNOWN", "XXX")

# Exclusion des variables non pertinentes pour ce test
cols_txt <- setdiff(names(d)[sapply(d, is.character)],
                    c("loan_id", "origination_date", "employment_start_date"))

scan_txt <- map_dfr(cols_txt, function(col) {
  d %>%
    filter(toupper(trimws(.data[[col]])) %in% mots_vides) %>%
    count(valeur = .data[[col]], name = "nb_lignes") %>%
    mutate(colonne = col)
})

# Réinitialisation de sécurité de l'option globale et affichage
options(na.print = NULL)
print(as_tibble(scan_txt))
print(lapply(d[cols_txt], table))

# On a ici aucune modalité texte vide ou déguisée




## III. UNICITE :


# Isole la liste unique des identifiants (loan_id) qui apparaissent en plusieurs exemplaires
ids_dupliques <- unique(d$loan_id[duplicated(d$loan_id) | duplicated(d$loan_id, fromLast = TRUE)])

# Crée un vecteur logique (Vrai/Faux) marquant chaque ligne de la base dont l'identifiant appartient à cette liste de doublons
lignes_id_dupliquees <- d$loan_id %in% ids_dupliques

# Identifie les lignes qui sont des clones parfaits, en flaguant aussi bien la ligne originale que ses copies.
dup_ligne <- duplicated(d) | duplicated(d, fromLast = TRUE)


cat("\n UNICITE \n")

cat("Lignes concernées (toutes occurrences) :", sum(lignes_id_dupliquees), "\n")
cat("Lignes strictement identiques (toutes colonnes) :", sum(dup_ligne), "\n")
# Dénombre les identifiants qui ne respectent pas le format défini pour les ids --> mauvaise saisies.
cat("Identifiants au mauvais format (AUTO + 7 chiffres) :", sum(!grepl("^AUTO[0-9]{7}$", d$loan_id)), "\n")




## IV. CONSTRUCTION DES CONTROLES (les "flags") :


# Un peu de vocabullaire sur ce qu'on s'apprête à faire :

# ctrl = copie de d avec des colonnes de controle en plus
# flag = TRUE quand la ligne pose probleme pour le controle concerne
# COM = completude, UNI = unicite, VAL = validite, COH = coherence inter-champs,
# PLA = plausibilite, SOLV = solvabilite, TMP = temporel


ctrl <- d %>%
  mutate(
    
    # 0. Calcule des valeurs intermédiaires sans modifier les colonnes d'origine :
    
    date_octroi = as.Date(origination_date, format = "%Y-%m-%d"),
    date_debut_emploi = as.Date(employment_start_date, format = "%Y-%m-%d"),
    anciennete_dates= as.numeric(date_octroi - date_debut_emploi) / 365.25,   # en annees
    taux_mensuel = interest_rate_pct / 100 / 12,
    # Mensualite theorique (formule d'amortissement) : NA si le taux est hors domaine
    mensualite_theo = ifelse(taux_mensuel > 0,
                               loan_amount * taux_mensuel / (1 - (1 + taux_mensuel)^(-loan_term_months)),
                               NA_real_),
    km_par_an = ifelse(vehicle_age_years > 0, vehicle_mileage_km / vehicle_age_years, NA),
    # Taux d'effort du nouveau pret (% du revenu) ; NA si revenu <- 0 (division impossible)
    taux_effort = ifelse(monthly_net_income > 0, 100 * monthly_installment / monthly_net_income, NA),
    # Taux d'endettement total = dette existante + nouvelle mensualite
    taux_total= existing_debt_ratio + taux_effort,
    
    
    ## 1. COMPLETUDE :
    
    
    flag_COM_valeur_manquante = rowSums(is.na(d)) > 0,
    
    
    ## 2. UNICITE :
    
    
    flag_UNI_loan_id_doublon = lignes_id_dupliquees,
    flag_UNI_ligne_identique = dup_ligne,
    
    
    ## 3. VALIDITE :
    
    
    flag_VAL_id_format = !grepl("^AUTO[0-9]{7}$", loan_id),
    flag_VAL_age_hors_18_80 = borrower_age < 18 | borrower_age > 80,
    flag_VAL_genre = !borrower_gender %in% genres_autorises,
    flag_VAL_modalites_hors_dico = !marital_status %in% statuts_maritaux_autorises |
      !employment_status %in% statuts_emploi_autorises |
      !housing_status %in% statuts_logement_autorises |
      !vehicle_type %in% types_vehicule_autorises |
      !vehicle_condition %in% conditions_vehicule_autorisees,
    flag_VAL_pays_hors_liste = !country %in% pays_autorises,
    flag_VAL_anciennete_hors_0_45 = job_seniority_years < 0 | job_seniority_years > 45,
    flag_VAL_revenu_hors_750_15000 = monthly_net_income < 750 | monthly_net_income > 15000,
    flag_VAL_revenu_hors_dico_mais_plausible = (monthly_net_income < 750 & monthly_net_income > 0) | 
      (monthly_net_income > 15000 & monthly_net_income <= 50000),
    flag_VAL_endettement_hors_0_60 = existing_debt_ratio < 0 | existing_debt_ratio > 60,
    flag_VAL_score_hors_300_850 = credit_bureau_score < 300 | credit_bureau_score > 850,
    flag_VAL_nb_prets_hors_0_6  = nb_previous_loans < 0 | nb_previous_loans > 6,
    flag_VAL_prix_hors_4000_140000 = vehicle_price < 4000 | vehicle_price > 140000,
    flag_VAL_age_vehicule_hors_0_12 = vehicle_age_years < 0 | vehicle_age_years > 12,
    flag_VAL_duree_hors_liste = !loan_term_months %in% durees_autorisees,
    flag_VAL_taux_hors_1_5_15 = interest_rate_pct < 1.5 | interest_rate_pct > 15,
    flag_VAL_ltv_hors_0_1 = ltv_ratio < 0 | ltv_ratio > 1,
    flag_VAL_default_flag_hors_0_1 = !default_flag %in% c(0, 1),
    flag_VAL_mensualite_negative = monthly_installment <= 0,
    flag_VAL_apport_negatif = down_payment_amount < 0,
    flag_VAL_montant_pret_nul_ou_negatif = loan_amount <= 0,
    flag_VAL_km_negatif = vehicle_mileage_km < 0,
    flag_VAL_dpd_negatif = days_past_due < 0,
    flag_VAL_dpd_9999 = days_past_due == 9999,   # valeur "code", pas un vrai retard
    # rend visible les lignes que la formule d'amortissement ne peut pas evaluer
    # (taux <= 0), au lieu de les faire disparaitre silencieusement du controle COH suivant
    flag_VAL_mensualite_theo_incalculable = is.na(mensualite_theo),
    
    
    ## 4. COHERENCE INTER-CHAMPS :
    
    
    flag_COH_pret_diff_prix_moins_apport = abs(vehicle_price - down_payment_amount - loan_amount) > 1,
    flag_COH_apport_sup_ou_egal_prix = down_payment_amount >= vehicle_price,
    flag_COH_pret_sup_prix = loan_amount > vehicle_price,
    flag_COH_ltv_incoherent = abs(loan_amount / vehicle_price - ltv_ratio) > 0.001,
    # Mensualite vs formule d'amortissement classique (tolerance 1 %) ; NA si non calculable
    flag_COH_mensualite_incoherente = ifelse(!is.na(mensualite_theo),
                                                  abs(mensualite_theo - monthly_installment) / mensualite_theo > 0.01, NA),
    # Definition du defaut : >= 90 jours de retard
    # On exclut le code sentinelle 9999 de l'analyse des vrais retards
    flag_COH_dpd_ge90_mais_sain = days_past_due >= 90 & days_past_due != 9999 & default_flag == 0,
    flag_COH_defaut_mais_dpd_lt90 = days_past_due < 90 & days_past_due != 9999 & default_flag == 1,
    flag_COH_neuf_avec_age_positif = vehicle_condition == "Neuf" & vehicle_age_years > 0,
    flag_COH_occasion_age_zero = vehicle_condition == "Occasion" & vehicle_age_years == 0,
    flag_COH_neuf_km_sup_100 = vehicle_condition == "Neuf" & vehicle_mileage_km > 100,
    flag_COH_type_vs_marque = (vehicle_type == "Véhicule particulier" & vehicle_brand %in% marques_utilitaires) |
      (vehicle_type == "Véhicule utilitaire" & !vehicle_brand %in% marques_utilitaires),
    flag_COH_retraite_moins_de_55_ans = employment_status == "Retraité" & borrower_age < 55,
    flag_COH_ancien_debut_avant_16_ans = borrower_age - job_seniority_years < 16,
    flag_COH_sans_emploi_avec_anciennete = employment_status == "Sans emploi" & job_seniority_years > 0,
    flag_COH_retraite_avec_anciennete = employment_status == "Retraité" & job_seniority_years > 0, 
    # Typologie du controle defaut/retard pour le rapport (reprend les 2 flags ci-dessus)
    type_incoherence_defaut = case_when(
      flag_COH_dpd_ge90_mais_sain   ~ "Sous-estimation (retard >= 90j, non marqué en défaut)",
      flag_COH_defaut_mais_dpd_lt90 ~ "Sur-estimation (marqué en défaut, retard < 90j)",
      TRUE ~ "Conforme"
    ),
    
    
    ## 5. EXACTITUDE / PLAUSIBILITE :
    
    flag_PLA_revenu_aberrant = monthly_net_income <= 0 | monthly_net_income > 50000,
    flag_PLA_sans_emploi_revenu_sup_5000 = employment_status == "Sans emploi" & monthly_net_income > 5000,
    flag_PLA_prix_extreme_sup_300000 = vehicle_price > 300000,
    flag_PLA_pret_sup_140000 = loan_amount > 140000,   
    flag_PLA_occasion_km_par_an_sup_30000 = vehicle_condition == "Occasion" & vehicle_age_years > 0 & km_par_an > 30000,
    flag_PLA_occasion_km_inf_1000 = vehicle_condition == "Occasion" & vehicle_age_years > 0 & vehicle_mileage_km < 1000,
    
    
    ## 6. SOLVABILITE :
    
    
    flag_SOLV_mensualite_sup_revenu = monthly_installment > monthly_net_income,
    flag_SOLV_endettement_total_sup_60 = !is.na(taux_total) & taux_total > 60,
    flag_SOLV_charges_sup_100_du_revenu = !is.na(taux_total) & taux_total >= 100,
    flag_SOLV_prix_sup_3_ans_de_revenu = vehicle_price > 36 * monthly_net_income,
    
    
    ## 7. FRAICHEUR / COHERENCE TEMPORELLE :
    
    
    flag_TMP_format_date = !grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", origination_date) |
      !grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", employment_start_date) |
      is.na(date_octroi) | is.na(date_debut_emploi),
    flag_TMP_octroi_hors_periode = date_octroi < debut_prod | date_octroi > fin_prod,
    flag_TMP_debut_emploi_apres_octroi = date_debut_emploi > date_octroi,
    flag_TMP_debut_emploi_apres_arrete = date_debut_emploi > date_obs,
    flag_TMP_anciennete_vs_dates = abs(anciennete_dates - job_seniority_years) > 1,
    flag_TMP_debut_emploi_avant_15_ans = (borrower_age - anciennete_dates) < 15,
    flag_TMP_dpd_sup_jours_ecoules = days_past_due > as.numeric(date_obs - date_octroi),
    flag_TMP_dpd_sup_730 = days_past_due > 730   # fenêtre de performance = 24 mois
  )


# La base brute n'a pas bougé
stopifnot(identical(dim(d), dim_originale))


# SYNTHESE CHIFFREE (nb et % de lignes touchées par contrôle) :


flags <- ctrl %>% select(starts_with("flag_"))
nb_par_controle <- colSums(flags, na.rm = TRUE)

synthese <- data.frame(
  controle  = names(nb_par_controle),
  nb_lignes = as.integer(nb_par_controle),
  pct_base  = round(100 * nb_par_controle / nrow(d), 2)
) %>% arrange(controle)

rownames(synthese) <- NULL   # évite la colonne dupliquée dans les tableaux kable

cat("\n SYNTHESE DES CONTROLES \n")
print(synthese, row.names = FALSE)

# Sortie d'un .csv avec le tableau des anomalies :
# write.csv(synthese, "synthese_anomalies.csv", row.names = FALSE)

# Création d'un beau tableau global avec kable (l'équivalent du tableau .csv mais ici) :

tableau_synthese <- synthese %>%
  kable(
    col.names = c("Nom du Contrôle", "Nombre de Lignes", "Pourcentage de la Base (%)"),
    caption = "Synthèse globale des anomalies identifiées",
    align = c("l", "c", "c") # Alignement 
  ) 

print(tableau_synthese)



## V. EXEMPLES DE LIGNES PROBLEMATIQUES (Sélection des pires cas) :


cat("\n EXEMPLES DE LIGNES PROBLEMATIQUES \n")

cat("\n Âge hors limites (18-80 ans) :\n")
# On trie par âge croissant pour faire remonter immédiatement les cas choquants.
print(ctrl %>% 
        filter(flag_VAL_age_hors_18_80) %>%
        arrange(borrower_age) %>% 
        select(loan_id, borrower_age, employment_status, loan_amount) %>% 
        head(5))


cat("\n Retard >= 90j mais marqué sain (Les retards les plus longs) :\n")
print(ctrl %>% 
        filter(flag_COH_dpd_ge90_mais_sain) %>%
        arrange(desc(days_past_due)) %>% 
        select(loan_id, days_past_due, default_flag) %>% 
        head(5))


cat("\n Mensualité incohérente vs formule d'amortissement (Les plus grands écarts) :\n")
print(ctrl %>% 
        filter(flag_COH_mensualite_incoherente) %>%
        mutate(ecart_euros = abs(monthly_installment - mensualite_theo)) %>%
        arrange(desc(ecart_euros)) %>% 
        select(loan_id, loan_amount, interest_rate_pct, monthly_installment, mensualite_theo) %>% 
        head(5))


cat("\n Incohérence métier : Retraités avec de l'ancienneté professionnelle :\n")
print(ctrl %>% 
        filter(flag_COH_retraite_avec_anciennete) %>%
        arrange(desc(job_seniority_years)) %>% # On montre les retraités avec 40 ans d'ancienneté "en cours"
        select(loan_id, employment_status, borrower_age, job_seniority_years) %>% 
        head(5))


cat("\n Revenus aberrants (valeurs négatives ou nulles) :\n")
print(ctrl %>% 
        filter(flag_PLA_revenu_aberrant, monthly_net_income <= 0) %>%
        arrange(monthly_net_income) %>% # Du plus négatif vers 0
        select(loan_id, monthly_net_income, employment_status, loan_amount) %>% 
        head(5))


cat("\n Revenus aberrants (valeurs extrêmes positives > 50 000 EUR) :\n")
print(ctrl %>% 
        filter(flag_PLA_revenu_aberrant, monthly_net_income > 50000) %>%
        arrange(desc(monthly_net_income)) %>% # Du plus élevé au moins élevé
        select(loan_id, monthly_net_income, employment_status, loan_amount) %>% 
        head(5))


cat("\n Solvabilité : Mensualité supérieure au revenu (Les pires ratios) :\n")
print(ctrl %>% 
        filter(flag_SOLV_mensualite_sup_revenu) %>%
        arrange(desc(monthly_installment - monthly_net_income)) %>% 
        select(loan_id, monthly_net_income, monthly_installment, employment_status) %>% 
        head(5))


cat("\n VERIFICATIONS GLOBALES \n")

cat(" Fenetre de performance de 24 mois complete pour tous les prets ? (0 attendu) :",
    sum(ctrl$date_octroi + 730 > date_obs), "\n")

cat("Prets dont la duree en mois est plus courte que la fenetre d'observation (24 mois) :",
    sum(d$loan_term_months < 24), "\n")

cat("Repartition des octrois par annee :\n")
print(table(format(ctrl$date_octroi, "%Y")))



## VI. POIDS DES ANOMALIES DANS L'ANALYSE :


#  Qu'est-ce qu'il se passe si on ne tient pas compte de ces problemes ?

# On separe les anomalies "données" (COM, UNI, VAL, COH, PLA, TMP) de la
# solvabilité (SOLV), qui relève du risque du dossier et non d'une erreur de saisie
flags_donnees <- ctrl %>%
  select(starts_with("flag_")) %>%
  select(-starts_with("flag_SOLV"), -flag_VAL_revenu_hors_dico_mais_plausible)

ctrl$nb_anomalies <- rowSums(flags_donnees, na.rm = TRUE)
ctrl$au_moins_une <- ctrl$nb_anomalies > 0

cat("\n IMPACT DES ANOMALIES \n")

cat("Repartition du nombre d'anomalies par contrat :\n")
print(table(ctrl$nb_anomalies))

cat("% de lignes avec au moins 1 anomalie :", round(100 * mean(ctrl$au_moins_une), 2), "\n")
cat("% de lignes avec 3 anomalies ou plus :", round(100 * mean(ctrl$nb_anomalies >= 3), 2), "\n")


cat("\nTaux de defaut selon la presence d'une anomalie de donnee :\n")
print(ctrl %>% group_by(au_moins_une) %>%
        summarise(nb = n(), taux_defaut_pct = round(100 * mean(default_flag), 2)))


# Ici on rajoute des Tests statistiques afin de souligner l'importance de ces changements avant l'inspection


cat("\n TESTS STATISTIQUES DE SOLVABILITE \n")

# 1. Impact de l'endettement critique (> 60%) sur le défaut
cat("\n Taux de defaut selon le seuil critique d'endettement (> 60%) :\n")
print(ctrl %>%
        filter(!is.na(flag_SOLV_endettement_total_sup_60)) %>%
        group_by(flag_SOLV_endettement_total_sup_60) %>%
        summarise(nb = n(), taux_defaut_pct = round(100 * mean(default_flag), 2)))

cat("\n Test du Khi2 (Significativité de l'écart de défaut selon l'endettement) :\n")
tab_chi2 <- table(ctrl$flag_SOLV_endettement_total_sup_60, ctrl$default_flag)
print(chisq.test(tab_chi2))


# 2. Comparaison des moyennes/médianes du taux d'effort total (Sains vs Défauts)
cat("\n Test de Wilcoxon (Non-paramétrique : Ecart du taux d'effort selon statut) :\n")

# On utilise Wilcoxon car le taux d'endettement ne suit pas une loi normale parfaite
print(wilcox.test(taux_total ~ default_flag, data = ctrl))

cat("\nTest-t (Comparaison des moyennes globales du taux d'effort) :\n")
print(t.test(taux_total ~ default_flag, data = ctrl))



## VII. GRAPHIQUES :

theme_set(
  theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", color = "#1a252f", size = 13), # Titre bleu-noir
      plot.subtitle = element_text(color = "gray40", size = 10),
      panel.grid.minor = element_blank(), # Supprime le quadrillage secondaire inutile
      panel.grid.major.x = element_blank(), # Enlève les lignes verticales (inutiles sur des barplots)
      legend.position = "bottom"
    )
)

expansion_haut <- expansion(mult = c(0, 0.15))   # marge pour les étiquettes de %/effectifs

p_age <- ggplot(d, aes(x = borrower_age)) +
  geom_histogram(binwidth = 1, fill = "#cdb8f2", color = "black") +
  geom_vline(xintercept = c(18, 80), color = "red", linetype = "dashed") +
  labs(title = "Répartition de l'âge des emprunteurs", x = "Âge", y = "Effectif")


p_revenu_brut <- ggplot(d, aes(x = "", y = monthly_net_income)) +
  geom_boxplot(fill = "#2f64b5", color = "black") +
  labs(title = "Revenu net mensuel (brut)", x = NULL, y = "EUR")


p_revenu_zoom <- ggplot(d, aes(x = "", y = monthly_net_income)) +
  geom_boxplot(fill = "#2f64b5", color = "black") +
  coord_cartesian(ylim = c(0, 15000)) +
  labs(title = "Revenu net mensuel (zoom 0-15 000, domaine du dictionnaire)", x = NULL, y = "EUR")


p_top_anomalies <- synthese %>%
  filter(nb_lignes > 0) %>%
  slice_max(pct_base, n = 10, with_ties = FALSE) %>%
  ggplot(aes(x = reorder(controle, pct_base), y = pct_base)) +
  geom_col(fill = "#cdb8f2", color = "black") +
  geom_text(aes(label = paste0(pct_base, "%")), hjust = -0.1, size = 3, color = "black") +
  coord_flip() +
  scale_y_continuous(expand = expansion_haut) +
  labs(title = "Top 10 des contrôles les plus touchés", x = NULL, y = "% de la base")


p_nb_anomalies <- ggplot(ctrl, aes(x = nb_anomalies)) +
  geom_bar(fill = "#cdb8f2", color = "black") +
  labs(title = "Nombre d'anomalies par contrat", x = "Nombre d'anomalies", y = "Nombre de contrats")


# Échantillonnage à 1% des dossiers sains pour éviter la sur-impression visuelle 
# et alléger la mémoire, tout en conservant 100% des anomalies pour bien marquer la rupture.
df_anomalies_apport <- d %>% filter(down_payment_amount >= vehicle_price)
df_conformes_sample <- d %>% filter(down_payment_amount < vehicle_price) %>% sample_frac(0.01)

p_apport_prix <- bind_rows(df_anomalies_apport, df_conformes_sample) %>%
  ggplot(aes(x = vehicle_price, y = down_payment_amount,
             color = down_payment_amount >= vehicle_price)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  scale_color_manual(values = c("FALSE" = "black", "TRUE" = "red"),
                     labels = c("Conforme (échantillon 1%)", "Anomalie (100% affichées)")) +
  labs(title = "Cohérence financière : apport initial vs prix du véhicule",
       x = "Prix du véhicule", y = "Apport initial", color = "Statut") +
  theme(legend.position = "bottom")


# Visualisation des incohérences croisées entre le statut de défaut officiel 
# et les jours de retard réels (identification des faux positifs et faux négatifs).
p_defaut_retard <- ggplot(ctrl, aes(x = type_incoherence_defaut, fill = type_incoherence_defaut)) +
  geom_bar(color = "black", show.legend = FALSE) +
  scale_fill_manual(values = c("Conforme" = "#43d95c", 
                               "Sous-estimation (retard >= 90j, non marqué en défaut)" = "#cdb8f2", 
                               "Sur-estimation (marqué en défaut, retard < 90j)" = "#2f64b5")) +
  geom_text(stat = "count", aes(label = after_stat(count)), hjust = -0.1, size = 3, color = "black") +
  coord_flip() +
  scale_y_continuous(expand = expansion_haut) +
  labs(title = "Statut de défaut vs jours de retard", x = NULL, y = "Nombre de contrats") +
  theme_minimal() # Plus clair avec ce thème ici


p_octrois <- ggplot(ctrl, aes(x = date_octroi)) +
  geom_histogram(binwidth = 30, fill = "#43d95c", color = "black") +
  geom_vline(xintercept = as.numeric(c(debut_prod, fin_prod)), color = "red", linetype = "dashed") +
  labs(title = "Distribution des dates d'octroi",
       subtitle = "En rouge : période réglementaire attendue (2020-2022)",
       x = "Date d'octroi", y = "Nombre de contrats") 


p_defaut_statut <- d %>%
  group_by(employment_status) %>%
  summarise(taux_defaut_pct = round(100 * mean(default_flag), 2)) %>%
  ggplot(aes(x = reorder(employment_status, taux_defaut_pct), y = taux_defaut_pct)) +
  geom_col(fill = "#2f64b5", color = "black") +
  coord_flip() +
  labs(title = "Taux de défaut par statut professionnel", x = NULL, y = "% de défaut")


# Justification visuelle du seuil critique d'endettement : démontre le décrochage 
# du risque de défaut à partir de 60% d'endettement total.
p_endettement_defaut <- ctrl %>%
  filter(!is.na(taux_total) & taux_total <= 100) %>%
  mutate(tranche_endettement = cut(taux_total, breaks = seq(0, 100, by = 10))) %>%
  group_by(tranche_endettement) %>%
  summarise(taux_defaut_pct = round(100 * mean(default_flag), 2)) %>%
  filter(!is.na(tranche_endettement)) %>%
  ggplot(aes(x = tranche_endettement, y = taux_defaut_pct)) +
  geom_col(fill = "#2f64b5", color = "black") +
  geom_hline(yintercept = 7.11, color = "red", linetype = "dashed") +
  labs(title = "Taux de défaut selon la tranche d'endettement total",
       subtitle = "Ligne rouge : taux moyen hors surendettement",
       x = "Tranche d'endettement (%)", y = "% de défaut")



# Sorties des graphiques :

print(p_age)
print(p_revenu_brut) # Pour bien voir les outliers
print(p_revenu_zoom) # Comprendre à quoi ressemble nos vraies nos vraies données
print(p_top_anomalies)
print(p_nb_anomalies) # On voit bien que généralement on en a pas mais que il peut y en avoir entre 1 et 10 par contrat
print(p_apport_prix) # Différence entre apport et le prix du véhicule
print(p_defaut_retard)
print(p_octrois) # On s'arrure que les dates vont être là où l'on veut
print(p_defaut_statut) # Le statut sans emploi va être le plus propice aux erreurs car c'est celui qui va jouer sur les variables d'argent
print(p_endettement_defaut) # Plus on est endetté plus on va être enclin à avoir des défauts de payement



cat("Base brute inchangee :", identical(dim(d), dim_originale), "\n")



