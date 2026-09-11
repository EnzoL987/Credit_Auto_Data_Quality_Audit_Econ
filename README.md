# 📊 Audit de Qualité des Données – Portefeuille Crédit Auto Retail Europe

---

## 📌 Contexte du Projet

Dans le cadre d'une préparation à une inspection prudentielle du régulateur européen (BCE / EBA), ce projet a pour mission de réaliser un **audit approfondi de la qualité des données (Data Quality Management)** sur un portefeuille de financement automobile retail (**803 250 contrats × 28 variables**).

L'enjeu consiste à identifier, quantifier et documenter l'ensemble des anomalies techniques et fonctionnelles pouvant impacter le provisionnement comptable (**IFRS 9**), le calcul des exigences en fonds propres (**Bâle III/CRR**) et la performance des modèles prédictifs de risque de crédit.

---

## 🎯 Périmètre & Dimensions d'Audit

L'évaluation de la qualité des données est structurée autour de **6 dimensions fondamentales** :

1. **Complétude** : Détection des valeurs manquantes (`NA`, chaînes vides, valeurs sentinelles).
2. **Unicité** : Contrôle des clés primaires (`loan_id`) et identification des lignes en doublon strict.
3. **Validité** : Respect des domaines de définition et contraintes univariées (codes pays ISO, formats, bornes d'âge, revenus, ratios LTV, durées de crédit).
4. **Cohérence Inter-champs** : Vérifications logiques et arithmétiques multivariées :
   - Relation financière : $\text{Prix du véhicule} - \text{Apport initial} = \text{Montant financé}$.
   - Conformité réglementaire du défaut : $\text{days\_past\_due} \ge 90 \iff \text{default\_flag} = 1$.
   - Adéquation véhicule : Statut "Neuf" vs kilométrage et âge.
5. **Exactitude & Plausibilité Métier** :
   - Formule actuarielle d'amortissement de la mensualité (`monthly_installment`).
   - Vraisemblance socio-professionnelle (statut retraité vs âge, ancienneté vs âge de début d'activité).
   - Taux d'effort et cohérence des apports par rapport au prix d'achat.
6. **Fraîcheur & Cohérence Temporelle** :
   - Conformité de la fenêtre de production (octrois entre 2020 et 2022).
   - Antériorité de la date d'embauche par rapport à l'octroi.
   - Cohérence entre l'écart temporel (`origination_date` - `employment_start_date`) et l'ancienneté déclarée (`job_seniority_years`).

---

## 🔍 Chiffres Clés & Principaux Enseignements

- **Intégrité de surface** : Aucune valeur `NA` brute constatée sur le jeu de données, masquant des anomalies sous-jacentes de cohérence logique.
- **Rupture réglementaire sur le Défaut (EBA / CRR)** :
  - **17 125 contrats en anomalie (2,13 % du portefeuille)**.
  - **8 771 faux sains (1,09 %)** : $\text{DPD} \ge 90$ jours mais `default_flag == 0`, représentant un risque critique de sous-provisionnement IFRS 9 (non-classement en Stage 3).
  - **8 354 faux défauts (1,04 %)** : $\text{DPD} < 90$ jours mais `default_flag == 1`, entraînant une sur-immobilisation de capital réglementaire et un biais pour l'apprentissage des modèles de scoring.
- **Cohérence Arithmétique & Véhicules** : Détection d'incohérences de calcul du montant du prêt et d'anomalies de saisie sur le parc de véhicules neufs/occasions.

---

## 🛠️ Stack Technique

- **Langage** : R (v4.x)
- **Environnement** : RStudio / R Markdown (génération de rapport d'audit PDF)
- **Packages principaux** :
  - `tidyverse` (`dplyr`, `ggplot2`, `readr`, `tibble`, `purrr`) : Manipulation de données à grande échelle et visualisations
  - `lubridate` : Manipulation et contrôles chronologiques des dates
  - `knitr` / `kableExtra` : Mise en page tabulaire pour reporting exécutif
