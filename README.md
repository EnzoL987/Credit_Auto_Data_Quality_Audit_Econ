# Data Quality Audit : Retail Auto Credit Portfolio

![R](https://img.shields.io/badge/R-276DC3?style=for-the-badge&logo=r&logoColor=white)
![RStudio](https://img.shields.io/badge/RStudio-75AADB?style=for-the-badge&logo=rstudio&logoColor=white)
![Tidyverse](https://img.shields.io/badge/Tidyverse-1A5276?style=for-the-badge&logo=r&logoColor=white)
![Finance](https://img.shields.io/badge/Risk_Management-Banking-darkgreen?style=for-the-badge)

## 📝 Project Overview
Acting as Data Analysts within the risk department of **"CREDIT AUTO EUROPE"**, our objective was to perform a comprehensive, regulatory-grade **Data Quality Audit** on a retail auto credit database containing over 800,000 credit contracts and 28 variables ahead of an on-site European regulatory inspection.

The goal is to provide a rigorous and automated audit trail evaluating risk governance, accounting standards (IFRS 9 / Basel Committee), and financial data integrity.

## 🏗️ Overall Architecture

*(Overview of the data structure and analysis process)*

```mermaid 
graph TD
    %% Source
    A[(Auto Credit Database <br/> 800,000+ records)]:::blue --> B[Data Import and Typing]:::white

    %% Audit of the 6 dimensions
    B --> C[Data Quality Audit]:::light
    
    C --> D[1. Completeness & 2. Uniqueness]:::purple
    C --> E[3. Validity of Domains]:::purple
    C --> F[4. Cross-Field Consistency]:::purple
    C --> G[5. Plausibility & 6. Temporal]:::purple

    %% Report
    D --> H((Final Audit Report)):::green
    E --> H
    F --> H
    G --> H


    %% Styles
    classDef blue fill:#1a8cff,stroke:#000,stroke-width:1px,color:#fff;
    classDef white fill:#ffffff,stroke:#333,stroke-width:1px,color:#000;
    classDef light fill:#e6f0ff,stroke:#333,stroke-width:1px,color:#000;
    classDef purple fill:#b366ff,stroke:#333,stroke-width:1px,color:#fff;
    classDef green fill:#00cc99,stroke:#000,stroke-width:2px,color:#fff;
```

## 🎯 Audit Scope and Quality Dimensions

The audit is structured around the 6 specific quality and risk dimensions coded in the analysis:

1. **Completeness:** 
   * Detection of missing values (`NA`) and blank/empty string entries across all columns.
   * Identification of sentinel values masking as data, specifically `9999` for the `days_past_due` variable.
2. **Uniqueness:** 
   * Identification of multi-column duplicate rows and primary key (`loan_id`) conflicts or overlaps.
3. **Validity:** 
   * Verification of values against the data dictionary domains, including `loan_id` format ("AUTO" + 7 digits), borrower age bounds (18-80), accepted country/status lists, and interest rate thresholds (1.5% to 15%).
4. **Cross-Field Consistency:** 
   * Financial arithmetic: Validating that $\text{Loan Amount} = \text{Vehicle Price} - \text{Down Payment}$ and $\text{LTV} = \frac{\text{Loan Amount}}{\text{Vehicle Price}}$.
   * Actuarial checks: Comparing the reported monthly installment against the theoretical amortization formula.
   * Regulatory logic: Validating the default status (`default_flag`) against the 90-day arrears threshold (`days_past_due >= 90`).
   * Professional logic: Ensuring consistency between employment status and job seniority (e.g., retirees with active seniority).
5. **Accuracy & Plausibility:** 
   * Flagging extreme business outliers, such as net incomes over €50,000, vehicle prices over €300,000, or abnormally high mileage for used cars (> 30,000 km/year).
6. **Freshness & Temporal Consistency:** 
   * Verifying that loan origination dates fall within the expected 2020–2022 production window.
   * Ensuring chronological logic, such as employment starting before the loan origination, and reconciling calculated career duration against declared job seniority.

## 🛠️ Technologies & Tools
* **Language:** R
* **Environment:** RStudio
* **Core Packages:** `tidyverse` (`dplyr`, `ggplot2`), `knitr` for executive summary tables.

## 📂 Repository Structure
* `Rapport_Projet_Lillian_Enzo.R`: The automated R audit script containing all data wrangling, validation flags, sensitivity analysis, and `ggplot2` visual reporting.
* `credit_auto_retail_europe.csv`: The audit dataset of retail auto credit files.
* `dictionnaire donnees_Projet_Etude_R.pdf`: The official data dictionary guiding the validity thresholds.
* `TDR_EnzoL_LillianC.pdf`: The final management report outlining the data quality assessment and risk impacts.

🌍 *Note: The final PDF report `TDR_EnzoL_LillianC.pdf` is written in French. However, I would be more than happy to discuss the methodology, the code, or the results in English! Feel free to reach out.*