# BrADI: Brazilian Area Deprivation Index

## Overview

The **Brazilian Area Deprivation Index (BrADI)** is a socioeconomic deprivation index developed using data from the **2000, 2010, and 2022 Brazilian Demographic Censuses (IBGE)**. BrADI quantifies area-level deprivation across Brazil using harmonized socioeconomic indicators aggregated at the census tract level.

A separate BrADI is computed for each census year using year-specific indicators and factor loadings, enabling both cross-sectional and longitudinal analyses of area-level deprivation in Brazil.

The index was developed to support:

- Epidemiological research
- Social determinants of health analyses
- Public policy and regional planning

BrADI follows international Area Deprivation Index (ADI) frameworks while adapting variable selection and methodology to the Brazilian context.

---

## Methodological Summary

### Census Years and Indicator Availability

Indicators were derived from three census years. Variable availability differs across years due to differences in census questionnaires and data suppression rules applied by IBGE. Indicators unavailable in a given year are included in the dataset as `NA` to preserve a consistent structure across years.

| Indicator | 2000 | 2010 | 2022 |
|---|:---:|:---:|:---:|
| Illiteracy rate (≥15 years) | ✓ | ✓ | ✓ |
| Homeownership | ✓ | ✓ | — |
| Lack of piped water | ✓ | ✓ | ✓ |
| Lack of bathroom/toilet | ✓ | ✓ | ✓ |
| Lack of electricity | — | ✓ | — |
| Extreme poverty | — | ✓ | — |
| Poverty line | — | ✓ | — |
| Lack of sewage treatment | ✓ | ✓ | ✓ |
| Lack of waste collection | ✓ | ✓ | ✓ |
| Household income | ✓ | ✓ | ✓ |

### Indicators

The table below describes each deprivation indicator, its variable name in the dataset, and its inclusion in the final BrADI score by census year.

| Indicator | Variable | Description | 2000 | 2010 | 2022 | Included in BrADI |
|---|---|---|:---:|:---:|:---:|:---:|
| Illiteracy rate (≥15 years) (%) | `prop_pess_15anos_analfab` | Proportion of people aged 15 or older who are illiterate in each census tract | ✓ | ✓ | ✓ | All years |
| Household income (R$) | `renda_mensal_dom` / `renda_responsavel` | Mean monthly nominal income of permanent private households (`renda_mensal_dom`) or household heads (`renda_responsavel`) per census tract | ✓ | ✓ | ✓ | 2000 and 2022 (`renda_responsavel`); 2010 (`renda_mensal_dom`) |
| Extreme poverty (%) | `prop_extrema_pobreza` | Proportion of permanent private households with per capita income ≤ 1/4 minimum wage | — | ✓ | — | 2010 only |
| Poverty line (%) | `prop_linha_pobreza` | Proportion of permanent private households with per capita income ≤ 1/2 minimum wage | — | ✓ | — | 2010 only |
| Homeownership (%) | `prop_propriedade` | Proportion of permanent private households that are not owner-occupied (rented or other tenure) | ✓ | ✓ | — | Not included* |
| Lack of piped water (%) | `prop_sem_agua` | Proportion of permanent private households without piped water supply from the general network | ✓ | ✓ | ✓ | All years |
| Lack of bathroom/toilet (%) | `prop_sem_banheiro` | Proportion of permanent private households without bathroom or toilet | ✓ | ✓ | ✓ | All years |
| Lack of sewage treatment (%) | `prop_sem_esgoto` | Proportion of permanent private households with bathroom or toilet but without adequate sewage treatment | ✓ | ✓ | ✓ | All years |
| Lack of electricity (%) | `prop_sem_energia` | Proportion of permanent private households without electricity | — | ✓ | — | 2010 only |
| Lack of adequate waste collection (%) | `prop_sem_coleta_lixo` | Proportion of permanent private households without waste collection by public cleaning service | ✓ | ✓ | ✓ | All years |

\* `prop_propriedade` was excluded from the final BrADI across all years: factor loading = −0.03 in 2000 (below the |0.20| cutoff) and −0.23 in 2010 (low communality = 0.051 and counterintuitive direction relative to other deprivation indicators. Not available in 2022. The variable is retained in the dataset for reference and potential use in future analyses.

### Additional Variables in the Dataset

The processed datasets include additional variables not used as deprivation indicators but available for other analyses:

| Variable | Description |
|---|---|
| `Cod_setor` | 15-digit census tract code (character, zero-padded) |
| `UF` | Brazilian state abbreviation (e.g., "SP", "MG") |
| `V0003` (2000) / `V002` (2010) / `V00001` (2022) | Number of permanent private households in the census tract |
| `situacao_urbana` | Urban/rural classification (1 = urban, 0 = rural) |

### Data Processing

- Variables with right-skewed distributions were log-transformed: `log(x + 1)`
- All indicators were standardized to z-scores: `z = (x − mean) / sd`
- Income variables (`renda_mensal_dom` or `renda_responsavel`) were sign-reversed so that higher values consistently indicate greater deprivation

### Variable Naming Conventions

Processed variables follow a suffix-based naming convention to identify transformations applied:

- **`_std`** suffix: variable was z-score standardized (e.g., `prop_pess_15anos_analfab_std`)
- **`_log_std`** suffix: variable was log-transformed and then z-score standardized (e.g., `prop_extrema_pobreza_log_std`, `renda_mensal_dom_log_std`)

The original (untransformed) variables are also retained in the processed datasets alongside their transformed versions.

### Factor Analysis

Exploratory factor analysis (EFA) was conducted using principal axis factoring because the indicators did not meet the assumption of multivariate normality. The number of factors retained was determined based on eigenvalues greater than one, visual inspection of the scree plot, and conceptual interpretability. A single-factor solution was retained. Variables with factor loadings < 0.20 or low communalities were excluded from the final index (e.g., `prop_propriedade_std`). The remaining indicators were weighted according to their factor loadings to compute the BrADI score.

Because indicator availability differs across years, factor loadings are not directly comparable between 2000, 2010, and 2022.

### Final Scoring

Each standardized indicator was weighted by its factor loading:

```
BrADI_raw = Σ(z_i × loading_i)
```

The raw score was then rescaled to:

```
BrADI = (standardized raw score × 20) + 100
```

### Interpretation

- Higher BrADI = greater deprivation
- Lower BrADI = lower deprivation
- Mean = 100, SD = 20 (same scale as the US ADI)
- Sectors with missing data on some indicators are still scored using the remaining available indicators (`na.rm = TRUE`)

---

## Project Structure

```text
BrADI/
│
├── README.md
│
├── scripts/
│   ├── 00_setup.R                          # Load required packages
│   ├── 01_functions.R                      # build_indicators_2000/2010/2022() + standardize_bradi()
│   ├── 02_build_indicators_bradi_2000.R    # Calculate, transform, standardize (2000)
│   ├── 03_build_indicators_bradi_2010.R    # Calculate, transform, standardize (2010)
│   ├── 04_build_indicators_bradi_2022.R    # Calculate, transform, standardize (2022)
│   ├── 05_calculate_bradi_2000.R           # Factor analysis and BrADI scoring (2000)
│   ├── 06_calculate_bradi_2010.R           # Factor analysis, scoring, map, validation (2010)
│   └── 07_calculate_bradi_2022.R           # Factor analysis and BrADI scoring (2022)
│
├── data/
│   ├── raw/                                # Not included — see Raw Data section below
│   │   ├── census_2000/
│   │   │   └── setor_censitario/           # XLS files by UF (SP split into SP1/SP2)
│   │   ├── census_2010/
│   │   │   ├── setor_censitario/           # CSV + EXCEL files by UF (SP split into SP1/SP2)
│   │   │   ├── shape_files_estados_completo/  # Census tract shapefiles (2010)
│   │   │   └── ibp_setor_censitario-1.csv  # IBP — Allik et al. (2025), used for validation
│   │   └── census_2022/
│   │       └── setor_censitario/           # CSV files, national (all UFs in single files)
│   │
│   └── processed/
│       ├── df_indicators_bradi_2000.csv    # Indicators + standardized variables (2000)
│       ├── df_indicators_bradi_2010.csv    # Indicators + standardized variables (2010)
│       ├── df_indicators_bradi_2022.csv    # Indicators + standardized variables (2022)
│       ├── df_BrADI_2000.csv              # Final BrADI scores (2000)
│       ├── df_BrADI_2010.csv              # Final BrADI scores (2010)
│       └── df_BrADI_2022.csv              # Final BrADI scores (2022)
│
├── docs/
│   ├── dicionario de variaveis por setor censitario_2000.pdf            # IBGE variable dictionary (2000 Census)
│   ├── dicionario de variaveis por setor censitario_2010.pdf            # IBGE variable dictionary (2010 Census)
│   ├── dicionario_de_dados_agregados_por_setores_censitarios_20260520_2022.xlsx  # IBGE variable dictionary (2022 Census)
│   └── dicionario_de_dados_renda_responsavel_20260508.xlsx              # IBGE income variable dictionary (2022)
│
└── results/
    ├── fa_results_2000.csv                 # Factor analysis results (2000)
    ├── fa_results_2010.csv                 # Factor analysis results (2010)
    ├── fa_results_2022.csv                 # Factor analysis results (2022)
    ├── histogram_bradi_2000.png            # BrADI score distribution (2000)
    ├── histogram_bradi_2010.png            # BrADI score distribution (2010)
    ├── histogram_bradi_2022.png            # BrADI score distribution (2022)
    ├── continuous_map_bradi_2010.png       # National choropleth map (2010)
    ├── scatter_bradi_ibp_2010.png          # Convergent validity scatter plot
    └── correlation_bradi_ibp_2010.csv      # BrADI vs IBP correlation results
```

### Analytical Flow

```text
Raw Census Data (2000 / 2010 / 2022)
        ↓
Indicator Calculation [01_functions.R + scripts 02/03/04]
        ↓
Log-transform + Standardization [01_functions.R + scripts 02/03/04]
        ↓
Factor Analysis + BrADI Scoring [scripts 05/06/07]
        ↓
Map + Convergent Validation [script 06, 2010 only]
```

---

## Script Descriptions

### `00_setup.R`
Loads all required R packages for the pipeline.

### `01_functions.R`
Defines the core functions used throughout the pipeline:
- `build_indicators_2000()` — reads XLS files, computes indicators for a given UF
- `build_indicators_2010()` — reads CSV files with automatic XLS fallback for UFs with known `Cod_setor` precision issues
- `build_indicators_2022()` — reads national CSV files (all UFs in a single set of files), returns the full Brazil dataset
- `standardize_bradi()` — z-score standardization of indicators (shared across all three years)

### `02_build_indicators_bradi_2000.R` / `03_build_indicators_bradi_2010.R` / `04_build_indicators_bradi_2022.R`
For each census year: runs the indicator function for all UFs, merges results, performs diagnostics, applies log-transforms, standardizes, and saves `df_indicators_bradi_<year>.csv`.

### `05_calculate_bradi_2000.R` / `07_calculate_bradi_2022.R`
Loads standardized indicators, runs exploratory factor analysis, computes weighted BrADI scores, rescales to mean = 100 / SD = 20, generates the BrADI score distribution histogram, and saves `df_BrADI_<year>.csv`.

### `06_calculate_bradi_2010.R`
Same as above for 2010, plus: generates a national choropleth map and tests convergent validity against the IBP (Allik et al., 2025) via Pearson correlation.

---

## Key Technical Notes

### Census sector codes (`Cod_setor`)
All sector codes are preserved as 15-digit zero-padded character strings to prevent loss of precision during numeric conversion. The first two digits identify the state (UF).

### São Paulo split (2000 and 2010)
IBGE distributes São Paulo data across two folders (`SP1` and `SP2`) in the 2000 and 2010 censuses. The indicator functions must be called separately for each (`uf = "SP1"` and `uf = "SP2"`), and results are combined via `bind_rows()` before further processing.

### 2022 census: national files
Unlike 2000 and 2010, the 2022 census data are distributed as national files (one per topic, covering all states). `build_indicators_2022()` reads these files directly and returns data for all of Brazil. UF identification is derived from `Cod_setor` using the first two digits.

### IBGE data correction (15/06/2026)
IBGE issued a correction to all 2010 census sector CSV files on 15/06/2026, fixing corruption in the `Cod_setor` column. All 2010 data used in this project were downloaded after this correction. Additionally, the separator used in São Paulo files (`SP2`) changed from comma (`,`) to semicolon (`;`) as part of this update.

### XLS fallback (2010)
Some UFs in 2010 (DF, MG, PE, RS) had `Entorno01` CSV files where `Cod_setor` was stored in European scientific notation with insufficient precision. `read_entorno()` automatically falls back to the XLS file for these UFs when truncation is detected. As of the IBGE correction of 15/06/2026, this issue was confirmed resolved (0% truncation across all affected UFs). The fallback is retained as a safeguard for reproducibility on pre-correction files.

### Missing data
`NA` values arise from two sources: (1) sectors with zero or suppressed households (IBGE uses `"X"` as a code for statistically suppressed data); and (2) indicators not available for a given census year. Sectors with missing data on some indicators are still scored using the remaining available indicators (`na.rm = TRUE` in score calculation).

---

## BrADI Score Distribution

Histograms of BrADI score distributions for each census year are available in `results/`:

- `histogram_bradi_2000.png`
- `histogram_bradi_2010.png` 
- `histogram_bradi_2022.png`

---

## Validation

Convergent validity of BrADI 2010 was assessed by Pearson correlation with the **Índice Brasileiro de Privação (IBP)** (Allik et al., 2025): https://doi.org/10.23889/ijpds.v10i3.2974

Results are saved in `results/correlation_bradi_ibp_2010.csv`.

---

## Raw Data

Raw census data are not included in this repository due to file size. Data can be downloaded from:

- **Census data (2000, 2010, 2022)**: https://www.ibge.gov.br/estatisticas/sociais/populacao/9663-censo-demografico  
  Navigate to the desired census year and select *Agregados por Setores Censitários*
- **Shapefiles (2010)**: https://geoftp.ibge.gov.br/organizacao_do_territorio/malhas_territoriais/
- **IBP (BrazDep)**: Available at https://cidacs.bahia.fiocruz.br/ibp/ 

---

## Funding

This project was supported by a grant from the **Global Brain Health Institute (GBHI), Alzheimer's Association, Alzheimer's Society** [grant number: GBHI ALZ UK-25-1289657].

---

## License

The code in this repository is licensed under the **MIT License** — see [LICENSE](LICENSE) for details.

The BrADI index values and derived datasets are made available for research and non-commercial use. If you use BrADI in your research, please cite as indicated below.

---

## Citation

If using BrADI, please cite: XXXXX