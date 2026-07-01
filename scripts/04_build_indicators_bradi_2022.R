# =============================================================================
# BrADI: Brazilian Area Deprivation Index
# Script 04 — Build index: calculate indicators for all states using data from 
#             2022 demographic census, merge, transform, standardize, and save
# =============================================================================
# Description:
#   Runs build_indicators_2022() for all 27 Brazilian UFs, 
#   standardizes all indicators, and saves the final dataset.
# =============================================================================

source("scripts/00_setup.R")
source("scripts/01_functions.R")

# ---- 1. Calculate indicators for all UFs ------------------------------------

df <- build_indicators_2022()

# create variable "UF" to indentify the states
code_uf <- c(
  "11" = "RO", "12" = "AC", "13" = "AM", "14" = "RR", "15" = "PA",
  "16" = "AP", "17" = "TO", "21" = "MA", "22" = "PI", "23" = "CE",
  "24" = "RN", "25" = "PB", "26" = "PE", "27" = "AL", "28" = "SE",
  "29" = "BA", "31" = "MG", "32" = "ES", "33" = "RJ", "35" = "SP",
  "41" = "PR", "42" = "SC", "43" = "RS", "50" = "MS", "51" = "MT",
  "52" = "GO", "53" = "DF"
)

df <- df %>% 
  mutate(UF = code_uf[substr(Cod_setor, 1, 2)])
head(df, 15)

# ---- 2. Diagnostics ------------------------------------------------------------

# Proportion of NA per indicator 
df %>%
  summarise(across(everything(), ~ mean(is.na(.)) * 100)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "pct_na") %>%
  filter(pct_na > 0) %>%
  arrange(desc(pct_na)) %>%
  print()

# Checking variables with more than 20% of NA 
df %>%
  filter(is.na(prop_sem_esgoto)) %>%
  count(UF) %>%
  arrange(desc(n))

df %>%
  filter(is.na(prop_sem_coleta_lixo)) %>%
  count(UF) %>%
  arrange(desc(n))

df %>%
  filter(is.na(prop_sem_banheiro)) %>%
  count(UF) %>%
  arrange(desc(n))

# - expected 100% of NA values for: prop_sem_energia, renda_mensal_setor, prop_propriedade,
#  fam_extrema_pobreza, fam_linha_pobreza, prop_extrema_pobreza, prop_linha_pobreza
# - variables not available at 2022 (kept in the final "df" for cross census analyses)

# NaN check 
if (any(is.nan(as.matrix(select(df, where(is.numeric)))))) {
  warning("NaN values found in variables. Please check the data.")
} else {
  message("Final check: no NaN values found in variables.")
}

# Checking overall for NA, zeros, NaN, Inf, and summary statistics 
vars_check <- c(
  "prop_pess_15anos_analfab", "prop_sem_agua",
  "prop_sem_banheiro", "prop_sem_esgoto", "prop_sem_coleta_lixo",
  "renda_responsavel"
)

# Diagnostic per variable 
diagnostics <- sapply(df[vars_check], function(x) {
  c(
    n = length(x),
    n_NA = sum(is.na(x)),
    n_NaN = sum(is.nan(x)),
    n_Inf = sum(is.infinite(x)),
    n_zero = sum(x == 0, na.rm = TRUE),
    min = suppressWarnings(min(x, na.rm = TRUE)),
    max = suppressWarnings(max(x, na.rm = TRUE)),
    median = suppressWarnings(median(x, na.rm = TRUE)),
    mean = suppressWarnings(mean(x, na.rm = TRUE)),
    sd = suppressWarnings(sd(x, na.rm = TRUE))
  )
})
t(diagnostics)

# - expected 100% of NA values for: prop_sem_energia, renda_mensal_setor, prop_propriedade,
#  fam_extrema_pobreza, fam_linha_pobreza, prop_extrema_pobreza, prop_linha_pobreza
# - variables not available at 2022 (kept in the final "df" for cross census analyses)

# Convert into long_format 
df_long <- df %>%
  select(all_of(vars_check)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "value"
  )

# Histograms
ggplot(df_long, aes(x = value)) +
  geom_histogram(bins = 50) +
  facet_wrap(~ variable, scales = "free", ncol = 3) +
  scale_x_continuous(labels = scales::label_number()) +
  theme_minimal() +
  labs(
    title = "Distribution of BrADI variables (2022 Census)",
    x = "Value",
    y = "Frequency"
  )

# Number of households per census tract 
ggplot(df, aes(x = V00001)) +
  geom_histogram(bins = 100, fill = "steelblue", color = "white") +
  scale_x_log10() +                  
  labs(
    title = "Distribution of census tract sizes (number of households per tract)",
    x     = "V00001 — domicílios particulares permanentes (log scale)",
    y     = "Frequência"
  ) +
  theme_minimal()

# ---- 3. Log-transform skewed variables --------------------------------------
#
# One variable is heavily right-skewed:
# renda_responsavel
# Log(x + 1) is applied to reduce skew before standardization.
#
# renda_responsavel is sign-reversed so that
# higher values indicate greater deprivation (consistent with other indicators).

df <- df %>%
  mutate(renda_responsavel_log = -log(renda_responsavel + 1))

glimpse(df)

vars_check <- c(
  "prop_pess_15anos_analfab", "prop_sem_agua",
  "prop_sem_banheiro", "prop_sem_esgoto", "prop_sem_coleta_lixo",
  "renda_responsavel", "renda_responsavel_log"
)

# Convert into long_format 
df_long <- df %>%
  select(all_of(vars_check)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "value"
  )

# Histograms
ggplot(df_long, aes(x = value)) +
  geom_histogram(bins = 50) +
  facet_wrap(~ variable, scales = "free", ncol = 3) +
  scale_x_continuous(labels = scales::label_number()) +
  theme_minimal() +
  labs(
    title = "Distribution of BrADI variables (2022 Census)",
    x = "Value",
    y = "Frequency"
  )


# ---- 4. Transform + standardize ----------------------------------------------
vars_std_2022 <- c(
  "prop_pess_15anos_analfab", "prop_sem_agua",
  "prop_sem_banheiro", "prop_sem_esgoto", "prop_sem_coleta_lixo",
  "renda_responsavel_log"
)

df <- standardize_bradi(df, vars_to_standardize = vars_std_2022)
glimpse(df)

# Check distribution of the standardized variables 

vars_std_check <- c(
  "prop_pess_15anos_analfab_std", "prop_sem_agua_std",
  "prop_sem_banheiro_std", "prop_sem_esgoto_std", "prop_sem_coleta_lixo_std",
  "renda_responsavel_log_std"
)

#  Convert into long format to visualize the variables' distribution 
df_long <- df %>%
  select(all_of(vars_std_check)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "value"
  )

# Histograms
ggplot(df_long, aes(x = value)) +
  geom_histogram(bins = 50) +
  facet_wrap(~ variable, scales = "free", ncol = 3) +
  scale_x_continuous(labels = scales::label_number()) +
  scale_y_continuous(labels = scales::label_number()) +
  theme_minimal() +
  labs(
    title = "Distribution of the standardized BrADI variables (2022 Census)",
    x = "Value",
    y = "Frequency"
  )


# ---- 5. Save ------------------------------------------------------------------
write_csv(df, "data/processed/df_indicators_bradi_2022.csv")
message("Final dimensions: ", nrow(df), " sectors x ", ncol(df), " columns")
