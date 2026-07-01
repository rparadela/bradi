# =============================================================================
# BrADI: Brazilian Area Deprivation Index
# Script 05 — Calculate BrADI (2000)
# =============================================================================
# Description:
#   Loads standardized indicators from the 2000 Census,
#   performs exploratory factor analysis (1-factor principal axis solution),
#   computes weighted BrADI scores,
#   and generates summary diagnostics.
#
# Methodological note:
#   Factor loadings from a one-factor principal axis solution are used to
#   weight each standardized indicator. The composite score is then rescaled
#   to a mean of 100 and a standard deviation of 20 (same scale as the US ADI).
#   Higher BrADI scores indicate greater deprivation.
#
# Input:
#   data/processed/df_indicators_bradi_2000.csv
#
# Output:
#   data/processed/df_BrADI_2000.csv
#   results/fa_results_2000.csv
#   results/histogram_bradi_2000.png
# =============================================================================

source("scripts/00_setup.R") # load packages

# ---- 1. Load data -----------------------------------------------------------

df <- read_csv("data/processed/df_indicators_bradi_2000.csv",
               col_types = cols(Cod_setor = col_character()),
               show_col_types = FALSE)

# Select the standardized indicators

# prop_propriedade_std excluded from factor analysis / final BrADI:
# - 2000: loading = -0.03 (below |0.2| cutoff)
# - 2010: loading = -0.23, but low communality (0.051) and
#   counterintuitive direction 
# - 2022: not available 
# - Excluded across all years for consistency and comparability
# - Variable retained in the dataset (prop_propriedade_std) for reference
#   and potential use in future research

vars_std <- c(
  "prop_pess_15anos_analfab_std", "prop_sem_agua_std",
  "prop_sem_banheiro_std", "prop_sem_esgoto_std", "prop_sem_coleta_lixo_std",
  "renda_responsavel_log_std"
)

# ---- 2. Factor analysis (1 factor, PA extraction) ----------------------------

indicators_std <- df %>% select(all_of(vars_std))

# Scree plot — run interactively to check factor retention
scree(indicators_std, factors = FALSE)

# Exploratory factor analysis
fa_results <- fa(indicators_std, nfactors = 1, fm = "pa")

# Cronbach's alpha (internal consistency)
alpha_results <- alpha(indicators_std, na.rm = TRUE, check.keys = TRUE)

loadings <- fa_results$loadings[, 1]

print(round(loadings, 3)) # Factor loadings

fa_table_2000 <- data.frame(
  variable           = rownames(fa_results$loadings),
  PA1                = round(as.numeric(fa_results$loadings[, 1]), 4),
  h2                 = round(fa_results$communalities, 4), # communality 
  u2                 = round(fa_results$uniquenesses,  4), # uniqueness
  variance_explained = round(fa_results$Vaccounted["Proportion Var", 1], 4),
  cronbach_alpha     = round(alpha_results$total$raw_alpha, 4)
)

write_csv(fa_table_2000, "results/fa_results_2000.csv")

# ---- 3. Compute BrADI score ------------------------------------------------

# Weight each standardized indicator by its factor loading
index_raw <- apply(indicators_std, 1, function(x) {
  sum(x * loadings, na.rm = TRUE)
})

# Rescale to mean = 100, SD = 20 (ADI convention)
df$BrADI_2000 <- as.numeric(scale(index_raw)) * 20 + 100

message("  Min: ", round(min(df$BrADI_2000, na.rm = TRUE), 1))
message("  Max: ", round(max(df$BrADI_2000, na.rm = TRUE), 1))
message("  Mean: ", round(mean(df$BrADI_2000, na.rm = TRUE), 1))

# ---- BrADI diagnostics ----------------------------------------------------- 

cat("\n=== BrADI summary ===\n")
summary(df$BrADI_2000)

png(
  "results/histogram_bradi_2000.png",
  width = 8,
  height = 6,
  units = "in",
  res = 300
)

hist(df$BrADI_2000,
     main   = "Distribution of BrADI - Year 2000",
     xlab   = "BrADI",
     col    = "steelblue",
     border = "white", 
     xlim = c(60,240), 
     xaxt   = "n")

axis(1, at = seq(60, 240, by = 20))

min_val <- round(min(df$BrADI_2000, na.rm = TRUE), 1)
max_val <- round(max(df$BrADI_2000, na.rm = TRUE), 1)
min_uf <- df$UF[which.min(df$BrADI_2000)]
max_uf <- df$UF[which.max(df$BrADI_2000)]

legend("topright", 
       legend = c(
         paste("Min:", min_val, "(", min_uf, ")"), 
         paste("Max:", max_val, "(", max_uf, ")")
       ), 
       bty = "n")

dev.off()

cat("\n=== Top 5 most deprived census tracts (highest BrADI) ===\n")
df %>%
  arrange(desc(BrADI_2000)) %>%
  select(UF, Cod_setor, BrADI_2000) %>%
  slice(1:5) %>%
  print()

cat("\n=== Top 5 least deprived census tracts (lowest BrADI) ===\n")
df %>%
  arrange(BrADI_2000) %>%
  select(UF, Cod_setor, BrADI_2000) %>%
  slice(1:5) %>%
  print()

cat("\n=== Mean BrADI by state (ordered by deprivation) ===\n")
df %>%
  group_by(UF) %>%
  summarise(BrADI_mean = mean(BrADI_2000, na.rm = TRUE)) %>%
  arrange(desc(BrADI_mean)) %>%
  print()


# ---- 4. Save the dataset with BrADI scores -----------------------------------

write_csv(df, "data/processed/df_BrADI_2000.csv")
