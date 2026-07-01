# =============================================================================
# BrADI: Brazilian Area Deprivation Index
# Script 06 — Calculate BrADI (2010)
# =============================================================================
# Description:
#   Loads standardized indicators from the 2010 Census,
#   performs exploratory factor analysis (1-factor principal axis solution),
#   computes weighted BrADI scores,
#   generates a national BrADI map, and
#   evaluates convergent validity by correlating BrADI with the
#   Índice Brasileiro de Privação (IBP; Allik et al., 2025).
#
# Methodological note:
#   Factor loadings from a one-factor principal axis solution are used to
#   weight each standardized indicator. The composite score is then rescaled
#   to a mean of 100 and a standard deviation of 20 (same scale as the US ADI).
#   Higher BrADI scores indicate greater deprivation.
#
# Input:
#   data/processed/df_indicators_bradi_2010.csv
#   data/raw/census_2010/shape_files_estados_completo/
#   data/raw/census_2010/ibp_setor_censitario-1.csv
#
# Output:
#   data/processed/df_BrADI_2010.csv
#   results/fa_results_2010.csv
#   results/histogram_bradi_2010.png
#   results/continuous_map_bradi_2010.png
#   results/scatter_bradi_ibp_2010.png
#   results/correlation_bradi_ibp_2010.csv
# =============================================================================

source("scripts/00_setup.R") # load packages

# ---- 1. Load data -----------------------------------------------------------

df <- read_csv("data/processed/df_indicators_bradi_2010.csv",
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
  "prop_sem_banheiro_std", "prop_sem_esgoto_std", "prop_sem_energia_std", 
  "prop_sem_coleta_lixo_std","prop_extrema_pobreza_log_std", "prop_linha_pobreza_log_std",
  "renda_mensal_dom_log_std"
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

fa_table_2010 <- data.frame(
  variable           = rownames(fa_results$loadings),
  PA1                = round(as.numeric(fa_results$loadings[, 1]), 4),
  h2                 = round(fa_results$communalities, 4), # communality 
  u2                 = round(fa_results$uniquenesses,  4), # uniqueness
  variance_explained = round(fa_results$Vaccounted["Proportion Var", 1], 4),
  cronbach_alpha     = round(alpha_results$total$raw_alpha, 4)
)

write_csv(fa_table_2010, "results/fa_results_2010.csv")

# ---- 3. Compute BrADI score ------------------------------------------------

# Weight each standardized indicator by its factor loading
index_raw <- apply(indicators_std, 1, function(x) {
  sum(x * loadings, na.rm = TRUE)
})

# Rescale to mean = 100, SD = 20 (ADI convention)
df$BrADI_2010 <- as.numeric(scale(index_raw)) * 20 + 100

message("  Min: ", round(min(df$BrADI_2010, na.rm = TRUE), 1))
message("  Max: ", round(max(df$BrADI_2010, na.rm = TRUE), 1))
message("  Mean: ", round(mean(df$BrADI_2010, na.rm = TRUE), 1))

# ---- BrADI diagnostics ----------------------------------------------------- 

cat("\n=== BrADI summary ===\n")
summary(df$BrADI_2010)

png(
  "results/histogram_bradi_2010.png",
  width = 8,
  height = 6,
  units = "in",
  res = 300
)

hist(df$BrADI_2010,
     main   = "Distribution of BrADI - Year 2010",
     xlab   = "BrADI",
     col    = "steelblue",
     border = "white", 
     xlim = c(60,240), 
     xaxt   = "n")

axis(1, at = seq(60, 240, by = 20))

min_val <- round(min(df$BrADI_2010, na.rm = TRUE), 1)
max_val <- round(max(df$BrADI_2010, na.rm = TRUE), 1)
min_uf <- df$UF[which.min(df$BrADI_2010)]
max_uf <- df$UF[which.max(df$BrADI_2010)]

legend("topright", 
       legend = c(
         paste("Min:", min_val, "(", min_uf, ")"), 
         paste("Max:", max_val, "(", max_uf, ")")
       ), 
       bty = "n")

dev.off()

cat("\n=== Top 5 most deprived census tracts (highest BrADI) ===\n")
df %>%
  arrange(desc(BrADI_2010)) %>%
  select(UF, Cod_setor, BrADI_2010) %>%
  slice(1:5) %>%
  print()

cat("\n=== Top 5 least deprived census tracts (lowest BrADI) ===\n")
df %>%
  arrange(BrADI_2010) %>%
  select(UF, Cod_setor, BrADI_2010) %>%
  slice(1:5) %>%
  print()

cat("\n=== Mean BrADI by state (ordered by deprivation) ===\n")
df %>%
  group_by(UF) %>%
  summarise(BrADI_mean = mean(BrADI_2010, na.rm = TRUE)) %>%
  arrange(desc(BrADI_mean)) %>%
  print()

# ---- 4. Save the dataset with BrADI scores -----------------------------------

write_csv(df, "data/processed/df_BrADI_2010.csv")


# ---- 5. Map: continuous BrADI (national) -------------------------------------

message("Reading shapefiles...")

shp_files <- list.files(
  path = "data/raw/census_2010/shape_files_estados_completo",
  pattern = "\\.shp$",
  full.names = TRUE,
  recursive = TRUE
)

if (length(shp_files) == 0) {
  stop("No shapefile found in data/raw/census_2010/shapefiles/")
}

br_shp <- map_dfr(shp_files, st_read, quiet = TRUE) %>%
  mutate(Cod_setor = formatC(
    as.numeric(gsub(",", ".", as.character(CD_GEOCODI))),
    format = "fg", flag = "0", width = 15
  ))

# Merge BrADI scores
br_shp_BrADI <- br_shp %>%
  left_join(
    df %>% select(Cod_setor, BrADI_2010, UF),
    by = "Cod_setor"
  )

message("Census tracts in the shapefile: ", nrow(br_shp))
message("Census tracts with BrADI after the merge: ", sum(!is.na(br_shp_BrADI$BrADI)))

# Continuous map with 2nd–98th percentile squish for better contrast
lims <- quantile(br_shp_BrADI$BrADI_2010, c(0.02, 0.98), na.rm = TRUE)

continuous_map <- ggplot(br_shp_BrADI) +
  geom_sf(aes(fill = BrADI_2010), color = NA) +
  scale_fill_distiller(
    palette   = "RdYlBu",
    direction = -1,
    limits    = lims,
    oob       = scales::squish,
    na.value  = "grey90",
    name      = "BrADI"
  ) +
  labs(
    title    = "Spatial distribution of BrADI",
    subtitle = "Census tracts, 2010",
    caption  = ""
  ) +
  theme_minimal() +
  theme(panel.grid = element_blank())

continuous_map

ggsave(
  "results/continuous_map_bradi_2010.png",
  continuous_map,
  width  = 10,
  height = 12,
  dpi    = 300
)

# ---- 6. Convergent validity: BrADI vs IBP correlation ----------------------
# IBP: Indice Brasileiro de Privação
# IBP data source: Allik et al. (2025) — https://doi.org/10.23889/ijpds.v10i3.2974

ibp <- read_csv("data/raw/census_2010/ibp_setor_censitario-1.csv") %>% 
  mutate(Cod_setor = formatC(
    as.numeric(gsub(",", ".", as.character(Cod_setor))),
    format = "fg", flag = "0", width = 15
  ))

table(is.na(ibp$BrazDep_measure)) # Small-area deprivation measure for Brazil


# Merge BrADI and IBP by census tract
df_val <- df %>%
  select(Cod_setor, UF, BrADI_2010) %>%
  inner_join(ibp, by = "Cod_setor") %>% 
  select(Cod_setor, UF, BrADI_2010, BrazDep_measure)

message("Matched census tracts: ", nrow(df_val))
message("Census tracts without IBP match: ", nrow(df) - nrow(df_val))

# ---- Overall correlation ---------------------------------------------------

cor_overall <- cor.test(df_val$BrADI_2010, df_val$BrazDep_measure,
                        method = "pearson",
                        use    = "complete.obs")

# BrADI vs IBP: Pearson correlation 
message("  r = ", round(cor_overall$estimate, 3)) # r = 0.981
message("  95% CI: [", round(cor_overall$conf.int[1], 5), # 95% CI: [0.98081, 0.98108]
        ", ",          round(cor_overall$conf.int[2], 5), "]")
message("  p-value: ", format.pval(cor_overall$p.value, digits = 3)) #<0.001


# ---- Scatter plot ----------------------------------------------------------

scatter_bradi_ibp <- ggplot(df_val, aes(x = BrazDep_measure, y = BrADI_2010)) +
  geom_point(alpha = 0.1, size = 0.5, color = "steelblue") +
  geom_smooth(method = "lm", color = "darkred", linewidth = 0.8) +
  annotate("text",
           x = Inf, y = -Inf,
           hjust = 1.1, vjust = -1,
           label = paste0("r = ", round(cor_overall$estimate, 3),
                          "\nn = ", format(nrow(df_val), big.mark = ",")),
           size = 3.5) +
  labs(
    title    = "Pearson correlation: BrADI vs IBP (Year 2010)",
    subtitle = "Census tract level",
    x        = "IBP score",
    y        = "BrADI score",
    caption  = "IBP: Allik et al. (2025). Each point represents one census tract."
  ) +
  theme_minimal()

scatter_bradi_ibp

ggsave(
  "results/scatter_bradi_ibp_2010.png",
  scatter_bradi_ibp,
  width  = 8,
  height = 6,
  dpi    = 300
)

# ---- Save correlation results ----------------------------------------------

cor_results <- data.frame(
  comparison         = "BrADI vs IBP (Year 2010)",
  n_sectors          = nrow(df_val),
  pearson_r          = round(cor_overall$estimate, 4),
  ci_lower           = round(cor_overall$conf.int[1], 4),
  ci_upper           = round(cor_overall$conf.int[2], 4),
  p_value            = format.pval(cor_overall$p.value, digits = 3)
)

write_csv(cor_results, "results/correlation_bradi_ibp_2010.csv")

