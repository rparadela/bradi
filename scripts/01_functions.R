# =============================================================================
# BrADI: Brazilian Area Deprivation Index
# Script: functions
# =============================================================================
#
# Description:
# Functions to compute BrADI indicators for all Brazilian states
# using variables from the Brazilian demographic censuses:
#   - 2000 Census: build_indicators_2000()
#   - 2010 Census: build_indicators_2010()
#   - 2022 Census: build_indicators_2022()
#
# Additional functions:
#
# Helper functions:
#   - normalize_census_tract_code():
#     Reads census sector codes as character strings to prevent loss of
#     precision and incomplete codes. Used in all three census years
#     (2000, 2010, 2022).
#
#   - standardize_census_tract_name():
#     Standardizes sector code variable names to ensure that the final
#     data frames contain a common identifier (Cod_setor). Used in 2000
#     and 2022. 
#
#   - read_census():
#     Reads census files, replaces "X" values with NA, and applies
#     normalize_census_tract_code() in all three census years. 
#     Applies standardize_census_tract_name() only in 2000 and 2022. 
#
#   - read_entorno():
#     read Entorno01 file from 2010 census with automatic XLS fallback
#
#   - census_tract_code_truncated():
#     check if Cod_setor is truncated (lost precision in scientific notation)
#     for some files in 2010 census 
#
# Indicator standardization:
#   - standardize_bradi():
#     Standardizes BrADI indicators using z-scores.
#
# Notes:
#   - São Paulo is split into two folders/files in the 2000 and 2010 censuses
#     ("SP1" and "SP2"). Functions should be called separately for each file.
#
#   - For 2000 census, the names of the folders containing data for the capital 
#     of São Paulo (SP1) and the State of São Paulo, excluding the capital 
#     (SP2), were renamed to:
#     Agregado_de_setores_2000_SP1 
#     Agregado_de_setores_2000_SP2
#     
#     The same was done for 2010 census: 
#     Base informa‡oes setores2010 universo SP1 
#     Base informa‡oes setores2010 universo SP2
#
#   - Cod_setor is always read as a character variable to prevent automatic
#     numeric conversion and loss of precision.
#
#   - Files from the 2000 Census are available only in Excel format; therefore,
#     read_census() uses read_xls(). Files from the 2010 and 2022 censuses are
#     read from .csv files.
#
#   - NaN values resulting from 0/0 divisions (e.g., sectors with no households
#     or missing data) are converted to NA. During BrADI calculation, sectors
#     are scored using the remaining available indicators (na.rm = TRUE).
#
#   - Ten candidate deprivation indicators were derived from the 2010 Census.
#     To harmonize data across the 2000, 2010, and 2022 censuses, the same
#     indicator structure was retained for all years. Indicators unavailable
#     in a given census year were created and assigned NA values, preserving
#     a consistent data structure and enabling the construction of both
#     census-specific and harmonized versions of the BrADI.
#
# =============================================================================

# ---- Function: build_indicators_2000 --------------------------------------

#' Calculate BrADI indicators for a given Brazilian state (UF) using data from the 
#' demographic census of 2000
#'
#' @param uf       Two-letter state code (e.g. "SP1", "SP2", "MG", "BA").
#'                 SP is split into "SP1" and "SP2"; call separately and bind_rows().
#' @param base_dir Path to the root directory containing UF folders
#'
#' @return         A data frame with one row per census sector and columns:
#'                 Cod_setor, V0003 (number of households), UF, and 
#'                 the deprivation indicators.

build_indicators_2000 <- function(uf,
                                     base_dir = "data/raw/census_2000/setor_censitario") {
  
  # -- Locate UF folder --------------------------------------------------------
  state_dirs       <- list.dirs(base_dir, recursive = FALSE, full.names = TRUE)
  state_dir <- state_dirs[grepl(paste0("_", uf, "$"), state_dirs)]
  
  if (length(state_dir) == 0) {
    stop("State directory not found: ", uf)
  }
  
  data_dir <- state_dir
  
  # Helper: normalize Cod_setor to 15-digit zero-padded string
  normalize_census_tract_code <- function(df) {
    df %>%
      mutate(Cod_setor = formatC(
        as.numeric(gsub(",", ".", as.character(Cod_setor))),
        format = "fg", flag = "0", width = 15
      ))
  }
  
  # Helper: standardizes the name of the sector code column to "Cod_setor"
  standardize_census_tract_name <- function(df) {
    possible_names <- c("CD_SETOR","CD_setor", "setor", "Cod_setor")
    matched_name <- intersect(possible_names, names(df))
    
    if (length(matched_name) == 0) {
      stop("No census tract code column found. Available columns: ",
           paste(names(df), collapse = ", "))
    }
    
    df %>% rename(Cod_setor = all_of(matched_name[1]))
  }
  
  # Helper: read_census for XLS files — standardizes name + normalizes code
  read_census <- function(file) {
    if (!file.exists(file)) stop("File not found: ", file)
    
    read_xls(file, col_types = "text") %>%
      mutate(across(everything(), ~ na_if(.x, "X"))) %>%
      standardize_census_tract_name() %>%
      normalize_census_tract_code()
  }
  
  # -- Instrucao1: literacy (>= 15 years) ---------------------------------------
  instrucao1 <- read_census(
    file.path(data_dir, paste0("Instrucao1_", uf, ".XLS"))
  )
  
  cols_alfab <- paste0("V", 2260:2325) # Literate people aged 15 or older
  instrucao1 <- instrucao1 %>%
    mutate(across(all_of(cols_alfab), as.numeric)) %>%
    mutate(pess_alfab_15anos = rowSums(across(all_of(cols_alfab)), na.rm = TRUE)) %>%
    select(Cod_setor, pess_alfab_15anos)
  
  # -- Pessoa1: total number of people >= 15 years -------------------------------
  pessoa1 <- read_census(
    file.path(data_dir, paste0("Pessoa1_", uf, ".XLS"))
  )
  
  cols_pop <- paste0("V", 1362:1447) # all people aged 15 or older
  pessoa1 <- pessoa1 %>%
    mutate(across(all_of(cols_pop), as.numeric)) %>%
    mutate(pess_15anos = rowSums(across(all_of(cols_pop)), na.rm = TRUE)) %>%
    select(Cod_setor, pess_15anos)
  
  # -- Indicator: proportion of illiterate people >= 15 years --------------------
  pessoa <- instrucao1 %>%
    left_join(pessoa1, by = "Cod_setor") %>%
    mutate(
      prop_pess_15anos_analfab = (pess_15anos - pess_alfab_15anos) / pess_15anos,
      prop_pess_15anos_analfab = if_else(is.nan(prop_pess_15anos_analfab) | is.infinite(prop_pess_15anos_analfab),
                                         NA_real_, prop_pess_15anos_analfab)
    )
  
  # -- Domicilio: housing conditions ---------------------------------------------
  domicilio <- read_census(
    file.path(data_dir, paste0("Domicilio_", uf, ".XLS"))
  )
  
  vars_dom <- c("V0003", "V0009", "V0010", "V0018", "V0030", "V0031", "V0036", "V0049", "V0050")
  domicilio <- domicilio %>%
    mutate(across(all_of(vars_dom), as.numeric)) %>%
    mutate(
      prop_propriedade     = 1 - ((V0009 + V0010) / V0003),
      prop_sem_agua        = 1 - (V0018 / V0003),
      prop_sem_banheiro    = V0036 / V0003,
      prop_sem_esgoto      = 1 - ((V0030 + V0031) / V0003),
      prop_sem_energia     = NA_real_,                      # not available in 2000
      prop_sem_coleta_lixo = 1 - ((V0049 + V0050) / V0003)
    ) %>%
    mutate(across(starts_with("prop_"), ~ if_else(is.nan(.) | is.infinite(.), NA_real_, .))) %>%
    select(Cod_setor, V0003,
           prop_propriedade, prop_sem_agua, prop_sem_banheiro, prop_sem_esgoto,
           prop_sem_energia, prop_sem_coleta_lixo)
  
  # -- Responsavel1: income of household heads ------------------------------------
  # renda_mensal_setor, fam_extrema_pobreza, fam_linha_pobreza, prop_extrema_pobreza,
  # prop_linha_pobreza: not available in 2000, kept as NA for cross-census
  # comparability with 2010.
  responsavel_renda <- read_census(
    file.path(data_dir, paste0("Responsavel1_", uf, ".XLS"))
  )
  
  vars_responsavel_renda <- c("V0623", "V0402")
  responsavel_renda <- responsavel_renda %>%
    mutate(across(all_of(vars_responsavel_renda), as.numeric)) %>%
    mutate(
      renda_responsavel    = V0623 / V0402,
      renda_responsavel = if_else(is.nan(renda_responsavel) | is.infinite(renda_responsavel),
                                  NA_real_, renda_responsavel),
      renda_mensal_setor    = NA_real_,
      fam_extrema_pobreza   = NA_real_,
      fam_linha_pobreza     = NA_real_,
      prop_extrema_pobreza  = NA_real_,
      prop_linha_pobreza    = NA_real_
    ) %>%
    select(Cod_setor, renda_responsavel, renda_mensal_setor, fam_extrema_pobreza,
           fam_linha_pobreza, prop_extrema_pobreza, prop_linha_pobreza)
  
  # -- Basico: urban situation -----------------------------------------------------
  entorno01 <- read_census(
    file.path(data_dir, paste0("Basico_", uf, ".XLS"))
  ) %>%
    mutate(
      situacao_urbana = if_else(as.integer(Situacao) <= 3, 1L, 0L)
    ) %>%
    select(Cod_setor, situacao_urbana)
  
  # -- Final Merge -------------------------------------------------------------
  final <- pessoa %>%
    select(Cod_setor, prop_pess_15anos_analfab) %>%
    left_join(domicilio, by = "Cod_setor") %>%
    left_join(responsavel_renda, by = "Cod_setor") %>%
    left_join(entorno01, by = "Cod_setor") %>%
    mutate(UF = uf)
  
  return(final)
}


# ---- Function: build_indicators_2010 -----------------------------------------

#' Calculate BrADI indicators for a given Brazilian state (UF) using data from the demographic census of 2010
#'
#' @param uf         Two-letter state code (e.g. "SP1", "SP2", "MG", "BA").
#'                   SP is split into "SP1" and "SP2"; call separately and bind_rows().
#' @param base_dir   Path to the root directory containing UF folders
#' @param sep_pessoa Separator used in the files.
#'                   The ";" is the default.
#'
#' @return           A data frame with one row per census sector and columns:
#'                   Cod_setor, V002 (number of households), UF, and 
#'                   the deprivation indicators.

build_indicators_2010 <- function(uf,
                                     base_dir   = "data/raw/census_2010/setor_censitario",
                                     sep_pessoa = ";") {
  
  # -- Locate UF folder --------------------------------------------------------
  state_dirs       <- list.dirs(base_dir, recursive = FALSE, full.names = TRUE)
  state_dir <- state_dirs[grepl(paste0(" ", uf, "$"), state_dirs)]
  
  if (length(state_dir) == 0) {
    stop("State directory not found: ", uf)
  }
  
  data_dir  <- file.path(state_dir, "csv")
  excel_dir <- file.path(state_dir, "EXCEL")
  
  # Helper: normalize Cod_setor to 15-digit zero-padded string
  normalize_census_tract_code <- function(df) {
    df %>%
      mutate(Cod_setor = formatC(
        as.numeric(gsub(",", ".", as.character(Cod_setor))),
        format = "fg", flag = "0", width = 15
      ))
  }
  
  # Helper: check if Cod_setor is truncated (lost precision in scientific notation)
  census_tract_code_truncated <- function(df) {
    exemplos <- head(df$Cod_setor, 20)
    mean(grepl("0{6}$", exemplos)) > 0.5  # >50% ending in 6+ zeros = truncated
  }
  
  # Helper: read csv with explicit separator, always reading Cod_setor as character
  read_census <- function(file, sep = ";", encoding = "UTF-8") {
    if (!file.exists(file)) stop("File not found: ", file)
    
    if (sep == ";") {
      read_csv2(file,
                locale = locale(encoding = encoding),
                col_types = cols(Cod_setor = col_character()),
                show_col_types = FALSE)
    } else {
      read_csv(file,
               locale = locale(encoding = encoding),
               col_types = cols(Cod_setor = col_character()),
               show_col_types = FALSE)
    }
  }
  
  # Helper: read Entorno01 with automatic XLS fallback
  # Some UFs (DF, MG, PE, RS) had Entorno01 CSV files where Cod_setor is
  # stored with insufficient precision in scientific notation. For these,
  # the XLS file in the EXCEL subfolder contains the correct codes.
  read_entorno <- function() {
    file_csv <- file.path(data_dir,  paste0("Entorno01_", uf, ".csv"))
    file_xls <- file.path(excel_dir, paste0("Entorno01_", uf, ".XLS"))
    
    if (file.exists(file_csv)) {
      df <- read_census(file_csv, encoding = "Latin1") %>%
        normalize_census_tract_code()
      
      if (census_tract_code_truncated(df)) {
        message("  Entorno01_", uf, ".csv: Cod_setor truncated — using XLS file.")
        
        if (!file.exists(file_xls)) {
          stop("XLS file not found: ", file_xls)
        }
        
        df <- read_xls(file_xls, col_types = "text") %>%
          mutate(across(everything(), ~ na_if(.x, "X"))) %>%
          normalize_census_tract_code()
      }
      
      return(df)
    }
    
    if (file.exists(file_xls)) {
      message("  Entorno01_", uf, ".csv not found — using XLS file.")
      return(
        read_xls(file_xls, col_types = "text") %>%
          mutate(across(everything(), ~ na_if(.x, "X"))) %>%
          normalize_census_tract_code()
      )
    }
    
    stop("Entorno01 not found for ", uf, " (neither CSV nor XLS).")
  }
  
  # -- Pessoa01: literacy (>= 15 years) ----------------------------------------
  pessoa01 <- read_census(
    file.path(data_dir, paste0("Pessoa01_", uf, ".csv")),
    sep = sep_pessoa
  ) %>%
    normalize_census_tract_code()
  
  cols_alfab <- paste0("V0", 12:77) # Literate people aged 15 or older
  pessoa01 <- pessoa01 %>%
    mutate(across(all_of(cols_alfab), as.numeric)) %>%
    mutate(pess_alfab_15anos = rowSums(across(all_of(cols_alfab)), na.rm = TRUE)) %>%
    select(Cod_setor, pess_alfab_15anos)
  
  # -- Pessoa13: total number of people >= 15 years ----------------------------
  pessoa13 <- read_census(
    file.path(data_dir, paste0("Pessoa13_", uf, ".csv")),
    sep = sep_pessoa
  ) %>%
    normalize_census_tract_code()
  
  cols_pop <- paste0("V", sprintf("%03d", 49:134)) # all people aged 15 or older
  pessoa13 <- pessoa13 %>%
    mutate(across(all_of(cols_pop), as.numeric)) %>%
    mutate(pess_15anos = rowSums(across(all_of(cols_pop)), na.rm = TRUE)) %>%
    select(Cod_setor, pess_15anos)
  
  # -- Indicator: proportion of illiterate people >= 15 years ------------------
  pessoa <- pessoa01 %>%
    left_join(pessoa13, by = "Cod_setor") %>%
    mutate(
      prop_pess_15anos_analfab = (pess_15anos - pess_alfab_15anos) / pess_15anos,
      prop_pess_15anos_analfab = if_else(is.nan(prop_pess_15anos_analfab) | is.infinite(prop_pess_15anos_analfab),
                                         NA_real_, prop_pess_15anos_analfab)
    )
  
  # -- Domicilio01: housing conditions -----------------------------------------
  domicilio01 <- read_census(
    file.path(data_dir, paste0("Domicilio01_", uf, ".csv"))
  ) %>%
    normalize_census_tract_code()
  
  vars_dom <- c("V002", "V006", "V007", "V012", "V017", "V018", "V023", "V036", "V037", "V046")
  domicilio01 <- domicilio01 %>%
    mutate(across(all_of(vars_dom), as.numeric)) %>%
    mutate(
      prop_propriedade     = 1 - ((V006 + V007) / V002),
      prop_sem_agua        = 1 - (V012 / V002),
      prop_sem_banheiro    = V023 / V002,
      prop_sem_esgoto      = 1 - ((V017 + V018) / V002),
      prop_sem_energia     = V046 / V002,
      prop_sem_coleta_lixo = 1 - ((V036 + V037) / V002)
    ) %>%
    mutate(across(starts_with("prop_"), ~ if_else(is.nan(.) | is.infinite(.), NA_real_, .)))
  
  # -- DomicilioRenda: income and poverty --------------------------------------
  domicilio_renda <- read_census(
    file.path(data_dir, paste0("DomicilioRenda_", uf, ".csv"))
  ) %>%
    normalize_census_tract_code()
  
  vars_renda <- c("V003", "V005", "V006", "V007", "V014")
  domicilio_renda <- domicilio_renda %>%
    mutate(across(all_of(vars_renda), as.numeric)) %>%
    mutate(
      fam_extrema_pobreza = V005 + V006 + V014,           # <= 1/4 minimum wage per capita
      fam_linha_pobreza   = V005 + V006 + V007 + V014,    # <= 1/2 minimum wage per capita
      renda_mensal_setor  = V003
    ) %>%
    select(Cod_setor, fam_extrema_pobreza, fam_linha_pobreza, renda_mensal_setor)
  
  # -- Domicilio (final): housing + income-derived proportions -----------------
  domicilio <- domicilio01 %>%
    left_join(domicilio_renda, by = "Cod_setor") %>%
    mutate(
      prop_extrema_pobreza = fam_extrema_pobreza / V002,
      prop_linha_pobreza   = fam_linha_pobreza   / V002,
      renda_mensal_dom     = renda_mensal_setor  / V002,
      prop_extrema_pobreza = if_else(is.nan(prop_extrema_pobreza) | is.infinite(prop_extrema_pobreza),
                                     NA_real_, prop_extrema_pobreza),
      prop_linha_pobreza   = if_else(is.nan(prop_linha_pobreza) | is.infinite(prop_linha_pobreza),
                                     NA_real_, prop_linha_pobreza),
      renda_mensal_dom     = if_else(is.nan(renda_mensal_dom) | is.infinite(renda_mensal_dom),
                                     NA_real_, renda_mensal_dom)
    ) %>%
    select(Cod_setor, V002,
           prop_propriedade, prop_sem_agua, prop_sem_banheiro, prop_sem_esgoto,
           prop_sem_energia, prop_sem_coleta_lixo,
           renda_mensal_dom, prop_extrema_pobreza, prop_linha_pobreza)
  
  # -- ResponsavelRenda: income of household heads -----------------------------
  responsavel_renda <- read_census(
    file.path(data_dir, paste0("ResponsavelRenda_", uf, ".csv"))
  ) %>%
    normalize_census_tract_code()
  
  vars_responsavel_renda <- c("V022", "V020")
  responsavel_renda <- responsavel_renda %>%
    mutate(across(all_of(vars_responsavel_renda), as.numeric)) %>%
    mutate(
      renda_responsavel = V022 / V020, 
      renda_responsavel = if_else(is.nan(renda_responsavel) | is.infinite(renda_responsavel),
                                    NA_real_, renda_responsavel)) %>%
    select(Cod_setor, renda_responsavel)
  
  # -- Entorno01: urban situation -----------------------------------------------
  # Uses read_entorno() which automatically falls back to XLS if CSV has
  # truncated Cod_setor values (known issue in DF, MG, PE, RS).
  entorno01 <- read_entorno() %>%
    mutate(
      situacao_urbana = if_else(as.integer(Situacao_setor) <= 3, 1L, 0L)
    ) %>%
    select(Cod_setor, situacao_urbana)
  
  # -- Final Merge -------------------------------------------------------------
  final <- pessoa %>%
    select(Cod_setor, prop_pess_15anos_analfab) %>%
    left_join(domicilio, by = "Cod_setor") %>%
    left_join(responsavel_renda, by = "Cod_setor") %>%
    left_join(entorno01, by = "Cod_setor") %>%
    mutate(UF = uf)
  
  return(final)
}

# ---- Function: build_indicators_2022 -----------------------------------------

#' Calculate BrADI indicators for all Brazilian states using data from the demographic census of 2022
#'
#' @param base_dir   Path to the root directory containing UF folders
#' @param sep_pessoa Separator used in the files.
#'                    The ";" is the default.
#'
#' @return           A data frame with one row per census sector and columns:
#'                   Cod_setor, V00001 (number of households), and 
#'                   the deprivation indicators.

build_indicators_2022 <- function(base_dir   = "data/raw/census_2022/setor_censitario",
                                     sep_pessoa = ";") {
  
  # -- Locate folder --------------------------------------------------------
  data_dir <- base_dir
  
  if (!dir.exists(data_dir)) {
    stop("Data directory not found: ", data_dir)
  }
  
  # Helper: normalize Cod_setor to 15-digit zero-padded string
  normalize_census_tract_code <- function(df) {
    df %>%
      mutate(Cod_setor = formatC(
        as.numeric(gsub(",", ".", as.character(Cod_setor))),
        format = "fg", flag = "0", width = 15
      ))
  }
  
  # Helper: standardizes the name of the sector code column to "Cod_setor"
  standardize_census_tract_name <- function(df) {
    possible_names <- c("CD_SETOR","CD_setor", "setor", "Cod_setor")
    matched_name <- intersect(possible_names, names(df))
    
    if (length(matched_name) == 0) {
      stop("No census tract code column found. Available columns: ",
           paste(names(df), collapse = ", "))
    }
    
    df %>% rename(Cod_setor = all_of(matched_name[1]))
  }
  
  # Helper: read csv — standardizes name + normalizes code 
  read_census <- function(file, sep = ";", encoding = "UTF-8") {
    if (!file.exists(file)) stop("File not found: ", file)
    
    df <- if (sep == ";") {
      read_csv2(file, locale = locale(encoding = encoding), show_col_types = FALSE)
    } else {
      read_csv(file, locale = locale(encoding = encoding), show_col_types = FALSE)
    }
    
    df %>%
      standardize_census_tract_name() %>%
      normalize_census_tract_code()
  }
  
  # -- Pessoa: literacy (>= 15 years) -------------------------------------------
  pessoa <- read_census(
    file.path(data_dir, "Agregados_por_setores_alfabetizacao_BR.csv"),
    sep = sep_pessoa
  )
  
  cols_alfab <- paste0("V00", 748:760) # Literate people aged 15 or older
  cols_pop   <- paste0("V00", 644:656) # all people aged 15 or older
  pessoa <- pessoa %>%
    mutate(across(all_of(c(cols_alfab, cols_pop)), as.numeric)) %>%
    mutate(
      pess_alfab_15anos = rowSums(across(all_of(cols_alfab)), na.rm = TRUE),
      pess_15anos       = rowSums(across(all_of(cols_pop)), na.rm = TRUE),
      prop_pess_15anos_analfab = (pess_15anos - pess_alfab_15anos) / pess_15anos,
      prop_pess_15anos_analfab = if_else(is.nan(prop_pess_15anos_analfab) | is.infinite(prop_pess_15anos_analfab),
                                         NA_real_, prop_pess_15anos_analfab)
    ) %>%
    select(Cod_setor, pess_alfab_15anos, pess_15anos, prop_pess_15anos_analfab)
  
  # -- Domicilio: housing conditions ---------------------------------------------
  # prop_propriedade and prop_sem_energia:
  # not available in 2022 at tract census level, 
  # kept as NA for cross-census comparability.
  domicilio01 <- read_census(
    file.path(data_dir, "Agregados_por_setores_caracteristicas_domicilio1_BR.csv")
  ) %>%
    select(Cod_setor, V00001)
  
  domicilio02 <- read_census(
    file.path(data_dir, "Agregados_por_setores_caracteristicas_domicilio2_BR_20250417.csv")
  ) %>%
    select(Cod_setor, V00111, V00238, V00236, V00309, V00310, V00311, V00397, V00398)
   
  domicilio <- domicilio01 %>%
    left_join(domicilio02, by = "Cod_setor")
  
  vars_dom <- c("V00001", "V00111", "V00236", "V00238", "V00309", "V00310", "V00311", "V00397", "V00398")
  domicilio <- domicilio %>%
    mutate(across(all_of(vars_dom), as.numeric)) %>%
    mutate(
      prop_propriedade     = NA_real_,                 # not available
      prop_sem_agua        = 1 - (V00111 / V00001),
      prop_sem_banheiro    = (V00238 + V00236) / V00001,
      prop_sem_esgoto      = 1 - ((V00309 + V00310 + V00311) / V00001),
      prop_sem_energia     = NA_real_,                 # not available
      prop_sem_coleta_lixo = 1 - ((V00397 + V00398) / V00001)
    ) %>%
    mutate(across(starts_with("prop_"), ~ if_else(is.nan(.) | is.infinite(.), NA_real_, .))) %>%
    select(Cod_setor, V00001,
           prop_propriedade, prop_sem_agua, prop_sem_banheiro, prop_sem_esgoto,
           prop_sem_energia, prop_sem_coleta_lixo)
  
  # -- Responsavel: income of household heads -------------------------------------
  # fam_extrema_pobreza, fam_linha_pobreza, renda_mensal_setor, prop_extrema_pobreza,
  # prop_linha_pobreza, renda_mensal_dom: not available in 2022 at tract census level, 
  # kept as NA for cross-census comparability.
  responsavel_renda <- read_census(
    file.path(data_dir, "Agregados_por_setores_renda_responsavel_BR.csv")
  )
  
  vars_responsavel_renda <- "V06004"
  responsavel_renda <- responsavel_renda %>%
    mutate(across(all_of(vars_responsavel_renda), as.numeric)) %>%
    mutate(
      renda_responsavel    = V06004,        # average income, 
      renda_responsavel = if_else(is.nan(renda_responsavel) | is.infinite(renda_responsavel),
                                  NA_real_, renda_responsavel), 
      renda_mensal_setor    = NA_real_,
      fam_extrema_pobreza   = NA_real_,
      fam_linha_pobreza     = NA_real_,
      prop_extrema_pobreza  = NA_real_,
      prop_linha_pobreza    = NA_real_,
      renda_mensal_dom      = NA_real_
    ) %>%
    select(Cod_setor, renda_responsavel, fam_extrema_pobreza, fam_linha_pobreza,
           renda_mensal_setor, prop_extrema_pobreza, prop_linha_pobreza, renda_mensal_dom)
  
  # -- Entorno: urban situation ----------------------------------------------------
  entorno01 <- read_census(
    file.path(data_dir, "Agregados_por_setores_basico_BR.csv")
  ) %>%
    mutate(
      situacao_urbana = if_else(as.integer(CD_SIT) <= 3, 1L, 0L)
    ) %>%
    select(Cod_setor, situacao_urbana)
  
  # -- Final Merge -------------------------------------------------------------
  # No UF filter here: source files are national, and this function currently
  # returns all sectors in Brazil. 
  final <- pessoa %>%
    select(Cod_setor, prop_pess_15anos_analfab) %>%
    left_join(domicilio, by = "Cod_setor") %>%
    left_join(responsavel_renda, by = "Cod_setor") %>%
    left_join(entorno01, by = "Cod_setor")
  
  return(final)
}


# -- Standardize BrADI indicators (z-score) ------------------------------------
#'
#' Applies as.numeric(scale(x)) to each variable listed, creating a new
#' "<var>_std" column.
#'
#' @param df                   Output of build_indicators_*(), after any
#'                             manual log-transform / sign-reversal already applied
#' @param vars_to_standardize  Names of variables to z-score standardize
#'
#' @return                     df with added "<var>_std" columns

standardize_bradi <- function(df, vars_to_standardize) {
  
  existing_vars <- intersect(vars_to_standardize, names(df))
  missing_vars    <- setdiff(vars_to_standardize, names(df))
  
  if (length(missing_vars) > 0) {
    message("Variables not found in this census (ignored): ",
            paste(missing_varso, collapse = ", "))
  }
  
  for (v in existing_vars) {
    df[[paste0(v, "_std")]] <- as.numeric(scale(df[[v]]))
  }
  
  # Diagnostic: NaN check on standardized columns
  std_cols <- df %>% select(ends_with("_std"))
  n_nan <- sum(sapply(std_cols, function(x) sum(is.nan(x))))
  if (n_nan > 0) {
    warning(n_nan, " NaN values found in standardized variables.")
  } else {
    message("Check completed: no NaN values found in standardized variables.")
  }
  
  return(df)
}
