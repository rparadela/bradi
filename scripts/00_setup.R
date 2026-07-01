# =============================================================================
# BrADI: Brazilian Area Deprivation Index
# Script 00 — Setup: load required packages
# =============================================================================
# Description:
#   Loads all packages used throughout the BrADI pipeline:
#     01_functions.R            
#     02_build_indicators_bradi_2000.R
#     03_build_indicators_bradi_2010.R
#     04_build_indicators_bradi_2022.R
#     05_calculate_bradi_2000.R
#     06_calculate_bradi_2010.R
#     07_calculate_bradi_2022.R
# =============================================================================

# ---- Packages ----------------------------------------------------------------

library(tidyverse)      # data wrangling, ggplot2, readr (read_csv2/read_csv)
library(readxl)         # read_xls() 
library(sf)             # spatial data handling
library(writexl)        # export to Excel, if needed
library(psych)          # factor analysis / scale diagnostics
library(GPArotation)    # rotation methods used by psych


