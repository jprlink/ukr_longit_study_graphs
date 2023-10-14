
######################### (1) Load libraries and parameters ######################### 

library(readxl)
library(writexl)
library(scales) 
library(tidyverse)
library(stringi)
library(stringr)
library(lubridate)
library(ggthemes)

source("scripts/functions.R")

source("scripts/load_parameters.R")

######################### (2) Create consolidated long table with all results ######################### 

source("scripts/create_long_df.R")

######################### (3) Create graphs ######################### 

# df_long <- readRDS(sprintf("output/ukr_longit_analysis_table_round_%s.RDS", round_latest))
source("scripts/create_graphs.R")


