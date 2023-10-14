
# load libraries and functions
library(readxl)
library(writexl)
library(scales) 
library(tidyverse)
library(stringi)
library(stringr)
library(lubridate)
library(ggthemes)
library(extrafont)
loadfonts(quiet = T)
source("scripts/functions.R")

# specify latest DC round to be used for factsheet and the output directory for graphs
round_latest <- 16
dir_output_graphs <- paste0("output/graphs", "/r",round_latest)

# load data frame with all results and parameter file 
# (run the script create_long_df to produce the data frame with the results in long format)

# source("scripts/create_long_df.R")
df_long <- readRDS(sprintf("output/ukr_longit_analysis_table_round_%s.RDS", round_latest))
df_params <- read_excel("input/list_graphs.xlsx", 2)
df_rounds <- read_excel("input/list_graphs.xlsx", 3)

head(df_long)
head(df_params)

# define range of color palette
color_start <- "#44546A"
color_end <- "#93B8D2"

# define font family
font_family <- "Leelawadee"

# base font size
base_size <- 12
  
######## run the rest from here
round_previous <- round_latest - 1
round_latest <- as.character(round_latest)
round_previous <- as.character(round_previous)
rounds <- c(round_previous, round_latest)

theme_set(theme_longit_bars_vert(base_size = base_size))

# Load DAFs
daf_refugees <- read_xlsx("input/daf_refugees.xlsx", guess_max = 100000)
daf_returnees <- read_xlsx("input/daf_returnees.xlsx", guess_max = 100000)
daf_combined <- read_xlsx("input/daf_combined.xlsx", guess_max = 100000)

# Determine unique disaggregation variables
dis_vars_ref <- unique(na.omit(daf_refugees$disaggregations))
dis_vars_ret <- unique(na.omit(daf_returnees$disaggregations))
dis_vars_comb <- unique(na.omit(daf_combined $disaggregations))
dis_vars <- unique(c(dis_vars_ref, dis_vars_ret, dis_vars_comb))

# filter for overall results (no disaggregations)
dis_vars_names <- df_long %>% select(all_of(dis_vars)) %>% names()
df_long <- df_long %>%
  filter(rowSums(sapply(.[, dis_vars_names, drop = FALSE], function(col) col == "overall")) == length(dis_vars_names))

# export filtered results
df_long %>% select(-all_of(dis_vars_names)) %>% 
  write_xlsx(sprintf("output/ukr_longit_analysis_table_round_%s_%s_overall.xlsx", round_latest, Sys.Date()))

# create bar graphs
create_bar_graph_vertical(df_long, df_params, round_latest, dir_output_graphs, color_start, color_end, font_family)


