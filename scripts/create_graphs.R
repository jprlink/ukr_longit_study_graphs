
######################### Script to create bar graphs for the longitudinal survey ######################### 

# load libraries and functions
library(readxl)
library(writexl)
library(scales) 
library(tidyverse)
library(stringi)
library(stringr)
library(lubridate)
library(ggthemes)
library(svglite)
source("scripts/functions.R")

# IMPORTANT before running the script: 
# (1) Replace the analysis tables and the DAFs for refugees, returnees and combined 
# in the input folder with those you need for graphs.
# (2) Make sure the parameters in section (1) below are specified correctly.

######################### (1) Provide parameters

# Specify latest DC round to be used for graphs
round_latest <- 16

# Define range of color palette
color_start <- "#44546A"
color_end <- "#93B8D2"

# Define font family
font_family <- "Leelawadee"

# Set base font size and theme
base_size <- 12
theme_set(theme_longit_bars(base_size = base_size))

######################### (2) Load remaining parameters

# Output directory for graphs
dir_output_graphs <- paste0("output/graphs", "/r",round_latest)

# Remaining parameters
df_params <- read_excel("input/list_graphs.xlsx", 2)
df_rounds <- read_excel("input/list_graphs.xlsx", 3)

# Define rounds
round_previous <- round_latest - 1
round_latest <- as.character(round_latest)
round_previous <- as.character(round_previous)
rounds <- c(round_previous, round_latest)

# Load DAFs
daf_refugees <- read_xlsx("input/daf_refugees.xlsx", guess_max = 100000)
daf_returnees <- read_xlsx("input/daf_returnees.xlsx", guess_max = 100000)
daf_combined <- read_xlsx("input/daf_combined.xlsx", guess_max = 100000)

# Determine unique disaggregation variables
dis_vars_ref <- unique(na.omit(daf_refugees$disaggregations))
dis_vars_ret <- unique(na.omit(daf_returnees$disaggregations))
dis_vars_comb <- unique(na.omit(daf_combined $disaggregations))
dis_vars <- unique(c(dis_vars_ref, dis_vars_ret, dis_vars_comb))

# Load excel sheets to relabel response options and sheets
df_relabel_choices <- read_xlsx("input/renaming_labels.xlsx", guess_max = 100000)
df_relabel_vars <- read_xlsx("input/renaming_labels.xlsx", 2, guess_max = 100000)

# Base columns to keep
col_keep_base <- c("sheet", "strata", "num_samples")

######################### (3) Create consolidated long table with all results

# Load Excel data into list of data frames
df_list_ref <- load_excel_sheets("input/analysis_refugees.xlsx")
df_list_ret <- load_excel_sheets("input/analysis_returnees.xlsx")

# Process the data
df_long_ref <- process_data(df_list_ref, col_keep_base, dis_vars_ref, "refugee")
df_long_ret <- process_data(df_list_ret, col_keep_base, dis_vars_ret, "returnee")
df_long_comb <- process_data(df_list_ret, col_keep_base, dis_vars_ret, "overall")

# Combine both datasets
all_cols <- unique(c(colnames(df_long_ref), colnames(df_long_ret), colnames(df_long_comb)))

df_long_ref <- align_and_add_missing_cols(df_long_ref, all_cols)
df_long_ret <- align_and_add_missing_cols(df_long_ret, all_cols)
df_long_comb <- align_and_add_missing_cols(df_long_comb, all_cols)

df_long <- rbind(df_long_ref,
                 df_long_ret,
                 df_long_comb)

###### finalize dataframe

# replace rows with NAs for disagg vars
df_long <- df_long %>% filter(!strata %in% c("overall"),
                              !choice_label %in% c("min", "max"))
df_long <- df_long %>% filter_all(all_vars(!. %in% c("<TOTAL>")))
df_long[dis_vars][is.na(df_long[dis_vars])] <- "overall"

# filter out empty results
df_long <- df_long %>% filter(!is.na(result))

# relabel choices
df_long <- relabel_values(df_long, df_relabel_choices, "choice_label")

# Define vectors for question_choice_id
question_choice_id_vars <- c("question_code", "choice_label", "strata", "disp_status", dis_vars)

# Generate question_id
df_long <- df_long %>%
  mutate(question_code = gsub("\\_str.*", "", sheet))

df_long <- relabel_values_containment(df_long, df_relabel_vars, "question_code")

# Generate question_choice_id using apply
df_long$question_choice_id <- apply(df_long[, question_choice_id_vars], 1, function(row) {
  tolower(paste0(row, collapse = "_"))
})

df_long <- df_long %>%
  rename(round = strata)

# check if there are any duplicates
df_long %>% filter(duplicated(question_choice_id))

# format numeric variables
vars_num <- c("result", "num_samples")
df_long$result <- sub("%", "", df_long$result)
df_long[vars_num] <- lapply(df_long[vars_num], as.numeric)

# sort columns
df_long <- df_long %>% 
  select(question_choice_id, question_code, choice_label, round,  disp_status, all_of(dis_vars), num_samples, result)

# export all disaggregated results
df_long %>% write_xlsx(sprintf("output/ukr_longit_analysis_table_round_%s_%s.xlsx", round_latest, Sys.Date()))
df_long %>% saveRDS(sprintf("output/ukr_longit_analysis_table_round_%s.RDS", round_latest))

# filter for overall results (no disaggregations)
dis_vars_names <- df_long %>% select(all_of(dis_vars)) %>% names()
df_long <- df_long %>%
  filter(rowSums(sapply(.[, dis_vars_names, drop = FALSE], function(col) col == "overall")) == length(dis_vars_names))

# export filtered overall results (used for graphs)
df_long <- df_long %>% select(-all_of(dis_vars_names)) 

df_long %>% 
  write_xlsx(sprintf("output/ukr_longit_analysis_table_round_%s_%s_overall.xlsx", round_latest, Sys.Date()))

df_long %>% 
  saveRDS(sprintf("output/ukr_longit_analysis_table_round_%s_overall.RDS", round_latest))

######################### (4) Create graphs

df_long <- readRDS(sprintf("output/ukr_longit_analysis_table_round_%s_overall.RDS", round_latest))

create_bar_graph(df_long, df_params, round_latest, dir_output_graphs, color_start, color_end, font_family)

