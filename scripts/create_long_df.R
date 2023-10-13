
############ source this as part of script "create_graphs" ############

###### Load data

# load excel sheets to relabel response options and sheets
df_relabel_choices <- read_xlsx("input/renaming_labels.xlsx", guess_max = 100000)
df_relabel_vars <- read_xlsx("input/renaming_labels.xlsx", 2, guess_max = 100000)

# Load Excel data into list of data frames
df_list_ref <- load_excel_sheets("input/analysis_refugees.xlsx")
df_list_ret <- load_excel_sheets("input/analysis_returnees.xlsx")

# Base columns to keep
col_keep_base <- c("sheet", "strata", "num_samples")

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


###### finalize dataset

# replace rows with NAs for disagg vars
df_long <- df_long %>% filter(!strata %in% c("overall"),
                              !choice_label %in% c("min", "max"))
df_long <- df_long %>% filter_all(all_vars(!. %in% c("<TOTAL>")))
#df_long <- df_long[rowSums(is.na(df_long [ , dis_vars])) < length(dis_vars), ]
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

# export results
df_long %>% write_xlsx(sprintf("output/ukr_longit_analysis_table_round_%s_%s.xlsx", round_latest, Sys.Date()))
df_long %>% saveRDS(sprintf("output/ukr_longit_analysis_table_round_%s.RDS", round_latest))

