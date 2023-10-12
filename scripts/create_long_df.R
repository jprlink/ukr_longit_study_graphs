
############ source this as part of script "create_graphs" ############

# load excel sheet to relabel response options
df_relabel <- read_xlsx("input/renaming_labels.xlsx", guess_max = 100000)

###### load refugee analysis

# specify columns to keep in long pivot
col_keep <- c("sheet", "strata", 
              "num_samples", 
              dis_vars)

# accessing all the sheets 
sheet_ref <- excel_sheets("input/analysis_refugees.xlsx")

# applying sheet names to dataframe names
df_list_ref<- lapply(setNames(sheet_ref, sheet_ref), 
                     function(x) read_excel("input/analysis_refugees.xlsx", sheet=x))

# Filter out rows where second column is NA in each data frame
filtered_df_list_ref <- lapply(df_list_ref, function(df) {
  df[!is.na(df[[2]]), ]
})

# Bind all the filtered data frames together
df_wide_ref <- bind_rows(filtered_df_list_ref, .id="sheet")

# Store all colums as character
df_wide_ref <- df_wide_ref %>% mutate(across(everything(), as.character))

# Add empty variables to the dataframe when they don't exist
variables_to_add_ref <- col_keep[!col_keep %in% names(df_wide_ref)]
df_wide_ref <- add_empty_vars(df_wide_ref, variables_to_add_ref )

# pivot longer
df_long_ref <- pivot_longer(df_wide_ref, cols = -one_of(col_keep), names_to = "choice_label", values_to = "result") %>%
  filter(!is.na(result)) %>% 
  mutate(disp_status = "refugee")


###### load returnee analysis

# accessing all the sheets 
sheet_ret <- excel_sheets("input/analysis_returnees.xlsx")

# applying sheet names to dataframe names
df_list_ret<- lapply(setNames(sheet_ret, sheet_ret), 
                     function(x) read_excel("input/analysis_returnees.xlsx", sheet=x))

# Filter out rows where second column is NA in each data frame
filtered_df_list_ret <- lapply(df_list_ret, function(df) {
  df[!is.na(df[[2]]), ]
})

# Bind all the filtered data frames together
df_wide_ret <- bind_rows(filtered_df_list_ret, .id="sheet")

df_wide_ret <- df_wide_ret %>% mutate(across(everything(), as.character))

# Add empty variables to the dataframe when they don't exist
variables_to_add_ret <- col_keep[!col_keep %in% names(df_wide_ret)]
df_wide_ret <- add_empty_vars(df_wide_ret, variables_to_add_ret)

# pivot longer
df_long_ret <- pivot_longer(df_wide_ret, cols = -one_of(col_keep), names_to = "choice_label", values_to = "result") %>%
  filter(!is.na(result)) %>% 
  mutate(disp_status = "returnee")

###### merge refugee and returnee df

names(df_long_ref)
names(df_long_ret)

df_long <- rbind(df_long_ref,
                 df_long_ret)

# replace rows with NAs for disagg vars
df_long <- df_long %>% filter(!strata %in% c("overall"),
                              !choice_label %in% c("min", "max"))
df_long <- df_long %>% filter_all(all_vars(!. %in% c("<TOTAL>")))
#df_long <- df_long[rowSums(is.na(df_long [ , dis_vars])) < length(dis_vars), ]
df_long[dis_vars][is.na(df_long[dis_vars])] <- "overall"

# filter out empty results
df_long <- df_long %>% filter(!is.na(result))

# relabel choices
df_long <- relabel_values(df_long, df_relabel, "choice_label")

# Define vectors for question_choice_id
question_choice_id_vars <- c("question_code", "choice_label", "strata", "disp_status", dis_vars)

# Generate question_id
df_long <- df_long %>%
  mutate(question_code = gsub("\\_str.*", "", sheet))

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

