
############ source this as part of script "run_all" ############

# specify latest DC round to be used for factsheet and the output directory for graphs
round_latest <- 16
dir_output_graphs <- paste0("output/graphs", "/r",round_latest)

# define range of color palette
color_start <- "#44546A"
color_end <- "#93B8D2"

# define font family
font_family <- "Leelawadee"

# base font size and set theme
base_size <- 12
theme_set(theme_longit_bars_vert(base_size = base_size))

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

# load excel sheets to relabel response options and sheets
df_relabel_choices <- read_xlsx("input/renaming_labels.xlsx", guess_max = 100000)
df_relabel_vars <- read_xlsx("input/renaming_labels.xlsx", 2, guess_max = 100000)

# Base columns to keep
col_keep_base <- c("sheet", "strata", "num_samples")