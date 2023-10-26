---
title: "README: ukr_longit_study_graphs"
output:
  html_document: default
  output: github_document
---

## Overview

The `ukr_longit_study_graphs` R project is designed to generate png and svg versions of horizontal and vertical bar graphs for factsheets, briefs, presentations and other outputs of IMPACT's longitudinal survey with Ukrainian refugees and returnees. It uses as input the analysis outputs generated from the DAF. 

The project consists of the main R script `create_graphs.R` and the sourced R script `functions.R`. The scripts work with any data collection round and a minimum of two rounds of results contained in the analysis tables. 

## Prerequisites

- R (>= 3.6)
- R Libraries:
  - `readxl`
  - `writexl`
  - `scales`
  - `tidyverse`
  - `stringi`
  - `stringr`
  - `lubridate`
  - `ggthemes`

## Directory Structure

- `/input`: Place your input files here, such as DAFs and Excel sheets with the DAF analysis outputs. Make sure the analysis tables are in wide and tab format.
- `/output`: Graphs and tables generated will be saved in this directory. Sub-folders for each round and population group are automatically created in the graphs folder.
- `/scripts`: Contains the R scripts.

## Quick Start

1. **Clone the Repository**
   ```bash
   git clone https://github.com/your_username/ukr_longit_study_graphs.git
   ```

2. **Install Dependencies**
   ```{r, eval=FALSE}
   install.packages(c("readxl", "writexl", "scales", "tidyverse", "stringi", "stringr", "lubridate", "ggthemes"))
   ```

3. **Input Data Preparation**
    - Replace the analysis tables and DAFs for refugees, returnees, and combined datasets in the `/input` folder.
    - Update `list_graphs.xlsx` and `renaming_labels.xlsx` in the `/input` folder.

4. **Update Parameters**
    - Open the R project.
    - Open `create_graphs.R` and modify the parameters in section (1) according to your needs.

5. **Run Script**
    - Run `create_graphs.R`.

## Scripts Overview

### `create_graphs.R`

#### What It Does

1. Imports required R packages and sources functions from `functions.R`.
   
2. Sets up manual parameters such as the latest DC round, color palette, and font details.
   
3. Reads in additional parameters like output directories and DAFs from Excel sheets.
  
4. Transforms the separate analysis tables (DAF analysis outputs) for refugees, returnees and overall population in wide and tab format into a single long consolidated table.
  
5. Utilizes the processed data to generate the final graphs in both svg and png formats.

#### Usage

Before running `create_graphs.R`, make sure to:

1. Replace the input analysis tables and DAFs as per your requirements. Best practice is to use analysis tables and DAFs including as many rounds as possible, so that a wider variety of graphs can be created through the parameters in the list_graphs.xlsx file. 

2. Specify the parameters (DC round, color palette, etc.) in the script.

### `functions.R`

The `functions.R` script provides utility functions for graph creation and data validation. The main function in this script is `create_bar_graph`, which is invoked by `create_graphs.R`. `create_bar_graph` is a wrapper function for several helper functions specific to each step of the graph creation:

1. **Data Validation (`validate_data`)**: The function begins by validating the input dataset and parameters. If the validation fails, the script will stop execution.
    - Input: `data_df`, `params_df`
  
2. **Data Filtering (`filter_data`)**: Filters the dataset based on the parameters defined for each graph.
    - Input: `data_df`, `params`, `round_latest`, `round_previous`
  
3. **Rank Assignment (`assign_ranks`)**: Assigns ranks to the choices based on custom orderings. This rank is used for the proper placement of bars in the graphs.
    - Input: `filtered_df`, `custom_order`, `custom_order2`, `top_n`, `graph_type`
  
4. **Graph Plotting with ggplot**: Creates a ggplot object (`p`) and populates it with data. It also decides whether to display results as percentages or raw numbers.
    - Conditionally sets the fill color based on `latest_round` and `earliest_round`.
  
5. **Graph Customization (`customize_plot`)**: Further customizes the ggplot object based on several variables like number of choices, number of title lines, and number of rounds. It also decides on the plot width and adjusted height.
    - Input: `p`, `filtered_df`, `params`, `num_choices`, `num_title_lines`, `num_rounds`, `color_start`, `color_end`, `font_family`, `base_size`, `graph_type`
  
6. **File Operations (`handle_file_ops`)**: Saves the finalized plots in the specified output folder.
    - Input: `p`, `params`, `output_folder`, `plot_width`, `adjusted_height`

Thus, the function not only creates but also validates, customizes, and saves the bar graphs based on the given inputs and parameters.

