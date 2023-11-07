
# Function to relabel values in a variable based on full string containment
relabel_values_containment <- function(df, mapping_df, variable_name) {
  # Convert to character for string matching
  df[[variable_name]] <- as.character(df[[variable_name]])
  mapping_df$search_string <- as.character(mapping_df$search_string)
  mapping_df$new_string <- as.character(mapping_df$new_string)
  
  # Loop through each search_string and new_string pair in the mapping_df
  for(i in 1:nrow(mapping_df)) {
    search_str <- mapping_df$search_string[i]
    new_str <- mapping_df$new_string[i]
    
    # Identify rows where the entire search string is contained within the variable's value
    matching_rows <- str_detect(df[[variable_name]], fixed(search_str, ignore_case = TRUE))
    
    # Update those rows with the new string
    df[[variable_name]][matching_rows] <- new_str
  }
  
  return(df)
}

# Function to load Excel sheets into a list of data frames
load_excel_sheets <- function(file_path) {
  sheet_names <- excel_sheets(file_path)
  df_list <- lapply(setNames(sheet_names, sheet_names), function(x) read_excel(file_path, sheet=x))
  return(df_list)
}

# Function to filter and manipulate data
process_data <- function(df_list, col_keep_base, dis_vars, status_label) {
  col_keep <- c(col_keep_base, dis_vars)
  
  # Filter out rows where second column is NA
  filtered_df_list <- lapply(df_list, function(df) {
    df[!is.na(df[[2]]), ]
  })
  
  # Bind all the filtered data frames together
  df_wide <- bind_rows(filtered_df_list, .id="sheet")
  
  # Store all columns as character
  df_wide <- df_wide %>% mutate(across(everything(), as.character))
  
  # Add empty variables to the dataframe when they don't exist
  variables_to_add <- col_keep[!col_keep %in% names(df_wide)]
  df_wide <- add_empty_vars(df_wide, variables_to_add)
  
  # Pivot longer
  df_long <- pivot_longer(df_wide, cols = -one_of(col_keep), names_to = "choice_label", values_to = "result") %>%
    filter(!is.na(result)) %>% 
    mutate(disp_status = status_label)
  
  return(df_long)
}

align_and_add_missing_cols <- function(df, all_cols) {
  missing_cols <- setdiff(all_cols, colnames(df))
  for (col in missing_cols) {
    df[[col]] <- NA
  }
  # Reorder columns to match 'all_cols' for consistency
  df <- df[, all_cols, drop=FALSE]
  return(df)
}

# Function to relabel values in a variable based on a mapping data frame
relabel_values <- function(df, mapping_df, variable_name) {
  # Convert to character for string matching
  df[[variable_name]] <- as.character(df[[variable_name]])
  mapping_df$old_name <- as.character(mapping_df$old_name)
  mapping_df$new_name <- as.character(mapping_df$new_name)
  
  # Create a vector of new values based on the mapping
  new_values <- mapping_df$new_name[match(df[[variable_name]], mapping_df$old_name)]
  
  # Replace NA values with the original ones (i.e., keep values that did not have a mapping)
  new_values[is.na(new_values)] <- df[[variable_name]][is.na(new_values)]
  
  # Update the column in the original data frame
  df[[variable_name]] <- new_values
  
  return(df)
}

# Function to add empty character variables filled with NA to a dataframe
add_empty_vars <- function(df, variables_to_add) {
  existing_vars <- colnames(df)
  
  for (var in variables_to_add) {
    if (!(var %in% existing_vars)) {
      df[[var]] <- as.character(rep(NA, nrow(df)))
    }
  }
  
  return(df)
}

# Create theme
theme_longit_bars <- function (base_size, base_family = "Leelawadee", ticks = F) 
{
  ret <- theme_bw(base_family = base_family, base_size = base_size) + 
    theme(
      legend.background = element_blank(),
      legend.key = element_blank(),
      panel.background = element_rect(fill = "transparent"),  # Set to transparent
      panel.border = element_blank(),
      strip.background = element_blank(),
      plot.background = element_rect(fill = "transparent"),  # Set to transparent
      axis.line = element_blank(),
      axis.line.x = element_line(color = "black", size = 0.5),  # Line for the X-axis
      axis.line.y = element_line(color = "black", size = 0.5),  # Line for the Y-axis
      axis.ticks.x = element_blank(),  # Ticks for the X-axis
      axis.ticks.y = element_blank(),  # Ticks for the Y-axis
      panel.grid = element_blank()
    )
  if (!ticks) {
    ret <- ret + theme(axis.ticks = element_blank())
  }
  ret
}


label_wrap <- function(width = 20, max_lines = 3) {
  function(x) {
    sapply(x, function(single_x) {
      wrapped_text <- stringr::str_wrap(single_x, width)
      line_count <- stringr::str_count(wrapped_text, "\n") + 1  # Count lines
      
      if (line_count > max_lines) {
        # Truncate text and add '...' to indicate truncation
        truncated_text <- unlist(strsplit(wrapped_text, "\n"))[1:max_lines]
        return(paste0(paste(truncated_text, collapse = "\n"), "..."))
      } else {
        return(wrapped_text)
      }
    })
  }
}

# Function to format labels based on type
format_label <- function(x, result_type) {
  if (result_type == "percent") {
    return(sprintf("%.1f%%", x))
  } else if (result_type == "integer") {
    return(scales::comma(x))
  } else {
    return(as.character(x))
  }
}

# Function to sanitize titles
sanitize_title <- function(title) {
  sanitized_title <- tolower(gsub("[^a-zA-Z0-9]", "_", title))
  sanitized_title <- gsub("__+", "_", sanitized_title)  # Replace multiple underscores with a single underscore
  return(sanitized_title)
}

# Data validation function
validate_data <- function(data_df, params_df) {
  # Validation logic
  required_cols_data <- c("question_code", "choice_label", "result", "round", "num_samples", "disp_status")
  required_cols_params <- c("title", "graph_type", "result_type", "disp_status", "main_variable", "top", 
                            "data_labels", "exclude_pns", "exclude_dk", "exclude_other", "wrap_title", "label_orientation", 
                            "plot_width", "plot_height", "legend_position", "data_label_size", "aggregate_other") 
  
  missing_data_cols <- setdiff(required_cols_data, names(data_df))
  missing_params_cols <- setdiff(required_cols_params, names(params_df))
  
  if (length(missing_data_cols) > 0 || length(missing_params_cols) > 0) {
    if (length(missing_data_cols) > 0) {
      stop(paste("Validation failed. Missing columns in data_df: ", paste(missing_data_cols, collapse = ", ")))
    }
    if (length(missing_params_cols) > 0) {
      stop(paste("Validation failed. Missing columns in params_df: ", paste(missing_params_cols, collapse = ", ")))
    }
  }
  return(TRUE)
}


# Data filtering function
filter_data <- function(data_df, params, df_rounds, round_latest, round_previous) {
  # Filter based on main variable and disp_status
  filtered_df <- data_df %>% 
    filter(
      question_code == params$main_variable,
      disp_status == params$disp_status
    )
  
  # Exclude 'other' labels if specified
  if (!is.na(params$exclude_other) && tolower(params$exclude_other) == 'yes') {
    exclude_other_labels <- c("other (please_specify)", "other (please specify)", "Other (please specify)", "other", "Other")
    filtered_df <- filtered_df %>% filter(!choice_label %in% exclude_other_labels)
  }
  
  # Exclude 'dk' labels if specified
  if (!is.na(params$exclude_dk) && tolower(params$exclude_dk) == 'yes') {
    exclude_dk_labels <- c("don't know", "Don't know", "dk")
    filtered_df <- filtered_df %>% filter(!grepl(paste(exclude_dk_labels, collapse="|"), choice_label, ignore.case = TRUE))
  }
  
  # Exclude 'pns' labels if specified
  if (!is.na(params$exclude_pns) && tolower(params$exclude_pns) == 'yes') {
    exclude_pns_labels <- c("Prefer not to say", "prefer not to say", "pns")
    filtered_df <- filtered_df %>% filter(!choice_label %in% exclude_pns_labels)
  }
  
  # Convert the round column to character in both dataframes
  filtered_df$round <- as.character(filtered_df$round)
  df_rounds$round <- as.character(df_rounds$round)
  
  # Join to get the month information and sample sizes
  filtered_df <- filtered_df %>% 
    left_join(df_rounds, by = "round")
  
  # Extract unique month values from df_rounds in the order they appear
  ordered_months <- unique(df_rounds$month)
  
  # Convert the 'month' column in filtered_df to an ordered factor
  filtered_df$month <- factor(filtered_df$month, levels = ordered_months, ordered = TRUE)
  
  # Filter for rounds
  if (as.character(params$latest_round) == "latest" && as.character(params$earliest_round) == "latest") {
    filtered_df <- filtered_df %>% filter(round %in% round_latest)
  } else {
    # Replace 'latest' or 'previous' with actual values from round_latest or round_previous
    earliest_round <- ifelse(as.character(params$earliest_round) == "previous", round_previous, as.character(params$earliest_round))
    latest_round <- ifelse(as.character(params$latest_round) == "latest", round_latest, as.character(params$latest_round))
    
    # Convert to numeric for calculations
    earliest_round_num <- as.numeric(earliest_round)
    latest_round_num <- as.numeric(latest_round)
    
    # Calculate the total number of rounds between earliest and latest
    total_rounds <- latest_round_num - earliest_round_num + 1
    
    # Check if rounds_skipped is NA
    if (!is.na(params$rounds_skipped)) {
      # Calculate how many rounds would be included with the given 'rounds_skipped'
      rounds_included <- floor((total_rounds - 1) / (as.numeric(params$rounds_skipped) + 1)) + 1
      
      # If at least two rounds would be included, proceed to generate the sequence
      if (rounds_included >= 2) {
        rounds_to_include <- seq(from = earliest_round_num, to = latest_round_num, by = as.numeric(params$rounds_skipped) + 1)
      } else {
        message(paste("The number of rounds to be skipped (", params$rounds_skipped, ") would result in fewer than two rounds being included for comparison. Skipping not considered."))
        rounds_to_include <- seq(from = earliest_round_num, to = latest_round_num)
      }
    } else {
      # If rounds_skipped is NA, generate a sequence without considering it
      rounds_to_include <- seq(from = earliest_round_num, to = latest_round_num)
    }
    
    # Convert to character for filtering
    filtered_rounds <- as.character(rounds_to_include)
    filtered_df <- filtered_df %>% filter(round %in% filtered_rounds)
    
  }
  
  return(filtered_df)
}

# Rank assignment function
assign_ranks <- function(filtered_df, custom_order, custom_order2, top_n = NULL, graph_type = "bar_vertical", params) {
  
  # Check if any of the custom labels from custom_order are present in choice_label
  if (any(filtered_df$choice_label %in% custom_order)) {
    # Create a custom rank based on custom_order
    filtered_df <- filtered_df %>% 
      mutate(rank = match(choice_label, custom_order))
  } 
  
  # Check if all of the custom labels from custom_order2 are present in choice_label
  else if (all(custom_order2 %in% filtered_df$choice_label)) {
    # Create a custom rank based on custom_order2
    filtered_df <- filtered_df %>% 
      mutate(rank = match(choice_label, custom_order2))
  } else {
    # Get the rank from the latest round data
    latest_round_data <- filtered_df %>% filter(round == max(as.numeric(as.character(round)))) %>% 
      arrange(desc(result)) %>% 
      mutate(rank = row_number())
    
    # Update rank in filtered_df based on latest round's result
    filtered_df <- filtered_df %>% 
      left_join(latest_round_data %>% select(choice_label, rank), by = "choice_label")
  }
  
  # If graph_type is "bar_horizontal", reorder based on round
  if (graph_type == "bar_horizontal") {
    filtered_df <- filtered_df %>% arrange(desc(rank), round)
  }
  
  # If 'top' is not NA and not an empty string, and is smaller than the total number of unique choice_labels
  if (!is.null(top_n) && top_n < length(unique(filtered_df$choice_label))) {
    top_labels <- latest_round_data %>% 
      arrange(rank) %>% 
      head(top_n) %>% 
      pull(choice_label)
    
    filtered_df <- filtered_df %>% 
      filter(choice_label %in% top_labels)
    
    
    if (!is.na(params$aggregate_other) && params$aggregate_other == "yes" && 
        !is.na(params$result_type) && params$result_type == "percent") {
      
      # Initialize an empty data frame to store 'Other' rows
      other_rows <- data.frame()
      
      # Loop through each unique round
      for (r in unique(filtered_df$round)) {
        
        # Calculate the sum of 'result' for the current round
        sum_top_results <- sum(filtered_df$result[filtered_df$round == r], na.rm = TRUE)
        other_value = 100 - sum_top_results
        
        # Extract a row from filtered_df for the same round, to clone its structure
        template_row <- filtered_df[filtered_df$round == r, , drop = FALSE][1, , drop = FALSE]
        
        # Modify the template to create the 'Other' row
        template_row$choice_label <- "Other"
        template_row$result <- other_value
        template_row$rank <- max(filtered_df$rank[filtered_df$round == r], na.rm = TRUE) + 1
        
        # Append the 'Other' row to the other_rows data frame
        other_rows <- rbind(other_rows, template_row)
      }
      
      # Add the 'Other' rows to the original data frame
      filtered_df <- rbind(filtered_df, other_rows)
    }
    
    
  }
  
  return(filtered_df)
}

# Function to create custom color palette
create_custom_palette <- function(color_start, color_end, n) {
  interpolator <- colorRampPalette(c(color_start, color_end))
  return(interpolator(n))
}

# Custom y-label formatting function
format_y_labels <- function(x, y_labels) {
  if (y_labels == "percent") {
    return(scales::percent(x))
  } else if (y_labels == "integer") {
    return(scales::comma(x))
  } else {
    return(x)
  }
}

# Customize plot function
customize_plot <- function(p, filtered_df, params, num_choices, num_title_lines, num_rounds, color_start, color_end, font_family, base_size, graph_type) {
  # Determine dodge_width based on conditions
  if(num_rounds > 1) {
    dodge_width <- 0.8
  } else {
    dodge_width <- 0.9  # You can set this to a default value or another conditional value
  }
  
  # Calculate plot_width and adjusted_height here
  
  if (graph_type == "bar_vertical") {

    plot_height <- 6
    
    # Determine the number of unique bars (num_choices)
    if (num_choices < 3) {
      plot_width <- 6
    } else if (num_choices < 5) {
      plot_width <- 8
    } else {
      plot_width <- 10
    }
  } else if (graph_type == "bar_horizontal") {
    
    plot_width <- 8
    
    # Determine the number of unique bars (num_choices)
      if (num_choices < 5) {
          plot_height <- 4
      } else {
          plot_height <- 7
        }
      }
    
  # define plot width and height manually if specified in parameter input
  if(!is.na(params$plot_width)) {
    
    if (graph_type == "bar_vertical") {
     
      plot_width <- as.numeric(params$plot_width)
      
    } else if (graph_type == "bar_horizontal") {
    
      new_xlim_upper <- as.numeric(params$plot_width)
    }
  } else {
    
    new_xlim_upper <- NULL
  }
  
  if(!is.na(params$plot_height)) {
    plot_height <- as.numeric(params$plot_height)
  }
  
  # Determine conditions for reducing font size
  max_label_length <- max(nchar(unique(filtered_df$choice_label)))
  
  # Set a fixed or calculated font size for choice category labels
  y_axis_font_size <- base_size
  
  # Conditionally set font size
  font_size_value <- ifelse(max_label_length > 20 || 
                              num_choices > 8 || 
                              num_rounds > 8, 11, base_size)
  
  # Ensure the font size for x-axis labels is not larger than y-axis labels
  font_size_value <- min(font_size_value, y_axis_font_size)  # Take the minimum of calculated and y-axis font size
  
  # Initialize angle_value
  angle_value <- 50  # default
  
  # Set angle_value based on conditions
  if (num_choices < 6 |
      max_label_length < 4 |
      (!is.na(params$label_orientation) & 
       params$label_orientation == "horizontal")) {
    angle_value <- 0
  } else if (!is.na(params$label_orientation) & params$label_orientation == "diagonal") {
    angle_value <- 50
  }
  
  # Function to wrap labels at 8 characters
  label_wrap_8 <- function(x) {
    sapply(x, function(single_x) {
      stringr::str_wrap(single_x, 8)
    })
  }
  
  # Function to wrap labels at 15 characters
  label_wrap_15 <- function(x) {
    sapply(x, function(single_x) {
      stringr::str_wrap(single_x, 15)
    })
  }
  
  # Function to wrap labels at 30 characters
  label_wrap_30 <- function(x) {
    sapply(x, function(single_x) {
      stringr::str_wrap(single_x, 30)
    })
  }
  
  # Determine vertical justification based on angle
  vjust_value <- ifelse(angle_value == 0, 0.5, 0.5)
  
  # Generate custom palette
  
  if (graph_type == "bar_vertical") {
    
  chosen_palette <- create_custom_palette(color_end, color_start, num_rounds)
  
  } else if (graph_type == "bar_horizontal") {
    
  chosen_palette <- create_custom_palette(color_end, color_start, num_rounds)
  
  }
  
  # Generate legend labels based on month and num_samples in filtered_df
  legend_labels <- paste0(unique(filtered_df$month), " (N=", scales::comma(unique(filtered_df$num_samples)), ")")
  
  # Check for wrap_title being 'no'
  if (!is.na(params$wrap_title) & tolower(as.character(params$wrap_title)) == 'no') {
    wrapped_title_text <- params$title  # Use the original title without wrapping
    adjusted_height <- plot_height  # Default height
    
  } else if (is.na(params$wrap_title) || tolower(as.character(params$wrap_title)) == 'yes') {
    
    # Calculate the wrap width based on the plot width
    wrap_width <- ifelse(plot_width < 10, 40, 70)
    wrapped_title <- strwrap(params$title, width = wrap_width)  # Dynamic wrapping width
    
    num_title_lines <- length(wrapped_title)  # Calculate the number of lines in the title
    wrapped_title_text <- paste(wrapped_title, collapse = "\n")  # Concatenate lines with newline characters
    
    # Determine the height of the graph based on the number of title lines
    adjusted_height <- plot_height + 0.5 * (num_title_lines - 1)  # Increase the height by 0.5 unit per extra line
  } else {
    wrapped_title_text <- params$title  # Use the original title
    adjusted_height <- plot_height
  }
  
  if (graph_type == "bar_vertical") {
  p <- p + theme(
    axis.text.x = element_text(
      angle = angle_value,  # Conditionally set angle
      hjust = 0.5,  # center-align
      vjust = vjust_value,  # Vertically adjust labels
      size = font_size_value,  # Conditionally set font size
      margin = margin(t = 10, r = 10, b = 10, l = 10),  # Add space around labels
      family = font_family,  # Set font family
      face = "bold"
    ),
    plot.margin = margin(1, 1, 1.5, 1, "cm"),  # Increase bottom margin of the plot
    plot.title = element_text(vjust = 2, size = font_size_value + 4, family = font_family, face = "bold"),  # Set font family and make it bold,
    axis.title.x=element_blank(), 
    axis.title.y=element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank(),
    legend.title=element_text(size=font_size_value, family = font_family, face = "bold"), 
    legend.text=element_text(size=font_size_value, family = font_family)
  )
  } else if(graph_type == "bar_horizontal") {
    # Calculate maximum length of y-axis labels
    max_y_label_length <- max(nchar(unique(filtered_df$choice_label)))
    
    # Count the number of unique y-axis labels
    num_y_labels <- length(unique(filtered_df$choice_label))
    
    # Conditionally set left margin based on y-axis labels
    if (num_y_labels > 10 || max_y_label_length > 30) {
      left_margin <- 2  # Increase margin
      p <- p + scale_y_discrete(labels = label_wrap(width = 30))
    } else {
      left_margin <- 1.2  # Default value
      p <- p + scale_y_discrete(labels = label_wrap_15)  # Wrap at 15 characters
    }
    
    p <- p + theme(
      plot.margin = margin(1, 1.5, 1.5, left_margin, "cm"),  # Conditionally set left margin
      plot.title = element_text(vjust = 2, size = font_size_value + 4, family = font_family, face = "bold"),  # Set font family and make it bold
      axis.text.y = element_text(size = font_size_value,  # Conditionally set font size
                                 margin = margin(t = 10, r = 10, b = 10, l = 10),  # Add space around labels
                                 family = font_family,
                                 face = "bold"),  # Set font family for y-axis text
      axis.title.x = element_blank(), 
      axis.title.y = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.line.x = element_blank(),
      legend.title = element_text(size = font_size_value, family = font_family, face = "bold"), 
      legend.text = element_text(size = font_size_value, family = font_family)
    )
    
  }
  
  # by default set position of legend to top left
  
  if (is.na(params$legend_position) || params$legend_position == 'top') {
    
  p <- p + theme(
  legend.position='top', 
  legend.justification='left',
  legend.direction='horizontal',
  legend.margin=margin(0, 0, 0, 0))
  
  } else  if (params$legend_position == 'right') {
    
    p <- p + theme(
      legend.position='right')
  }
  
  # Add subtitle only if there is one round
  if (num_rounds == 1) {
    single_round_month <- unique(filtered_df$month)
    single_round_samples <- unique(filtered_df$num_samples)
    p <- p + labs(subtitle = paste0(single_round_month, " (N=", scales::comma(single_round_samples), ")")) +
        theme(plot.subtitle = element_text(size = font_size_value, family = font_family)
      )
  }
  
  # for vertical graphs
  
  if (graph_type == "bar_vertical") {
    
  # Add title and legend
  p <- p + labs(
    title = wrapped_title_text,  # Make sure this variable is defined based on 'wrap_title'
    fill = "Round"  # Legend title
  ) + scale_fill_manual(values = chosen_palette, labels = legend_labels)  # Custom legend
  
  # Add data labels based on conditions and params$data_labels
  if (is.na(params$data_labels)) {
    # Default behavior: Show labels only for the latest round
    
    latest_round_data <- filtered_df %>% filter(round == max(round))
    if(num_rounds == 1) {
      
      # Determine label size based on the number of rounds and choice labels
      if (num_choices <= 3) {
        label_size <- 4.3
      } else if (num_choices <= 6) {
        label_size <- 4
      } else if (num_choices <= 10) {
        label_size <- 3.2
      } else {
        label_size <- 3
      }
      
      if (!is.na(params$data_label_size)) {
        label_size <- as.numeric(params$data_label_size)
      }  
        
      # Ungrouped graph, center the label
      p <- p + geom_text(
        data = latest_round_data,
        aes(label = format_label(result, params$result_type), group = round, fontface = "bold"),
        size = label_size,
        vjust = -1,
        hjust = 0.5  # Center the label
      )
    } else {
      # Grouped graph
      # Determine adj_val based on the number of rounds and choice labels
      if (num_choices <= 3) {
        adj_val <- 4 + 0.5 * (3 - num_choices)
        label_size <- 4.3
      } else if (num_choices <= 6) {
        adj_val <- 4 + 0.35 * (6 - num_choices)  # Increased base value for 4-6 groups
        label_size <- 4
      } else {
        adj_val <- 3 - 0.25 * (num_rounds - 2)
        label_size <- 2.7
      }
      
      if (!is.na(params$data_label_size)) {
        label_size <- as.numeric(params$data_label_size)
      }  
      
      p <- p + geom_text(
        data = latest_round_data,
        aes(label = format_label(result, params$result_type), group = round, fontface = "bold"),
        size = label_size,
        vjust = -1,
        nudge_x = dodge_width / adj_val  # Adjust the label's position
      )
    }
  } else {
    add_data_labels <- tolower(as.character(params$data_labels))
    
    if (add_data_labels == 'yes') {
      
      # Determine label size based on the number of rounds and choice labels
      if (num_choices <= 3) {
        label_size <- 4.3
      } else if (num_choices <= 6) {
        label_size <- 4
      } else if (num_choices <= 10) {
        label_size <- 3.2
      } else {
        label_size <- 2.7
      }
      
      if (!is.na(params$data_label_size)) {
        label_size <- as.numeric(params$data_label_size)
      }  
      
      # Show labels for all rounds
      p <- p + geom_text(
        aes(label = format_label(result, params$result_type), group = round, fontface = "bold"),
        size = label_size,
        vjust = -1,
        position = position_dodge(dodge_width)  # This line ensures the labels are dodged like the bars
      )
    } else if (add_data_labels == 'no') {
      # Do not add any labels
    }
  }
  
  # Add custom y-axis labels
  y_labels <- params$result_type  # Assume params has a result_type that can be 'percent', 'integer', etc.

  p <- p + scale_y_continuous(labels = function(x) format_y_labels(x, y_labels), 
                                expand = expansion(mult = c(0, 0.1)))
  
  # Add custom x-axis labels
  if (angle_value == 0) {
    p <- p + scale_x_discrete(labels = label_wrap_8)  # Wrap at 8 characters
  } else {
    p <- p + scale_x_discrete(labels = label_wrap(width = 15))
  }
  
  # for horizontal graphs
  
  } else if (graph_type == "bar_horizontal") {
    
    # Add title and legend
    p <- p + labs(
      title = wrapped_title_text,  # Make sure this variable is defined based on 'wrap_title'
      fill = "Round"  # Legend title
    ) + scale_fill_manual(values = chosen_palette, 
                          labels = legend_labels)# Custom legend
    
    # Add data labels based on conditions and params$data_labels
    if (is.na(params$data_labels)) {
      # Default behavior: Show labels only for the latest round
      
      latest_round_data <- filtered_df %>% filter(round == max(round))
      if(num_rounds == 1) {
        # Ungrouped graph, center the label
        
        # Determine label size based on the number of rounds and choice labels
        if (num_choices <= 3) {
          label_size <- 4.1
        } else if (num_choices <= 6) {
          label_size <- 4
        } else {
          label_size <- 3.8
        }
        
        p <- p + geom_text(
          data = latest_round_data,
          aes(label = format_label(result, params$result_type), group = round, fontface = "bold"),
          size = label_size,
          hjust = -0.3,
          vjust = 0.5  # Center the label
        )
      } else {
        # Grouped graph
        # Determine adj_val based on the number of rounds and choice labels
        if (num_choices <= 3) {
          adj_val <- 4 + 0.5 * (3 - num_choices)
          label_size <- 4.1
        } else if (num_choices <= 6) {
          adj_val <- 4 + 0.35 * (6 - num_choices)  # Increased base value for 4-6 groups
          label_size <- 4
        } else {
          adj_val <- 3 - 0.25 * (num_rounds - 2)
          label_size <- 3.8
        }
        
        p <- p + geom_text(
          data = latest_round_data,
          aes(label = format_label(result, params$result_type), group = round, fontface = "bold"),
          size = label_size,
          hjust = -0.3,
          nudge_y = dodge_width / adj_val  # Adjust the label's position
        )
      }
    } else {
      add_data_labels <- tolower(as.character(params$data_labels))
      
      if (add_data_labels == 'yes') {
        
        # Determine label size based on the number of rounds and choice labels
        if (num_choices <= 3) {
          label_size <- 4.1
        } else if (num_choices <= 6) {
          label_size <- 4
        } else {
          label_size <- 3.8
        }
        
        # Show labels for all rounds
        p <- p + geom_text(
          aes(label = format_label(result, params$result_type), group = round, fontface = "bold"),
          size = label_size,
          hjust = -0.3,
          position = position_dodge(dodge_width),  # This line ensures the labels are dodged like the bars
          family = font_family
        )
      } else if (add_data_labels == 'no') {
        # Do not add any labels
      }
    }
    
    if (!is.na(params$data_label_size) && (is.na(params$data_labels) || params$data_labels == 'yes')) {
      
      label_size <- as.numeric(params$data_label_size)
      
      p <- p + geom_text(size = label_size)
    }  
    
    # Add custom x-axis labels
    y_labels <- params$result_type  # Assume params has a result_type that can be 'percent', 'integer', etc.
    
    p <- p + scale_x_continuous(labels = function(x) format_y_labels(x, y_labels),
                                expand = expansion(mult = c(0, 0.1))
                                )
    
    if(is.null(new_xlim_upper)) {
    # Determine the maximum result value for the choice labels
    max_result_value <- max(filtered_df$result)

    if(!is.na(max_result_value)) {
    # Conditionally set the xlim
    if (max_result_value > 1000) {
      new_xlim_upper <- 1200
    } else if (max_result_value > 100) {
      new_xlim_upper <- 1000
    } else if (max_result_value > 80) {
      new_xlim_upper <- 1.3
    } else if (max_result_value > 60) {
      new_xlim_upper <- 0.8 
    } else if (max_result_value > 40) {
      new_xlim_upper <- 0.6 
    } else if (max_result_value > 20) {
      new_xlim_upper <- 0.4 
    } else {
      new_xlim_upper <- 0.2
    }
    } else{
      paste0("Error: no results for question_code ", filtered_df$question_code)
      
    }
    }
    
    # Apply the coord_cartesian to set the xlim
    p <- p + coord_cartesian(xlim = c(NA, new_xlim_upper))

  }

  # Change legend order
  p <- p + guides(fill = guide_legend(reverse = T, keyheight = unit(0.4, "cm"), keywidth = unit(0.4, "cm")))
  
  return(list(customized_plot = p, plot_width = plot_width, adjusted_height = adjusted_height))
}

# Main function
create_bar_graph <- function(data_df, params_df, round_latest, output_folder, color_start, color_end, font_family) {
  
  # Validate the data
  if (!validate_data(data_df, params_df)) {
    stop("Validation failed.")
  }
  
  custom_order <- c('0', '1', '2', '3', '4', '4+', '5', '5+', '6', '6+', '7', '7+', '8', '8+', '9', '9+', '10', '10+'
                    )
  
  custom_order2 <- c("Completely safe", "Somewhat safe", "Somewhat unsafe", "Completely unsafe", "I prefer not to say"
  )
  
  # Loop through each row in params_df
  for (i in seq_len(nrow(params_df))) {
    params <- params_df[i, , drop = FALSE]
    
    # Read graph_type from params
    graph_type <- as.character(params$graph_type)
    
    # Filter the data
    filtered_df <- filter_data(data_df, params, df_rounds, round_latest, round_previous)
    
    if (nrow(filtered_df) == 0) {
      message("Failure: Could not export graph as filtered_df is empty or NULL")
      next  # Skip to the next iteration of the loop
    }
    
    if (all(is.na(filtered_df$choice_label))) {
      message("Failure: Could not export graph as choice_label column contains only NA values.")
      next
    }
    
    if (!is.na(params$top) && params$top != "") {
      top_n <- as.numeric(params$top)  # Convert 'top' to numeric
    } else {
      top_n <- NULL
    }
    
    # Assign ranks
    filtered_df <- assign_ranks(filtered_df, custom_order, custom_order2, top_n, graph_type, params)
    
    # Calculate num_rounds
    num_rounds <- length(unique(filtered_df$round))
    
    # Calculate num_choices based on filtered_df
    num_choices <- length(unique(filtered_df$choice_label))
    
    # Calculate max_label_length based on filtered_df
    max_label_length <- max(nchar(unique(filtered_df$choice_label)))
    
    # Count the number of lines in the title
    num_title_lines <- length(strwrap(params$title, width = 70))
    
    # Create the basic plot based on graph_type
    if (graph_type == "bar_vertical") {
      
      if (params$result_type == "percent") {
        p <- ggplot(filtered_df, aes(x = reorder(choice_label, rank), y = (result / 100)))
      } else {
        p <- ggplot(filtered_df, aes(x = reorder(choice_label, rank), y = result))
      }
      
      if(num_rounds == 1) {   
        p <- p + geom_bar(stat = "identity", fill = color_start, width = 0.8, position = "dodge")
      } else {
        p <- p + geom_bar(stat = "identity", aes(fill = round), width = 0.8, position = "dodge")
      }
      
    } else if (graph_type == "bar_horizontal") {
      
      if (params$result_type == "percent") {
        p <- ggplot(filtered_df, aes(x = (result / 100), y = reorder(choice_label, desc(rank))))
      } else {
        p <- ggplot(filtered_df, aes(x = result, y = reorder(choice_label, desc(rank))))
      }
      
      if(num_rounds == 1) {
        p <- p + geom_bar(stat = "identity", fill = color_start, width = 0.6, position = "dodge")

      } else {
        p <- p + geom_bar(stat = "identity", aes(fill = round), width = 0.6, position = "dodge")
      }
    }
    
    # Customize the plot
    customization_result <- customize_plot(p, filtered_df, params, num_choices, num_title_lines, num_rounds, color_start, color_end, font_family, base_size, graph_type)
    p <- customization_result$customized_plot
    plot_width <- customization_result$plot_width
    adjusted_height <- customization_result$adjusted_height
    
    # Handle file operations
    if (!handle_file_ops(p, params, output_folder, plot_width, adjusted_height)) {
      message("File operation failed.")
    }
  }
}


# File operations function
handle_file_ops <- function(p, params, output_folder, plot_width, adjusted_height) {
  # Determine the subfolder based on the "disp_status" column
  subfolder <- ifelse(params$disp_status == "refugee", "refugees", 
                      ifelse(params$disp_status == "returnee", "returnees", "overall"))
  
  # Generate sanitized, lowercase file name from title
  sanitized_title <- sanitize_title(params$title)
  
  # Check the length of the sanitized title
  max_title_length <- 50  # You can adjust this limit as needed
  if (nchar(sanitized_title) > max_title_length) {
    sanitized_title <- substr(sanitized_title, 1, max_title_length)
    message("Sanitized title truncated due to excessive length.")
  }
  
  # Loop through each file extension
  for(file_extension in c(".svg", ".png")) {
    # Determine the device based on the file extension
    device_function <- ifelse(file_extension == ".svg", "svg", "png")
    
    # Combine sanitized title with file extension
    output_file_name <- paste0(sanitized_title, file_extension)
    
    # Create the subfolder if it doesn't exist
    subfolder_path <- file.path(output_folder, subfolder, device_function)
    if (!dir.exists(subfolder_path)) {
      dir.create(subfolder_path, recursive = TRUE)
    }
    
    # Generate the full output file path including subfolder
    output_file_path <- file.path(subfolder_path, output_file_name)
    
    # Save the plot in the current format
    ggplot2::ggsave(output_file_path, plot = p, device = device_function, width = plot_width, height = adjusted_height)
    
    if (file.exists(output_file_path)) {
      message(paste("Successfully created graph:", output_file_path))
    } else {
      message(paste("Failure: Could not export graph as", output_file_path))
    }
  }
  
  return(TRUE)
}


