
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
theme_longit_bars_vert <- function (base_size = 11, base_family = "Leelawadee", ticks = TRUE) 
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
      axis.ticks.x = element_line(color = "black", size = 0.5),  # Ticks for the X-axis
      axis.ticks.y = element_line(color = "black", size = 0.5),  # Ticks for the Y-axis
      panel.grid = element_blank()
    )
  if (!ticks) {
    ret <- ret + theme(axis.ticks = element_blank())
  }
  ret
}


# Sub-function to create custom color palette
create_custom_palette <- function(color_start, color_end, n) {
  interpolator <- colorRampPalette(c(color_start, color_end))
  return(interpolator(n))
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
    return(scales::percent(x / 100))
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

# Function to create and save vertical bar graphs with percentages
create_bar_graph_vertical <- function(data_df, params_df, round_latest, output_folder, color_start, color_end) {
  
  # Convert to ordered factor for comparison
  data_df$round <- factor(data_df$round, levels = c("overall", as.character(1:50)), ordered = TRUE)
  
  # Validate that mandatory columns exist
  required_cols_data <- c("question_code", "choice_label", "result", "round", "num_samples", "disp_status")
  required_cols_params <- c("title", "graph_type", "result_type", "disp_status", "main_variable", "top", 
                            "data_labels", "exclude_pns",	"exclude_dk",	"exclude_other", "wrap_title") 
  
  if (!all(required_cols_data %in% names(data_df)) || !all(required_cols_params %in% names(params_df))) {
    stop("Required columns missing in data or parameter dataframe.")
  }
  
  custom_order <- c('0', '1', '2', '3', '4', '4+', '5', '5+', '6', '6+', '7', '7+', '8', '8+', '9', '9+', '10', '10+')
  
  # Loop through each row in params_df
  for (i in seq_len(nrow(params_df))) {
    params <- params_df[i, , drop = FALSE]
    
    # Check if the graph should be created based on graph_type and result_type
    if (params$graph_type != "bar" || !(params$result_type %in% c("percent", "integer")) || params$orientation != "vertical") {
      message(paste("Skipping due to incompatible graph_type, orientation or result_type"))
      next
    }
    
    # Filter the data
    filtered_df <- data_df %>% 
      filter(
        question_code == params$main_variable,
        disp_status == params$disp_status
      )
    
    # Check for exclusion criteria based on parameters
    if (!is.na(params$exclude_other) && tolower(params$exclude_other) == 'yes') {
      exclude_other_labels <- c("other (please_specify)", "other (please specify)", "Other (please specify)", "other", "Other")
      filtered_df <- filtered_df %>% filter(!choice_label %in% exclude_other_labels)
    }
    
    if (!is.na(params$exclude_dk) && tolower(params$exclude_dk) == 'yes') {
      exclude_dk_labels <- c("don't know", "Don't know", "dk")
      filtered_df <- filtered_df %>% filter(!grepl(paste(exclude_dk_labels, collapse="|"), choice_label, ignore.case = TRUE))
    }
    
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
    
    # Check for 'overall' or specific rounds
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
    
    # Check if any of the custom labels are present in choice_label
    if (any(filtered_df$choice_label %in% custom_order)) {
      # Create a custom rank based on custom_order
      filtered_df <- filtered_df %>% 
        mutate(rank = match(choice_label, custom_order))
    } else {
      # Get the rank from the latest round data
      latest_round_data <- filtered_df %>% filter(round == round_latest) %>% 
        arrange(desc(result)) %>% 
        mutate(rank = row_number())
      
      # Update rank in filtered_df based on latest round's result
      filtered_df <- filtered_df %>% 
        left_join(latest_round_data %>% select(choice_label, rank), by = "choice_label")
    }
    
    # If 'top' is not NA and not an empty string, filter to keep only top X choice labels
    if (!is.na(params$top) && params$top != "") {
      top_n <- as.numeric(params$top)  # Convert 'top' to numeric
      top_labels <- latest_round_data %>% 
        head(top_n) %>% 
        pull(choice_label)
      
      filtered_df <- filtered_df %>% 
        filter(choice_label %in% top_labels)
    }
    
    if (nrow(filtered_df) == 0) {
      message("filtered_df is empty or NULL")
      next
    }
    
    num_rounds <- length(unique(filtered_df$round))
    num_choices <- length(unique(filtered_df$choice_label))
    
    # Generate custom palette based on the number of rounds
    chosen_palette <- create_custom_palette(color_start, color_end, num_rounds)
    
    # Create the plot
    if (as.character(params$latest_round) != "latest" || as.character(params$earliest_round) != "latest") {
      # Decrease dodge width for closer bars within the same group
      dodge_width <- 0.8 

      legend_labels <- paste0(unique(filtered_df$month), " (N=", scales::comma(unique(filtered_df$num_samples)), ")")
      
      p <- ggplot(filtered_df, aes(x = reorder(choice_label, rank), y = (result / 100), fill = round, group = round)) +
        geom_col(position = position_dodge(width = dodge_width), width = 0.8) +
        labs(fill = "Round") +  # Add legend title "Round"
        scale_fill_manual(values = chosen_palette, labels = legend_labels)  # Use custom palette for grouping
      plot_width <- 10  # Default plot width for grouped graphs
      
    } else {
      
      # Determine the number of unique bars
      num_bars <- length(unique(filtered_df$choice_label))
      
      # Conditionally set bar width and plot width based on the number of bars
      if (num_bars < 5) {
        bar_width <- 0.5
        plot_width <- 5
      } else {
        bar_width <- 0.8
        plot_width <- 10
      }
      
      p <- ggplot(filtered_df, aes(x = reorder(choice_label, rank), y = (result / 100))) +
        geom_bar(stat = "identity", fill = color_start, width = bar_width, position = "dodge") 
    }
    
    # Add subtitle only if there is one round
    if (num_rounds == 1) {
      single_round_month <- unique(filtered_df$month)
      single_round_samples <- unique(filtered_df$num_samples)
      p <- p + labs(subtitle = paste0(single_round_month, " (N=", scales::comma(single_round_samples), ")"))
    }
    
    # Decision to add data labels based on "data_labels" column from params_df
    if (is.na(params$data_labels)) {
      add_data_labels <- NULL  # Default behavior
    } else {
      add_data_labels <- tolower(as.character(params$data_labels)) == 'yes'
    }
    
    # Logic for adding or suppressing labels
    if (is.null(add_data_labels)) {
      if (num_rounds > 2 || num_choices > 8) {
        
        # Determine adj_val based on the number of rounds
        adj_val <- 3 - 0.25 * (num_rounds - 2)
        
        # Display label only for the most recent round when there are too many rounds or choices
        latest_round_data <- filtered_df %>% filter(round == max(round))
        p <- p + geom_text(
          data = latest_round_data,
          aes(label = format_label(result, params$result_type)),
          size = 3,
          vjust = -1,
          nudge_x = dodge_width / adj_val  # Adjust the label's position
        )
      } else {
        # Display labels for all rounds and choices
        p <- p + geom_text(
          aes(label = format_label(result, params$result_type)),
          size = 3,
          vjust = -1,
          position = position_dodge(.9)
        )
      }
    } else if (is.logical(add_data_labels) && add_data_labels) {
      # Always display labels when explicitly set to 'yes'
      p <- p + geom_text(
        aes(label = format_label(result, params$result_type)),
        size = 3,
        vjust = -1,
        position = position_dodge(.9)
      )
    } else if (is.logical(add_data_labels) && !add_data_labels) {
      # Suppress all labels when explicitly set to 'no'
    }
    
    # Determine conditions for reducing font size
    max_label_length <- max(nchar(unique(filtered_df$choice_label)))
    num_labels <- num_choices
    
    # Conditionally set font size
    font_size_value <- ifelse(max_label_length > 20 || num_labels > 15, 8, 12)
    
    # Determine the angle for x-axis text
    angle_value <- ifelse(all(nchar(unique(filtered_df$choice_label)) <= 3), 0, 50)
    
    # Determine vertical justification based on angle
    vjust_value <- ifelse(angle_value == 0, 0.5, 0.5)
    
    # Check for wrap_title being NA or 'yes'
    if (is.na(params$wrap_title) || tolower(params$wrap_title) == 'yes') {
      
      # Calculate the wrap width based on the plot width
      wrap_width <- ifelse(plot_width < 10, 40, 70)
      wrapped_title <- strwrap(params$title, width = wrap_width)  # Dynamic wrapping width
      
      num_title_lines <- length(wrapped_title)  # Calculate the number of lines in the title
      wrapped_title_text <- paste(wrapped_title, collapse = "\n")  # Concatenate lines with newline characters
      
      # Determine the height of the graph based on the number of title lines
      adjusted_height <- 6 + 0.5 * (num_title_lines - 1)  # Increase the height by 0.5 unit per extra line
    } else {
      wrapped_title_text <- params$title  # Use the original title
      adjusted_height <- 6  # Default height
    }
    
    # Decide the y-axis labels based on the result_type
    y_labels <- if (params$result_type == "percent") {
      "percent"
    } else if (params$result_type == "integer") {
      "integer"
    } else {
      "default"
    }
    
    # Custom y-label formatting function
    format_y_labels <- function(x, y_labels) {
      if (y_labels == "percent") {
        return(scales::percent(x))
      } else if (y_labels == "integer") {
        if (all(floor(x) == x)) {
          return(scales::comma(x * 100))
        } else {
          return(scales::comma(x))
        }
      } else {
        return(x)
      }
    }
    
    # Assign the wrapped title to the ggplot object
    p <- p + labs(
      title = wrapped_title_text,  # Use the wrapped title
      x = NULL,
      y = NULL
     ) + 
      scale_x_discrete(labels = label_wrap(width = 30)) +
      scale_y_continuous(labels = function(x) format_y_labels(x, y_labels), expand = expand_scale(mult = c(0, 0.1))) +
      theme(
        axis.text.x = element_text(
          angle = angle_value,  # Conditionally set angle
          hjust = 0.5,  # center-align
          vjust = vjust_value,  # Vertically adjust labels
          size = font_size_value,  # Conditionally set font size
          margin = margin(t = 10, r = 10, b = 10, l = 10),  # Add space around labels
          family = "Leelawadee"  # Set font family
        ),
        plot.margin = margin(1, 1, 1.5, 1, "cm"),  # Increase bottom margin of the plot
        plot.title = element_text(vjust = 2, family = "Leelawadee"),  # Set font family for title
        axis.text.y = element_text(family = "Leelawadee")  # Set font family for y-axis text
      )
    
    # Determine the subfolder based on the "disp_status" column
    subfolder <- ifelse(params$disp_status == "refugee", "refugees", ifelse(params$disp_status == "returnee",  "returnees", "overall"))
    
    # Generate sanitized, lowercase file name from title
    file_extension <- ifelse(params$export == "pdf", ".pdf", ".png")
    output_file_name <- paste0(sanitize_title(params$title), file_extension)
    
    # Create the subfolder if it doesn't exist
    subfolder_path <- file.path(output_folder, subfolder)
    if (!dir.exists(subfolder_path)) {
      dir.create(subfolder_path)
    }
    
    # Generate the full output file path including subfolder
    output_file_path <- file.path(subfolder_path, output_file_name)
    ggplot2::ggsave(output_file_path, plot = p, width = plot_width, height = adjusted_height)
    
    if (file.exists(output_file_path)) {
      message(paste("Successfully created graph:", output_file_path))
    } else {
      message(paste("Failure: Could not export graph as", output_file_path))
    }
  }
}


