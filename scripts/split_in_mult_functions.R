

# Data validation function
validate_data <- function(data_df, params_df) {
  # Validation logic
  required_cols_data <- c("question_code", "choice_label", "result", "round", "num_samples", "disp_status")
  required_cols_params <- c("title", "graph_type", "result_type", "disp_status", "main_variable", "top", 
                            "data_labels", "exclude_pns", "exclude_dk", "exclude_other", "wrap_title") 
  
  if (!all(required_cols_data %in% names(data_df)) || !all(required_cols_params %in% names(params_df))) {
    return(FALSE)
  }
  return(TRUE)
}

# Data filtering function
filter_data <- function(data_df, params) {
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
  
  return(filtered_df)
}

# Rank assignment function
assign_ranks <- function(filtered_df, custom_order) {
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
  return(filtered_df)
}

# Function to create custom color palette
create_custom_palette <- function(color_start, color_end, n) {
  interpolator <- colorRampPalette(c(color_start, color_end))
  return(interpolator(n))
}

# Customize plot function
customize_plot <- function(p, filtered_df, params, num_choices, num_title_lines, num_rounds, color_start, color_end) {
  
  # Extract title from params dataframe
  plot_title <- params$title
  
  # Check for wrap_title being NA or 'yes'
  if (is.na(params$wrap_title) || tolower(params$wrap_title) == 'yes') {
    wrapped_title <- strwrap(plot_title, width = 40)  # Split the title into lines of up to 40 characters
    num_title_lines <- length(wrapped_title)  # Calculate the number of lines in the title
    wrapped_title_text <- paste(wrapped_title, collapse = "\n")  # Concatenate lines with newline characters
  } else {
    wrapped_title_text <- plot_title  # Use the original title
  }
  
  # Determine conditions for reducing font size
  max_label_length <- max(nchar(unique(filtered_df$choice_label)))
  
  # Conditionally set font size
  font_size_value <- ifelse(max_label_length > 20 || num_choices > 15, 8, 12)
  
  # Determine the angle for x-axis text
  angle_value <- ifelse(all(nchar(unique(filtered_df$choice_label)) <= 3), 0, 50)
  
  # Determine vertical justification based on angle
  vjust_value <- ifelse(angle_value == 0, 0.5, 0.5)
  
  # Generate custom palette
  chosen_palette <- create_custom_palette(color_start, color_end, num_rounds)
  
  # Generate legend labels based on month and num_samples in filtered_df
  legend_labels <- paste0(unique(filtered_df$month), " (N=", scales::comma(unique(filtered_df$num_samples)), ")")
  
  # Determine dodge_width based on conditions
  if (as.character(params$latest_round) != "latest" || as.character(params$earliest_round) != "latest") {
    dodge_width <- 0.8
  } else {
    dodge_width <- 0.9  # You can set this to a default value or another conditional value
  }
  
  # Calculate plot_width and adjusted_height here
  if (as.character(params$latest_round) != "latest" || as.character(params$earliest_round) != "latest") {
    plot_width <- 10  # Default plot width for grouped graphs
  } else {
    # Determine the number of unique bars (num_choices)
    if (num_choices < 5) {
      plot_width <- 5
    } else {
      plot_width <- 10
    }
  }
  
  # Calculate the adjusted_height based on the number of title lines
  adjusted_height <- 6 + 0.5 * (num_title_lines - 1)  # Increase the height by 0.5 unit per extra line
  
  p <- p + theme(
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
  
  # Add subtitle only if there is one round
  if (num_rounds == 1) {
    single_round_month <- unique(filtered_df$month)
    single_round_samples <- unique(filtered_df$num_samples)
    p <- p + labs(subtitle = paste0(single_round_month, " (N=", scales::comma(single_round_samples), ")"))
  }
  
  # Add data labels based on conditions from the original function
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
  
  # Add title and legend
  p <- p + labs(
    title = wrapped_title_text,  # Make sure this variable is defined based on 'wrap_title'
    fill = "Round"  # Legend title
  ) + scale_fill_manual(values = chosen_palette, labels = legend_labels)  # Custom legend
  
  return(list(customized_plot = p, plot_width = plot_width, adjusted_height = adjusted_height))
}

# File operations function
handle_file_ops <- function(p, params, output_folder, plot_width, adjusted_height) {
  # Determine the subfolder based on the "disp_status" column
  subfolder <- ifelse(params$disp_status == "refugee", "refugees", 
                      ifelse(params$disp_status == "returnee", "returnees", "overall"))
  
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
    return(TRUE)
  } else {
    message(paste("Failure: Could not export graph as", output_file_path))
    return(FALSE)
  }
}


# Main function
create_bar_graph_vertical <- function(data_df, params_df, round_latest, output_folder, color_start, color_end) {
  
  # Validate the data
  if (!validate_data(data_df, params_df)) {
    stop("Validation failed.")
  }
  
  custom_order <- c('0', '1', '2', '3', '4', '4+', '5', '5+', '6', '6+', '7', '7+', '8', '8+', '9', '9+', '10', '10+')
  
  # Loop through each row in params_df
  for (i in seq_len(nrow(params_df))) {
    params <- params_df[i, , drop = FALSE]
    
    # Filter the data
    filtered_df <- filter_data(data_df, params)
    
    if (nrow(filtered_df) == 0) {
      message("Failure: Could not export graph as filtered_df is empty or NULL")
      next  # Skip to the next iteration of the loop
    }
    
    if (all(is.na(filtered_df$choice_label))) {
      message("Failure: Could not export graph as choice_label column contains only NA values.")
      next
    }
    
    # Calculate num_rounds
    num_rounds <- length(unique(filtered_df$round))
    
    # Calculate num_choices based on filtered_df
    num_choices <- length(unique(filtered_df$choice_label))
    
    # Calculate max_label_length based on filtered_df
    max_label_length <- max(nchar(unique(filtered_df$choice_label)))
    
    # Count the number of lines in the title
    num_title_lines <- length(strwrap(params$title, width = 70))
    
    # Assign ranks
    filtered_df <- assign_ranks(filtered_df, custom_order)
    
    # Create the basic plot
    p <- ggplot(filtered_df, aes(x = reorder(choice_label, rank), y = (result / 100))) +
      geom_bar(stat = "identity", fill = color_start, width = 0.8, position = "dodge")
    
    # Customize the plot
    customization_result <- customize_plot(p, filtered_df, params, num_choices, num_title_lines, num_rounds, color_start, color_end)
    p <- customization_result$customized_plot
    plot_width <- customization_result$plot_width
    adjusted_height <- customization_result$adjusted_height
    
    # Handle file operations
    if (!handle_file_ops(p, params, output_folder, plot_width, adjusted_height)) {
      message("File operation failed.")
    }
  }
}
