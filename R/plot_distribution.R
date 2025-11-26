#' Plot distribution with histogram and density overlay
#'
#' @param data Numeric vector or data frame with numeric column
#' @param bins Number of histogram bins (default: NULL, no histogram). Use "Sturges" for Sturges' rule or a numeric value.
#' @param groups Numeric vector of threshold values to split data into groups (default: NULL, no grouping). When provided, splits data at thresholds and plots each group with its own color.
#' @param title Plot title (default: "Distribution")
#' @return ggplot2 object showing histogram with density curve and statistical markers
#' @export
#' @examples
#' plot_distribution(rnorm(1000))
#' plot_distribution(rnorm(1000), groups = c(0), title = "Split at 0")
#' plot_distribution(rnorm(1000), groups = c(-1, 1), title = "Three groups")
plot_distribution <- function(data, bins = NULL, groups = NULL, title = "Distribution") {
  # Handle different input types
  if (is.vector(data) || is.atomic(data)) {
    data <- tibble::tibble(value = data)
  } else if (is.data.frame(data)) {
    numeric_cols <- sapply(data, is.numeric)
    if (!any(numeric_cols)) {
      stop("No numeric columns found in the data")
    }
    first_numeric <- names(data)[which(numeric_cols)[1]]
    data <- tibble::tibble(value = data[[first_numeric]])
  }

  # Calculate number of bins using Sturges' rule if specified
  if (!is.null(bins) && bins == "Sturges") {
    n <- nrow(data)
    bins <- 1 + ceiling(3.3322 * log10(n))
  }

  # Define color palette for groups
  group_colors <- c("#329f32", "#d62728", "#b8860b", "#1f77b4", "#9467bd", "#8c564b", "#e377c2")
  
  # If no groups specified, plot as single distribution
  if (is.null(groups)) {
    mean_val <- mean(data$value, na.rm = TRUE)
    sd_val <- sd(data$value, na.rm = TRUE)
    
    p <- ggplot2::ggplot(data, ggplot2::aes(x = value))
    
    if (!is.null(bins)) {
      p <- p +
        ggplot2::geom_histogram(
          ggplot2::aes(y = ggplot2::after_stat(density)),
          bins = bins,
          fill = group_colors[1],
          alpha = 0.35
        )
    }
    
    p <- p +
      ggplot2::geom_density(
        ggplot2::aes(y = ggplot2::after_stat(density)),
        fill = group_colors[1],
        alpha = .45,
        linewidth = .1
      ) +
      ggplot2::geom_vline(xintercept = mean_val, color = "#000000", linetype = "dashed", linewidth = .8) +
      ggplot2::geom_vline(xintercept = mean_val + sd_val, color = "#4e4e4e", linetype = "dashed", linewidth = .5) +
      ggplot2::geom_vline(xintercept = mean_val - sd_val, color = "#4e4e4e", linetype = "dashed", linewidth = .5) +
      ggplot2::geom_vline(xintercept = mean_val + 2 * sd_val, color = "#a3a3a3", linetype = "dashed", linewidth = .35) +
      ggplot2::geom_vline(xintercept = mean_val - 2 * sd_val, color = "#a3a3a3", linetype = "dashed", linewidth = .35)
    
    # Create legend text
    legend_text <- sprintf("μ=%.2f  σ=%.2f", mean_val, sd_val)
    
    p <- p +
      ggplot2::annotate("label", x = Inf, y = Inf, label = legend_text, 
                       hjust = 1.05, vjust = 1.1, size = 3.5, 
                       fill = "white", alpha = 0.9, label.padding = ggplot2::unit(0.25, "lines")) +
      ggplot2::labs(title = title, x = "Value", y = "Density") +
      ggplot2::theme_minimal() +
      ggplot2::theme(panel.grid = ggplot2::element_blank())
    
    return(p)
  }
  
  # Groups specified - split data and plot by group
  groups <- sort(groups)
  n_groups <- length(groups) + 1
  group_labels <- paste0("g", 1:n_groups)
  
  # Assign each value to a group (similar to analyse())
  data <- data |>
    dplyr::mutate(
      group = dplyr::case_when(
        value < groups[1] ~ group_labels[1],
        TRUE ~ group_labels[n_groups]
      )
    )
  
  if (length(groups) > 1) {
    for (i in 2:length(groups)) {
      data <- data |>
        dplyr::mutate(
          group = dplyr::if_else(
            value >= groups[i - 1] & value < groups[i],
            group_labels[i],
            group
          )
        )
    }
  }
  
  # Create base plot
  p <- ggplot2::ggplot(data, ggplot2::aes(x = value))
  
  # Reverse colors so rightmost group gets first color (green), leftmost gets last
  reversed_colors <- rev(group_colors[1:n_groups])
  
  # Add histogram if specified (combined for all groups)
  if (!is.null(bins)) {
    p <- p +
      ggplot2::geom_histogram(
        ggplot2::aes(y = ggplot2::after_stat(density), fill = group),
        bins = bins,
        alpha = 0.35,
        position = "identity"
      ) +
      ggplot2::scale_fill_manual(values = reversed_colors)
  }
  
  # Add density curves and statistics for each group, collect legend data
  legend_data <- list()
  
  for (i in 1:n_groups) {
    group_data <- data |> dplyr::filter(group == group_labels[i])
    if (nrow(group_data) > 0) {
      # Use reversed color: rightmost group (highest i) gets first color
      group_color <- reversed_colors[i]
      mean_val <- mean(group_data$value, na.rm = TRUE)
      sd_val <- sd(group_data$value, na.rm = TRUE)
      
      # Store for legend
      legend_data[[i]] <- list(color = group_color, mean = mean_val, sd = sd_val)
      
      # Add density curve for this group
      p <- p +
        ggplot2::geom_density(
          data = group_data,
          ggplot2::aes(y = ggplot2::after_stat(density)),
          fill = group_color,
          alpha = .45,
          linewidth = .1
        )
      
      # Add mean and SD lines for this group
      p <- p +
        ggplot2::geom_vline(xintercept = mean_val, color = group_color, linetype = "dashed", linewidth = .8, alpha = 0.8) +
        ggplot2::geom_vline(xintercept = mean_val + sd_val, color = group_color, linetype = "dashed", linewidth = .5, alpha = 0.6) +
        ggplot2::geom_vline(xintercept = mean_val - sd_val, color = group_color, linetype = "dashed", linewidth = .5, alpha = 0.6) +
        ggplot2::geom_vline(xintercept = mean_val + 2 * sd_val, color = group_color, linetype = "dashed", linewidth = .35, alpha = 0.4) +
        ggplot2::geom_vline(xintercept = mean_val - 2 * sd_val, color = group_color, linetype = "dashed", linewidth = .35, alpha = 0.4)
    }
  }
  
  # Add stacked colored legend boxes  
  for (i in seq_along(legend_data)) {
    item <- legend_data[[i]]
    legend_text <- sprintf("μ=%.2f σ=%.2f", item$mean, item$sd)
    vjust_offset <- 1.1 + (i - 1) * 2
    
    p <- p +
      ggplot2::annotate("label", x = Inf, y = Inf, label = legend_text,
                       hjust = 1.05, vjust = vjust_offset, size = 5,
                       fill = item$color, color = "white", alpha = 0.9,
                       label.padding = ggplot2::unit(0.3, "lines"),
                       fontface = "bold")
  }
  
  # Add vertical lines at group boundaries with the color of the group to the right
  for (i in 1:length(groups)) {
    # Group i+1 is to the right of boundary i, and uses reversed_colors[i+1]
    boundary_color <- reversed_colors[i + 1]
    p <- p +
      ggplot2::geom_vline(
        xintercept = groups[i],
        color = boundary_color,
        linetype = "solid",
        linewidth = .6,
        alpha = .9
      )
  }
  
  p <- p +
    ggplot2::labs(title = title, x = "Value", y = "Density") +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid = ggplot2::element_blank(), legend.position = "none")
  
  return(p)
}
