#' Plot All Distribution Pie Charts
#'
#' This function automatically plots pie charts for all distribution fields in the statistics data.
#'
#' @param stats_data Statistics data list containing multiple distribution fields
#' @param plot_names Optional vector of distribution names to plot (default: all available)
#'
#' @return A combined ggplot2 object if patchwork is available, otherwise a list of ggplot2 objects
#'
#' @examples
#' \dontrun{
#' data <- get_project_stats('PRJDB13875')
#' plot_distributions(data)
#' plot_distributions(data, plot_names = c('countries', 'systems'))
#' }
#'
#' @export
plot_distributions <- function(stats_data, plot_names = NULL) {
  if (!is.list(stats_data)) {
    stop("Input data must be a list.")
  }
  available_plots <- names(stats_data)[sapply(stats_data, function(x) {
    is.list(x) && !is.null(x$distribution)
  })]
  if (is.null(plot_names)) {
    plot_names <- available_plots
  } else {
    # Check if any requested plot_names are not available
    invalid_plots <- plot_names[!plot_names %in% available_plots]
    if (length(invalid_plots) > 0) {
      stop(paste("The following plot names are not available:", paste(invalid_plots, collapse = ", "),
                   "\nAvailable plots:", paste(available_plots, collapse = ", ")))
    }
    plot_names <- plot_names[plot_names %in% available_plots]
  }
  if (length(plot_names) == 0) {
    stop(paste("No plottable distribution data found. Available plots:", paste(available_plots, collapse = ", ")))
  }
  plot_list <- list()
  for (i in seq_along(plot_names)) {
    plot_name <- plot_names[i]
    plot_data <- stats_data[[plot_name]]
    dist_data <- plot_data$distribution
    if (is.matrix(dist_data)) {
      dist_df <- as.data.frame(dist_data, stringsAsFactors = FALSE)
    } else {
      dist_df <- dist_data
    }
    col_names <- colnames(dist_df)
    # Guess category and count columns
    category_col <- col_names[length(col_names)-1]
    count_col <- col_names[length(col_names)]

    df <- data.frame(
      category = dist_df[[category_col]],
      count = as.numeric(dist_df[[count_col]]),
      stringsAsFactors = FALSE
    )
    df$percentage <- df$count / sum(df$count) * 100
    
    # Sort by count (largest to smallest) and limit categories
    df_sorted <- df[order(df$count, decreasing = TRUE), ]
    
    if (length(plot_names) > 1 && nrow(df) > 5) {
      # For multiple plots: show only top 5 categories
      df_top <- df_sorted[1:5, ]
      others_count <- sum(df_sorted$count[6:nrow(df_sorted)])
      others_percentage <- sum(df_sorted$percentage[6:nrow(df_sorted)])
    } else if (nrow(df) > 10) {
      # For single plot: show only top 10 categories
      df_top <- df_sorted[1:10, ]
      others_count <- sum(df_sorted$count[11:nrow(df_sorted)])
      others_percentage <- sum(df_sorted$percentage[11:nrow(df_sorted)])
    } else {
      # No need to limit categories
      df_top <- df_sorted
      others_count <- 0
      others_percentage <- 0
    }
    
    # Add "Others" category if needed
    if (others_count > 0) {
      df_others <- data.frame(
        category = "Others",
        count = others_count,
        percentage = others_percentage,
        stringsAsFactors = FALSE
      )
      df <- rbind(df_top, df_others)
    } else {
      df <- df_top
    }
    
    # Recalculate positions for the new data
    df$percentage <- df$count / sum(df$count) * 100
    
    df$label <- paste0(df$category, "\n", df$count, " (", round(df$percentage, 1), "%)")
    df$ymax <- cumsum(df$percentage)
    df$ymin <- c(0, head(df$ymax, n = -1))
    df$labelPosition <- (df$ymax + df$ymin) / 2
    # Title
    title_map <- c(
      countries = "Country Distribution",
      systems = "System Distribution",
      bodySites = "Body Site Distribution",
      phenotypes = "Phenotype Distribution",
      variableRegions = "Variable Region Distribution",
      sequencingTypes = "Sequencing Type Distribution"
    )
    title <- title_map[plot_name]
    if (is.null(title)) title <- paste0(plot_name, " Distribution")
        # Colors - use a color palette that can handle any number of categories
    n_categories <- length(unique(df$category))
      # Use a color palette that can generate many colors
    colors <- grDevices::colorRampPalette(c("#FF6B6B", "#4ECDC4", "#45B7D1", "#96CEB4", "#FFEAA7",
                                            "#DDA0DD", "#98D8C8", "#F7DC6F", "#BB8FCE", "#85C1E9"))(n_categories)
    p <- ggplot2::ggplot(df, ggplot2::aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = category)) +
     ggplot2::geom_rect() +
     ggplot2::coord_polar(theta = "y") +
     ggplot2::xlim(c(2, 4)) +
     ggplot2::scale_fill_manual(values = colors) +
     ggplot2::labs(title = title, fill = "Category") +
     ggplot2::theme_void() +
     ggplot2::theme(
       plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
       legend.position = "right",
       legend.title = ggplot2::element_text(size = 12),
       legend.text = ggplot2::element_text(size = 10)
     ) +
           ggrepel::geom_text_repel(
        ggplot2::aes(x = 3.5, y = labelPosition, label = label),
        size = 4,
        color = "black",
        fontface = "bold",
        max.overlaps = Inf,
        min.segment.length = 0,
        segment.color = "gray50",
        segment.size = 0.5
      )
    plot_list[[i]] <- p
  }
  if (length(plot_list) == 1) {
    return(plot_list[[1]])
      } else if (requireNamespace("patchwork", quietly = TRUE)) {
      # Limit the number of plots to avoid display issues
      max_plots <- 6
      if (length(plot_list) > max_plots) {
        warning(paste("Too many plots (", length(plot_list), "). Showing only first", max_plots, "plots."))
        plot_list <- plot_list[1:max_plots]
      }
      
      combined_plot <- plot_list[[1]]
      for (i in 2:length(plot_list)) {
        combined_plot <- combined_plot + plot_list[[i]]
      }
      
      # Adjust layout based on number of plots
      if (length(plot_list) <= 2) {
        return(combined_plot + patchwork::plot_layout(ncol = 2))
      } else if (length(plot_list) <= 4) {
        return(combined_plot + patchwork::plot_layout(ncol = 2, nrow = 2))
      } else {
        return(combined_plot + patchwork::plot_layout(ncol = 3, nrow = 2))
      }
    } else {
      warning("Install patchwork for combined chart display. Returning list of plots.")
      return(plot_list)
    }
}
