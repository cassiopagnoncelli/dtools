#' @description Display returns distribution
#' @param report A report object.
#' @param chart A logical indicating whether to display the plot.
#' @param export_chart A logical indicating whether to save the plot as an HTML
#' file.
#' @param file_name A character string specifying the name of the HTML file to
#' save the plot to.
#' @param log_returns A logical indicating whether to use log of returns + 1. Default is TRUE.
chart_returns <- function(report,
                          chart = TRUE,
                          export_chart = FALSE,
                          file_name = "returns_plot.html",
                          log_returns = TRUE,
                          include_density_fun = FALSE) {
  df <- df_returns(report, log_returns)

  # Custom bandwidth for density estimation
  bandwidth <- 0.045

  # Initialize plotly plot
  plotly_plot <- plotly::plot_ly()

  if (nrow(df[df$outcome == "Wins", ]) > 0) {
    # Create density plot for Wins
    wins_density <- density(df$returns[df$outcome == "Wins"], bw = bandwidth)

    # Calculate the mean of wins
    mean_wins <- mean(df$returns[df$outcome == "Wins"])

    # Add Wins density and mean to the plot
    plotly_plot <- plotly_plot %>%
      plotly::add_trace(
        x = wins_density$x,
        y = wins_density$y,
        type = "scatter",
        mode = "lines",
        name = "Wins",
        line = list(color = "#2ce02c", width = .4),
        hoverinfo = "none"
      ) %>%
      plotly::add_trace(
        x = c(mean_wins, mean_wins),
        y = c(0, max(wins_density$y)),
        type = "scatter",
        mode = "lines",
        name = "Mean Wins",
        line = list(color = "#2ce02c", dash = "dash", width = 1),
        hoverinfo = "none"
      ) %>%
      plotly::add_trace(
        x = wins_density$x,
        y = wins_density$y,
        type = "scatter",
        mode = "lines",
        fill = "tozeroy",
        fillcolor = "rgba(44, 165, 44, 0.45)",
        name = "Wins Area",
        line = list(color = "transparent"),
        hoverinfo = "none"
      ) %>%
      plotly::add_trace(
        x = c(mean_wins),
        y = c(0),
        type = "scatter",
        mode = "markers+text",
        name = "Mean Wins Mark",
        marker = list(color = "#2ce02c", size = 6),
        text = paste0("Avg Win: ", scales::percent(mean_wins, accuracy = 0.1)),
        textposition = "bottom",
        hoverinfo = "none",
        textfont = list(size = 10)
      )
  }

  if (nrow(df[df$outcome == "Losses", ]) > 0) {
    # Create density plot for Losses
    losses_density <- density(df$returns[df$outcome == "Losses"], bw = bandwidth)

    # Calculate the mean of losses
    mean_losses <- mean(df$returns[df$outcome == "Losses"])

    # Add Losses density and mean to the plot
    plotly_plot <- plotly_plot %>%
      plotly::add_trace(
        x = losses_density$x,
        y = losses_density$y,
        type = "scatter",
        mode = "lines",
        name = "Losses",
        line = list(color = "#d62728", width = .4),
        hoverinfo = "none"
      ) %>%
      plotly::add_trace(
        x = c(mean_losses, mean_losses),
        y = c(0, max(losses_density$y)),
        type = "scatter",
        mode = "lines",
        name = "Mean Losses",
        line = list(color = "#d62728", dash = "dash", width = 1),
        hoverinfo = "none"
      ) %>%
      plotly::add_trace(
        x = losses_density$x,
        y = losses_density$y,
        type = "scatter",
        mode = "lines",
        fill = "tozeroy",
        fillcolor = "rgba(214, 39, 40, 0.3)",
        name = "Losses Area",
        line = list(color = "transparent"),
        hoverinfo = "none"
      ) %>%
      plotly::add_trace(
        x = c(mean_losses),
        y = c(0),
        type = "scatter",
        mode = "markers+text",
        name = "Mean Losses Mark",
        marker = list(color = "#d62728", size = 6),
        text = paste0("Avg Loss: ", scales::percent(mean_losses, accuracy = 0.1)),
        textposition = "bottom",
        hoverinfo = "none",
        textfont = list(size = 10)
      )
  }

  # Finalize layout
  plotly_plot <- plotly_plot %>%
    plotly::layout(
      title = list(
        text = NA,
        font = list(family = "Arial, sans-serif", size = 16, color = "#333")
      ),
      xaxis = list(
        title = "Returns (%)",
        zeroline = TRUE,
        zerolinecolor = "#787878",
        tickformat = ".0%",
        tickfont = list(family = "Arial, sans-serif", size = 12, color = "#333")
      ),
      yaxis = list(
        title = NA,
        showticklabels = FALSE,
        tickfont = list(family = "Arial, sans-serif", size = 12, color = "#333")
      ),
      plot_bgcolor = "#f9f9f9",
      paper_bgcolor = "#f9f9f9",
      showlegend = FALSE
    )

  # Display the plot
  if (chart) {
    print(plotly_plot)
  }

  # Export the plot as an HTML file
  if (export_chart) {
    htmlwidgets::saveWidget(plotly_plot, file_name)
  }

  return(plotly_plot)
}
