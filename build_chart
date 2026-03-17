# Install and load required packages
if (!require("quantmod")) install.packages("quantmod")
if (!require("dplyr")) install.packages("dplyr")
if (!require("lubridate")) install.packages("lubridate")
if (!require("plotly")) install.packages("plotly")
if (!require("htmlwidgets")) install.packages("htmlwidgets")

library(quantmod)
library(dplyr)
library(lubridate)
library(plotly)
library(htmlwidgets)

# Custom color palette
custom_colors <- c( "#4CAF50", "#FF6B6B", "#5D9CEC", "#9C27B0", "#fdc500")

# Download S&P 500 data and clean duplicate rows
tomorrow_date <- Sys.Date() + 1
sp500 <- getSymbols("^GSPC", from = "2007-01-01", to = tomorrow_date, auto.assign = FALSE)
index(sp500) <- as.Date(index(sp500))
sp500 <- sp500[!duplicated(index(sp500), fromLast = TRUE), ]

# Convert to data frame
sp500_df <- data.frame(Date = index(sp500), coredata(sp500))
sp500_df <- sp500_df %>% select(Date, GSPC.Adjusted)
sp500_df$Date <- as.Date(sp500_df$Date)

# Function to create indexed series from a particular date
create_indexed_series <- function(data, start_date) {
  filtered_data <- data %>% filter(Date >= start_date)
  start_price <- filtered_data$GSPC.Adjusted[1]
  filtered_data$pct_change <- (filtered_data$GSPC.Adjusted / start_price - 1) * 100
  filtered_data$days_since_peak <- 0:(nrow(filtered_data)-1)
  
  first_negative <- which(filtered_data$pct_change < 0)[1]
  if (!is.na(first_negative)) {
    negative_period <- filtered_data[first_negative:nrow(filtered_data), ]
    first_positive_after_negative <- which(negative_period$pct_change >= 0)[1]
    
    if (!is.na(first_positive_after_negative)) {
      end_row <- first_negative + first_positive_after_negative - 1
    } else {
      end_row <- nrow(filtered_data) 
    }
  } else {
    end_row <- nrow(filtered_data) 
  }
  
  result <- filtered_data[1:end_row, ]
  result$start_date <- as.character(start_date)
  
  if (end_row < nrow(filtered_data)) {
    result$end_date <- as.character(filtered_data$Date[end_row])
  } else {
    result$end_date <- "No recovery yet"
  }
  return(result)
}

# Create all five series
series1 <- create_indexed_series(sp500_df, as.Date("2007-10-09"))
series2 <- create_indexed_series(sp500_df, as.Date("2020-02-19"))
series3 <- create_indexed_series(sp500_df, as.Date("2022-01-03"))
series4 <- create_indexed_series(sp500_df, as.Date("2025-02-19"))
series5 <- create_indexed_series(sp500_df, as.Date("2026-01-27"))

# Combine the series for plotting
all_series <- rbind(series1, series2, series3, series4, series5)

# Create custom legend labels
all_series <- all_series %>%
  group_by(start_date) %>%
  mutate(
    label = case_when(
      start_date == "2007-10-09" ~ "2008/09 recession (2007-10-09 to 2013-03-28)",
      start_date == "2020-02-19" ~ "2020 pandemic (2020-02-19 to 2020-08-18)",
      start_date == "2022-01-03" ~ "2022/2023 wokecession (2022-01-03 to 2024-01-19)",
      start_date == "2025-02-19" ~ "2025 Bidencession (2025-02-19 to 2025-06-27)",
      start_date == "2026-01-27" ~ "2026 Bidencession (2026-01-27 to present)"
    )
  ) %>%
  ungroup()

all_series$label <- factor(all_series$label, 
                           levels = c("2008/09 recession (2007-10-09 to 2013-03-28)", 
                                      "2020 pandemic (2020-02-19 to 2020-08-18)", 
                                      "2022/2023 wokecession (2022-01-03 to 2024-01-19)",
                                      "2025 Bidencession (2025-02-19 to 2025-06-27)",
                                      "2026 Bidencession (2026-01-27 to present)"
                                      ))

# Create plotly plot
sp500_plot <- plot_ly(data = all_series, x = ~days_since_peak, y = ~pct_change, 
                      color = ~label, 
                      colors = custom_colors,
                      type = 'scatter', 
                      mode = 'lines',
                      line=list(width=1.4)
                      ) %>%
  layout(
    title = list(text = "S&P 500 declines indexed from market peaks", x = 0.05), # Added title directly to plot
    margin = list(l = 80, r = 50, t = 80, b = 100, pad = 10),
    xaxis = list(title = list(text = "Days from peak", standoff = 10), rangeslider = list(visible = TRUE, thickness = 0.08), range = c(0, 300), showgrid = FALSE, zeroline = FALSE),
    yaxis = list(title = list(text = "Indexed change (%)", standoff = 10), showgrid = TRUE, range = c(-61, 5), zeroline = TRUE),
    legend = list(orientation = "v", xanchor = "left", yanchor = "top", x = 0, y = 1.3),
    autosize = TRUE
  ) %>% config(displayModeBar = FALSE, responsive = TRUE)

# Save the plot directly as an HTML file
saveWidget(sp500_plot, "index.html", selfcontained = TRUE, title = "Comparing crises")
