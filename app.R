# ============================================
# Bike Sharing Demand Dashboard
# ============================================

library(shiny)
library(shinydashboard)
library(ggplot2)
library(reticulate)
library(dplyr)
library(scales)
library(shinyjs)
library(packcircles)
library(tidyr)

# ================================
# 1. Setup & Configuration
# ================================

# Define Primary Color
primary_color <- "#32CD32"

# Use reticulate to source the Python script
# Ensure the path is correct relative to app.R
# We assume app.R is in the root and the script is in "Data product/"
python_script_path <- "Data product/linear-regression.py"

# Check if file exists
if (!file.exists(python_script_path)) {
  stop(paste("Python script not found at:", python_script_path))
}

# Configure reticulate to use the local virtual environment
# We use file.path(getwd(), ".venv") to ensure we point to the local folder
# and not the default ~/.virtualenvs location.
venv_path <- file.path(getwd(), ".venv")
if (dir.exists(venv_path)) {
  use_virtualenv(venv_path, required = TRUE)
}

# Source the Python script
# This will run the top-level code in the script (training the model)
# and make functions like get_predictions available.
source_python(python_script_path)

# Load Data for Tab 1 (Insight)
# Replace with your actual path if different
csv_path <- "Data product/bikehour.csv"
if (file.exists(csv_path)) {
  df <- read.csv(csv_path)
} else {
  # Placeholder if file not found, to prevent crash during dev
  warning("CSV file not found. Using placeholder data.")
  df <- data.frame(
    weekday = rep(0:6, each = 24),
    hr = rep(0:23, 7),
    cnt = sample(10:500, 24 * 7, replace = TRUE),
    weathersit = sample(1:3, 24 * 7, replace = TRUE),
    registered = sample(5:400, 24 * 7, replace = TRUE),
    casual = sample(5:100, 24 * 7, replace = TRUE)
  )
}

# Pre-process data for plotting
# Map integer weekday to labels
weekday_labels <- c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")
df$weekday_label <- factor(df$weekday, levels = 0:6, labels = weekday_labels)

# Load Feature Importance Data
# Using reticulate to use pandas to read excel
pd <- import("pandas")
feature_imp_path <- "Feature importance.xlsx"
if (file.exists(feature_imp_path)) {
  feature_df <- pd$read_excel(feature_imp_path)

  # Fix for malformed Excel (all data in one column)
  if (ncol(feature_df) == 1) {
    # The file seems to have all data in one column separated by whitespace
    # Header is in the column name, data is in the rows

    # Extract header names
    header_line <- names(feature_df)[1]
    col_names <- scan(text = header_line, what = "character", quiet = TRUE)

    # Extract data
    # We assume the data rows have an index number at the beginning which we want to discard
    # if the number of columns in data is 1 greater than header
    data_lines <- as.character(feature_df[[1]])
    temp_data <- read.table(text = data_lines, header = FALSE, stringsAsFactors = FALSE)

    if (ncol(temp_data) == length(col_names) + 1) {
      # Drop the first column (index)
      feature_df <- temp_data[, -1]
    } else {
      feature_df <- temp_data
    }

    colnames(feature_df) <- col_names
  }
} else {
  warning("Feature importance file not found.")
  feature_df <- data.frame(feature = character(), importance_cnt = numeric())
}

# Load Model Comparison Data
model_path <- "Model.xlsx"
if (file.exists(model_path)) {
  model_df <- pd$read_excel(model_path)

  # Fix for malformed Excel (all data in one column)
  if (ncol(model_df) == 1) {
    # Extract header names from the column name
    header_line <- names(model_df)[1]
    # Remove "..." or extra spaces if any
    col_names <- scan(text = header_line, what = "character", quiet = TRUE)

    # Extract data
    data_lines <- as.character(model_df[[1]])
    temp_data <- read.table(text = data_lines, header = FALSE, stringsAsFactors = FALSE)

    # Handle potential index column mismatch
    if (ncol(temp_data) == length(col_names) + 1) {
      model_df <- temp_data[, -1]
    } else {
      model_df <- temp_data
    }

    colnames(model_df) <- col_names

    # Ensure numeric columns are numeric
    # Assuming columns are Model, MAE, RMSE, R^2
    # We need to handle "R^2" specifically if it has special chars
    # But based on debug output, it seems to be just text

    # Convert numeric columns
    # We assume the first column is Model (text), rest are numeric
    for (i in 2:ncol(model_df)) {
      model_df[[i]] <- as.numeric(model_df[[i]])
    }
  }
} else {
  warning("Model comparison file not found.")
  model_df <- data.frame(Model = character(), MAE = numeric(), RMSE = numeric(), "R^2" = numeric())
}


# ================================
# 2. UI Definition
# ================================

ui <- dashboardPage(
  skin = "green", # Closest built-in skin, we will override with CSS
  dashboardHeader(
    title = "Bike Sharing Dashboard",
    tags$li(
      class = "dropdown",
      tags$img(src = "logo.png", height = "40px", style = "margin-top: 5px; margin-right: 10px;")
    )
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Insight & Exploration", tabName = "insight", icon = icon("chart-bar")),
      menuItem("Prediction & Decision", tabName = "prediction", icon = icon("robot")),
      menuItem("Linear Regression", tabName = "linear_pred", icon = icon("chart-line")),
      menuItem("Model Performance", tabName = "model", icon = icon("balance-scale"))
    )
  ),
  dashboardBody(
    useShinyjs(),
    # Custom CSS for Primary Color #32CD32
    tags$head(tags$style(HTML(paste0("
      .skin-green .main-header .logo { background-color: ", primary_color, "; }
      .skin-green .main-header .navbar { background-color: ", primary_color, "; }
      .skin-green .main-sidebar .sidebar .sidebar-menu .active a { border-left-color: ", primary_color, "; }
      .box.box-solid.box-primary>.box-header { background-color: ", primary_color, "; background: ", primary_color, "; }
      .btn-primary { background-color: ", primary_color, "; border-color: ", primary_color, "; }
      .btn-primary:hover { background-color: #28a428; border-color: #28a428; }

      /* Decision Card Styles */
      .decision-card {
        padding: 20px;
        border-radius: 10px;
        color: white;
        font-weight: bold;
        font-size: 1.2em;
        margin-top: 20px;
        box-shadow: 0 4px 8px rgba(0,0,0,0.2);
      }
      .decision-alert { background-color: #d9534f; } /* Red */
      .decision-surge { background-color: #f0ad4e; } /* Orange */
      .decision-promo { background-color: #5bc0de; } /* Blue */
      .decision-normal { background-color: #5cb85c; } /* Green */

      /* Segmented Control Styles */
      .segmented-control {
        display: inline-block;
        border: 1px solid #333;
        border-radius: 20px;
        padding: 0;
        overflow: hidden;
        background-color: transparent;
        float: right;
        margin-top: -5px; /* Adjust vertical alignment */
      }
      .segmented-control a {
        padding: 5px 15px;
        display: inline-block;
        text-decoration: none;
        color: #333;
        font-weight: normal;
        transition: all 0.3s ease;
        cursor: pointer;
      }
      .segmented-control a:hover {
        background-color: rgba(0,0,0,0.1);
      }
      .segmented-control a.active {
        background-color: #333;
        color: white;
      }

      /* Chart Transition Container */
      .chart-container {
        position: relative;
        height: 500px;
        width: 100%;
      }
      .chart-layer {
        position: absolute;
        top: 0;
        left: 0;
        width: 100%;
        height: 100%;
        transition: opacity 0.5s ease-in-out;
      }
      .chart-visible {
        opacity: 1;
        z-index: 10;
        pointer-events: auto;
      }
      .chart-hidden {
        opacity: 0;
        z-index: 1;
        pointer-events: none;
      }

    ")))),
    tabItems(
      # ----------------------------
      # Tab 1: Insight & Exploration
      # ----------------------------
      tabItem(
        tabName = "insight",
        fluidRow(
          box(
            title = "Filters", status = "success", solidHeader = TRUE, width = 3,
            checkboxGroupInput("weather_filter", "Weather Condition:",
              choices = list(
                "Clear/Few Clouds" = 1,
                "Mist/Cloudy" = 2,
                "Light Rain/Snow" = 3,
                "Heavy Rain/Snow" = 4
              ),
              selected = c(1, 2, 3, 4)
            ),
            radioButtons("user_type", "User Type:",
              choices = list(
                "All Users" = "cnt",
                "Membership" = "registered",
                "Non-Membership" = "casual"
              ),
              selected = "cnt"
            ),
            helpText("Note: Registered users are typically commuters, while Casual users are tourists.")
          ),
          box(
            title = div(
              "Demand Heatmap & Feature Importance",
              div(
                class = "segmented-control",
                tags$a(id = "btn_time", class = "active", onclick = "Shiny.setInputValue('view_mode', 'time');", "By Time"),
                tags$a(id = "btn_feature", onclick = "Shiny.setInputValue('view_mode', 'feature');", "By Feature")
              )
            ), status = "success", solidHeader = TRUE, width = 9,
            div(
              class = "chart-container",
              div(
                id = "layer_time", class = "chart-layer chart-visible",
                plotOutput("heatmap_plot", height = "100%")
              ),
              div(
                id = "layer_feature", class = "chart-layer chart-hidden",
                plotOutput("feature_plot", height = "100%")
              )
            )
          )
        )
      ),

      # ----------------------------
      # Tab 2: Prediction & Decision
      # ----------------------------
      tabItem(
        tabName = "prediction",
        fluidRow(
          box(
            title = "Scenario Simulation", status = "success", solidHeader = TRUE, width = 3,
            dateInput("pred_date", "Select Date:", value = "2012-07-01"),
            selectInput("pred_weather", "Weather Forecast:",
              choices = list(
                "Clear/Few Clouds" = 1,
                "Mist/Cloudy" = 2,
                "Light Rain/Snow" = 3
              ),
              selected = 1
            ),
            sliderInput("pred_temp", "Temperature (°C):",
              min = -10, max = 40, value = 25
            ),
            sliderInput("pred_start_hour", "Start Hour:", min = 0, max = 23, value = 8),
            sliderInput("pred_n_hours", "Duration (Hours):", min = 1, max = 24, value = 12),
            sliderInput("pred_hum", "Humidity (%):", min = 0, max = 100, value = 60),
            sliderInput("pred_wind", "Wind Speed (Normalized):", min = 0, max = 1, value = 0.2, step = 0.05),
            actionButton("run_pred", "Run Prediction", icon = icon("play"), class = "btn-primary")
          ),
          box(
            title = "Predicted Demand Curve (24 Hours)", status = "success", solidHeader = TRUE, width = 9,
            plotOutput("pred_plot"),
            tags$div(
              style = "margin-top: 20px; padding: 15px; background-color: #f9f9f9; border-left: 5px solid #32CD32;",
              h4("Actionable Advice"),
              textOutput("action_advice")
            )
          )
        )
      ),

      # ----------------------------
      # Tab 4: Linear Regression Prediction
      # ----------------------------
      tabItem(
        tabName = "linear_pred",
        fluidRow(
          box(
            title = "Scenario Simulation (Linear Regression)", status = "info", solidHeader = TRUE, width = 3,
            dateInput("pred_date_lr", "Select Date:", value = "2012-07-01"),
            selectInput("pred_weather_lr", "Weather Forecast:",
              choices = list(
                "Clear/Few Clouds" = 1,
                "Mist/Cloudy" = 2,
                "Light Rain/Snow" = 3
              ),
              selected = 1
            ),
            sliderInput("pred_temp_lr", "Temperature (°C):",
              min = -10, max = 40, value = 25
            ),
            sliderInput("pred_start_hour_lr", "Start Hour:", min = 0, max = 23, value = 8),
            sliderInput("pred_n_hours_lr", "Duration (Hours):", min = 1, max = 24, value = 12),
            sliderInput("pred_hum_lr", "Humidity (%):", min = 0, max = 100, value = 60),
            sliderInput("pred_wind_lr", "Wind Speed (Normalized):", min = 0, max = 1, value = 0.2, step = 0.05),
            actionButton("run_pred_lr", "Run Prediction (LR)", icon = icon("play"), class = "btn-primary")
          ),
          box(
            title = "Predicted Demand Curve (Linear Regression)", status = "info", solidHeader = TRUE, width = 9,
            plotOutput("pred_plot_lr"),
            tags$div(
              style = "margin-top: 20px; padding: 15px; background-color: #f9f9f9; border-left: 5px solid #00c0ef;",
              h4("Actionable Advice"),
              textOutput("action_advice_lr")
            )
          )
        )
      ),

      # ----------------------------
      # Tab 3: Model Performance
      # ----------------------------
      tabItem(
        tabName = "model",
        fluidRow(
          # Value Boxes for Best Model (Random Forest)
          valueBoxOutput("best_model_mae", width = 4),
          valueBoxOutput("best_model_rmse", width = 4),
          valueBoxOutput("best_model_r2", width = 4)
        ),
        fluidRow(
          box(
            title = "Model Performance Comparison", status = "success", solidHeader = TRUE, width = 12,
            plotOutput("model_plot", height = "600px"),
            tags$div(
              style = "margin-top: 20px; padding: 15px; background-color: #f9f9f9; border-left: 5px solid #32CD32;",
              h4("Performance Metrics Explained"),
              tags$ul(
                tags$li(tags$b("MAE (Mean Absolute Error)"), ": Lower is better. Represents the average absolute difference between predicted and actual values."),
                tags$li(tags$b("RMSE (Root Mean Squared Error)"), ": Lower is better. Penalizes larger errors more than MAE."),
                tags$li(tags$b("R² (Coefficient of Determination)"), ": Higher is better. Represents the proportion of variance explained by the model.")
              ),
              p("As shown in the chart, ", tags$b("RandomForestRegressor"), " achieves the lowest error (MAE/RMSE) and highest R², making it the best performing model for this task.")
            )
          )
        )
      )
    )
  )
)

# ================================
# 3. Server Logic
# ================================

server <- function(input, output) {
  # ----------------------------
  # Tab 1: Insight Logic
  # ----------------------------

  # Handle View Mode Toggle with JS
  observeEvent(input$view_mode, {
    mode <- input$view_mode

    if (mode == "time") {
      runjs('
        $("#btn_time").addClass("active");
        $("#btn_feature").removeClass("active");
        $("#layer_time").removeClass("chart-hidden").addClass("chart-visible");
        $("#layer_feature").removeClass("chart-visible").addClass("chart-hidden");
      ')
    } else {
      runjs('
        $("#btn_feature").addClass("active");
        $("#btn_time").removeClass("active");
        $("#layer_feature").removeClass("chart-hidden").addClass("chart-visible");
        $("#layer_time").removeClass("chart-visible").addClass("chart-hidden");
      ')
    }
  })

  output$heatmap_plot <- renderPlot({
    # Filter data
    filtered_df <- df %>%
      filter(weathersit %in% input$weather_filter)

    # Aggregate data: Mean count by Weekday and Hour
    # We use the selected user type column (cnt, registered, or casual)
    agg_df <- filtered_df %>%
      group_by(weekday_label, hr) %>%
      summarise(avg_demand = mean(.data[[input$user_type]], na.rm = TRUE), .groups = "drop")

    # Plot
    ggplot(agg_df, aes(x = weekday_label, y = hr, fill = avg_demand)) +
      geom_tile(color = "white") +
      scale_fill_gradient(low = "#e5f7e5", high = "#006400", name = "Avg Rentals") +
      scale_y_reverse(breaks = 0:23) + # 0 at top
      labs(x = "Weekday", y = "Hour of Day", title = "Average Bike Rentals Heatmap") +
      theme_minimal() +
      theme(
        panel.grid = element_blank(),
        axis.text = element_text(size = 12),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
      )
  })

  output$feature_plot <- renderPlot({
    req(feature_df)

    # Determine which column to plot based on user_type input
    col_name <- paste0("importance_", input$user_type)

    if (!col_name %in% names(feature_df)) {
      return(NULL)
    }

    plot_data <- feature_df
    plot_data$importance <- plot_data[[col_name]]

    # Filter out non-positive values as packcircles ignores them, causing row mismatch
    plot_data <- plot_data %>% filter(importance > 0)

    if (nrow(plot_data) == 0) {
      return(NULL)
    }

    # Bubble Chart Design using packcircles
    # Generate layout
    # We use importance as area
    packing <- circleProgressiveLayout(plot_data$importance, sizetype = "area")

    # Add layout info to plot_data
    plot_data <- cbind(plot_data, packing)
    plot_data$id <- 1:nrow(plot_data)

    # Generate vertices for drawing circles
    # To make them look more dispersed (less dense), we reduce the radius slightly for drawing
    packing_draw <- packing
    packing_draw$radius <- packing_draw$radius * 0.95 # Create gaps
    dat.gg <- circleLayoutVertices(packing_draw, npoints = 50)

    # Join with feature info for coloring
    # We only need 'id' and 'feature' from plot_data to avoid x/y collision
    plot_data_join <- plot_data %>% select(id, feature)
    dat.gg <- left_join(dat.gg, plot_data_join, by = "id")

    ggplot() +
      # Draw circles with 20% transparency
      geom_polygon(data = dat.gg, aes(x, y, group = id, fill = feature), colour = "white", alpha = 0.2) +
      # Draw text
      geom_text(
        data = plot_data, aes(x, y, label = paste0(feature, "\n", round(importance, 3))),
        size = 4, color = "black", fontface = "bold"
      ) +
      scale_fill_viridis_d(option = "D", guide = "none") +
      theme_void() +
      theme(
        plot.margin = margin(0, 0, 0, 0),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
      ) +
      labs(title = "Feature Importance Analysis") +
      coord_equal()
  })

  # ----------------------------
  # Tab 2: Prediction Logic
  # ----------------------------

  # Reactive expression to fetch predictions from Python
  predictions <- eventReactive(input$run_pred,
    {
      # Show loading notification
      id <- showNotification("Running Python Model...", duration = NULL, closeButton = FALSE)
      on.exit(removeNotification(id), add = TRUE)

      # Call Python function
      # Ensure inputs are correct types
      date_str <- as.character(input$pred_date)
      weather_cat <- as.integer(input$pred_weather)
      temp_c <- as.numeric(input$pred_temp)
      start_hour <- as.integer(input$pred_start_hour)
      n_hours <- as.integer(input$pred_n_hours)
      hum <- as.numeric(input$pred_hum) / 100 # Convert % to 0-1
      windspeed <- as.numeric(input$pred_wind)

      # Call the function defined in Python script
      res <- get_predictions(date_str, weather_cat, temp_c, start_hour, n_hours, hum, windspeed, model_type = "rf")

      return(res)
    },
    ignoreNULL = FALSE
  ) # Run once on startup


  output$pred_plot <- renderPlot({
    req(predictions())
    pred_data <- predictions()

    ggplot(pred_data, aes(x = Hour, y = Predicted_Demand)) +
      # Confidence Interval Ribbon
      geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI), fill = primary_color, alpha = 0.2) +
      # Main Line
      geom_line(color = primary_color, size = 1.5) +
      geom_point(color = primary_color, size = 3) +
      # Aesthetics
      # Aesthetics
      scale_x_continuous(breaks = seq(0, 23, by = 1)) +
      labs(
        x = "Hour (0-23)", y = "Predicted Demand",
        title = paste("Demand Prediction for", input$pred_date),
        subtitle = "Shaded area represents 90% Confidence Interval"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 12)
      )
  })

  output$action_advice <- renderText({
    req(predictions())
    pred_data <- predictions()

    max_demand <- max(pred_data$Predicted_Demand)
    max_hour <- pred_data$Hour[which.max(pred_data$Predicted_Demand)]

    # Simple logic for advice
    if (max_demand > 800) {
      paste0(
        "⚠️ High Demand Alert! We expect a peak of ", round(max_demand),
        " bikes around ", max_hour, ":00. It is recommended to dispatch extra bikes to stations beforehand. ",
        "Consider implementing surge pricing during these hours to balance demand."
      )
    } else if (max_demand < 100) {
      paste0(
        "📉 Low Demand Expected. Peak demand is only ", round(max_demand),
        " bikes. This is a good time for maintenance or to offer promotional discounts to encourage ridership."
      )
    } else {
      paste0(
        "✅ Normal Operations. Demand is stable with a peak of ", round(max_demand),
        " bikes around ", max_hour, ":00. Standard fleet distribution should be sufficient."
      )
    }
  })

  # ----------------------------
  # Tab 4: Linear Regression Logic
  # ----------------------------

  # Reactive expression for LR predictions
  predictions_lr <- eventReactive(input$run_pred_lr,
    {
      id <- showNotification("Running Linear Regression Model...", duration = NULL, closeButton = FALSE)
      on.exit(removeNotification(id), add = TRUE)

      date_str <- as.character(input$pred_date_lr)
      weather_cat <- as.integer(input$pred_weather_lr)
      temp_c <- as.numeric(input$pred_temp_lr)
      start_hour <- as.integer(input$pred_start_hour_lr)
      n_hours <- as.integer(input$pred_n_hours_lr)
      hum <- as.numeric(input$pred_hum_lr) / 100
      windspeed <- as.numeric(input$pred_wind_lr)

      # Call Python function with model_type="lr"
      res <- get_predictions(date_str, weather_cat, temp_c, start_hour, n_hours, hum, windspeed, model_type = "lr")

      return(res)
    },
    ignoreNULL = FALSE
  )

  output$pred_plot_lr <- renderPlot({
    req(predictions_lr())
    pred_data <- predictions_lr()

    # Use blue color for LR to distinguish
    lr_color <- "#00c0ef"

    ggplot(pred_data, aes(x = Hour, y = Predicted_Demand)) +
      geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI), fill = lr_color, alpha = 0.2) +
      geom_line(color = lr_color, size = 1.5) +
      geom_point(color = lr_color, size = 3) +
      scale_x_continuous(breaks = seq(0, 23, by = 1)) +
      labs(
        x = "Hour (0-23)", y = "Predicted Demand",
        title = paste("Demand Prediction (Linear Regression) for", input$pred_date_lr),
        subtitle = "Shaded area represents 90% Confidence Interval"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 12)
      )
  })

  output$action_advice_lr <- renderText({
    req(predictions_lr())
    pred_data <- predictions_lr()

    max_demand <- max(pred_data$Predicted_Demand)
    max_hour <- pred_data$Hour[which.max(pred_data$Predicted_Demand)]

    if (max_demand > 800) {
      paste0(
        "⚠️ High Demand Alert! We expect a peak of ", round(max_demand),
        " bikes around ", max_hour, ":00."
      )
    } else if (max_demand < 100) {
      paste0(
        "📉 Low Demand Expected. Peak demand is only ", round(max_demand),
        " bikes."
      )
    } else {
      paste0(
        "✅ Normal Operations. Demand is stable with a peak of ", round(max_demand),
        " bikes around ", max_hour, ":00."
      )
    }
  })

  # ----------------------------
  # Tab 3: Model Comparison Logic
  # ----------------------------

  # Helper to get best model data
  best_model_data <- reactive({
    req(model_df)
    model_df %>% filter(Model == "RandomForestRegressor")
  })

  output$best_model_mae <- renderValueBox({
    data <- best_model_data()
    val <- if (nrow(data) > 0) round(as.numeric(data$MAE), 2) else "N/A"
    valueBox(
      val, "Lowest MAE (Best)",
      icon = icon("arrow-down"),
      color = "green"
    )
  })

  output$best_model_rmse <- renderValueBox({
    data <- best_model_data()
    val <- if (nrow(data) > 0) round(as.numeric(data$RMSE), 2) else "N/A"
    valueBox(
      val, "Lowest RMSE (Best)",
      icon = icon("arrow-down"),
      color = "green"
    )
  })

  output$best_model_r2 <- renderValueBox({
    data <- best_model_data()
    val <- if (nrow(data) > 0) round(as.numeric(data[["R^2"]]), 3) else "N/A"
    valueBox(
      val, "Highest R² (Best)",
      icon = icon("arrow-up"),
      color = "green"
    )
  })

  output$model_plot <- renderPlot({
    req(model_df)

    # Reshape data for plotting
    plot_data <- model_df %>%
      pivot_longer(cols = c("MAE", "RMSE", "R^2"), names_to = "Metric", values_to = "Value")

    # Define highlight color
    plot_data$Highlight <- ifelse(plot_data$Model == "RandomForestRegressor", "Best", "Other")

    # Custom labels for facets to indicate direction
    metric_labels <- c(
      "MAE" = "MAE (Lower is Better)",
      "RMSE" = "RMSE (Lower is Better)",
      "R^2" = "R² (Higher is Better)"
    )

    ggplot(plot_data, aes(x = reorder(Model, Value), y = Value, fill = Highlight)) +
      geom_bar(stat = "identity", position = "dodge", width = 0.7) +
      geom_text(aes(label = round(Value, 3)), hjust = -0.1, size = 3.5) +
      coord_flip() +
      facet_wrap(~Metric, scales = "free_x", labeller = as_labeller(metric_labels)) +
      scale_fill_manual(values = c("Best" = primary_color, "Other" = "gray70")) +
      labs(x = NULL, y = "Value", title = "Model Performance Comparison") +
      theme_minimal() +
      theme(
        legend.position = "none",
        strip.text = element_text(size = 12, face = "bold"),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        panel.grid.major.y = element_blank()
      )
  })
}

# ================================
# 4. Run App
# ================================

shinyApp(ui = ui, server = server)
