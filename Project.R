df<-read.csv(file.choose(),header = TRUE)
head(df)
View(df)
names(df)
names(df)
# Loading necessary libraries
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr) # For drop_na()

#Trend Analysis

# Clean column names for easier use in R
# We select and rename the columns we need
df_clean <- df %>%
  select(
    Year,
    gdp = `GDP...`,
    gdp_growth = `GDP.Growth.rate`,
    co2_emissions = `CO2.emission.in.tons.`,
    temp_change = `Temperature.change.from.CO2`,
    population = `Population`
  )

# Define key events and periods
key_events <- data.frame(
  year = c(1991, 2008, 2020),
  label = c("1991 Reforms", "2008 Slowdown", "2020 COVID Dip")
)

key_periods <- data.frame(
  start = c(2000, 2010),
  end = c(2008, 2020),
  label = c("2000s Mfg. Boom", "2010s Renewable Era")
)


# --- Plotting Function (Modified) ---
# This function now creates and returns the plot to be displayed
plot_trend_r <- function(data, y_col, title, y_label) {
  
  # Ensure the y_col argument is correctly interpreted by ggplot
  y_col_aes <- sym(y_col)
  
  # Remove NA values for the specific column being plotted
  plot_data <- data %>%
    drop_na(!!y_col_aes)
  
  # Find a good y-position for event labels
  y_pos_events <- max(plot_data[[y_col]], na.rm = TRUE) * 0.8
  
  plot <- ggplot(plot_data, aes(x = Year, y = !!y_col_aes)) +
    # Add shaded regions for key periods
    geom_rect(
      data = key_periods,
      aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf, fill = label),
      inherit.aes = FALSE,
      alpha = 0.2
    ) +
    # Add the line and points
    geom_line(color = "blue") +
    geom_point(color = "blue", size = 1) +
    
    # Add vertical lines for key events
    geom_vline(
      data = key_events,
      aes(xintercept = year),
      color = "red",
      linetype = "dashed"
    ) +
    
    # Add text labels for key events
    geom_text(
      data = key_events,
      aes(x = year + 0.5, y = y_pos_events, label = label),
      color = "red",
      angle = 90,
      hjust = 0.5,
      size = 3
    ) +
    
    # Set titles and labels
    labs(
      title = title,
      x = "Year",
      y = y_label,
      fill = "Key Periods" # Legend title for the shaded areas
    ) +
    
    # Use a clean theme
    theme_minimal() +
    
    # Adjust legend position
    theme(legend.position = "bottom") +
    
    # Define fill colors
    scale_fill_manual(values = c("grey50", "grey80"))
  
  # Return the plot object so it displays in your R session
  return(plot)
}


# --- Generate Plots ---
# Running each of these lines will now display the plot in R

# 1. GDP Trend
plot_trend_r(df_clean, 
             y_col = "gdp", 
             title = "India's GDP Trend", 
             y_label = "GDP (in $)")

# 2. GDP Growth Rate Trend
plot_trend_r(df_clean, 
             y_col = "gdp_growth", 
             title = "India's GDP Growth Rate Trend", 
             y_label = "GDP Growth Rate")

# 3. CO2 Emissions Trend
plot_trend_r(df_clean, 
             y_col = "co2_emissions", 
             title = "India's CO₂ Emissions Trend", 
             y_label = "CO₂ Emissions (in tons)")

# 4. Temperature Change Trend
plot_trend_r(df_clean, 
             y_col = "temp_change", 
             title = "India's Temperature Change Trend", 
             y_label = "Temperature Change (°C)")

#------------------------------------------------------------------------------

# --- B. Correlation Analysis ---

# Ensure the required libraries are loaded
library(dplyr)

# Select the columns for correlation from your 'df_clean' data frame
cols_for_corr <- df_clean %>%
  select(gdp, gdp_growth, co2_emissions, temp_change)

# Compute the correlation matrix
# We use "pairwise.complete.obs" to handle missing values (NA)
# by only using complete pairs of observations for each correlation.
corr_matrix <- cor(cols_for_corr, use = "pairwise.complete.obs")

# Print the full matrix to the console
print("Correlation Matrix:")
print(corr_matrix)

# --- Print Specific Requested Correlations ---

# 1. GDP and CO₂ emissions
gdp_co2_corr <- corr_matrix["gdp", "co2_emissions"]
print(paste("Correlation (GDP and CO2):", round(gdp_co2_corr, 3)))

# 2. GDP growth rate and CO₂ emissions
growth_co2_corr <- corr_matrix["gdp_growth", "co2_emissions"]
print(paste("Correlation (GDP Growth and CO2):", round(growth_co2_corr, 3)))

# 3. CO₂ emissions and temperature change
co2_temp_corr <- corr_matrix["co2_emissions", "temp_change"]
print(paste("Correlation (CO2 and Temp Change):", round(co2_temp_corr, 3)))

#-------------------------------------------------------------------------------

# --- C. Regression Analysis ---

# Ensure the required libraries are loaded
library(dplyr)
library(tidyr) # For drop_na()

# Prepare the data for modeling by dropping any rows with NA values
# The lm() function in R can handle this, but it's good practice
# to create a complete dataset.
df_model_r <- df_clean %>%
  drop_na(gdp, gdp_growth, co2_emissions, temp_change)

print(paste("Original data rows:", nrow(df_clean), 
            "Rows used for modeling:", nrow(df_model_r)))

# --- Model 1: Simple Linear Regression ---
# Formula: co2_emissions ~ gdp
print("--- Model 1: Simple Regression (CO2 vs. GDP) ---")

# The lm() function automatically includes an intercept (β₀)
model1_r <- lm(co2_emissions ~ gdp, data = df_model_r)

# Print the detailed model summary
print(summary(model1_r))
print("Coefficients for Model 1:")
print(coef(model1_r))


# --- Model 2: Multiple Linear Regression ---
# Formula: co2_emissions ~ gdp + gdp_growth + temp_change
print("--- Model 2: Multiple Regression (CO2 vs. GDP, Growth, Temp) ---")

model2_r <- lm(co2_emissions ~ gdp + gdp_growth + temp_change, data = df_model_r)

# Print the detailed model summary
print(summary(model2_r))
print("Coefficients for Model 2:")
print(coef(model2_r))



#---------------------------------------------------------------------------------------------------------

# --- E. Change-Point Analysis ---

# Ensure libraries are loaded
library(dplyr)
library(tidyr)

# Prepare data:
# We need the 'df_clean' data frame from our first step.
# Make sure it's available in your R session.
df_chow <- df_clean %>%
  drop_na(gdp, co2_emissions, Year)

# 1. Define the breakpoint and create the dummy variable
breakpoint_year <- 2010
df_chow <- df_chow %>%
  mutate(
    post_2010 = ifelse(Year >= breakpoint_year, 1, 0)
  )

# 2. Create the interaction term
# In R's lm(), you can create this automatically in the formula
# The syntax `gdp * post_2010` includes:
#   1. gdp (β₁)
#   2. post_2010 (β₂)
#   3. gdp:post_2010 (the interaction term, β₃)

print("--- Change-Point Analysis (Chow Test equivalent) ---")
print("Testing for a structural break post-2010.")

# 3. Run the regression model
# We use `gdp * post_2010` as a shortcut
chow_model <- lm(co2_emissions ~ gdp * post_2010, data = df_chow)

# 4. Print and interpret the results
print(summary(chow_model))

# --- Interpretation ---
print("--- Interpretation of Results ---")
print("Check the p-value (Pr(>|t|)) for the 'gdp:post_2010' term.")
print("If p-value < 0.05, the change is significant.")

# You can get the coefficients directly:
coefs <- coef(chow_model)
slope_before_2010 <- coefs["gdp"]
slope_interaction <- coefs["gdp:post_2010"]
slope_after_2010 <- slope_before_2010 + slope_interaction

print(paste("Slope before 2010 (GDP effect):", slope_before_2010))
print(paste("Slope after 2010 (GDP effect):", slope_after_2010))


#---------------------------------------------------------------------------------------------------------





