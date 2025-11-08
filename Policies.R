# --- Required Packages ---
install_if_missing <- function(pkgs){
  new <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
  if(length(new)) install.packages(new)
}
install_if_missing(c("readxl", "dplyr", "ggplot2"))
library(readxl); library(dplyr); library(ggplot2)

# --- Load the data ---
data <- read_excel("India Case new.xlsx", sheet = "India Case 2")

# --- Ensure Year column is numeric ---
data <- data %>% mutate(Year = as.numeric(Year))

# --- Add Policy Period for New Industrial Policy (1991) ---
data <- data %>%
  mutate(Policy_Period = ifelse(Year >= 1991, "Post-1991 (Reform Era)", "Pre-1991 (Before Reforms)"))

# --- Summary statistics for key indicators ---
summary_stats <- data %>%
  group_by(Policy_Period) %>%
  summarise(
    Mean_GDP = mean(`GDP($)`, na.rm = TRUE),
    Mean_GDP_Growth = mean(`GDP Growth rate`, na.rm = TRUE),
    Mean_Export = mean(`Export($)`, na.rm = TRUE),
    Mean_Import = mean(`Import($)`, na.rm = TRUE),
    Mean_CO2 = mean(`CO2 emission(in tons)`, na.rm = TRUE),
    Mean_CO2_pc = mean(`CO2 per capita`, na.rm = TRUE),
    Mean_TempChange = mean(`Temperature change from CO2`, na.rm = TRUE),
    Count = n()
  )

print(summary_stats)

# --- Visualization 1: GDP Growth Rate before & after 1991 ---
p1 <- ggplot(data, aes(x = Year, y = `GDP Growth rate`, color = Policy_Period)) +
  geom_line(size = 1) +
  geom_vline(xintercept = 1991, linetype = "dashed") +
  labs(title = "GDP Growth Rate: Before and After 1991 New Industrial Policy",
       x = "Year", y = "GDP Growth Rate") +
  theme_minimal()
print(p1)

# --- Visualization 2: Export trend before & after 1991 ---
p2 <- ggplot(data, aes(x = Year, y = `Export($)`, color = Policy_Period)) +
  geom_line(size = 1) +
  geom_vline(xintercept = 1991, linetype = "dashed") +
  labs(title = "Export Trend: Before and After 1991 Liberalization",
       x = "Year", y = "Exports (in $)") +
  theme_minimal()
print(p2)

# --- Visualization 3: CO2 Emission trend before & after 1991 ---
p3 <- ggplot(data, aes(x = Year, y = `CO2 emission(in tons)`, color = Policy_Period)) +
  geom_line(size = 1) +
  geom_vline(xintercept = 1991, linetype = "dashed") +
  labs(title = "CO2 Emission Trend: Before and After 1991 Policy",
       x = "Year", y = "CO2 Emission (in tons)") +
  theme_minimal()
print(p3)
# --- Required packages ---
install_if_missing <- function(pkgs){
  new <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
  if(length(new)) install.packages(new)
}
install_if_missing(c("readxl","dplyr","ggplot2"))
library(readxl); library(dplyr); library(ggplot2)

# --- Load the data (adjust path if needed) ---
data <- read_excel("India Case new.xlsx", sheet = "India Case 2")

# --- Quick check of column names (run this if you want to confirm) ---
print(names(data))

# --- Ensure Year is numeric (if it's read as character) ---
data <- data %>% mutate(Year = as.numeric(Year))

# --- Add Paris Agreement policy indicator (2015) while keeping original names --- 
data <- data %>%
  mutate(Policy_Period = ifelse(Year >= 2015, "Post-Paris (2015+)", "Pre-Paris (<2015)"))

# --- Summary statistics by policy period (using the original column names) ---
summary_stats <- data %>%
  group_by(Policy_Period) %>%
  summarise(
    Mean_GDP = mean(`GDP($)`, na.rm = TRUE),
    Mean_GDP_Growth = mean(`GDP Growth rate`, na.rm = TRUE),
    Mean_Export = mean(`Export($)`, na.rm = TRUE),
    Mean_Import = mean(`Import($)`, na.rm = TRUE),
    Mean_CO2 = mean(`CO2 emission(in tons)`, na.rm = TRUE),
    Mean_CO2_pc = mean(`CO2 per capita`, na.rm = TRUE),
    Mean_TempChange = mean(`Temperature change from CO2`, na.rm = TRUE),
    Count = n()
  )

print(summary_stats)

# --- Plot 1: GDP Growth Rate over time (original name used) ---
p1 <- ggplot(data, aes(x = Year, y = `GDP Growth rate`, color = Policy_Period)) +
  geom_line(size = 1) +
  geom_vline(xintercept = 2015, linetype = "dashed") +
  labs(title = "GDP Growth Rate: Pre vs Post Paris Agreement (2015)",
       x = "Year", y = "GDP Growth rate") +
  theme_minimal()
print(p1)

# --- Plot 2: CO2 Emissions over time (original name used) ---
p2 <- ggplot(data, aes(x = Year, y = `CO2 emission(in tons)`, color = Policy_Period)) +
  geom_line(size = 1) +
  geom_vline(xintercept = 2015, linetype = "dashed") +
  labs(title = "CO2 Emissions (tons): Pre vs Post Paris Agreement (2015)",
       x = "Year", y = "CO2 emission (in tons)") +
  theme_minimal()
print(p2)

# --- Plot 3: Temperature change from CO2 over time (original name used) ---
p3 <- ggplot(data, aes(x = Year, y = `Temperature change from CO2`, color = Policy_Period)) +
  geom_line(size = 1) +
  geom_vline(xintercept = 2015, linetype = "dashed") +
  labs(title = "Temperature Change from CO2: Pre vs Post Paris (2015)",
       x = "Year", y = "Temperature change from CO2") +
  theme_minimal()
print(p3)

# --- OPTIONAL: Basic t-test example (uncomment to run) ---
# This tests whether mean CO2 emission differs pre vs post 2015.
# Note: t-test assumptions (independence/normality) may not hold for time series.
# t_result <- t.test(`CO2 emission(in tons)` ~ Policy_Period, data = data)
# print(t_result)
# --- Load Packages ---
library(readxl)
library(ggplot2)
library(strucchange)
library(dplyr)

# --- Load Data ---
data <- read_excel("India Case new.xlsx", sheet = "India Case 2")

# --- Clean Data ---
data <- data %>%
  select(Year, `GDP($)`, `GDP Growth rate`, `CO2 emission(in tons)`, `CO2 per capita`, `Temperature change from CO2`) %>%
  filter(!is.na(Year))

# --- Change Point Analysis (to detect any shift around 2008) ---
co2_ts <- ts(data$`CO2 emission(in tons)`, start = min(data$Year))
bp <- breakpoints(co2_ts ~ 1)

# Plot to visualize potential break points
plot(bp)
lines(bp)

# Show break years
summary(bp)

# --- Interrupted Time Series (Policy Impact) ---
data <- data %>%
  mutate(
    policy = ifelse(Year >= 2008, 1, 0),          # Policy dummy: 1 after 2008
    time = Year - min(Year),                      # Time variable
    post_time = ifelse(Year >= 2008, Year - 2008, 0)  # Time since policy
  )

# Run regression model
model <- lm(`CO2 emission(in tons)` ~ time + policy + post_time, data = data)
summary(model)

# --- Visualization ---
ggplot(data, aes(x = Year, y = `CO2 emission(in tons)`)) +
  geom_line(color = "steelblue", size = 1) +
  geom_vline(xintercept = 2008, linetype = "dashed", color = "red") +
  geom_smooth(method = "lm", se = FALSE, color = "darkgreen") +
  labs(title = "Impact of NAPCC (2008) on CO₂ Emissions",
       subtitle = "Dashed line shows year of policy introduction",
       y = "CO₂ Emissions (tons)",
       x = "Year") +
  theme_minimal()

