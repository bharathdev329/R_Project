# ------------------ EKC: India Case new.xlsx ------------------

# Install and load required packages
pkgs <- c("readxl", "ggplot2", "dplyr", "car", "broom")
for (p in pkgs) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
library(readxl); library(ggplot2); library(dplyr); library(car); library(broom)

# Step 1: Import data
data <- read_excel("India Case new.xlsx", sheet = "India Case 2")

# Step 2: Select and rename key columns
df <- data %>%
  dplyr::select(Year, `GDP($)`, `CO2 emission(in tons)`, `CO2 per capita`, `Temperature change from CO2`) %>%
  rename(
    Year = Year,
    GDP = `GDP($)`,
    CO2_emission = `CO2 emission(in tons)`,
    CO2_per_capita = `CO2 per capita`,
    Temp_change = `Temperature change from CO2`
  )

# Step 3: Create GDP squared term
df <- df %>%
  mutate(GDP_sq = GDP^2)

# Step 4: Fit EKC model (CO2 ~ GDP + GDP²)
ekc_model <- lm(CO2_emission ~ GDP + GDP_sq, data = df)

# Step 5: Display model summary
summary(ekc_model)

# Step 6: Check coefficients (inverted-U if GDP > 0 and GDP² < 0)
coefs <- coef(ekc_model)
b1 <- coefs["GDP"]
b2 <- coefs["GDP_sq"]
cat("b1 (GDP):", b1, "\n")
cat("b2 (GDP²):", b2, "\n")

# Step 7: Compute turning point (GDP where CO2 peaks)
turning_point <- -b1 / (2 * b2)
cat("Turning point GDP ($):", turning_point, "\n")

# Step 8: Plot EKC curve with confidence interval
ggplot(df, aes(x = GDP, y = CO2_emission)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), color = "red", se = TRUE) +
  labs(
    title = "Environmental Kuznets Curve (India)",
    subtitle = "CO2 Emissions vs GDP",
    x = "GDP ($)",
    y = "CO2 Emission (in tons)"
  ) +
  theme_minimal()

# Step 9: Add turning point annotation
tp_y <- predict(ekc_model, newdata = data.frame(GDP = turning_point, GDP_sq = turning_point^2))
ggplot(df, aes(x = GDP, y = CO2_emission)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), color = "darkred", se = FALSE) +
  geom_vline(xintercept = turning_point, linetype = "dashed") +
  geom_point(aes(x = turning_point, y = tp_y), color = "red", size = 3) +
  annotate("text", x = turning_point, y = tp_y, label = paste0("Turning point: ", round(turning_point,2)), vjust = -1) +
  theme_minimal() +
  labs(
    title = "Kuznets Curve with Turning Point",
    x = "GDP ($)",
    y = "CO2 Emission (in tons)"
  )

# Step 10: VIF check (collinearity)
vif(ekc_model)

# Step 11: Optional – check log-transformed EKC
df <- df %>%
  mutate(logGDP = log(GDP), logGDP_sq = logGDP^2, logCO2 = log(CO2_emission))
ekc_log <- lm(logCO2 ~ logGDP + logGDP_sq, data = df)
summary(ekc_log)

# Step 12: Plot log-log EKC
ggplot(df, aes(x = logGDP, y = logCO2)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), color = "darkgreen", se = TRUE) +
  labs(
    title = "Log-Log Environmental Kuznets Curve",
    x = "log(GDP)",
    y = "log(CO2 Emissions)"
  ) +
  theme_minimal()

# ------------------ End of EKC Code ------------------
