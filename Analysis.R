# -------------------------------------------------------------------------
# Complete analysis script for:
# "Industrialization and Carbon Emissions in India (1960-2024)"
# Uses Excel: "India Case new.xlsx", sheet "India Case 2"
# Columns used: Year, `GDP($)`, `CO2 emission(in tons)`, `CO2 per capita`, `Temperature change from CO2`
# -------------------------------------------------------------------------

# 0. USER-EDITABLE SETTINGS ------------------------------------------------
filepath   <- "India Case new.xlsx"       # path to file (change if needed)
sheet_name <- "India Case 2"              # sheet name
save_plots <- FALSE                       # set TRUE to save PNGs of plots
out_dir    <- "."                         # output directory for saved plots/results

# 1. Install & load packages ----------------------------------------------
req_pkgs <- c("readxl","dplyr","ggplot2","broom","car","corrplot",
              "vars","forecast","tseries","strucchange","scales","gridExtra")
for (p in req_pkgs) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
library(readxl); library(dplyr); library(ggplot2); library(broom)
library(car); library(corrplot); library(vars); library(forecast)
library(tseries); library(strucchange); library(scales); library(gridExtra)

# 2. Read & clean data ----------------------------------------------------
raw <- read_excel(filepath, sheet = sheet_name, guess_max = 2000)
# Show column names if you want
cat("Columns in sheet:\n"); print(colnames(raw))

# Select and rename used columns (exact names from your file)
df <- raw %>%
  dplyr::select(Year,
                `GDP($)`,
                `CO2 emission(in tons)`,
                `CO2 per capita`,
                `Temperature change from CO2`) %>%
  rename(
    Year = Year,
    GDP = `GDP($)`,
    CO2 = `CO2 emission(in tons)`,
    CO2_pc = `CO2 per capita`,
    Temp_change = `Temperature change from CO2`
  )

# Ensure numeric and drop NA rows for these vars
df <- df %>%
  mutate(
    Year = as.integer(Year),
    GDP = as.numeric(GDP),
    CO2 = as.numeric(CO2),
    CO2_pc = as.numeric(CO2_pc),
    Temp_change = as.numeric(Temp_change)
  ) %>%
  filter(!is.na(Year) & !is.na(GDP) & !is.na(CO2))

cat("Observations after cleaning:", nrow(df), "\n")
# optional: view head
print(head(df))

# 3. Derived variables ----------------------------------------------------
# GDP growth rate (pct), decade factor, policy dummies
df <- df %>%
  arrange(Year) %>%
  mutate(
    GDP_growth_pct = (GDP / lag(GDP) - 1) * 100,
    Decade = paste0(floor(Year/10)*10, "s"),
    NAPCC2008 = ifelse(Year >= 2008, 1, 0),      # policy dummy for 2008 NAPCC
    Paris2015 = ifelse(Year >= 2015, 1, 0)       # policy dummy for Paris Agreement
  )

# EKC terms
df <- df %>% mutate(GDP_sq = GDP^2,
                    logGDP = ifelse(GDP>0, log(GDP), NA),
                    logCO2 = ifelse(CO2>0, log(CO2), NA),
                    logGDP_sq = logGDP^2)

# 4. Descriptive statistics & plots ---------------------------------------
summary_stats <- df %>%
  summarise(
    n = n(), Year_min = min(Year), Year_max = max(Year),
    GDP_min = min(GDP), GDP_mean = mean(GDP, na.rm=TRUE), GDP_max = max(GDP),
    CO2_min = min(CO2), CO2_mean = mean(CO2, na.rm=TRUE), CO2_max = max(CO2)
  )
print(summary_stats)

# Time series plots: GDP and CO2
p1 <- ggplot(df, aes(x=Year)) +
  geom_line(aes(y = GDP/1e12), size=1) +
  labs(title="GDP over time (trillions $)", y="GDP (trillions $)", x="Year") +
  theme_minimal()
p2 <- ggplot(df, aes(x=Year)) +
  geom_line(aes(y = CO2), color="darkgreen", size=1) +
  labs(title="CO2 Emissions over time (tons)", y="CO2 (tons)", x="Year") +
  theme_minimal()
grid.arrange(p1, p2, nrow=2)
if (save_plots) { ggsave(file.path(out_dir,"GDP_CO2_timeseries.png"), arrangeGrob(p1,p2,nrow=2), width=8, height=8) }

# Scatter: GDP vs CO2 with EKC smooth
p_scatter <- ggplot(df, aes(x=GDP, y=CO2)) +
  geom_point() +
  geom_smooth(method="lm", formula = y ~ x + I(x^2), se=TRUE, color="red") +
  scale_x_continuous(labels = comma) +
  labs(title="CO2 vs GDP with EKC polynomial fit", x="GDP ($)", y="CO2 (tons)") +
  theme_minimal()
print(p_scatter)
if (save_plots) ggsave(file.path(out_dir,"Scatter_EKC.png"), p_scatter, width=8, height=5)

# 5. Correlation matrix ---------------------------------------------------
cor_vars <- df %>% dplyr::select(GDP, CO2, CO2_pc, Temp_change, GDP_growth_pct)
cor_mat <- cor(cor_vars, use="pairwise.complete.obs")
print(cor_mat)
corrplot(cor_mat, method="number")

# 6. Simple & multiple regressions ----------------------------------------
# (a) Simple linear: CO2 ~ GDP
m1 <- lm(CO2 ~ GDP, data=df)
cat("\nSimple linear model summary (CO2 ~ GDP):\n"); print(summary(m1))

# (b) Multiple: CO2 ~ GDP + CO2_pc + Temp_change (control)
m2 <- lm(CO2 ~ GDP + CO2_pc + Temp_change, data=df)
cat("\nMultiple regression summary (CO2 ~ GDP + CO2_pc + Temp_change):\n"); print(summary(m2))

# (c) EKC polynomial: CO2 ~ GDP + GDP^2
ekc <- lm(CO2 ~ GDP + GDP_sq, data=df)
cat("\nEKC polynomial model summary (CO2 ~ GDP + GDP^2):\n"); print(summary(ekc))
tidy(ekc)

# Compute turning point (if b2 != 0)
b1 <- coef(ekc)["GDP"]; b2 <- coef(ekc)["GDP_sq"]
if (!is.na(b1) && !is.na(b2) && b2 != 0) {
  turning_point_GDP <- - b1 / (2*b2)
  cat("EKC turning point (GDP $):", turning_point_GDP, "\n")
} else turning_point_GDP <- NA

# (d) Log-log EKC: log(CO2) ~ log(GDP) + log(GDP)^2
ekc_log <- lm(logCO2 ~ logGDP + logGDP_sq, data=df)
cat("\nLog-log EKC model summary:\n"); print(summary(ekc_log))

# 7. Policy impact regressions (differences / dummies) --------------------
# Add interactions to test if post-policy slope changes
m_policy1 <- lm(CO2 ~ GDP * NAPCC2008 + GDP_sq * NAPCC2008, data=df)
cat("\nPolicy (2008) interaction model summary:\n"); print(summary(m_policy1))

m_policy2 <- lm(CO2 ~ GDP * Paris2015 + GDP_sq * Paris2015, data=df)
cat("\nPolicy (2015) interaction model summary:\n"); print(summary(m_policy2))

# 8. Decoupling / Elasticity analysis -------------------------------------
# Elasticity = (dCO2/dGDP) * (GDP/CO2) = (b1 + 2*b2*GDP) * (GDP/CO2) using ekc coefficients
df <- df %>% mutate(
  ekc_marginal = coef(ekc)["GDP"] + 2*coef(ekc)["GDP_sq"] * GDP,
  elasticity = ifelse(CO2>0, ekc_marginal * (GDP / CO2), NA)
)
# Show elasticity at recent years
tail(df %>% select(Year, GDP, CO2, ekc_marginal, elasticity), 8)

# 9. Structural break / change-point detection ----------------------------
# Using strucchange breakpoints on CO2 ~ GDP relationship
bp <- breakpoints(CO2 ~ GDP, data=df)
summary(bp)
# If breakpoints detected, show locations
if (length(bp$breakpoints[!is.na(bp$breakpoints)])>0) {
  cat("Breakpoints years (indices):", bp$breakpoints, "\n")
  bp_years <- df$Year[bp$breakpoints]
  cat("Approx break years:", bp_years, "\n")
}

