df<-read.csv(file.choose(),header = TRUE)
head(df)
View(df)
names(df)


#plotting trends in variables
install.packages('tidyverse')
install.packages('ggplot2')
library(ggplot2)
gdp_plot <- ggplot(df, aes(x = df$Year, y = df$GDP...)) +
  geom_line(color = "blue", size = 1) +
  geom_smooth(method = "lm", color = "red", se = FALSE, linetype = "dashed") +
  labs(title = "Trend of India's GDP Over Time",
       x = "Year",
       y = "GDP (in $)") +
  theme_minimal() +
  # Format y-axis labels to be more readable (e.g., "2T" for 2 Trillion)
  scale_y_continuous(labels = scales::unit_format(unit = "T", scale = 1e-12))
gdp_plot



gdpgrwt_plot <- ggplot(df, aes(x =df$Year, y =df$GDP.Growth.rate)) +
  geom_line(color = "blue", size = 1) +
  geom_smooth(method = "lm", color = "red", se = FALSE, linetype = "dashed") +
  labs(title = "Trend of India's GDP growth Over Time",
       x = "Year",
       y = "GDP growth(in %)") +
  theme_minimal()
gdpgrwt_plot


co2_plot <- ggplot(df, aes(x =df$Year, y =df$CO2.emission.in.tons.)) +
  geom_line(color = "blue", size = 1) +
  geom_smooth(method = "lm", color = "red", se = FALSE, linetype = "dashed") +
  labs(title = "Trend of India's Co2 emission Over Time",
       x = "Year",
       y = "Co2 emission(in tons)") +
  theme_minimal()
co2_plot
