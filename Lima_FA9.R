# Install and load necessary packages
install.packages(c("lme4","rstatix","haven", "tidyr", "dplyr", "ggplot2", "car"))
library(haven)
library(tidyr)
library(dplyr)
library(ggplot2)
library(car)
library(rstatix)
library(lme4)

speed_date_data <- read_sav("LooksOrPersonality.sav")

# Load the data
data <- data.frame(
  Gender = rep(c("Male", "Female"), each = 10),
  att_high = c(86, 91, 89, 89, 80, 80, 89, 100, 90, 89, 89, 84, 99, 86, 89, 89, 80, 82, 97, 95),
  av_high = c(84, 83, 88, 69, 81, 84, 85, 94, 74, 86, 91, 90, 100, 89, 87, 81, 92, 69, 92, 93),
  ug_high = c(67, 53, 48, 58, 57, 51, 61, 56, 54, 63, 93, 85, 89, 83, 80, 79, 85, 87, 90, 96),
  att_some = c(88, 83, 99, 86, 88, 96, 87, 86, 92, 80, 88, 95, 80, 86, 83, 86, 81, 95, 98, 79),
  av_some = c(69, 74, 70, 77, 71, 63, 79, 71, 71, 73, 65, 70, 79, 74, 74, 59, 66, 72, 64, 66),
  ug_some = c(50, 48, 48, 40, 50, 42, 44, 54, 58, 49, 54, 60, 53, 58, 43, 47, 47, 51, 53, 46),
  att_none = c(97, 86, 90, 87, 82, 92, 86, 84, 78, 91, 55, 50, 51, 58, 52, 58, 51, 50, 54, 52),
  av_none = c(48, 50, 45, 47, 50, 48, 50, 54, 38, 48, 48, 44, 48, 50, 48, 47, 45, 48, 53, 39),
  ug_none = c(47, 46, 48, 53, 45, 43, 45, 47, 45, 39, 52, 45, 44, 47, 40, 47, 47, 46, 45, 47)
)

# Check for missing values
data_cleaned <- na.omit(data)

# Reshape the data from wide to long
data_long <- data_cleaned %>%
  pivot_longer(cols = -Gender, names_to = "Variable", values_to = "Value")

# Check the reshaped data
head(data_long)


# Check for outliers in each cell using boxplots
data_cleaned %>%
  gather(key = "Variable", value = "Value", -Gender) %>%
  ggplot(aes(x = Gender, y = Value, fill = Variable)) +
  geom_boxplot() +
  facet_wrap(~Variable, scales = "free_y") +
  theme_minimal()

# Alternatively, you can use summary statistics to identify outliers
summary(data_cleaned)

# Shapiro-Wilk test for normality and Q-Q plots
shapiro_test_results <- data_cleaned %>%
  gather(key = "Variable", value = "Value", -Gender) %>%
  group_by(Variable) %>%
  summarize(shapiro_pvalue = shapiro.test(Value)$p.value)

# Print Shapiro-Wilk test results
print(shapiro_test_results)

# Q-Q plots
qq_plots <- data_cleaned %>%
  gather(key = "Variable", value = "Value", -Gender) %>%
  ggplot(aes(sample = Value)) +
  geom_qq() +
  facet_wrap(~Variable, scales = "free_y") +
  theme_minimal()

print(qq_plots)

# Perform Levene's test for each group
levene_atth <- leveneTest(att_high ~ Gender, data = data)
levene_avh <- leveneTest(av_high ~ Gender, data = data)
levene_ugh <- leveneTest(ug_high ~ Gender, data = data)
levene_atts <- leveneTest(att_some ~ Gender, data = data)
levene_avs <- leveneTest(av_some ~ Gender, data = data)
levene_ugs <- leveneTest(ug_some ~ Gender, data = data)
levene_attn <- leveneTest(att_none ~ Gender, data = data)
levene_avn <- leveneTest(av_none ~ Gender, data = data)
levene_ugn <- leveneTest(ug_none ~ Gender, data = data)

# Print the results
print(levene_atth)
print(levene_avh)
print(levene_ugh)
print(levene_atts)
print(levene_avs)
print(levene_ugs)
print(levene_attn)
print(levene_avn)
print(levene_ugn)


