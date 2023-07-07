# install.packages('tidyverse')
# install.packages('ggcorrplot')
# install.packages('moments')
# install.packages('car')

library(tidyverse)
library(ggcorrplot)
library(moments)

list.files(path = "input")

options(
  digits = 2,
  repr.plot.width = 10,
  repr.plot.height = 8
)

# Read data
# When you read data from certain file, kindly find "absolute path" of the file and input into file="".
happy2021 <- read.csv(
  file = "input/2021.csv",
  header = TRUE
)

# Exclude non-numeric columns
numeric_cols <- setdiff(colnames(happy2021), c("Country.name", "Regional.indicator"))

# Calculate the mean, median, mode, and variance values for each numeric column
mean_values <- sapply(happy2021[, numeric_cols], mean)
median_values <- sapply(happy2021[, numeric_cols], median)
mode_values <- sapply(happy2021[, numeric_cols], mode)
variance_values <- sapply(happy2021[, numeric_cols], var)

# Combine column names and mean, median, mode, and variance values into a data frame
mean_result <- data.frame(Column = numeric_cols, mean_value = mean_values)
median_result <- data.frame(Column = numeric_cols, median_value = median_values)
mode_result <- data.frame(Column = numeric_cols, mode_value = mode_values)
variance_result <- data.frame(Column = numeric_cols, variance_value = variance_values)

# Generate box plots with analyzable data
boxplot(happy2020[, numeric_cols], 
        main = "Box Plots of Numeric Columns",
        xlab = "Factors",
        ylab = "Values")

# Print the result
print(mean_result)
print(median_result)
print(mode_result)
print(variance_result)

head(happy2021)
summary(happy2021)

str(happy2021)
sum(is.na(happy2021))

# Colors
v_color <- viridis::viridis(
  n = nrow(happy2021)
)

happy2021$color <- v_color[Matrix::invPerm(
  p = order(
    x = happy2021$Ladder.score
  )
)]

pairs(
  formula = Ladder.score ~ Explained.by..Log.GDP.per.capita + Explained.by..Social.support +
    Explained.by..Healthy.life.expectancy + Explained.by..Freedom.to.make.life.choices +
    Explained.by..Generosity + Explained.by..Perceptions.of.corruption,
  data = happy2021,
  col = happy2021$color,
  pch = 19
)

# ================================================================================================
# Correlation of Variables
options(repr.plot.width = 12, repr.plot.height = 10)
cor_happy_2021 <- subset(happy2021, select = -c(1, 2, 20))

corr <- round(cor(cor_happy_2021), 1)

ggcorrplot(corr, hc.order = TRUE,
           #type = "lower",
           lab = TRUE,
           lab_size = 4,
           method = "circle",
           colors = c("red", "white", "green"),
           title = "Correlation of Variables",
           ggtheme = theme_bw)

# ================================================================================================
# Top 20 Countries by Overall Happiness
options(repr.plot.width = 12, repr.plot.height = 10)
top_score <- head(happy2021, n = 20)
top_score$Country.name <- factor(top_score$Country.name, levels = top_score$Country.name)

theme_set(theme_bw())

ggplot(data = top_score, aes(x = Country.name, y = Ladder.score)) +
  geom_point(size = 3) +
  geom_segment(aes(x = Country.name,
                   xend = Country.name,
                   y = 0,
                   yend = Ladder.score)) +
  labs(title = "Top 20 Countries by Overall Happiness",
       x = "Countries",
       y = "Score") +
  theme(axis.text.x = element_text(angle = 65, vjust = 0.6, size = 14),
        text = element_text(size = 16))

# ================================================================================================
# Top 20 Countries by High Whisker
options(repr.plot.width = 12, repr.plot.height = 10)
top_highwhisker <- happy2021[order(-happy2021$upperwhisker),]
top_highwhisker <- head(top_highwhisker, n = 20)
top_highwhisker$Country.name <- factor(top_highwhisker$Country.name, levels = top_highwhisker$Country.name)

theme_set(theme_bw())

ggplot(data = top_highwhisker, aes(x = Country.name, y = upperwhisker)) + 
  geom_point(size = 3) +
  geom_segment(aes(x = Country.name,
                   xend = Country.name,
                   y = 0,
                   yend = upperwhisker)) + 
  labs(title = "Top 20 Countries by Upper Whisker", 
       x = "Countries",
       y = "Whisker height") + 
  theme(axis.text.x = element_text(angle = 65, vjust = 0.6, size = 14),
        text = element_text(size = 16))

# ================================================================================================
# Top 20 Countries by Low Whisker
options(repr.plot.width = 12, repr.plot.height = 10)
top_lowwhisker <- happy2021[order(-happy2021$lowerwhisker),]
top_lowwhisker <- head(top_lowwhisker, n = 20)
top_lowwhisker$Country.name <- factor(top_lowwhisker$Country.name, levels = top_lowwhisker$Country.name)

theme_set(theme_bw())

ggplot(data = top_lowwhisker, aes(x = Country.name, y = lowerwhisker)) + 
  geom_point(size = 3) +
  geom_segment(aes(x = Country.name,
                   xend = Country.name,
                   y = 0,
                   yend = lowerwhisker)) + 
  labs(title = "Top 20 Countries by Lower Whisker", 
       x = "Countries",
       y = "Low Whisker") + 
  theme(axis.text.x = element_text(angle = 65, vjust = 0.6, size = 14),
        text = element_text(size = 16))

# ================================================================================================
# Top 20 Countries by GDP
options(repr.plot.width = 12, repr.plot.height = 10)
top_gdp <- happy2021[order(-happy2021$Explained.by..Log.GDP.per.capita),]
top_gdp <- head(top_gdp, n = 20)
top_gdp$Country.name <- factor(top_gdp$Country.name, levels = top_gdp$Country.name)

theme_set(theme_bw())

ggplot(data = top_gdp, aes(x = Country.name, y = Explained.by..Log.GDP.per.capita)) +
  geom_point(size = 3) +
  geom_segment(aes(x = Country.name,
                   xend = Country.name,
                   y = 0,
                   yend = Explained.by..Log.GDP.per.capita)) +
  labs(title = "Top 20 Countries by GDP Per Capita",
       x = "Countries",
       y = "GDP Per Capita") +
  theme(axis.text.x = element_text(angle = 65, vjust = 0.6, size = 14),
        text = element_text(size = 16))

# ================================================================================================
# Top 20 Countries by Social Support
options(repr.plot.width = 12, repr.plot.height = 10)
top_social <- happy2021[order(-happy2021$Explained.by..Social.support),]
top_social <- head(top_social, n = 20)
top_social$Country.name <- factor(top_social$Country.name, levels = top_social$Country.name)

theme_set(theme_bw())

ggplot(data = top_social, aes(x = Country.name, y = Explained.by..Social.support)) +
  geom_point(size = 3) +
  geom_segment(aes(x = Country.name,
                   xend = Country.name,
                   y = 0,
                   yend = Explained.by..Social.support)) +
  labs(title = "Top 20 Countries by Social Support",
       x = "Countries",
       y = "Social Support") +
  theme(axis.text.x = element_text(angle = 65, vjust = 0.6, size = 14),
        text = element_text(size = 16))

# ================================================================================================
# Top 20 Countries by Healthy Life Expectancy
options(repr.plot.width = 12, repr.plot.height = 10)
top_lifeexpectancy <- happy2021[order(-happy2021$Explained.by..Healthy.life.expectancy),]
top_lifeexpectancy <- head(top_lifeexpectancy, n = 20)
top_lifeexpectancy$Country.name <- factor(top_lifeexpectancy$Country.name, levels = top_lifeexpectancy$Country.name)

theme_set(theme_bw())

ggplot(data = top_lifeexpectancy, aes(x = Country.name, y = Explained.by..Healthy.life.expectancy)) +
  geom_point(size = 3) +
  geom_segment(aes(x = Country.name,
                   xend = Country.name,
                   y = 0,
                   yend = Explained.by..Healthy.life.expectancy)) +
  labs(title = "Top 20 Countries by Healthy Life Expectancy",
       x = "Countries",
       y = "Life Expectancy") +
  theme(axis.text.x = element_text(angle = 65, vjust = 0.6, size = 14),
        text = element_text(size = 16))

# ================================================================================================
# Top 20 Countries by Freedom
options(repr.plot.width = 12, repr.plot.height = 10)
top_free <- happy2021[order(-happy2021$Explained.by..Freedom.to.make.life.choices),]
top_free <- head(top_free, n = 20)
top_free$Country.name <- factor(top_free$Country.name, levels = top_free$Country.name)

theme_set(theme_bw())

ggplot(data = top_free, aes(x = Country.name, y = Explained.by..Freedom.to.make.life.choices)) +
  geom_point(size = 3) +
  geom_segment(aes(x = Country.name,
                   xend = Country.name,
                   y = 0,
                   yend = Explained.by..Freedom.to.make.life.choices)) +
  labs(title = "Top 20 Countries by Freedom",
       x = "Countries",
       y = "Freedom") +
  theme(axis.text.x = element_text(angle = 65, vjust = 0.6, size = 14),
        text = element_text(size = 16))

# ================================================================================================
# Top 20 Most Generous Countries
options(repr.plot.width = 12, repr.plot.height = 10)
top_gen <- happy2021[order(-happy2021$Explained.by..Generosity),]
top_gen <- head(top_gen, n = 20)
top_gen$Country.name <- factor(top_gen$Country.name, levels = top_gen$Country.name)

theme_set(theme_bw())

ggplot(data = top_gen, aes(x = Country.name, y = Explained.by..Generosity)) +
  geom_point(size = 3) +
  geom_segment(aes(x = Country.name,
                   xend = Country.name,
                   y = 0,
                   yend = Explained.by..Generosity)) +
  labs(title = "Top 20 Most Generous Countries",
       x = "Countries",
       y = "Generosity") +
  theme(axis.text.x = element_text(angle = 65, vjust = 0.6, size = 14),
        text = element_text(size = 16))

# ================================================================================================
# Top 20 Countries With the Most Trust
options(repr.plot.width = 12, repr.plot.height = 10)
top_trust <- happy2021[order(-happy2021$Explained.by..Perceptions.of.corruption),]
top_trust <- head(top_trust, n = 20)
top_trust$Country.name <- factor(top_trust$Country.name, levels = top_trust$Country.name)

theme_set(theme_bw())

ggplot(data = top_trust, aes(x = Country.name, y = Explained.by..Perceptions.of.corruption)) +
  geom_point(size = 3) +
  geom_segment(aes(x = Country.name,
                   xend = Country.name,
                   y = 0,
                   yend = Explained.by..Perceptions.of.corruption)) +
  labs(title = "Top 20 Countries With the Most Trust",
       x = "Countries",
       y = "Perceptions of Corruption") +
  theme(axis.text.x = element_text(angle = 65, vjust = 0.6, size = 14),
        text = element_text(size = 16))

# ================================================================================================
# Multiple linear regression model with all parameters included
lm_happy <- lm(
  formula = Ladder.score ~ Explained.by..Log.GDP.per.capita + Explained.by..Social.support +
    Explained.by..Healthy.life.expectancy + Explained.by..Freedom.to.make.life.choices +
    Explained.by..Generosity + Explained.by..Perceptions.of.corruption,
  data = happy2021
)
summary(lm_happy)

options(scipen=-100, digits = 3)
anova(lm_happy)

options(scipen=10, digits=3)
vif <- round(car::vif(lm_happy),2)

cat("VIF of Original model\n")
cat("##########################\n\n")

cat("GDP Per Capita: ", vif[1])
cat("\nSocial Support: ", vif[2])
cat("\nLife Expectancy: ", vif[3])
cat("\nFreedom: ", vif[4])
cat("\nGenerosity: ", vif[5])
cat("\nPerceptions of Corruption: ", vif[6])

paste0("R-squared: ", round(summary(lm_happy)$r.squared, 2))

lm_new <- update(
  object = lm_happy,
  formula = Ladder.score ~ .  - Explained.by..Generosity - Explained.by..Healthy.life.expectancy - Explained.by..Perceptions.of.corruption)
summary(lm_new)
anova(lm_new, lm_happy)

cat("VIF of New model\n")
cat("#################\n\n")
vif_2 <- round(car::vif(lm_new), 2)

cat("GDP Per Capita: ", vif_2[1], "\n")
cat("Social Support: ", vif_2[2], "\n")
cat("Freedom: ", vif_2[3], "\n")

# ================================================================================================
# Q-Q Plot
par(mfrow=c(2,2))
qqnorm(lm_happy$residuals);qqline(lm_happy$residuals)
qqnorm(lm_new$residuals);qqline(lm_new$residuals)

# ================================================================================================
# Histograms
options(repr.plot.width = 14, repr.plot.height = 12)
par(mfrow=c(2,2))
h <- hist(
  x = lm_happy$residuals,
  xlab = "Residuals of Original Model",
  ylab = "Count",
  main = "Histogram of Original Model's Residuals",
  las = 1,
  ylim = c(0,65),
  col = heat.colors(8)
)
text(
  x = h$mids,
  y = h$counts,
  labels = h$counts,
  adj = c(0.5,-0.5)
)

h1 <- hist(
  x = lm_new$residuals,
  xlab = "Residuals of New Model",
  ylab = "Count",
  main = "Histogram of New Model's Residuals",
  las = 1,
  xlim = c(-3, 2),
  col = heat.colors(8)
)
text(
  x = h1$mids,
  y = h1$counts,
  labels = h1$counts,
  adj = c(0.5,-0.5)
)

# ================================================================================================
# Skewness
cat("##### Skewness of models #####\n\n")

cat("Skewness of original model: ", round(skewness(lm_happy$residuals),3))
cat("\nSkewness of new model: ", round(skewness(lm_new$residuals),3))

shapiro.test(lm_happy$residuals)
shapiro.test(lm_new$residuals)
anova(lm_new, lm_happy)
