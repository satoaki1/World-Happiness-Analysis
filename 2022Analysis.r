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
happy <- read.csv(
  file = "input/2022.csv",
  header = TRUE
)

# Exclude non-numeric columns
numeric_cols <- setdiff(colnames(happy), c("Country", "RANK"))

# Create a new data frame with only the numeric columns
numeric_data <- happy[, numeric_cols]

# Set larger plotting margins
par(mar = c(5, 5, 2, 2))

# Plot boxplot for each numeric column
boxplot(numeric_data, las = 2)

# Calculate the mean, median, mode, and variance values for each numeric column
mean_values <- sapply(happy[, numeric_cols], mean)
median_values <- sapply(happy[, numeric_cols], median)
mode_values <- sapply(happy[, numeric_cols], mode)
variance_values <- sapply(happy[, numeric_cols], var)

# Combine column names and mean, median, mode, and variance values into a data frame
mean_result <- data.frame(Column = numeric_cols, mean_value = mean_values)
median_result <- data.frame(Column = numeric_cols, median_value = median_values)
mode_result <- data.frame(Column = numeric_cols, mode_value = mode_values)
variance_result <- data.frame(Column = numeric_cols, variance_value = variance_values)

# Generate box plots with analyzable data
boxplot(happy[, numeric_cols], 
        main = "Box Plots of Numeric Columns",
        xlab = "Factors",
        ylab = "Values")

# Print the result
print(mean_result)
print(median_result)
print(mode_result)
print(variance_result)

head(happy)
summary(happy)

str(happy)
sum(is.na(happy))

# Colors
v_color <- viridis::viridis(
  n = nrow(happy)
)

happy$color <- v_color[Matrix::invPerm(
  p = order(
    x = happy$Happiness.score
  )
)]

pairs(
  formula = Happiness.score ~ Explained.by..GDP.per.capita + Explained.by..Social.support +
    Explained.by..Healthy.life.expectancy + Explained.by..Freedom.to.make.life.choices +
    Explained.by..Generosity + Explained.by..Perceptions.of.corruption,
  data = happy,
  col = happy$color,
  pch = 19
)

# ================================================================================================
# Correlation of Variables
options(repr.plot.width = 12, repr.plot.height = 10)
cor_happy <- subset(happy, select = -c(1, 2, 13))

corr <- round(cor(cor_happy), 1)

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
top_score <- head(happy, n = 20)
top_score$Country <- factor(top_score$Country, levels = top_score$Country)

theme_set(theme_bw())

ggplot(data = top_score, aes(x = Country, y = Happiness.score)) +
  geom_point(size = 3) +
  geom_segment(aes(x = Country,
                   xend = Country,
                   y = 0,
                   yend = Happiness.score)) +
  labs(title = "Top 20 Countries by Overall Happiness",
       x = "Countries",
       y = "Score") +
  theme(axis.text.x = element_text(angle = 65, vjust = 0.6, size = 14),
        text = element_text(size = 16))

# ================================================================================================
# Top 20 Countries by High Whisker
options(repr.plot.width = 12, repr.plot.height = 10)
top_highwhisker <- happy[order(-happy$Whisker.high),]
top_highwhisker <- head(top_highwhisker, n = 20)
top_highwhisker$Country <- factor(top_highwhisker$Country, levels = top_highwhisker$Country)

theme_set(theme_bw())

ggplot(data = top_highwhisker, aes(x = Country, y = Whisker.high)) + 
  geom_point(size = 3) +
  geom_segment(aes(x = Country,
                   xend = Country,
                   y = 0,
                   yend = Whisker.high)) + 
  labs(title = "Top 20 Countries by High Whisker", 
       x = "Countries",
       y = "Whisker height") + 
  theme(axis.text.x = element_text(angle = 65, vjust = 0.6, size = 14),
        text = element_text(size = 16))

# ================================================================================================
# Top 20 Countries by Low Whisker
options(repr.plot.width = 12, repr.plot.height = 10)
top_lowwhisker <- happy[order(-happy$Whisker.low),]
top_lowwhisker <- head(top_lowwhisker, n = 20)
top_lowwhisker$Country <- factor(top_lowwhisker$Country, levels = top_lowwhisker$Country)

theme_set(theme_bw())

ggplot(data = top_lowwhisker, aes(x = Country, y = Whisker.low)) + 
  geom_point(size = 3) +
  geom_segment(aes(x = Country,
                   xend = Country,
                   y = 0,
                   yend = Whisker.low)) + 
  labs(title = "Top 20 Countries by Low Whisker", 
       x = "Countries",
       y = "Low Whisker") + 
  theme(axis.text.x = element_text(angle = 65, vjust = 0.6, size = 14),
        text = element_text(size = 16))

# ================================================================================================
# Top 20 Countries by GDP
options(repr.plot.width = 12, repr.plot.height = 10)
top_gdp <- happy[order(-happy$Explained.by..GDP.per.capita),]
top_gdp <- head(top_gdp, n = 20)
top_gdp$Country <- factor(top_gdp$Country, levels = top_gdp$Country)

theme_set(theme_bw())

ggplot(data = top_gdp, aes(x = Country, y = Explained.by..GDP.per.capita)) +
  geom_point(size = 3) +
  geom_segment(aes(x = Country,
                   xend = Country,
                   y = 0,
                   yend = Explained.by..GDP.per.capita)) +
  labs(title = "Top 20 Countries by GDP",
       x = "Countries",
       y = "GDP Per Capita") +
  theme(axis.text.x = element_text(angle = 65, vjust = 0.6, size = 14),
        text = element_text(size = 16))

# ================================================================================================
# Top 20 Countries by Social Support
options(repr.plot.width = 12, repr.plot.height = 10)
top_social <- happy[order(-happy$Explained.by..Social.support),]
top_social <- head(top_social, n = 20)
top_social$Country <- factor(top_social$Country, levels = top_social$Country)

theme_set(theme_bw())

ggplot(data = top_social, aes(x = Country, y = Explained.by..Social.support)) +
  geom_point(size = 3) +
  geom_segment(aes(x = Country,
                   xend = Country,
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
top_lifeexpectancy <- happy[order(-happy$Explained.by..Healthy.life.expectancy),]
top_lifeexpectancy <- head(top_lifeexpectancy, n = 20)
top_lifeexpectancy$Country <- factor(top_lifeexpectancy$Country, levels = top_lifeexpectancy$Country)

theme_set(theme_bw())

ggplot(data = top_lifeexpectancy, aes(x = Country, y = Explained.by..Healthy.life.expectancy)) +
  geom_point(size = 3) +
  geom_segment(aes(x = Country,
                   xend = Country,
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
top_free <- happy[order(-happy$Explained.by..Freedom.to.make.life.choices),]
top_free <- head(top_free, n = 20)
top_free$Country <- factor(top_free$Country, levels = top_free$Country)

theme_set(theme_bw())

ggplot(data = top_free, aes(x = Country, y = Explained.by..Freedom.to.make.life.choices)) +
  geom_point(size = 3) +
  geom_segment(aes(x = Country,
                   xend = Country,
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
top_gen <- happy[order(-happy$Explained.by..Generosity),]
top_gen <- head(top_gen, n = 20)
top_gen$Country <- factor(top_gen$Country, levels = top_gen$Country)

theme_set(theme_bw())

ggplot(data = top_gen, aes(x = Country, y = Explained.by..Generosity)) +
  geom_point(size = 3) +
  geom_segment(aes(x = Country,
                   xend = Country,
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
top_trust <- happy[order(-happy$Explained.by..Perceptions.of.corruption),]
top_trust <- head(top_trust, n = 20)
top_trust$Country <- factor(top_trust$Country, levels = top_trust$Country)

theme_set(theme_bw())

ggplot(data = top_trust, aes(x = Country, y = Explained.by..Perceptions.of.corruption)) +
  geom_point(size = 3) +
  geom_segment(aes(x = Country,
                   xend = Country,
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
  formula = Happiness.score ~ Explained.by..GDP.per.capita + Explained.by..Social.support +
    Explained.by..Healthy.life.expectancy + Explained.by..Freedom.to.make.life.choices +
    Explained.by..Generosity + Explained.by..Perceptions.of.corruption,
  data = happy
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
  formula = Happiness.score ~ .  - Explained.by..Generosity - Explained.by..Healthy.life.expectancy - Explained.by..Perceptions.of.corruption)
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
