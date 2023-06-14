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
happy2019 <- read.csv(
  file = "input/2019.csv",
  header = TRUE
)

head(happy2019)
summary(happy2019)

str(happy2019)
sum(is.na(happy2019))

# Colors
v_color <- viridis::viridis(
  n = nrow(happy2019)
)

happy2019$color <- v_color[Matrix::invPerm(
  p = order(
    x = happy2019$Score
  )
)]

pairs(
  formula = Score ~ GDP.per.capita + Social.support +
    Healthy.life.expectancy + Freedom.to.make.life.choices +
    Generosity + Perceptions.of.corruption,
  data = happy2019,
  col = happy2019$color,
  pch = 19
)

# ================================================================================================
# Correlation of Variables
options(repr.plot.width = 12, repr.plot.height = 10)
cor_happy_2019 <- subset(happy2019, select = -c(1, 2, 10))

corr <- round(cor(cor_happy_2019), 1)

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
top_score <- head(happy2019, n = 20)
top_score$Country.or.region <- factor(top_score$Country.or.region, levels = top_score$Country.or.region)

theme_set(theme_bw())

ggplot(data = top_score, aes(x = Country.or.region, y = Score)) +
  geom_point(size = 3) +
  geom_segment(aes(x = Country.or.region,
                   xend = Country.or.region,
                   y = 0,
                   yend = Score)) +
  labs(title = "Top 20 Countries by Overall Happiness",
       x = "Countries",
       y = "Score") +
  theme(axis.text.x = element_text(angle = 65, vjust = 0.6, size = 14),
        text = element_text(size = 16))

# ================================================================================================
# Top 20 Countries by GDP
options(repr.plot.width = 12, repr.plot.height = 10)
top_gdp <- happy2019[order(-happy2019$GDP.per.capita),]
top_gdp <- head(top_gdp, n = 20)
top_gdp$Country.or.region <- factor(top_gdp$Country.or.region, levels = top_gdp$Country.or.region)

theme_set(theme_bw())

ggplot(data = top_gdp, aes(x = Country.or.region, y = GDP.per.capita)) +
  geom_point(size = 3) +
  geom_segment(aes(x = Country.or.region,
                   xend = Country.or.region,
                   y = 0,
                   yend = GDP.per.capita)) +
  labs(title = "Top 20 Countries by GDP Per Capita",
       x = "Countries",
       y = "GDP Per Capita") +
  theme(axis.text.x = element_text(angle = 65, vjust = 0.6, size = 14),
        text = element_text(size = 16))

# ================================================================================================
# Top 20 Countries by Social Support
options(repr.plot.width = 12, repr.plot.height = 10)
top_social <- happy2019[order(-happy2019$Social.support),]
top_social <- head(top_social, n = 20)
top_social$Country.or.region <- factor(top_social$Country.or.region, levels = top_social$Country.or.region)

theme_set(theme_bw())

ggplot(data = top_social, aes(x = Country.or.region, y = Social.support)) +
  geom_point(size = 3) +
  geom_segment(aes(x = Country.or.region,
                   xend = Country.or.region,
                   y = 0,
                   yend = Social.support)) +
  labs(title = "Top 20 Countries by Social Support",
       x = "Countries",
       y = "Social Support") +
  theme(axis.text.x = element_text(angle = 65, vjust = 0.6, size = 14),
        text = element_text(size = 16))

# ================================================================================================
# Top 20 Countries by Healthy Life Expectancy
options(repr.plot.width = 12, repr.plot.height = 10)
top_lifeexpectancy <- happy2019[order(-happy2019$Healthy.life.expectancy),]
top_lifeexpectancy <- head(top_lifeexpectancy, n = 20)
top_lifeexpectancy$Country.or.region <- factor(top_lifeexpectancy$Country.or.region, levels = top_lifeexpectancy$Country.or.region)

theme_set(theme_bw())

ggplot(data = top_lifeexpectancy, aes(x = Country.or.region, y = Healthy.life.expectancy)) +
  geom_point(size = 3) +
  geom_segment(aes(x = Country.or.region,
                   xend = Country.or.region,
                   y = 0,
                   yend = Healthy.life.expectancy)) +
  labs(title = "Top 20 Countries by Healthy Life Expectancy",
       x = "Countries",
       y = "Life Expectancy") +
  theme(axis.text.x = element_text(angle = 65, vjust = 0.6, size = 14),
        text = element_text(size = 16))

# ================================================================================================
# Top 20 Countries by Freedom
options(repr.plot.width = 12, repr.plot.height = 10)
top_free <- happy2019[order(-happy2019$Freedom.to.make.life.choices),]
top_free <- head(top_free, n = 20)
top_free$Country.or.region <- factor(top_free$Country.or.region, levels = top_free$Country.or.region)

theme_set(theme_bw())

ggplot(data = top_free, aes(x = Country.or.region, y = Freedom.to.make.life.choices)) +
  geom_point(size = 3) +
  geom_segment(aes(x = Country.or.region,
                   xend = Country.or.region,
                   y = 0,
                   yend = Freedom.to.make.life.choices)) +
  labs(title = "Top 20 Countries by Freedom",
       x = "Countries",
       y = "Freedom") +
  theme(axis.text.x = element_text(angle = 65, vjust = 0.6, size = 14),
        text = element_text(size = 16))

# ================================================================================================
# Top 20 Most Generous Countries
options(repr.plot.width = 12, repr.plot.height = 10)
top_gen <- happy2019[order(-happy2019$Generosity),]
top_gen <- head(top_gen, n = 20)
top_gen$Country.or.region <- factor(top_gen$Country.or.region, levels = top_gen$Country.or.region)

theme_set(theme_bw())

ggplot(data = top_gen, aes(x = Country.or.region, y = Generosity)) +
  geom_point(size = 3) +
  geom_segment(aes(x = Country.or.region,
                   xend = Country.or.region,
                   y = 0,
                   yend = Generosity)) +
  labs(title = "Top 20 Most Generous Countries",
       x = "Countries",
       y = "Generosity") +
  theme(axis.text.x = element_text(angle = 65, vjust = 0.6, size = 14),
        text = element_text(size = 16))

# ================================================================================================
# Top 20 Countries With the Most Trust
options(repr.plot.width = 12, repr.plot.height = 10)
top_trust <- happy2019[order(-happy2019$Perceptions.of.corruption),]
top_trust <- head(top_trust, n = 20)
top_trust$Country.or.region <- factor(top_trust$Country.or.region, levels = top_trust$Country.or.region)

theme_set(theme_bw())

ggplot(data = top_trust, aes(x = Country.or.region, y = Perceptions.of.corruption)) +
  geom_point(size = 3) +
  geom_segment(aes(x = Country.or.region,
                   xend = Country.or.region,
                   y = 0,
                   yend = Perceptions.of.corruption)) +
  labs(title = "Top 20 Countries With the Most Trust",
       x = "Countries",
       y = "Perceptions of Corruption") +
  theme(axis.text.x = element_text(angle = 65, vjust = 0.6, size = 14),
        text = element_text(size = 16))

# ================================================================================================
# Multiple linear regression model with all parameters included
lm_happy <- lm(
  formula = Score ~ GDP.per.capita + Social.support +
    Healthy.life.expectancy + Freedom.to.make.life.choices +
    Generosity + Perceptions.of.corruption,
  data = happy2019
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
  formula = Score ~ .  - Generosity - Healthy.life.expectancy - Perceptions.of.corruption)
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