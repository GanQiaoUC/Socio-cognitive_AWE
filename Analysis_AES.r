# Load necessary library
library(readr)
library(irr) #cohen's kappa
library(jsonlite)
library(tibble)
library(ggplot2)
library(tidyverse)
library(lmerTest)
library(tidyverse)
library(effects) 
library(party)
library(grid)
library(patchwork)
library(cowplot)
library(gridExtra)
library(dplyr)
library(sjPlot)
library(car)
library(Metrics)
library(MuMIn)
library(broom.mixed)
library(lme4)
library(buildmer)
library(psych)
library(MASS)

library(extrafont)
loadfonts()   # Load fonts for the R session

#t-test for comparing turbo and 4o####
data <- read_csv("Turbo_4o.csv")
summary(data)

t.test(data$Turbo, data$GPT4o, paired = TRUE) #significantly different

# Shapiro-Wilk Test for normality
shapiro.test(data$Turbo)
shapiro.test(data$GPT4o)

# spearman correlation
cor_test<-cor.test(data$Turbo, data$GPT4o, method = "spearman")
cor_test

mae_turbo <- mean(abs(data$human - data$Turbo))
mae_turbo
mae_4o <- mean(abs(data$human - data$GPT4o))
mae_4o

#JSON to csv####

# Read the JSON file
data <- fromJSON("essay_raw_scores.json")

# Convert to a data frame
df <- tibble(
  id = names(data),
  score1 = sapply(data, function(x) x[1]),
  score2 = sapply(data, function(x) x[2])
)

# View the result
print(df)

# Save to CSV
write.csv(df, "scores.csv", row.names = FALSE)

#correlation####
# Load CSV file
data <- read_csv("WC1.csv")
summary(data)

# Shapiro-Wilk Test for normality
shapiro.test(data$Ave1)
shapiro.test(data$Ave2)

# For Human scores
shapiro.test(data$Score[data$ScoreType == "Human"])
# For GPT4o scores
shapiro.test(data$Score[data$ScoreType == "GPT4o"])
# For Claude3.7 scores
shapiro.test(data$Score[data$ScoreType == "Claude3.7"])

# kendall correlation
cor_test<-cor.test(data$Score[data$ScoreType == "Claude3.7"], data$Score[data$ScoreType == "GPT4o"], method = "kendall")
cor_test

cor_test<-cor.test(data$Ave1, data$Ave2, method = "spearman")
cor_test

# Create scatter plot
plot(data$Score[data$ScoreType == "Human"], data$Score[data$ScoreType == "Claude3.7"], 
     xlab = "HumanScore", 
     ylab = "Claude3.7Score", 
     col = "blue", pch = 19)
abline(lm(data$Score[data$ScoreType == "Claude3.7"] ~ data$Score[data$ScoreType == "Human"]), col = "red", lwd = 2)

plot(data$human, data$gpt4o, 
     main = paste("Scatter Plot of GPT4oScore vs HumanScore\nCorrelation = ", round(cor_test$estimate, 2), 
                  "\np-value = ", round(cor_test$p.value, 55)),
     xlab = "GPT4oScore", 
     ylab = "HumanScore", 
     col = "blue", pch = 19)
# Add a regression line (linear model)
abline(lm(data$human ~ data$gpt4o), col = "red", lwd = 2)

#cohen's kapp####
data <- read.csv("SingleCouple.csv")
summary(data)

data <- read.csv("Linguistics_matched5.csv",stringsAsFactors = T)

# Extract the ratings for both raters
rater1 <- data$Claude3.7Score1
rater2 <- data$Claude3.7Score2

# Combine ratings into a data frame
ratings <- data.frame(rater1, rater2)

# Compute Quadratic Weighted Kappa
kappa2(ratings, weight = "squared")

# Calculate Cohen's Kappa
kappa_result <- kappa2(ratings)

# Print the result
print(kappa_result)

#distribution####
#Histogram
data <- read.csv("WC1.csv",stringsAsFactors = T)
summary(data)

#mean and SD
data %>%
  group_by(ScoreType) %>%
  summarise(
    mean_score = mean(Score, na.rm = TRUE),
    sd_score = sd(Score, na.rm = TRUE),
    n = n()
  )

#normality
data %>%
  group_by(ScoreType) %>%
  summarise(
    shapiro_result = list(shapiro.test(Score)),
    .groups = "drop"
  ) %>%
  mutate(
    statistic = map_dbl(shapiro_result, ~ .x$statistic),
    p_value = map_dbl(shapiro_result, ~ .x$p.value)
  ) %>%
  select(ScoreType, statistic, p_value)

ggplot(data, aes(x = Score, fill = ScoreType)) +
  geom_density(alpha = 0.8, color = NA) +
  scale_x_continuous(
    breaks = seq(1, 6, by = 0.5),
    limits = c(1, 6),
    expand = c(0, 0)
  ) +
  labs(
    title = "",
    x = "Essay Score",
    y = "Density",
    fill = "Score Type"
  ) +
  scale_fill_manual(
    values = c("Claude3.7" ="darkred",  "#1f77b4",  # professional blue
               "GPT4o"     = "darkblue", "#ff7f0e",  # orange
               "Human"     ="darkgreen", "#2ca02c")  # green
  ) +
  facet_grid(rows = vars(ScoreType), switch = "y") +  # switch facet labels to right
  theme_minimal(base_size = 10) +
  theme(
    strip.placement = "outside",                 # move strip outside the panel
    strip.text.y.left = element_text(angle = 0), # horizontal strip text
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    strip.text = element_text(face = "bold", size = 10),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    strip.background = element_blank()
  )


ggplot(data, aes(x = Score, fill = ScoreType)) +
  geom_bar(position = "dodge", alpha = 0.8, color = NA) +
  scale_x_continuous(
    breaks = seq(0.5, 6.5, by = 0.5),
    limits = c(0.5, 6.5),
    expand = c(0, 0)
  ) +
  labs(
    title = "",
    x = "Essay Score",
    y = "Count",
    fill = "Score Type"
  ) +
  scale_fill_manual(
    values = c(
      "Claude3.7" = "darkred",
      "GPT4o"     = "darkblue",
      "Human"     = "darkgreen"
    )
  ) +
  facet_grid(rows = vars(ScoreType), switch = "y") +
  theme_minimal(base_size = 10) +
  theme(
    strip.placement = "outside",
    strip.text.y.left = element_text(angle = 0),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    strip.text = element_text(face = "bold", size = 10),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    strip.background = element_blank()
  )


ggplot(data, aes(x = ScoreType, y = Score, fill = ScoreType)) +
  geom_violin(trim = FALSE, alpha = 0.7) +
  geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) +  # overlay boxplot
  labs(title = "Score Distribution by Scoring Source",
       x = "Score Type", y = "Essay Score") +
  scale_fill_manual(values = c("Claude3.7" = "#1f77b4", "GPT4o" = "#ff7f0e", "Human" = "#2ca02c")) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")



#regression modelling####
#check multicollinearity####
df <- read.csv("correlation1.csv",stringsAsFactors = T)
summary(data)

# Specify only the columns you want to include
selected_columns <- c("dependency_type_diversity","connective_variety")

# Subset the data
df_numeric <- df[, selected_columns]

# Compute correlation matrix
cor_matrix <- cor(df_numeric, use = "pairwise.complete.obs")

# Round to 2 decimal places (optional)
cor_matrix_rounded <- round(cor_matrix, 2)
cor_matrix_rounded

# Save to CSV
write.csv(cor_matrix_rounded, "correlation_matrix_keystroke.csv", row.names = TRUE)

# Optional: visualize
library(corrplot)
corrplot(cor_matrix, method = "color", tl.cex = 0.6)


data <- read.csv("WC1.csv",stringsAsFactors = T)
summary(data)
#prune the embedding first####
full_pca_model <- lm(Score ~ ScoreType*(PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + PC11 + PC12), data = data)
# Backward stepwise selection by BIC (k = log(n))
stepwise_model <- stepAIC(full_pca_model, direction = "both", k = log(nrow(data)))
summary(stepwise_model)

full_pca_model2 <- lm(Score ~ ScoreType*(PC1 + PC4 + PC5 + PC6 + PC7 + PC8), data = data)
# Backward stepwise selection by BIC (k = log(n))
stepwise_model2 <- stepAIC(full_pca_model2, direction = "both", k = log(nrow(data_scaled)))
summary(stepwise_model2)

vif_values <- vif(full_pca_model2)
print(vif_values[order(vif_values, decreasing = TRUE)])

#prune keystroke logging indices####
full_pca_model <- lm(Score ~ ScoreType*(P_burst + R_burst + MeanPause + WithinWordPause + BetweenWordPause + MeanDeletion +ProportionInsertion), data = data)
# Backward stepwise selection by BIC (k = log(n))
stepwise_model <- stepAIC(full_pca_model, direction = "both", k = log(nrow(data)))
summary(stepwise_model)

vif_values <- vif(full_pca_model)
print(vif_values[order(vif_values, decreasing = TRUE)])

#prune linguistic####
full_pca_model <- lm(Score ~ ScoreType*(MTLD + TUnitLength + DependencyCosSim + Readability + ArgumentOverlap + ConnectiveVariety +PC1+
                                          PC4+PC5+PC6+PC7+PC8), data = data)
# Backward stepwise selection by BIC (k = log(n))
stepwise_model <- stepAIC(full_pca_model, direction = "both", k = log(nrow(data)))
summary(stepwise_model)

vif_values <- vif(full_pca_model)
print(vif_values[order(vif_values, decreasing = TRUE)])

#prune social####
full_pca_model <- lm(Score ~ ScoreType*(Age + Gender + Ethnicity + Education), data = data)
# Backward stepwise selection by BIC (k = log(n))
stepwise_model <- stepAIC(full_pca_model, direction = "both", k = log(nrow(data)))
summary(stepwise_model)

vif_values <- vif(full_pca_model)
print(vif_values[order(vif_values, decreasing = TRUE)])


data$Gender <- factor(data$Gender, levels = c("Men","Women"))
data$Ethnicity <- factor(data$Ethnicity, levels = c("African","White","Latino","Asian"))
data$Education <- factor(data$Education, levels = c("HSBelow", "Bach","Mas","PhD"))
data$ScoreType <- factor(data$ScoreType, levels = c("Human", "GPT4o","Claude3.7"))

data_scaled <- data
numeric_vars <- c("Age", "P_burst", "R_burst", "MeanPause","WithinWordPause","BetweenWordPause","MeanDeletion",
                  "ProportionInsertion","MTLD","TUnitLength","DependencyCosSim","PC1",
                  "PC4","PC5","PC6","PC7","PC8","Readability","ConnectiveVariety","ArgumentOverlap")
data_scaled[numeric_vars] <- scale(data[numeric_vars])
summary(data_scaled)

model1 <- lmer(
  Score ~ ScoreType * (Age + Gender + Ethnicity + Education +
                         P_burst + R_burst + MeanPause + WithinWordPause + BetweenWordPause +
                         MeanDeletion + ProportionInsertion + MTLD + TUnitLength +
                         DependencyCosSim + Readability + ArgumentOverlap + ConnectiveVariety +
                         PC1 + PC4 + PC5 + PC6 + PC7 + PC8) +(1 | ID2) + (1 | Topic),data = data_scaled)
summary(model1)

model2 <- lmer(
  Score ~ ScoreType * (Age + Gender + Ethnicity + Education +
                         P_burst + R_burst + MeanPause + BetweenWordPause +
                          ProportionInsertion + MTLD +  Readability + ConnectiveVariety +
                         PC1 + PC4 + PC5 + PC6 + PC7 + PC8) +TUnitLength+DependencyCosSim+ArgumentOverlap+(1 | ID2) + (1 | Topic),data = data_scaled)
summary(model2)

model3 <- lmer(Score ~ ScoreType * (Age + Gender + Ethnicity + Education +
                         P_burst + R_burst + MeanPause + BetweenWordPause +
                         ProportionInsertion + MTLD +  Readability + ConnectiveVariety +
                         PC1 + PC4 + PC5 + PC6 + PC7 + PC8)^2 +(1 | ID2) + (1 | Topic),data = data_scaled)

summary(model3)
tab_model(model3)

model4 <- lmer(Score ~ ScoreType * (Readability*PC1+MTLD*PC1+ProportionInsertion *Readability+BetweenWordPause*PC6+MeanPause*PC8+
R_burst*PC6+R_burst*PC5+R_burst* Readability+ P_burst*ConnectiveVariety+ P_burst*BetweenWordPause+P_burst*MeanPause+Education*PC7+
  Education*PC6+Education*PC1+ Education*Readability+Education*ProportionInsertion+Education*R_burst+Ethnicity*R_burst+
  Ethnicity* Education +Gender*PC7+ Gender*PC4+Gender*BetweenWordPause+Age*PC1) +(1 | ID2) + (1 | Topic),data = data_scaled)
summary(model4)

model40 <- lmer(Score ~ ScoreType * (Readability*PC1+MTLD*PC1+MeanPause*PC8+
                                      R_burst*PC5+R_burst* Readability+ P_burst*ConnectiveVariety+ 
                                      Education*PC6+Education*Readability+Education*R_burst+Ethnicity*R_burst+
                                      Ethnicity* Education + Gender*PC4+Gender*BetweenWordPause+Age*PC1) +(1 | ID2) + (1 | Topic),data = data_scaled)
summary(model40)

model41 <- lmer(Score ~ ScoreType * (Readability*PC1+MTLD*PC1+R_burst*PC5+P_burst*ConnectiveVariety+ Education*Readability+Ethnicity*R_burst+
                                       Gender*PC4+Gender*BetweenWordPause) +(1 | ID2) + (1 | Topic),data = data_scaled)
summary(model41)
tab_model(model41)

model43 <- lmer(Score ~ ScoreType * (MTLD*PC1+R_burst*PC5+ Education*Readability+Ethnicity*R_burst+
                                       Gender*PC4+Gender*BetweenWordPause) +(1 | ID2) + (1 | Topic),data = data_scaled)
summary(model43)

model42 <- lmer(Score ~ ScoreType * (MTLD*PC1+R_burst*PC5+ Education*Readability+Ethnicity*R_burst+
                                       Gender*PC4) +(1 | ID2) + (1 | Topic),data = data_scaled)
summary(model42)#final

library(performance)
model_performance(model42)

#post-hoc analysis####
library(emmeans)
library(multcomp)
library(multcompView)
summary(data_scaled)
# Create emmeans for the three-way interaction
emm_mtld_pc1 <- emmeans(model42, ~ ScoreType * MTLD * PC1, 
                        at = list(MTLD = c(-0.7, 0, 0.6),
                                  PC1 = c(-0.6, 0, 0.7)))

# Pairwise comparisons within each combination
pairs_mtld_pc1 <- pairs(emm_mtld_pc1, by = c("MTLD", "PC1"))
summary(pairs_mtld_pc1, adjust = "bonferroni")

# Or compare across different levels
pairs_mtld_pc1_all <- pairs(emm_mtld_pc1)
summary(pairs_mtld_pc1_all, adjust = "bonferroni")

#Use this####
# Simple effects of MTLD at different levels of ScoreType and PC1
quantile(data_scaled$R_burst, probs = c(0.25, 0.5, 0.75))
quantile(data_scaled$PC5, probs = c(0.25, 0.5, 0.75))
simple_mtld <- emmeans(model42, ~ ScoreType * R_burst*PC5,
                       at = list(R_burst = c(0, 20),
                                 PC5 = c(-0.5,0.1, 0.6)))
pairs(simple_mtld, adjust = "bonferroni")

# Get unique education levels first
quantile(data_scaled$R_burst, probs = c(0.25, 0.5, 0.75))
emm_edu_read <- emmeans(model42, ~ ScoreType*Gender*PC4,
                        at = list(PC4 = c(-3.5, 4)))
# Pairwise comparisons
pairs_edu_read <- pairs(emm_edu_read, by = c("ScoreType", "PC4"))
summary(pairs_edu_read, adjust = "bonferroni")


library(interactions)
# For the three-way interaction: ScoreType * Readability * PC1
summary(data_scaled)

quantile(data_scaled$PC1, probs = c(0.25, 0.5, 0.75))
p2 <- interact_plot(model42, 
                    pred = MTLD,           
                    modx = PC1,                   
                    mod2 = ScoreType,
                    modx.values = c(-1.0, 0.2, 0.9),
                    modx.labels = c("Low PC1 (-1.0)", "Mid PC1 (0.2)", "High PC1 (0.9)"),
                    colors = c("red", "blue", "green")) +
  labs(x = "MTLD (standardized)", 
       y = "Predicted Score",
       color = "PC1 Level") +
  facet_wrap(~ScoreType, labeller = label_value) +
  scale_linetype_manual(values = c("solid", "solid", "solid")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 9, hjust = 0.5, color = "gray60"),
    axis.title = element_text(size = 10, face = "bold"),
    axis.text = element_text(size = 9),
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 8),
    strip.text = element_text(size = 10, face = "bold"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "gray80", fill = NA, size = 0.5)
  ) +
  guides(linetype = "none")
p2$facet$params$labeller <- as_labeller(function(x) gsub("ScoreType = ", "", x))
p2

quantile(data_scaled$PC5, probs = c(0.25, 0.5, 0.75))
p3 <- interact_plot(model42, 
                    pred = R_burst,           
                    modx = PC5,                   
                    mod2 = ScoreType,
                    modx.values = c(-0.5, 0.1, 0.6),
                    modx.labels = c("Low PC5 (-0.5)", "Mid PC5 (0.1)", "High PC5 (0.6)"),
                    colors = c("red", "blue", "darkgreen")) +
  labs(x = "R_burst (standardized)", 
       y = "Predicted Score") +
  facet_wrap(~ScoreType, labeller = label_value) +
  scale_linetype_manual(values = c("solid", "solid", "solid")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 9, hjust = 0.5, color = "gray60"),
    axis.title = element_text(size = 10, face = "bold"),
    axis.text = element_text(size = 9),
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 8),
    strip.text = element_text(size = 10, face = "bold"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "gray80", fill = NA, size = 0.5)
  ) +
  guides(linetype = "none")
p3$facet$params$labeller <- as_labeller(function(x) gsub("ScoreType = ", "", x))
p3


# For Education × Readability × ScoreType (where Education is categorical)
p4 <- interact_plot(model42, 
                   pred = Readability,           
                   modx = Education,                   
                   mod2 = ScoreType,
                   interval = F,
                   colors = c("darkred", "darkblue", "darkgreen","darkorange")) +
  labs(x = "Readability (standardized)", 
       y = "Predicted Score",
       color = "Education Level") +
  facet_wrap(~ScoreType, labeller = label_value) +
  scale_linetype_manual(values = c("solid", "solid", "solid", "solid")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 9, hjust = 0.5, color = "gray60"),
    axis.title = element_text(size = 10, face = "bold"),
    axis.text = element_text(size = 9),
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 8),
    strip.text = element_text(size = 10, face = "bold"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "gray80", fill = NA, size = 0.5)
  ) +
  guides(linetype = "none")  # Remove linetype from legend since all are solid
p4$facet$params$labeller <- as_labeller(function(x) gsub("ScoreType = ", "", x))
p4

p6 <- interact_plot(model42, 
                    pred = R_burst,           
                    modx = Ethnicity,                   
                    mod2 = ScoreType,
                    interval = F,
                    colors = c("darkred", "darkblue", "darkgreen","darkorange")) +
  labs(x = "R_burst (standardized)", 
       y = "Predicted Score",
       color = "Ethnicity") +
  facet_wrap(~ScoreType, labeller = label_value) +
  scale_linetype_manual(values = c("solid", "solid", "solid", "solid")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 9, hjust = 0.5, color = "gray60"),
    axis.title = element_text(size = 10, face = "bold"),
    axis.text = element_text(size = 9),
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 8),
    strip.text = element_text(size = 10, face = "bold"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "gray80", fill = NA, size = 0.5)
  ) +
  guides(linetype = "none")  # Remove linetype from legend since all are solid
p6$facet$params$labeller <- as_labeller(function(x) gsub("ScoreType = ", "", x))
p6

p7 <- interact_plot(model42, 
                    pred = PC4,           
                    modx = Gender,                   
                    mod2 = ScoreType,
                    interval = F,
                    colors = c("red", "blue", "green","orange")) +
  labs(x = "PC4 (standardized)", 
       y = "Predicted Score",
       color = "Gender") +
  facet_wrap(~ScoreType, labeller = label_value) +
  scale_linetype_manual(values = c("solid", "solid", "solid", "solid")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 9, hjust = 0.5, color = "gray60"),
    axis.title = element_text(size = 10, face = "bold"),
    axis.text = element_text(size = 9),
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 8),
    strip.text = element_text(size = 10, face = "bold"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "gray80", fill = NA, size = 0.5)
  ) +
  guides(linetype = "none")  # Remove linetype from legend since all are solid
p7$facet$params$labeller <- as_labeller(function(x) gsub("ScoreType = ", "", x))
p7

model5 <- Score ~ ScoreType * (Readability*PC1+MTLD*PC1+MeanPause*PC8+
                                 R_burst*PC5+R_burst* Readability+ P_burst*ConnectiveVariety+ 
                                 Education*PC6+Education*Readability+Education*R_burst+Ethnicity*R_burst+
                                 Ethnicity* Education + Gender*PC4+Gender*BetweenWordPause+Age*PC1) +(1 | ID2) + (1 | Topic)
# Run automatic model selection
pruned_model <- buildmer(model5, data = data_scaled, buildmerControl = list(direction = "backward", crit = "BIC"), family = gaussian)
summary(pruned_model@model)
tab_model(pruned_model@model)


model_formula <- Score ~ ScoreType * (Age + Gender + Ethnicity + Education + Topic +
                                        P_burst + R_burst + MeanPause + WithinWordPause + BetweenWordPause + MeanDeletion +
                                        ProportionInsertion +  MTLD + TUnitLength + 
                                        DependencyCosSim + Readability + ArgumentOverlap + ConnectiveVariety +
                                        PC1+PC4+PC5+PC6+PC7+PC8)^2 + (1 | ID2)
# Run automatic model selection
pruned_model <- buildmer(model_formula, data = data_scaled, buildmerControl = list(direction = "backward", crit = "BIC"), family = gaussian)
summary(pruned_model@model)


plot(model4)  # Residuals vs fitted
qqnorm(residuals(model4)); qqline(residuals(model4))  # Normality

vif(lm(Score ~ ScoreType*(Age +Gender+Ethnicity+Nativeness+Education+Topic+P_burst+R_burst+ 
                            MeanPause), data = data_scaled))  # Use lm version

performance::check_model(model_scaled4)  # From performance package
performance::r2(model42)           # Conditional and marginal R²
tab_model(model_scaled5)


# Get model predictions
preds <- predict(model_scaled3)

# Get true values
actuals <- data_scaled$Score  # or whatever your DV column is

#Compute RMSE####
df <- read.csv("your_file.csv")  # replace with your actual filename

# Ensure columns are numeric
df$Human <- as.numeric(df$Human)
df$Claude3.7 <- as.numeric(df$Claude3.7)

# Compute MAE and RMSE
mae_value <- mae(df$Human, df$Claude3.7)
rmse_value <- rmse(df$Human, df$Claude3.7)

# Print the results
cat("MAE:", mae_value, "\nRMSE:", rmse_value, "\n")


#condition index k for collinearity
collin.fnc.mer(model_scaled2)$cnumber 
#C-index
probs = 1/(1+exp(-fitted(model_scaled2)))
somers2(probs, as.numeric(df$Score)-1)

plot_model(model_scaled2, type = "est", sort.est = TRUE)  # Sort by effect size

# Extract model data
model_data <- plot_model(model_scaled2, type = "est")$data
# Filter for only the interactions
interactions_of_interest <- model_data %>%
  filter(grepl(":", term))

# Create custom plot
ggplot(interactions_of_interest, aes(y = reorder(term, estimate), x = estimate)) +
  geom_point() +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), width = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 10, face = "plain"),
    axis.title = element_text(size = 12, face = "plain")
  ) +
  labs(y = "", x = "Estimates")


# Get interaction effect
interact_effect <- Effect(c("ScoreType", "Age"), model_scaled4)
df_effect <- as.data.frame(interact_effect)
# Plot using ggplot2
age <- ggplot(df_effect, aes(x = Age, y = fit, color = ScoreType, fill = ScoreType)) +
  geom_line(linewidth = 0.5) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
  labs(title = "",
       x = "Age", y = "Predicted essay score") +
  scale_color_manual(values = c("Human" = "darkred","GPT4o" = "darkblue", "Claude3.7" = "darkgreen")) +
  scale_fill_manual(values = c("Human" = "darkred","GPT4o" = "darkblue", "Claude3.7" = "darkgreen")) +
  theme_bw()
age

PrimeType1 <- Effect(c("Gender", "ScoreType"), model41) %>% 
  as.data.frame() %>% 
  droplevels()
PrimeType1 <- ggplot(PrimeType1, aes(Gender, fit, group = ScoreType, color = ScoreType)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0,
                position = position_dodge(.2)) +
  geom_line(linetype = "dotted", position = position_dodge(.2)) +
  geom_point(size = 2.5, position = position_dodge(.2)) +
  theme_bw() +
  labs(y = "", x = "Gender") +
  scale_color_manual(values = c("Human" = "darkred","GPT4o" = "darkblue", "Claude3.7" = "darkgreen")) +
  guides(shape = guide_legend(ncol = 1, title.position = "top")) +
  theme(axis.text.x = element_text(size = 11),
        axis.title = element_text(size = 13),
        legend.position = "right",
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 10))
PrimeType1

## join plots for making 1 Figure 
combined_plot <- age + PrimeType1+
  plot_layout(ncol = 2, guides = "collect") & 
  theme(legend.position = "bottom",
        legend.title = element_blank())  # This hides the legend title

combined_plot <- combined_plot + plot_annotation(
  title = "",
  theme = theme(plot.title = element_text(hjust = 0.5))
)
combined_plot


PrimeType2<- Effect(c("FirstLanguage","ScoreType"), model_scaled2) %>% as.data.frame %>% droplevels
PrimeType2<-ggplot(PrimeType2, aes(FirstLanguage, fit, group=ScoreType, color=ScoreType)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width=0,
                position = position_dodge(.2)) +
  geom_line(linetype = "dotted", position=position_dodge(.2)) +
  geom_point(size=2.5, position=position_dodge(.2)) +
  theme_bw()+
  labs(y="Predicted essay score", x="FirstLanguage") +
  scale_color_manual(values = c("Human" = "darkred","GPT4o" = "darkblue", "Claude3.7" = "darkgreen")) +
  guides(shape=guide_legend(ncol=1,title.position = "top"))+
  theme(axis.text.x = element_text(size=11), axis.title = element_text(size=13))+
  theme(legend.position = "right",legend.title = element_text(size=11), legend.text = element_text(size=10))
PrimeType2

PrimeType4 <- Effect(c("Topic", "ScoreType"), model_scaled5) %>% 
  as.data.frame() %>% 
  droplevels()
PrimeType4<-ggplot(PrimeType4, aes(Topic, fit, group=ScoreType, color=ScoreType)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width=0,
                position = position_dodge(.2)) +
  geom_line(linetype = "dotted", position=position_dodge(.2)) +
  geom_point(size=2.5, position=position_dodge(.2)) +
  theme_bw()+
  labs(y="", x="Topic") +
  scale_color_manual(values = c("Human" = "darkred","GPT4o" = "darkblue", "Claude3.7" = "darkgreen")) +
  guides(shape=guide_legend(ncol=1,title.position = "top"))+
  theme(axis.text.x = element_text(size=11), axis.title = element_text(size=13))+
  theme(legend.position = "right",legend.title = element_text(size=11), legend.text = element_text(size=10))
PrimeType4

## join plots for making 1 Figure 
combined_plot <- PrimeType2 + PrimeType4+
  plot_layout(ncol = 2, guides = "collect") & 
  theme(legend.position = "bottom",
        legend.title = element_blank())  # This hides the legend title

combined_plot <- combined_plot + plot_annotation(
  title = "",
  theme = theme(plot.title = element_text(hjust = 0.5))
)
combined_plot

PrimeType3<- Effect(c("Ethnicity","ScoreType"), model_scaled5) %>% as.data.frame %>% droplevels
PrimeType3<-ggplot(PrimeType3, aes(Ethnicity, fit, group=ScoreType, color=ScoreType)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width=0,
                position = position_dodge(.2)) +
  geom_line(linetype = "dotted", position=position_dodge(.2)) +
  geom_point(size=2.5, position=position_dodge(.2)) +
  theme_bw()+
  labs(y="Predicted essay score", x="Ethnicity") +
  scale_color_manual(values = c("Human" = "darkred","GPT4o" = "darkblue", "Claude3.7" = "darkgreen")) +
  guides(shape=guide_legend(ncol=1,title.position = "top"))+
  theme(axis.text.x = element_text(size=11), axis.title = element_text(size=13))+
  theme(legend.position = "right",legend.title = element_blank(), legend.text = element_text(size=10))
PrimeType3


PrimeType5<- Effect(c("Education","ScoreType"), model_scaled2) %>% as.data.frame %>% droplevels
PrimeType5<-ggplot(PrimeType5, aes(Education, fit, group=ScoreType, color=ScoreType)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width=0,
                position = position_dodge(.2)) +
  geom_line(linetype = "dotted", position=position_dodge(.2)) +
  geom_point(size=2.5, position=position_dodge(.2)) +
  theme_bw()+
  labs(y="Predicted essay score", x="Education") +
  scale_color_manual(values = c("Human" = "darkred","GPT4o" = "darkblue", "Claude3.7" = "darkgreen")) +
  guides(shape=guide_legend(ncol=1,title.position = "top"))+
  theme(axis.text.x = element_text(size=11), axis.title = element_text(size=13))+
  theme(legend.position = "right",legend.title = element_blank(), legend.text = element_text(size=10))
PrimeType5

# Get interaction effect
interact_effect <- Effect(c("FirstLanguage", "R_burst"), model_scaled4)
df_effect <- as.data.frame(interact_effect)
# Plot using ggplot2
p6<-ggplot(df_effect, aes(x = R_burst, y = fit, color = FirstLanguage)) +
  geom_line(linewidth = 0.5) +
  scale_color_manual(values = c("Human" = "darkred","GPT4o" = "darkblue", "Claude3.7" = "darkgreen")) +
  scale_fill_manual(values = c("Human" = "darkred","GPT4o" = "darkblue", "Claude3.7" = "darkgreen")) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = FirstLanguage), alpha = 0.2, color = NA) +
  labs(title = "",
       x = "mean_p_burst_length", y = "Predicted essay score") +
  theme_bw()
p6

interact_effect <- Effect(c("ScoreType", "R_burst"), model41)
df_effect <- as.data.frame(interact_effect)
# Plot using ggplot2
p7<-ggplot(df_effect, aes(x = R_burst, y = fit, color = ScoreType)) +
  geom_line(linewidth = 0.5) +
  scale_color_manual(values = c("Human" = "darkred","GPT4o" = "darkblue", "Claude3.7" = "darkgreen")) +
  scale_fill_manual(values = c("Human" = "darkred","GPT4o" = "darkblue", "Claude3.7" = "darkgreen")) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = ScoreType), alpha = 0.2, color = NA) +
  labs(title = "",
       x = "mean_r_burst_length", y = "") +
  theme_bw()
p7

interact_effect <- Effect(c("ScoreType", "MeanPause"), model_scaled2)
df_effect <- as.data.frame(interact_effect)
# Plot using ggplot2
p8<-ggplot(df_effect, aes(x = MeanPause, y = fit, color = ScoreType)) +
  geom_line(linewidth = 0.5) +
  scale_color_manual(values = c("Human" = "darkred","GPT4o" = "darkblue", "Claude3.7" = "darkgreen")) +
  scale_fill_manual(values = c("Human" = "darkred","GPT4o" = "darkblue", "Claude3.7" = "darkgreen")) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = ScoreType), alpha = 0.2, color = NA) +
  labs(title = "",
       x = "mean_pause_length", y = "Predicted essay score") +
  theme_bw()
p8

interact_effect <- Effect(c("ScoreType", "ProportionInsertion"), model_scaled2)
df_effect <- as.data.frame(interact_effect)
# Plot using ggplot2
p9<-ggplot(df_effect, aes(x = ProportionInsertion, y = fit, color = ScoreType)) +
  geom_line(linewidth = 0.5) +
  scale_color_manual(values = c("Human" = "darkred","GPT4o" = "darkblue", "Claude3.7" = "darkgreen")) +
  scale_fill_manual(values = c("Human" = "darkred","GPT4o" = "darkblue", "Claude3.7" = "darkgreen")) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = ScoreType), alpha = 0.2, color = NA) +
  labs(title = "",
       x = "proportion_insertion", y = "") +
  theme_bw()
p9

interact_effect <- Effect(c("ScoreType", "IntervalVariance"), model_scaled2)
df_effect <- as.data.frame(interact_effect)
# Plot using ggplot2
p10<-ggplot(df_effect, aes(x = IntervalVariance, y = fit, color = ScoreType)) +
  geom_line(linewidth = 0.5) +
  scale_color_manual(values = c("Human" = "darkred","GPT4o" = "darkblue", "Claude3.7" = "darkgreen")) +
  scale_fill_manual(values = c("Human" = "darkred","GPT4o" = "darkblue", "Claude3.7" = "darkgreen")) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = ScoreType), alpha = 0.2, color = NA) +
  labs(title = "",
       x = "interval_variance", y = "Predicted essay score") +
  theme_bw()
p10

## join plots for making 1 Figure 
combined_plot <- p6 + p7+p8+p9+p10+
  plot_layout(ncol = 2, guides = "collect") & 
  theme(legend.position = "bottom",
        legend.title = element_blank())  # This hides the legend title

combined_plot <- combined_plot + plot_annotation(
  title = "",
  theme = theme(plot.title = element_text(hjust = 0.5))
)
combined_plot

# Get the effect
writing_effect <- Effect("Writing_Interest", model3)
df_writing <- as.data.frame(writing_effect)
# Plot
ggplot(df_writing, aes(x = Writing_Interest, y = fit)) +
  geom_point(size = 3, color = "#2a5674") +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, color = "#2a5674") +
  labs(title = "Main Effect of Writing Interest",
       x = "Writing Interest", y = "Predicted Score") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#trees####
#Conditional inference tree_______________________________________________________________________
set.seed(500)
# apply bonferroni correction (1 minus alpha multiplied by n of predictors) controls = ctree_control (testtype = "MonteCarlo", mincriterion = 0.95,minbucket=20)
dat.ctree <- ctree(
  Score ~ ScoreType + Age + Gender + Prompt + Ethnicity + Nativeness + Education + Writing_Interest + Writing_Strength,
  data = data_clean,
  control = ctree_control(testtype = "MonteCarlo", minbucket = 20, maxdepth = 4)
)

plot(dat.ctree, gp = gpar(fontsize = 8))

set.seed(500)
dat.rf <- cforest(Score ~ ScoreType + Age + Gender + Prompt + Ethnicity + Nativeness + Education + Writing_Interest + Writing_Strength, 
                  data = data_clean,controls=cforest_unbiased(ntree = 1500, mtry = 4))
dat.varimp <- varimp(dat.rf, conditional = TRUE)
round(dat.varimp, 3)
dotchart(sort(dat.varimp), main = "Conditional Importance of Variables")
abline(v=abs(min(dat.varimp)),color="red",lty="dotdash",lwd=2)

#keystroke logging analysis####
library(dplyr)
library(readr)
library(stringr)
library(purrr)
library(tidyverse)

# Set your data folder path
data_folder <- "E:/KeystrokeLogging/WritingTask/key"

# List all CSV files
csv_files <- list.files(data_folder, pattern = "\\.csv$", full.names = TRUE)

# Helper function to safely count characters in TextChange
count_chars <- function(change_str) {
  if (is.na(change_str) || change_str == "NoChange") return(0)
  
  # Case 1: Has a => pattern (edit)
  if (str_detect(change_str, "=>")) {
    parts <- str_split(change_str, "=>")[[1]]
    if (length(parts) == 2) {
      return(str_length(str_trim(parts[2])))
    } else {
      return(0)
    }
  }
  
  # Case 2: Blank string (space)
  if (str_trim(change_str) == "") {
    return(1)
  }
  
  # Case 3: Regular typed string
  return(str_length(str_trim(change_str)))
}

# Function to calculate all metrics for one file
analyze_file <- function(file_path) {
  df <- read_csv(file_path, show_col_types = FALSE)
  
  df <- df %>%
    mutate(
      PauseTimeSec = PauseTime / 1000,
      IsPause = PauseTime > 200,
      IsWithinWord = grepl("\\S", DownEvent) & lag(grepl("\\S", DownEvent)) & CursorPosition == lag(CursorPosition) + 1,
      IsBetweenWord = DownEvent == "Space" & PauseTime > 200
    )
  
  # --- P-bursts ---
  df$Pbreak <- df$PauseTime > 2000
  df$Pburst_id <- cumsum(df$Pbreak)
  
  pbursts <- df %>%
    filter(Activity == "Input") %>%
    group_by(Pburst_id) %>%
    summarise(length = n(), .groups = "drop") %>%
    filter(length > 1)
  
  mean_pburst <- mean(pbursts$length, na.rm = TRUE)
  
  # --- R-bursts ---
  df$Rbreak <- (df$Activity %in% c("Remove/Cut", "Replace")) & df$PauseTime > 2000
  df$Rburst_id <- cumsum(df$Rbreak)
  
  rbursts <- df %>%
    group_by(Rburst_id) %>%
    summarise(length = n(), .groups = "drop") %>%
    filter(length > 1)
  
  mean_rburst <- mean(rbursts$length, na.rm = TRUE)
  
  # --- Pause metrics ---
  mean_pause <- mean(df$PauseTimeSec[df$PauseTime > 200], na.rm = TRUE)
  mean_within_word <- mean(df$PauseTimeSec[df$IsPause & df$IsWithinWord], na.rm = TRUE)
  mean_between_word <- mean(df$PauseTimeSec[df$IsBetweenWord], na.rm = TRUE)
  
  # --- Insertions & Deletions ---
  # Filter for "Remove/Cut" (deletions) and "Replace" (potentially deletions or insertions)
  deletions <- df %>% filter(Activity %in% c("Remove/Cut", "Replace"))
  insertions <- df %>% filter(Activity == "Input")  # Assumes Input is for insertions
  
  # Calculate lengths for deletions and insertions
  del_lengths <- sapply(deletions$TextChange, count_chars)
  ins_lengths <- sapply(insertions$TextChange, count_chars)
  
  # Mean lengths
  mean_del_length <- mean(del_lengths, na.rm = TRUE)
  mean_ins_length <- mean(ins_lengths, na.rm = TRUE)
  
  # Total characters inserted or deleted
  total_chars <- sum(sapply(df$TextChange, count_chars), na.rm = TRUE)
  
  # Proportions of deletions and insertions
  prop_del <- sum(del_lengths, na.rm = TRUE) / total_chars
  prop_ins <- sum(ins_lengths, na.rm = TRUE) / total_chars
  
  # --- Interval variance ---
  df_prod <- df %>% filter(Activity == "Input")
  if (nrow(df_prod) >= 10) {
    total_time <- max(df_prod$UpTime) - min(df_prod$DownTime)
    interval_length <- total_time / 10
    
    df_prod$Interval <- floor((df_prod$DownTime - min(df_prod$DownTime)) / interval_length) + 1
    interval_rates <- df_prod %>%
      group_by(Interval) %>%
      summarise(chars = sum(sapply(TextChange, count_chars), na.rm = TRUE), .groups = "drop")
    
    interval_variance <- sd(interval_rates$chars, na.rm = TRUE)
  } else {
    interval_variance <- NA
  }
  
  return(tibble(
    file = basename(file_path),
    mean_pburst = mean_pburst,
    mean_rburst = mean_rburst,
    mean_pause = mean_pause,
    mean_within_word = mean_within_word,
    mean_between_word = mean_between_word,
    mean_deletion_length = mean_del_length,
    prop_deletion = prop_del,
    mean_insertion_length = mean_ins_length,
    prop_insertion = prop_ins,
    interval_variance = interval_variance
  ))
}

# Run the analysis on all files
results <- map_dfr(csv_files, analyze_file)

# Save results to CSV
write_csv(results, "keystroke_summary_metrics.csv")

#Better:keystroke 2####
# Keystroke Logging Data Analysis Script

# Load required libraries
library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(purrr)

# Function to analyze a single keystroke logging file
analyze_keystroke_file <- function(file_path) {
  # Read the CSV file
  data <- read.csv(file_path)
  
  # Ensure proper column types
  data$DownEventID <- as.integer(data$DownEventID)
  data$UpEventID <- as.integer(data$UpEventID)
  data$DownTime <- as.numeric(data$DownTime)
  data$UpTime <- as.numeric(data$UpTime)
  data$ActionTime <- as.numeric(data$ActionTime)
  data$CursorPosition <- as.integer(data$CursorPosition)
  data$PauseTime <- as.numeric(data$PauseTime)
  data$WordCount <- as.integer(data$WordCount)
  data$Activity <- as.factor(data$Activity)
  
  # Extract filename for identification
  file_name <- basename(file_path)
  
  # Calculate P-bursts (pauses exceeding 2 seconds)
  # A P-burst is a string of actions between pauses that exceed 2 seconds
  data$new_p_burst <- data$PauseTime >= 2000
  data$p_burst_id <- cumsum(data$new_p_burst)
  
  p_bursts <- data %>%
    filter(Activity %in% c("Input", "Remove/Cut", "Replace")) %>%
    group_by(p_burst_id) %>%
    summarize(
      burst_length = sum(ifelse(Activity == "Input" | Activity == "Replace", 1, 0)),
      .groups = "drop"
    )
  
  mean_p_burst_length <- mean(p_bursts$burst_length, na.rm = TRUE)
  
  # Calculate R-bursts (insertions or deletions exceeding 2 seconds)
  data$is_revision <- data$Activity %in% c("Remove/Cut", "Replace")
  data$new_r_burst <- c(FALSE, diff(as.integer(data$is_revision)) != 0)
  data$r_burst_id <- cumsum(data$new_r_burst)
  
  r_bursts <- data %>%
    filter(Activity %in% c("Input", "Remove/Cut", "Replace")) %>%
    group_by(r_burst_id) %>%
    summarize(
      burst_length = sum(ifelse(Activity == "Input" | Activity == "Replace", 1, 0)),
      .groups = "drop"
    )
  
  mean_r_burst_length <- mean(r_bursts$burst_length, na.rm = TRUE)
  
  # Calculate mean pause lengths (pauses exceeding 200ms)
  all_pauses <- data$PauseTime[data$PauseTime >= 200]
  mean_pause_length <- mean(all_pauses, na.rm = TRUE) / 1000  # Convert to seconds
  
  # Calculate within-word pauses
  # Assume within-word pauses occur when the Activity is "Input" or "Replace" and we're not at word boundaries
  within_word_indices <- which(data$Activity %in% c("Input", "Replace") & 
                                 !str_detect(data$TextChange, "\\s") & 
                                 data$PauseTime >= 200)
  
  # Exclude first character of each word since that would be between-word
  word_start_indices <- which(data$WordCount > lag(data$WordCount, default = 0))
  within_word_indices <- setdiff(within_word_indices, word_start_indices)
  
  mean_within_word_pause <- mean(data$PauseTime[within_word_indices], na.rm = TRUE) / 1000  # Convert to seconds
  
  # Calculate between-word pauses
  # Assume between-word pauses occur before the first character of a word
  between_word_indices <- which(data$Activity %in% c("Input", "Replace") & 
                                  (data$WordCount > lag(data$WordCount, default = 0) | 
                                     str_detect(data$TextChange, "\\s")) &
                                  data$PauseTime >= 200)
  
  mean_between_word_pause <- mean(data$PauseTime[between_word_indices], na.rm = TRUE) / 1000  # Convert to seconds
  
  # Calculate deletion metrics
  deletion_rows <- data %>% filter(Activity == "Remove/Cut")
  deletion_count <- nrow(deletion_rows)
  
  # Group consecutive deletions
  deletion_rows$deletion_group <- c(0, cumsum(diff(deletion_rows$DownEventID) > 1))
  deletion_groups <- deletion_rows %>% 
    group_by(deletion_group) %>% 
    summarize(chars_deleted = n(), .groups = "drop")
  
  mean_deletion_length <- mean(deletion_groups$chars_deleted, na.rm = TRUE)
  
  # Total characters produced
  total_chars <- sum(data$Activity %in% c("Input", "Replace")) + sum(data$Activity == "Remove/Cut")
  
  # Proportion of deletions
  prop_deletions <- sum(data$Activity == "Remove/Cut") / total_chars
  
  # Calculate insertion metrics (Input and Replace are insertions)
  insertion_rows <- data %>% filter(Activity %in% c("Input", "Replace"))
  insertion_count <- nrow(insertion_rows)
  
  # Group consecutive insertions
  insertion_rows$insertion_group <- c(0, cumsum(diff(insertion_rows$DownEventID) > 1))
  insertion_groups <- insertion_rows %>% 
    group_by(insertion_group) %>% 
    summarize(chars_inserted = n(), .groups = "drop")
  
  mean_insertion_length <- mean(insertion_groups$chars_inserted, na.rm = TRUE)
  
  # Proportion of insertions
  prop_insertions <- insertion_count / total_chars
  
  # Calculate interval variance
  # First, divide the writing process into 10 equal intervals
  max_time <- max(data$UpTime, na.rm = TRUE)
  min_time <- min(data$DownTime, na.rm = TRUE)
  total_time <- max_time - min_time
  interval_size <- total_time / 10
  
  data$interval <- ceiling((data$DownTime - min_time) / interval_size)
  data$interval[data$interval == 0] <- 1  # Ensure no zero intervals
  
  # Calculate production rate for each interval
  interval_rates <- data %>%
    filter(Activity %in% c("Input", "Replace")) %>%
    group_by(interval) %>%
    summarize(
      chars_produced = n(),
      .groups = "drop"
    ) %>%
    arrange(interval)
  
  # Fill in missing intervals with zero production
  all_intervals <- data.frame(interval = 1:10)
  interval_rates <- merge(all_intervals, interval_rates, by = "interval", all.x = TRUE)
  interval_rates$chars_produced[is.na(interval_rates$chars_produced)] <- 0
  
  # Calculate the standard deviation of production rates
  interval_variance <- sd(interval_rates$chars_produced, na.rm = TRUE)
  
  # Return all calculated metrics as a dataframe row
  result <- data.frame(
    file_name = file_name,
    mean_p_burst_length = mean_p_burst_length,
    mean_r_burst_length = mean_r_burst_length,
    mean_pause_length = mean_pause_length,
    mean_within_word_pause = mean_within_word_pause,
    mean_between_word_pause = mean_between_word_pause,
    mean_deletion_length = mean_deletion_length,
    proportion_deletions = prop_deletions,
    mean_insertion_length = mean_insertion_length,
    proportion_insertions = prop_insertions,
    interval_variance = interval_variance
  )
  
  return(result)
}

# Function to process all CSV files in a folder
process_keystroke_folder <- function(folder_path, output_file = "keystroke_analysis_results.csv") {
  # Get list of all CSV files in the folder
  csv_files <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)
  
  # Process each file and combine results
  results <- map_df(csv_files, safely(analyze_keystroke_file)) %>%
    filter(!is.null(result)) %>%
    unnest(result)
  
  # Write results to CSV
  write.csv(results, output_file, row.names = FALSE)
  
  cat("Analysis complete. Results saved to", output_file, "\n")
  return(results)
}


results <- process_keystroke_folder("E:/KeystrokeLogging/WritingTask/key")

#Best:keystroke 3####
# Updated Keystroke Logging Data Analysis Script

# Load required libraries
library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(purrr)

# Function to analyze a single keystroke logging file
analyze_keystroke_file <- function(file_path) {
  # Read the CSV file
  data <- read.csv(file_path)
  
  # Ensure proper column types
  data$DownEventID <- as.integer(data$DownEventID)
  data$UpEventID <- as.integer(data$UpEventID)
  data$DownTime <- as.numeric(data$DownTime)
  data$UpTime <- as.numeric(data$UpTime)
  data$ActionTime <- as.numeric(data$ActionTime)
  data$CursorPosition <- as.integer(data$CursorPosition)
  data$PauseTime <- as.numeric(data$PauseTime)
  data$WordCount <- as.integer(data$WordCount)
  data$Activity <- as.factor(data$Activity)
  
  # Extract filename for identification
  file_name <- basename(file_path)
  
  # Calculate P-bursts (pauses exceeding 2 seconds)
  # A P-burst is a string of actions between pauses that exceed 2 seconds
  data$new_p_burst <- data$PauseTime >= 2000
  data$p_burst_id <- cumsum(data$new_p_burst)
  
  p_bursts <- data %>%
    filter(Activity %in% c("Input", "Remove/Cut", "Replace", "Paste")) %>%
    group_by(p_burst_id) %>%
    summarize(
      burst_length = sum(ifelse(Activity %in% c("Input", "Replace", "Paste"), 1, 0)),
      .groups = "drop"
    )
  
  mean_p_burst_length <- mean(p_bursts$burst_length, na.rm = TRUE)
  
  # Calculate R-bursts (insertions or deletions exceeding 2 seconds)
  data$is_revision <- data$Activity %in% c("Remove/Cut", "Replace")
  data$new_r_burst <- c(FALSE, diff(as.integer(data$is_revision)) != 0)
  data$r_burst_id <- cumsum(data$new_r_burst)
  
  r_bursts <- data %>%
    filter(Activity %in% c("Input", "Remove/Cut", "Replace", "Paste")) %>%
    group_by(r_burst_id) %>%
    summarize(
      burst_length = sum(ifelse(Activity %in% c("Input", "Replace", "Paste"), 1, 0)),
      .groups = "drop"
    )
  
  mean_r_burst_length <- mean(r_bursts$burst_length, na.rm = TRUE)
  
  # Calculate mean pause lengths (pauses exceeding 200ms)
  all_pauses <- data$PauseTime[data$PauseTime >= 200]
  mean_pause_length <- mean(all_pauses, na.rm = TRUE) / 1000  # Convert to seconds
  
  # Calculate within-word pauses
  # Assume within-word pauses occur when the Activity is "Input", "Replace", or "Paste" and we're not at word boundaries
  within_word_indices <- which(data$Activity %in% c("Input", "Replace", "Paste") & 
                                 !str_detect(data$TextChange, "\\s") & 
                                 data$PauseTime >= 200)
  
  # Exclude first character of each word since that would be between-word
  word_start_indices <- which(data$WordCount > lag(data$WordCount, default = 0))
  within_word_indices <- setdiff(within_word_indices, word_start_indices)
  
  mean_within_word_pause <- mean(data$PauseTime[within_word_indices], na.rm = TRUE) / 1000  # Convert to seconds
  
  # Calculate between-word pauses
  # Assume between-word pauses occur before the first character of a word
  between_word_indices <- which(data$Activity %in% c("Input", "Replace", "Paste") & 
                                  (data$WordCount > lag(data$WordCount, default = 0) | 
                                     str_detect(data$TextChange, "\\s")) &
                                  data$PauseTime >= 200)
  
  mean_between_word_pause <- mean(data$PauseTime[between_word_indices], na.rm = TRUE) / 1000  # Convert to seconds
  
  # Calculate deletion metrics
  deletion_rows <- data %>% filter(Activity == "Remove/Cut")
  deletion_count <- nrow(deletion_rows)
  
  # Group consecutive deletions
  deletion_rows$deletion_group <- c(0, cumsum(diff(deletion_rows$DownEventID) > 1))
  deletion_groups <- deletion_rows %>% 
    group_by(deletion_group) %>% 
    summarize(chars_deleted = n(), .groups = "drop")
  
  mean_deletion_length <- mean(deletion_groups$chars_deleted, na.rm = TRUE)
  
  # Total characters produced (counting all productive actions)
  total_chars <- sum(data$Activity %in% c("Input", "Replace", "Paste")) + sum(data$Activity == "Remove/Cut")
  
  # Proportion of deletions
  prop_deletions <- sum(data$Activity == "Remove/Cut") / total_chars
  
  # Calculate insertion metrics (Input, Replace, and Paste are insertions)
  insertion_rows <- data %>% filter(Activity %in% c("Input", "Replace", "Paste"))
  insertion_count <- nrow(insertion_rows)
  
  # Group consecutive insertions
  insertion_rows$insertion_group <- c(0, cumsum(diff(insertion_rows$DownEventID) > 1))
  insertion_groups <- insertion_rows %>% 
    group_by(insertion_group) %>% 
    summarize(chars_inserted = n(), .groups = "drop")
  
  mean_insertion_length <- mean(insertion_groups$chars_inserted, na.rm = TRUE)
  
  # Proportion of insertions
  prop_insertions <- insertion_count / total_chars
  
  # Calculate paste operations specifically
  paste_rows <- data %>% filter(Activity == "Paste")
  paste_count <- nrow(paste_rows)
  
  # Mean paste length
  paste_length <- 0
  if(paste_count > 0) {
    paste_groups <- paste_rows %>% 
      group_by(DownEventID) %>% 
      summarize(chars_pasted = n(), .groups = "drop")
    paste_length <- mean(paste_groups$chars_pasted, na.rm = TRUE)
  }
  
  # Calculate interval variance
  # First, divide the writing process into 10 equal intervals
  max_time <- max(data$UpTime, na.rm = TRUE)
  min_time <- min(data$DownTime, na.rm = TRUE)
  total_time <- max_time - min_time
  interval_size <- total_time / 10
  
  data$interval <- ceiling((data$DownTime - min_time) / interval_size)
  data$interval[data$interval == 0] <- 1  # Ensure no zero intervals
  
  # Calculate production rate for each interval
  interval_rates <- data %>%
    filter(Activity %in% c("Input", "Replace", "Paste")) %>%
    group_by(interval) %>%
    summarize(
      chars_produced = n(),
      .groups = "drop"
    ) %>%
    arrange(interval)
  
  # Fill in missing intervals with zero production
  all_intervals <- data.frame(interval = 1:10)
  interval_rates <- merge(all_intervals, interval_rates, by = "interval", all.x = TRUE)
  interval_rates$chars_produced[is.na(interval_rates$chars_produced)] <- 0
  
  # Calculate the standard deviation of production rates
  interval_variance <- sd(interval_rates$chars_produced, na.rm = TRUE)
  
  # Return all calculated metrics as a dataframe row
  result <- data.frame(
    file_name = file_name,
    mean_p_burst_length = mean_p_burst_length,
    mean_r_burst_length = mean_r_burst_length,
    mean_pause_length = mean_pause_length,
    mean_within_word_pause = mean_within_word_pause,
    mean_between_word_pause = mean_between_word_pause,
    mean_deletion_length = mean_deletion_length,
    proportion_deletions = prop_deletions,
    mean_insertion_length = mean_insertion_length,
    proportion_insertions = prop_insertions,
    mean_paste_length = paste_length,
    paste_count = paste_count,
    interval_variance = interval_variance
  )
  
  return(result)
}

# Function to process all CSV files in a folder
process_keystroke_folder <- function(folder_path, output_file = "keystroke_analysis_results.csv") {
  # Get list of all CSV files in the folder
  csv_files <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)
  
  # Process each file and combine results
  results <- map_df(csv_files, safely(analyze_keystroke_file)) %>%
    filter(!is.null(result)) %>%
    unnest(result)
  
  # Write results to CSV
  write.csv(results, output_file, row.names = FALSE)
  
  cat("Analysis complete. Results saved to", output_file, "\n")
  return(results)
}


results <- process_keystroke_folder("E:/KeystrokeLogging/WritingTask/key")

#final:keystroke4####
# Fixed Keystroke Logging Data Analysis Script with improved error handling

# Load required libraries
library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(purrr)

# Function to analyze a single keystroke logging file
analyze_keystroke_file <- function(file_path) {
  # Create a tryCatch to handle potential errors in processing
  tryCatch({
    # Read the CSV file
    data <- read.csv(file_path)
    
    # Ensure proper column types
    data$DownEventID <- as.integer(data$DownEventID)
    data$UpEventID <- as.integer(data$UpEventID)
    data$DownTime <- as.numeric(data$DownTime)
    data$UpTime <- as.numeric(data$UpTime)
    data$ActionTime <- as.numeric(data$ActionTime)
    data$CursorPosition <- as.integer(data$CursorPosition)
    data$PauseTime <- as.numeric(data$PauseTime)
    data$WordCount <- as.integer(data$WordCount)
    data$Activity <- as.factor(data$Activity)
    
    # Extract filename for identification
    file_name <- basename(file_path)
    
    # Calculate P-bursts (pauses exceeding 2 seconds)
    # A P-burst is a string of actions between pauses that exceed 2 seconds
    data$new_p_burst <- data$PauseTime >= 2000
    data$p_burst_id <- cumsum(data$new_p_burst)
    
    p_bursts <- data %>%
      filter(Activity %in% c("Input", "Remove/Cut", "Replace", "Paste")) %>%
      group_by(p_burst_id) %>%
      summarize(
        burst_length = sum(ifelse(Activity %in% c("Input", "Replace", "Paste"), 1, 0)),
        .groups = "drop"
      )
    
    mean_p_burst_length <- mean(p_bursts$burst_length, na.rm = TRUE)
    
    # Calculate R-bursts (insertions or deletions exceeding 2 seconds)
    data$is_revision <- data$Activity %in% c("Remove/Cut", "Replace")
    data$new_r_burst <- c(FALSE, diff(as.integer(data$is_revision)) != 0)
    data$r_burst_id <- cumsum(data$new_r_burst)
    
    r_bursts <- data %>%
      filter(Activity %in% c("Input", "Remove/Cut", "Replace", "Paste")) %>%
      group_by(r_burst_id) %>%
      summarize(
        burst_length = sum(ifelse(Activity %in% c("Input", "Replace", "Paste"), 1, 0)),
        .groups = "drop"
      )
    
    mean_r_burst_length <- mean(r_bursts$burst_length, na.rm = TRUE)
    
    # Calculate mean pause lengths (pauses exceeding 200ms)
    all_pauses <- data$PauseTime[data$PauseTime >= 200]
    mean_pause_length <- mean(all_pauses, na.rm = TRUE) / 1000  # Convert to seconds
    
    # Calculate within-word pauses
    within_word_indices <- numeric(0)
    
    # Use tryCatch here to handle potential errors in lag() function
    tryCatch({
      # Assume within-word pauses occur when we're not at word boundaries
      within_word_indices <- which(data$Activity %in% c("Input", "Replace", "Paste") & 
                                     !str_detect(data$TextChange, "\\s") & 
                                     data$PauseTime >= 200)
      
      # Exclude first character of each word since that would be between-word
      word_start_indices <- which(data$WordCount > lag(data$WordCount, default = 0))
      within_word_indices <- setdiff(within_word_indices, word_start_indices)
    }, error = function(e) {
      warning(paste("Error calculating within-word pauses for", file_name, ":", e$message))
    })
    
    mean_within_word_pause <- ifelse(length(within_word_indices) > 0, 
                                     mean(data$PauseTime[within_word_indices], na.rm = TRUE) / 1000,
                                     NA)
    
    # Calculate between-word pauses
    between_word_indices <- numeric(0)
    
    # Use tryCatch here to handle potential errors in lag() function
    tryCatch({
      # Assume between-word pauses occur before the first character of a word
      between_word_indices <- which(data$Activity %in% c("Input", "Replace", "Paste") & 
                                      (data$WordCount > lag(data$WordCount, default = 0) | 
                                         str_detect(data$TextChange, "\\s")) &
                                      data$PauseTime >= 200)
    }, error = function(e) {
      warning(paste("Error calculating between-word pauses for", file_name, ":", e$message))
    })
    
    mean_between_word_pause <- ifelse(length(between_word_indices) > 0,
                                      mean(data$PauseTime[between_word_indices], na.rm = TRUE) / 1000,
                                      NA)
    
    # Calculate deletion metrics
    deletion_rows <- data %>% filter(Activity == "Remove/Cut")
    deletion_count <- nrow(deletion_rows)
    
    mean_deletion_length <- 0
    if(deletion_count > 0) {
      # Group consecutive deletions
      deletion_rows$deletion_group <- c(0, cumsum(diff(deletion_rows$DownEventID) > 1))
      deletion_groups <- deletion_rows %>% 
        group_by(deletion_group) %>% 
        summarize(chars_deleted = n(), .groups = "drop")
      
      mean_deletion_length <- mean(deletion_groups$chars_deleted, na.rm = TRUE)
    }
    
    # Total characters produced (counting all productive actions)
    total_chars <- sum(data$Activity %in% c("Input", "Replace", "Paste")) + sum(data$Activity == "Remove/Cut")
    
    # Proportion of deletions
    prop_deletions <- ifelse(total_chars > 0, sum(data$Activity == "Remove/Cut") / total_chars, 0)
    
    # Calculate insertion metrics (Input, Replace, and Paste are insertions)
    insertion_rows <- data %>% filter(Activity %in% c("Input", "Replace", "Paste"))
    insertion_count <- nrow(insertion_rows)
    
    mean_insertion_length <- 0
    if(insertion_count > 0) {
      # Group consecutive insertions
      insertion_rows$insertion_group <- c(0, cumsum(diff(insertion_rows$DownEventID) > 1))
      insertion_groups <- insertion_rows %>% 
        group_by(insertion_group) %>% 
        summarize(chars_inserted = n(), .groups = "drop")
      
      mean_insertion_length <- mean(insertion_groups$chars_inserted, na.rm = TRUE)
    }
    
    # Proportion of insertions
    prop_insertions <- ifelse(total_chars > 0, insertion_count / total_chars, 0)
    
    # Calculate paste operations specifically
    paste_rows <- data %>% filter(Activity == "Paste")
    paste_count <- nrow(paste_rows)
    
    # Mean paste length
    paste_length <- 0
    if(paste_count > 0) {
      paste_groups <- paste_rows %>% 
        group_by(DownEventID) %>% 
        summarize(chars_pasted = n(), .groups = "drop")
      paste_length <- mean(paste_groups$chars_pasted, na.rm = TRUE)
    }
    
    # Calculate interval variance
    # First, divide the writing process into 10 equal intervals
    max_time <- max(data$UpTime, na.rm = TRUE)
    min_time <- min(data$DownTime, na.rm = TRUE)
    total_time <- max_time - min_time
    
    interval_variance <- 0
    
    if(total_time > 0) {
      interval_size <- total_time / 10
      
      data$interval <- ceiling((data$DownTime - min_time) / interval_size)
      data$interval[data$interval == 0] <- 1  # Ensure no zero intervals
      data$interval[data$interval > 10] <- 10  # Cap at 10 in case of rounding errors
      
      # Calculate production rate for each interval
      interval_rates <- data %>%
        filter(Activity %in% c("Input", "Replace", "Paste")) %>%
        group_by(interval) %>%
        summarize(
          chars_produced = n(),
          .groups = "drop"
        ) %>%
        arrange(interval)
      
      # Fill in missing intervals with zero production
      all_intervals <- data.frame(interval = 1:10)
      interval_rates <- merge(all_intervals, interval_rates, by = "interval", all.x = TRUE)
      interval_rates$chars_produced[is.na(interval_rates$chars_produced)] <- 0
      
      # Calculate the standard deviation of production rates
      interval_variance <- sd(interval_rates$chars_produced, na.rm = TRUE)
    }
    
    # Return all calculated metrics as a dataframe row
    result <- data.frame(
      file_name = file_name,
      mean_p_burst_length = mean_p_burst_length,
      mean_r_burst_length = mean_r_burst_length,
      mean_pause_length = mean_pause_length,
      mean_within_word_pause = mean_within_word_pause,
      mean_between_word_pause = mean_between_word_pause,
      mean_deletion_length = mean_deletion_length,
      proportion_deletions = prop_deletions,
      mean_insertion_length = mean_insertion_length,
      proportion_insertions = prop_insertions,
      mean_paste_length = paste_length,
      paste_count = paste_count,
      interval_variance = interval_variance
    )
    
    return(result)
  }, error = function(e) {
    # Return a data frame with file name and NA values for all metrics
    warning(paste("Error processing file", file_path, ":", e$message))
    return(data.frame(
      file_name = basename(file_path),
      mean_p_burst_length = NA,
      mean_r_burst_length = NA,
      mean_pause_length = NA,
      mean_within_word_pause = NA,
      mean_between_word_pause = NA,
      mean_deletion_length = NA,
      proportion_deletions = NA,
      mean_insertion_length = NA,
      proportion_insertions = NA,
      mean_paste_length = NA,
      paste_count = NA,
      interval_variance = NA
    ))
  })
}

# Function to process all CSV files in a folder
process_keystroke_folder <- function(folder_path, output_file = "keystroke_analysis_results5.csv") {
  # Get list of all CSV files in the folder
  csv_files <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)
  
  # Process each file and collect results
  results_list <- list()
  errors_list <- character()
  
  for (file in csv_files) {
    # Print progress
    cat("Processing", basename(file), "\n")
    
    # Try to analyze the file
    tryCatch({
      result <- analyze_keystroke_file(file)
      results_list[[length(results_list) + 1]] <- result
    }, error = function(e) {
      # Record the error
      errors_list <<- c(errors_list, paste("Error with file", basename(file), ":", e$message))
    })
  }
  
  # Combine results if any were successful
  if (length(results_list) > 0) {
    results <- bind_rows(results_list)
    
    # Write results to CSV
    write.csv(results, output_file, row.names = FALSE)
    cat("Analysis complete. Results saved to", output_file, "\n")
    
    # Report any errors
    if (length(errors_list) > 0) {
      error_file <- paste0("keystroke_analysis_errors_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt")
      writeLines(errors_list, error_file)
      cat("Errors encountered with", length(errors_list), "files. Details saved to", error_file, "\n")
    }
    
    return(results)
  } else {
    stop("No files were successfully processed. Check error messages.")
  }
}

results <- process_keystroke_folder("~/Library/CloudStorage/Dropbox-UCEnterprise/Gan Qiao/Conferences/EMNLP/AES/here")

#combine social with cognitive####
library(dplyr)
csv1 <- read.csv("WC.csv")
csv2 <- read.csv("Linguistics_matched5.csv")

# Perform a left join by ID_org
combined_data <- left_join(csv1, csv2, by = "ID")

write.csv(combined_data, "WC1.csv", row.names = FALSE)



#Combine Claude with HUman and GPT4o####
#Read the original data
original_data <- read.csv("NewData12.csv")

# Read the Claude scores data
claude_data <- read.csv("Turbo_4o.csv")

# Create a dataframe with Claude scores in the same format as your original
claude_long <- claude_data %>%
  select(ID, Turbo) %>%  # Just using the Claude3.7 column
  rename(Score = Turbo) %>%
  mutate(ScoreType = "Turbo")

# Get the needed columns from original data for consistent merging
original_long <- original_data %>%
  select(ID, ScoreType, Score, everything())

# Combine the datasets
combined_long <- bind_rows(original_long, claude_long)

# For Claude rows, we need to copy over the demographic data
# This is a bit complex because we need to match on ID
claude_rows <- which(combined_long$ScoreType == "Turbo")

# For each Claude row, find the corresponding row in original data and copy over demographic info
for (i in claude_rows) {
  current_id <- combined_long$ID[i]
  matching_row <- which(original_data$ID == current_id)[1]  # Take first match if multiple
  
  if (length(matching_row) > 0) {
    # Copy all demographic columns
    demographic_cols <- setdiff(colnames(original_data), c("ScoreType", "Score"))
    combined_long[i, demographic_cols] <- original_data[matching_row, demographic_cols]
  }
}

# If you prefer wide format (one row per ID with all scores as columns):
wide_data <- combined_long %>%
  select(ID, ScoreType, Score) %>%  # Select just the scoring columns
  pivot_wider(
    id_cols = ID,
    names_from = ScoreType,
    values_from = Score
  )

# Now merge the wide scores with demographic data
demographic_data <- original_data %>%
  select(-ScoreType, -Score) %>%
  distinct(ID, .keep_all = TRUE)  # Keep one row per ID for demographics

final_wide_data <- wide_data %>%
  left_join(demographic_data, by = "ID")

# Save the outputs
write.csv(combined_long, "combined_scores_long.csv", row.names = FALSE)
write.csv(final_wide_data, "combined_scores_wide.csv", row.names = FALSE)

#newAnalysis####
# Read your results CSV file
keystroke_results <- read.csv("combined_data.csv")
summary(keystroke_results)
# Select only the numeric predictor columns you want to check for multicollinearity
# Exclude the file_name column and any other non-numeric or outcome variables
predictors <- keystroke_results[, c(
  "P_burst", 
  "R_burst", 
  "MeanPause",
  "WithinWordPause", 
  "BetweenWordPause", 
  "MeanDeletion",
  "ProportionDeletion",
  "MeanInsertion",
  "ProportionInsertion", 
  "IntervalVariance"
)]

# Calculate correlation matrix
cor_matrix <- cor(predictors, use = "complete.obs")

# Print correlation matrix
print(cor_matrix)

# Visualize with corrplot
library(corrplot)
corrplot(cor_matrix, method = "number", type = "upper", 
         number.cex = 0.7,  # Adjust the size of the numbers
         tl.cex = 0.7)      # Adjust the size of the labels

# Or create a heatmap with ggplot2
library(ggplot2)
library(reshape2)

# Melt the correlation matrix for ggplot
cor_melted <- melt(cor_matrix)

# Create the heatmap
ggplot(cor_melted, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        axis.text.y = element_text(size = 8)) +
  labs(title = "Correlation Matrix of Keystroke Measures",
       x = "", y = "") +
  geom_text(aes(label = sprintf("%.2f", value)), size = 2.5)
