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
library(emmeans)
library(interactions)
library(ranger)
library(caret)

#Nov2025####
#RQ2####
data <- read.csv("WC1.csv",stringsAsFactors = T)
summary(data)

#Complete code block to run:
# GROUP 1: Just z-score
data$Age <- as.vector(scale(data$Age))
data$ConnectiveVariety <- as.vector(scale(data$ConnectiveVariety))

# GROUP 2: Log + winsorize + z-score
# MeanPause
meanpause_log <- log(data$MeanPause)
lower_mp <- quantile(meanpause_log, 0.01, na.rm = TRUE)
upper_mp <- quantile(meanpause_log, 0.99, na.rm = TRUE)
data$MeanPause <- as.vector(scale(pmin(pmax(meanpause_log, lower_mp), upper_mp)))

# WithinWordPause
meanpause_log <- log(data$WithinWordPause)
lower_mp <- quantile(meanpause_log, 0.01, na.rm = TRUE)
upper_mp <- quantile(meanpause_log, 0.99, na.rm = TRUE)
data$WithinWordPause <- as.vector(scale(pmin(pmax(meanpause_log, lower_mp), upper_mp)))

# BetweenWordPause
meanpause_log <- log(data$BetweenWordPause)
lower_mp <- quantile(meanpause_log, 0.01, na.rm = TRUE)
upper_mp <- quantile(meanpause_log, 0.99, na.rm = TRUE)
data$BetweenWordPause <- as.vector(scale(pmin(pmax(meanpause_log, lower_mp), upper_mp)))

# P_burst
meanpause_log <- log(data$P_burst)
lower_mp <- quantile(meanpause_log, 0.01, na.rm = TRUE)
upper_mp <- quantile(meanpause_log, 0.99, na.rm = TRUE)
data$P_burst <- as.vector(scale(pmin(pmax(meanpause_log, lower_mp), upper_mp)))

# R_burst
meanpause_log <- log(data$R_burst)
lower_mp <- quantile(meanpause_log, 0.01, na.rm = TRUE)
upper_mp <- quantile(meanpause_log, 0.99, na.rm = TRUE)
data$R_burst <- as.vector(scale(pmin(pmax(meanpause_log, lower_mp), upper_mp)))

# MeanDeletion
meanpause_log <- log(data$MeanDeletion)
lower_mp <- quantile(meanpause_log, 0.01, na.rm = TRUE)
upper_mp <- quantile(meanpause_log, 0.99, na.rm = TRUE)
data$MeanDeletion <- as.vector(scale(pmin(pmax(meanpause_log, lower_mp), upper_mp)))

# MTLD
mtld_log <- log(data$MTLD)
lower_mtld <- quantile(mtld_log, 0.01, na.rm = TRUE)
upper_mtld <- quantile(mtld_log, 0.99, na.rm = TRUE)
data$MTLD <- as.vector(scale(pmin(pmax(mtld_log, lower_mtld), upper_mtld)))

# TUnitLength
tunit_log <- log(data$TUnitLength)
lower_tu <- quantile(tunit_log, 0.01, na.rm = TRUE)
upper_tu <- quantile(tunit_log, 0.99, na.rm = TRUE)
data$TUnitLength <- as.vector(scale(pmin(pmax(tunit_log, lower_tu), upper_tu)))

# ArgumentOverlap (add constant for zeros)
argov_log <- log(data$ArgumentOverlap + 0.001)
lower_ao <- quantile(argov_log, 0.01, na.rm = TRUE)
upper_ao <- quantile(argov_log, 0.99, na.rm = TRUE)
data$ArgumentOverlap <- as.vector(scale(pmin(pmax(argov_log, lower_ao), upper_ao)))

# GROUP 3: DependencyCosSim - just winsorize
lower_dep <- quantile(data$DependencyCosSim, 0.01, na.rm = TRUE)
upper_dep <- quantile(data$DependencyCosSim, 0.99, na.rm = TRUE)
data$DependencyCosSim <- as.vector(scale(pmin(pmax(data$DependencyCosSim, lower_dep), upper_dep)))

# SUMMARY CHECK
cat("=== FINAL SUMMARY OF ALL VARIABLES ===\n\n")
vars <- c("Age", "MeanPause", "MTLD", "ConnectiveVariety", "MeanDeletion","R_burst","P_burst","BetweenWordPause",
        "WithinWordPause","TUnitLength", "ArgumentOverlap", "DependencyCosSim")

for(v in vars) {
  cat(v, "range:", round(range(data[[v]]), 2), 
      "| % > |3| SD:", round(100 * sum(abs(data[[v]]) > 3) / length(data[[v]]), 2), "%\n")
}


# 1. Human scores
data_human <- data %>% 
  filter(ScoreType == "Human") %>%
  rename(Score_Human = Score)
summary(data_human)

set.seed(1234)
rf_human <- ranger(
  Score_Human ~ Age + Gender + Ethnicity + Education +
    P_burst + R_burst + MeanPause + WithinWordPause + BetweenWordPause + MeanDeletion+
    MTLD + TUnitLength + ConnectiveVariety + ArgumentOverlap +DependencyCosSim,
  data = data_human,
  importance = "impurity",  # gives Gini or variance importance
  num.trees = 1000
)

# 2. ChatGPT scores
data_gpt <- data %>% 
  filter(ScoreType == "GPT4o")%>%
  rename(Score_gpt = Score)
summary(data_gpt)

set.seed(1234)
rf_gpt <- ranger(
  Score_gpt ~ Age + Gender + Ethnicity + Education +
    P_burst + R_burst + MeanPause + WithinWordPause + BetweenWordPause + MeanDeletion+
    MTLD + TUnitLength + ConnectiveVariety + ArgumentOverlap +DependencyCosSim,
  data = data_gpt,
  importance = "impurity",
  num.trees = 1000
)

# 3. Claude scores
data_claude <- data %>% 
  filter(ScoreType == "Claude3.7")%>%
  rename(Score_claude = Score)
summary(data_claude)

set.seed(1234)
rf_claude <- ranger(
  Score_claude ~ Age + Gender + Ethnicity + Education +
    P_burst + R_burst + MeanPause + WithinWordPause + BetweenWordPause + MeanDeletion+
    MTLD + TUnitLength + ConnectiveVariety + ArgumentOverlap +DependencyCosSim,
  data = data_claude,
  importance = "impurity",
  num.trees = 1000
)

# Extract importance
importance_human  <- data.frame(Predictor = names(rf_human$variable.importance),
                                Importance = rf_human$variable.importance)

importance_gpt    <- data.frame(Predictor = names(rf_gpt$variable.importance),
                                Importance = rf_gpt$variable.importance)

importance_claude <- data.frame(Predictor = names(rf_claude$variable.importance),
                                Importance = rf_claude$variable.importance)

plot_rf <- function(df, title) {
  df %>%
    mutate(Predictor = reorder(Predictor, Importance)) %>%
    ggplot(aes(x = Predictor, y = Importance)) +
    geom_col(fill = "#4e79a7", width = 0.7) +
    coord_flip() +
    labs(title = title,
         x = NULL,  # Remove x-axis label since predictor names are self-explanatory
         y = "Variable Importance") +
    theme_minimal(base_size = 12) +
    theme(
      # Text formatting
      plot.title = element_text(face = "bold", size = 14, hjust = 0),
      axis.title = element_text(face = "bold", size = 11),
      axis.text = element_text(color = "black", size = 12),
      
      # Grid and panel
      panel.grid.major.y = element_blank(),  # Remove horizontal grid lines
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "grey90", linewidth = 0.3),
      
      # Plot margins
      plot.margin = margin(10, 15, 10, 10),
      
      # Remove background
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA)
    ) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05)))  # Start bars at 0
}

# Create plots
p_human  <- plot_rf(importance_human,  "Human")
p_gpt    <- plot_rf(importance_gpt,    "GPT-4o")
p_claude <- plot_rf(importance_claude, "Claude 3.7")

# Display
p_human
p_gpt
p_claude

# Save as high-quality PNGs
ggsave("figure_human_rf.png", plot = p_human, 
       width = 4.5, height = 5.5, dpi = 600, bg = "white")
ggsave("figure_gpt_rf.png", plot = p_gpt, 
       width = 4.5, height = 5.5, dpi = 600, bg = "white")
ggsave("figure_claude_rf.png", plot = p_claude, 
       width = 4.5, height = 5.5, dpi = 600, bg = "white")

#validation
# Set up cross-validation
train_control <- trainControl(
  method = "cv",           # k-fold cross-validation
  number = 10,             # 10 folds
  savePredictions = "final",
  verboseIter = FALSE
)

# Train Human model with CV
set.seed(1234)
cv_human <- train(
  Score_Human ~ Age + Gender + Ethnicity + Education +
    P_burst + R_burst + MeanPause + WithinWordPause + BetweenWordPause + MeanDeletion+
    MTLD + TUnitLength + ConnectiveVariety + ArgumentOverlap + DependencyCosSim,
  data = data_human,
  method = "ranger",
  trControl = train_control,
  tuneGrid = data.frame(
    mtry = sqrt(15),  # typical default for regression
    splitrule = "variance",
    min.node.size = 5
  ),
  num.trees = 1000,
  importance = "impurity"
)

# Train GPT model with CV
set.seed(1234)
cv_gpt <- train(
  Score_gpt ~ Age + Gender + Ethnicity + Education +
    P_burst + R_burst + MeanPause + WithinWordPause + BetweenWordPause + MeanDeletion+
    MTLD + TUnitLength + ConnectiveVariety + ArgumentOverlap + DependencyCosSim,
  data = data_gpt,
  method = "ranger",
  trControl = train_control,
  tuneGrid = data.frame(
    mtry = sqrt(15),
    splitrule = "variance",
    min.node.size = 5
  ),
  num.trees = 1000,
  importance = "impurity"
)

# Train Claude model with CV
set.seed(1234)
cv_claude <- train(
  Score_claude ~ Age + Gender + Ethnicity + Education +
    P_burst + R_burst + MeanPause + WithinWordPause + BetweenWordPause + MeanDeletion+
    MTLD + TUnitLength + ConnectiveVariety + ArgumentOverlap + DependencyCosSim,
  data = data_claude,
  method = "ranger",
  trControl = train_control,
  tuneGrid = data.frame(
    mtry = sqrt(15),
    splitrule = "variance",
    min.node.size = 5
  ),
  num.trees = 1000,
  importance = "impurity"
)

# Extract performance metrics
results_human <- cv_human$results
results_gpt <- cv_gpt$results
results_claude <- cv_claude$results

# Create comparison table
comparison <- data.frame(
  Model = c("Human", "GPT-4o", "Claude 3.7"),
  R_squared = c(results_human$Rsquared, results_gpt$Rsquared, results_claude$Rsquared),
  RMSE = c(results_human$RMSE, results_gpt$RMSE, results_claude$RMSE),
  MAE = c(results_human$MAE, results_gpt$MAE, results_claude$MAE)
)
print(comparison)

# If using cross-validation with caret, you can compare models
resamps <- resamples(list(Human = cv_human, GPT = cv_gpt, Claude = cv_claude))
summary(resamps)

# Statistical tests
diff_test <- diff(resamps)
summary(diff_test)

# Out-of-bag R² (default in ranger for regression)
rf_human$r.squared
rf_gpt$r.squared
rf_claude$r.squared

human_pred <- rf_human$predictions
rmse_human <- sqrt(mean((data$Score - human_pred)^2))
rmse_human

#aggregate feature importance by category (social vs keystroke vs linguistic) and show mean importance ± SD.
#This makes it even clearer which domain matters most.
# Create a data frame mapping predictors to categories
feature_categories <- data.frame(
  Predictor = c("Age","Gender","Ethnicity","Education",
                "P_burst","R_burst","MeanPause","WithinWordPause","BetweenWordPause","MeanDeletion",
                "MTLD","TUnitLength","ConnectiveVariety","ArgumentOverlap","DependencyCosSim"),
  Category  = c(rep("Social",4),
                rep("Keystroke",6),
                rep("Linguistic",5))
)

# Human RF
importance_human_df <- data.frame(
  Predictor = names(rf_human$variable.importance),
  Importance = rf_human$variable.importance
)
importance_human_df <- merge(importance_human_df, feature_categories, by="Predictor")

# GPT RF
importance_gpt_df <- data.frame(
  Predictor = names(rf_gpt$variable.importance),
  Importance = rf_gpt$variable.importance
)
importance_gpt_df <- merge(importance_gpt_df, feature_categories, by="Predictor")

# Claude RF
importance_claude_df <- data.frame(
  Predictor = names(rf_claude$variable.importance),
  Importance = rf_claude$variable.importance
)
importance_claude_df <- merge(importance_claude_df, feature_categories, by="Predictor")


aggregate_importance <- function(df, model_name){
  df %>%
    group_by(Category) %>%
    summarise(
      MeanImportance = mean(Importance),
      SDImportance   = sd(Importance)
    ) %>%
    mutate(Model = model_name)
}

agg_human  <- aggregate_importance(importance_human_df,  "Human_RF")
agg_gpt    <- aggregate_importance(importance_gpt_df,    "GPT_RF")
agg_claude <- aggregate_importance(importance_claude_df, "Claude_RF")

# Combine all models
agg_all <- bind_rows(agg_human, agg_gpt, agg_claude)
agg_all

#visulisation
library(RColorBrewer)

# Set Category order
agg_all$Category <- factor(agg_all$Category, levels = c("Linguistic", "Keystroke", "Social"))

# Choose a color palette (color-blind friendly)
colors <- brewer.pal(n = 3, name = "Set2")

# Create the plot
p <- ggplot(agg_all, aes(x=Category, y=MeanImportance, fill=Model)) +
  geom_bar(stat="identity", position=position_dodge(width=0.8), width=0.7) +
  geom_errorbar(aes(ymin=MeanImportance-SDImportance, ymax=MeanImportance+SDImportance),
                width=0.2, position=position_dodge(width=0.8)) +
  scale_fill_manual(values = colors) +
  labs(y="Mean Variable Importance ± SD",
       x="Feature Domain",
       fill="Model") +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    axis.title = element_text(face="bold"),
    axis.text = element_text(color="black"),
    panel.grid.major = element_line(color="grey90"),
    panel.grid.minor = element_blank()
  )
p

ggsave("RF_feature_importance.png", plot = p, width = 6, height = 4, dpi = 600)


#RQ3####
data <- read.csv("WC1.csv",stringsAsFactors = T)
summary(data)

data$ScoreType <- factor(data$ScoreType, levels = c("Human", "GPT4o","Claude3.7"))

#sum coding
data$Gender <- factor(data$Gender, levels = c("Women","Men"))
contrasts(data$Gender) <- contr.sum(2)

data$Ethnicity <- factor(data$Ethnicity, levels = c("African","Asian","White","Latino"))
contrasts(data$Ethnicity) <- contr.sum(4)

data$Education <- factor(data$Education, levels = c("HSBelow","Bachelor","Graduate"))
contrasts(data$Education) <- contr.sum(3)
#Information Criterion-Based Selection (More Rigorous)
# Automated backward elimination based on AIC
model_best <- buildmer(Score ~ ScoreType * (Age + Gender + Ethnicity + Education +
                                              P_burst + R_burst + MeanPause + WithinWordPause + 
                                              BetweenWordPause + MeanDeletion + MTLD + TUnitLength + 
                                              ConnectiveVariety + ArgumentOverlap + DependencyCosSim) + 
                         (1 | ID2) + (1 | Topic),
                       data = data,
                       buildmerControl = buildmerControl(
                         direction = "backward",
                         crit = "BIC"  # or "BIC" for more conservative selection
                       ))

# View results
summary(model_best)

# Get the final model
final_model <- model_best@model
summary(final_model)

final_model <- lmer(Score ~ Education + P_burst + BetweenWordPause +       
                      ScoreType*(Age + Gender + MeanPause +MeanDeletion + MTLD + ConnectiveVariety) +      
                      (1 | ID2), data = data, REML = TRUE)
summary(final_model)

# Extract fixed effects into a tidy dataframe
model_results <- tidy(final_model, effects = "fixed", conf.int = TRUE)

# Save to CSV
write.csv(model_results, "model_fixed_effects.csv", row.names = FALSE)

# Multicollinearity (VIF)
vif(final_model)  # Values > 10 are concerning, > 5 warrant attention
#performance
model_performance(final_model)

#post-hoc analysis
library(emmeans)
library(multcomp)

# Get estimated marginal means with confidence intervals
emm_education <- emmeans(final_model, ~ Education)
summary(emm_education)

# Save for reporting
emm_summary <- as.data.frame(emm_education)
write.csv(emm_summary, "education_emmeans.csv", row.names = FALSE)

# Get pairwise comparisons
pairs_education <- pairs(emm_education, adjust = "tukey")
summary(pairs_education)

# Save for reporting
pairs_summary <- as.data.frame(pairs_education)
write.csv(pairs_summary, "education_contrasts.csv", row.names = FALSE)

# Get simple slopes for Age by ScoreType
emtrends_age <- emtrends(final_model, ~ ScoreType, var = "Age")
# Test pairwise differences between slopes
pairs(emtrends_age)
test(emtrends_age)

# Repeat for other continuous predictors
emtrends_meanpause <- emtrends(final_model, ~ ScoreType, var = "MeanPause")
test(emtrends_meanpause)
pairs(emtrends_meanpause)

emtrends_MeanDeletion <- emtrends(final_model, ~ ScoreType, var = "MeanDeletion")
test(emtrends_MeanDeletion)
pairs(emtrends_MeanDeletion)

emtrends_MTLD <- emtrends(final_model, ~ ScoreType, var = "MTLD")
test(emtrends_MTLD)
pairs(emtrends_MTLD)

emtrends_ConnectiveVariety <- emtrends(final_model, ~ ScoreType, var = "ConnectiveVariety")
test(emtrends_ConnectiveVariety)
pairs(emtrends_ConnectiveVariety)

# Get estimated marginal means for Gender × ScoreType interaction
emm_gender <- emmeans(final_model, ~ Gender * ScoreType)
summary(emm_gender)

# Test the interaction: compare genders within each rater type
pairs(emm_gender, by = "ScoreType")

# Compare men vs women within each rater type
gender_by_rater <- pairs(emm_gender, by = "ScoreType")
summary(gender_by_rater)
summary(emm_gender)

#visualisations for the paper####
#ScoreType * Gender
# Estimated marginal means for each combination
emm_gender <- emmeans(final_model, ~ ScoreType | Gender)
# Convert to data frame for plotting
emm_gender_df <- as.data.frame(emm_gender)
# Professional, publication-ready plot
p_gender <- ggplot(emm_gender_df, aes(x = Gender, y = emmean, fill = ScoreType)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(aes(ymin = emmean - SE*1.96, ymax = emmean + SE*1.96),
                width = 0.2, position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = c("#1b9e77", "#d95f02", "#7570b3")) +  # distinct colors for ScoreTypes
  labs(
    x = "Gender",
    y = "Predicted Score",
    fill = "Rater Type",
    title = ""
    #title = "Predicted Essay Scores by Gender and Rater Type"
  ) +
  theme_minimal(base_size = 12) + scale_y_continuous(limits = c(0, 4), breaks = seq(0, 4, 1)) +
  theme(
    plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10, color = "black"),
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "gray80", fill = NA, linewidth = 0.5)
  )
p_gender
# Save high-quality PNG
ggsave("PredictedScore_Gender.png", plot = p_gender, width = 4, height = 3, dpi = 600)


# Estimated marginal means at a sequence of Age values
emm_age <- emmeans(final_model, ~ ScoreType | Age, 
                   at = list(Age = seq(min(data$Age), max(data$Age), length.out = 20)),
                   infer = c(TRUE, TRUE))  # request SE and confidence intervals
# Convert to data frame
emm_age_df <- as.data.frame(emm_age)
# Compute 95% CI manually if not already provided
emm_age_df$lower.CL <- emm_age_df$emmean - 1.96 * emm_age_df$SE
emm_age_df$upper.CL <- emm_age_df$emmean + 1.96 * emm_age_df$SE
# Professional line plot
p_age <- ggplot(emm_age_df, aes(x = Age, y = emmean, color = ScoreType, fill = ScoreType)) +
  geom_line(linewidth = 1.2) +
  geom_ribbon(aes(ymin = lower.CL, ymax = upper.CL), alpha = 0.2, color = NA) +
  scale_color_manual(values = c("#1b9e77", "#d95f02", "#7570b3")) +
  scale_fill_manual(values = c("#1b9e77", "#d95f02", "#7570b3")) +
  labs(
    x = "Age",
    y = "Predicted Score",
    color = "Rater Type",
    fill = "Rater Type",
    title = ""
  ) +
  theme_minimal(base_size = 12) + #scale_y_continuous(limits = c(3, 4.2)) +
  theme(
    plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10, color = "black"),
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "gray80", fill = NA, linewidth = 0.5)
  )
p_age
# Save high-quality PNG
ggsave("PredictedScore_Age.png", plot = p_age, width = 4, height = 3, dpi = 600)

# Estimated marginal means at a sequence of MeanPause values
emm_age <- emmeans(final_model, ~ ScoreType | MeanPause, 
                   at = list(MeanPause = seq(min(data$MeanPause), max(data$MeanPause), length.out = 20)),
                   infer = c(TRUE, TRUE))  # request SE and confidence intervals
# Convert to data frame
emm_age_df <- as.data.frame(emm_age)
# Compute 95% CI manually if not already provided
emm_age_df$lower.CL <- emm_age_df$emmean - 1.96 * emm_age_df$SE
emm_age_df$upper.CL <- emm_age_df$emmean + 1.96 * emm_age_df$SE
# Professional line plot
p_age <- ggplot(emm_age_df, aes(x = MeanPause, y = emmean, color = ScoreType, fill = ScoreType)) +
  geom_line(linewidth = 1.2) +
  geom_ribbon(aes(ymin = lower.CL, ymax = upper.CL), alpha = 0.2, color = NA) +
  scale_color_manual(values = c("#1b9e77", "#d95f02", "#7570b3")) +
  scale_fill_manual(values = c("#1b9e77", "#d95f02", "#7570b3")) +
  labs(
    x = "MeanPause",
    y = "Predicted Score",
    color = "Rater Type",
    fill = "Rater Type",
    title = ""
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10, color = "black"),
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "gray80", fill = NA, linewidth = 0.5)
  )
p_age
ggsave("PredictedScore_MeanPause.png", plot = p_age, width = 4, height = 3, dpi = 600)

#WithinWordPause_z + MTLD_z + ConnectiveVariety_z

# Estimated marginal means at a sequence of MeanDeletion values
emm_age <- emmeans(final_model, ~ ScoreType | MeanDeletion, 
                   at = list(MeanDeletion = seq(min(data$MeanDeletion), max(data$MeanDeletion), length.out = 20)),
                   infer = c(TRUE, TRUE))  # request SE and confidence intervals
# Convert to data frame
emm_age_df <- as.data.frame(emm_age)
# Compute 95% CI manually if not already provided
emm_age_df$lower.CL <- emm_age_df$emmean - 1.96 * emm_age_df$SE
emm_age_df$upper.CL <- emm_age_df$emmean + 1.96 * emm_age_df$SE
# Professional line plot
p_age <- ggplot(emm_age_df, aes(x = MeanDeletion, y = emmean, color = ScoreType, fill = ScoreType)) +
  geom_line(linewidth = 1.2) +
  geom_ribbon(aes(ymin = lower.CL, ymax = upper.CL), alpha = 0.2, color = NA) +
  scale_color_manual(values = c("#1b9e77", "#d95f02", "#7570b3")) +
  scale_fill_manual(values = c("#1b9e77", "#d95f02", "#7570b3")) +
  labs(
    x = "MeanDeletion",
    y = "Predicted Score",
    color = "Rater Type",
    fill = "Rater Type",
    title = ""
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10, color = "black"),
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "gray80", fill = NA, linewidth = 0.5)
  )
p_age
ggsave("PredictedScore_MeanDeletion.png", plot = p_age, width = 4, height = 3, dpi = 600)

emm_age <- emmeans(final_model, ~ ScoreType | MTLD, 
                   at = list(MTLD = seq(min(data$MTLD), max(data$MTLD), length.out = 20)),
                   infer = c(TRUE, TRUE))  # request SE and confidence intervals
# Convert to data frame
emm_age_df <- as.data.frame(emm_age)
# Compute 95% CI manually if not already provided
emm_age_df$lower.CL <- emm_age_df$emmean - 1.96 * emm_age_df$SE
emm_age_df$upper.CL <- emm_age_df$emmean + 1.96 * emm_age_df$SE
# Professional line plot
p_age <- ggplot(emm_age_df, aes(x = MTLD, y = emmean, color = ScoreType, fill = ScoreType)) +
  geom_line(linewidth = 1.2) +
  geom_ribbon(aes(ymin = lower.CL, ymax = upper.CL), alpha = 0.2, color = NA) +
  scale_color_manual(values = c("#1b9e77", "#d95f02", "#7570b3")) +
  scale_fill_manual(values = c("#1b9e77", "#d95f02", "#7570b3")) +
  labs(
    x = "MTLD",
    y = "Predicted Score",
    color = "Rater Type",
    fill = "Rater Type",
    title = ""
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10, color = "black"),
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "gray80", fill = NA, linewidth = 0.5)
  )
p_age
ggsave("PredictedScore_MTLD.png", plot = p_age, width = 4, height = 3, dpi = 600)

emm_age <- emmeans(final_model, ~ ScoreType | ConnectiveVariety, 
                   at = list(ConnectiveVariety = seq(min(data$ConnectiveVariety), max(data$ConnectiveVariety), length.out = 20)),
                   infer = c(TRUE, TRUE))  # request SE and confidence intervals
# Convert to data frame
emm_age_df <- as.data.frame(emm_age)
# Compute 95% CI manually if not already provided
emm_age_df$lower.CL <- emm_age_df$emmean - 1.96 * emm_age_df$SE
emm_age_df$upper.CL <- emm_age_df$emmean + 1.96 * emm_age_df$SE
# Professional line plot
p_age <- ggplot(emm_age_df, aes(x = ConnectiveVariety, y = emmean, color = ScoreType, fill = ScoreType)) +
  geom_line(linewidth = 1.2) +
  geom_ribbon(aes(ymin = lower.CL, ymax = upper.CL), alpha = 0.2, color = NA) +
  scale_color_manual(values = c("#1b9e77", "#d95f02", "#7570b3")) +
  scale_fill_manual(values = c("#1b9e77", "#d95f02", "#7570b3")) +
  labs(
    x = "ConnectiveVariety",
    y = "Predicted Score",
    color = "Rater Type",
    fill = "Rater Type",
    title = ""
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10, color = "black"),
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "gray80", fill = NA, linewidth = 0.5)
  )
p_age
ggsave("PredictedScore_ConnectiveVariety.png", plot = p_age, width = 4, height = 3, dpi = 600)


#extra notes for checking the data distribution before modelling####
# Check what you actually have:
summary(data$R_burst)  # Raw variable
summary(scale(data$R_burst))  # Z-scored version

# Find the extreme values
data[data$R_burst > 5, ]  # Check observations with R_burst > 5 SD

# How many extreme outliers?
sum(data$R_burst > 3)
sum(data$R_burst > 5)

#Log transform (all values are positive, so no need to shift)####
data$R_burst_log <- log(data$R_burst)
data$R_burst_z_log <- as.vector(scale(data$R_burst_log))

# Check the distribution
hist(data$R_burst, breaks = 50, main = "Original R_burst")
hist(data$R_burst_log, breaks = 50, main = "Log-transformed R_burst")

# Check the z-scored version
summary(data$R_burst_z_log)
range(data$R_burst_z_log)
sum(data$R_burst_z_log > 3)
sum(data$R_burst_z_log > 4)

# Check normality
qqnorm(data$R_burst_z_log)
qqline(data$R_burst_z_log)

#P_burst
data$P_burst_log <- log(data$P_burst)
data$P_burst_z_log <- as.vector(scale(data$P_burst_log))
# Check the z-scored version
summary(data$P_burst_z_log)
range(data$P_burst_z_log)

# How many extreme values?
sum(data$P_burst_z_log > 3)
sum(data$P_burst_z_log > 4)
sum(data$P_burst_z_log < -3)

# What percentage is that?
cat("% > 3 SD:", round(100 * sum(abs(data$P_burst_z_log) > 3) / length(data$P_burst_z_log), 2), "%\n")

summary(data)

#meandeletion
data$MeanDeletion_log <- log(data$MeanDeletion)
data$MeanDeletion_z_log <- as.vector(scale(data$MeanDeletion_log))
# Check the z-scored version
summary(data$MeanDeletion_z_log)
range(data$MeanDeletion_z_log)

# How many extreme values?
sum(data$MeanDeletion_z_log > 3)
sum(data$MeanDeletion_z_log > 4)
sum(data$MeanDeletion_z_log < -3)

# What percentage is that?
cat("% > 3 SD:", round(100 * sum(abs(data$MeanDeletion_z_log) > 3) / length(data$MeanDeletion_z_log), 2), "%\n")

# Winsorize at 99th percentile on the LOG scale, then z-score
mean_deletion_log <- log(data$MeanDeletion)  # or however you created it

lower_bound <- quantile(mean_deletion_log, 0.01, na.rm = TRUE)
upper_bound <- quantile(mean_deletion_log, 0.99, na.rm = TRUE)

data$MeanDeletion_log_win <- pmin(pmax(mean_deletion_log, lower_bound), upper_bound)
data$MeanDeletion_z_final <- as.vector(scale(data$MeanDeletion_log_win))

# Check improvement
cat("\n=== After log + winsorize ===\n")
summary(data$MeanDeletion_z_final)
range(data$MeanDeletion_z_final)
cat("% > 3 SD:", round(100 * sum(abs(data$MeanDeletion_z_final) > 3) / length(data$MeanDeletion_z_final), 2), "%\n")
cat("# > 4 SD:", sum(abs(data$MeanDeletion_z_final) > 4), "\n")

# Apply the same treatment to R_burst and P_burst for consistency
# R_burst
r_burst_log <- log(data$R_burst)
lower_r <- quantile(r_burst_log, 0.01, na.rm = TRUE)
upper_r <- quantile(r_burst_log, 0.99, na.rm = TRUE)
data$R_burst_log_win <- pmin(pmax(r_burst_log, lower_r), upper_r)
data$R_burst_z_final <- as.vector(scale(data$R_burst_log_win))

# P_burst  
p_burst_log <- log(data$P_burst)
lower_p <- quantile(p_burst_log, 0.01, na.rm = TRUE)
upper_p <- quantile(p_burst_log, 0.99, na.rm = TRUE)
data$P_burst_log_win <- pmin(pmax(p_burst_log, lower_p), upper_p)
data$P_burst_z_final <- as.vector(scale(data$P_burst_log_win))

# Check all three
cat("R_burst_z_final range:", range(data$R_burst_z_final), "\n")
cat("P_burst_z_final range:", range(data$P_burst_z_final), "\n")
cat("MeanDeletion_z_final range:", range(data$MeanDeletion_z_final), "\n")

# Check both variables
cat("=== WithinWordPause ===\n")
summary(data$WithinWordPause)
hist(data$WithinWordPause, breaks = 50, main = "WithinWordPause")

cat("\n=== BetweenWordPause ===\n")
summary(data$BetweenWordPause)
hist(data$BetweenWordPause, breaks = 50, main = "BetweenWordPause")

# Check skewness
cat("\nWithinWordPause - distance from median to max:", 
    max(data$WithinWordPause) - median(data$WithinWordPause), "\n")
cat("BetweenWordPause - distance from median to max:", 
    max(data$BetweenWordPause) - median(data$BetweenWordPause), "\n")

# Check for extreme values
quantile(data$WithinWordPause, probs = c(0.01, 0.25, 0.5, 0.75, 0.95, 0.99))
quantile(data$BetweenWordPause, probs = c(0.01, 0.25, 0.5, 0.75, 0.95, 0.99))

# WithinWordPause
within_log <- log(data$WithinWordPause)
lower_w <- quantile(within_log, 0.01, na.rm = TRUE)
upper_w <- quantile(within_log, 0.99, na.rm = TRUE)
data$WithinWordPause_log_win <- pmin(pmax(within_log, lower_w), upper_w)
data$WithinWordPause_z <- as.vector(scale(data$WithinWordPause_log_win))

# BetweenWordPause
between_log <- log(data$BetweenWordPause)
lower_b <- quantile(between_log, 0.01, na.rm = TRUE)
upper_b <- quantile(between_log, 0.99, na.rm = TRUE)
data$BetweenWordPause_log_win <- pmin(pmax(between_log, lower_b), upper_b)
data$BetweenWordPause_z <- as.vector(scale(data$BetweenWordPause_log_win))

# Check results
cat("=== WithinWordPause_z ===\n")
summary(data$WithinWordPause_z)
cat("Range:", range(data$WithinWordPause_z), "\n")
cat("% > |3| SD:", round(100 * sum(abs(data$WithinWordPause_z) > 3) / length(data$WithinWordPause_z), 2), "%\n\n")

cat("=== BetweenWordPause_z ===\n")
summary(data$BetweenWordPause_z)
cat("Range:", range(data$BetweenWordPause_z), "\n")
cat("% > |3| SD:", round(100 * sum(abs(data$BetweenWordPause_z) > 3) / length(data$BetweenWordPause_z), 2), "%\n")

# Function to quickly assess each variable
check_variable <- function(var, var_name, data) {
  cat("\n========================================\n")
  cat("===", var_name, "===\n")
  cat("========================================\n")
  
  print(summary(data[[var]]))
  
  cat("\nRange:", min(data[[var]], na.rm=T), "to", max(data[[var]], na.rm=T), "\n")
  cat("Median:", median(data[[var]], na.rm=T), "\n")
  cat("Distance from median to max:", max(data[[var]], na.rm=T) - median(data[[var]], na.rm=T), "\n")
  
  # Skewness indicator
  cat("Mean - Median:", mean(data[[var]], na.rm=T) - median(data[[var]], na.rm=T), 
      ifelse(mean(data[[var]], na.rm=T) - median(data[[var]], na.rm=T) > 0, 
             "(RIGHT skewed)", "(LEFT skewed)"), "\n")
  
  # Percentiles
  cat("\nKey percentiles:\n")
  print(quantile(data[[var]], probs = c(0.01, 0.25, 0.5, 0.75, 0.95, 0.99), na.rm=T))
  
  # Test z-score without transformation
  var_z <- scale(data[[var]])
  cat("\nIf just z-scored (no transformation):\n")
  cat("  Range:", round(range(var_z, na.rm=T), 2), "\n")
  cat("  % > |3| SD:", round(100 * sum(abs(var_z) > 3, na.rm=T) / length(var_z), 2), "%\n")
  cat("  # > |4| SD:", sum(abs(var_z) > 4, na.rm=T), "\n")
}

# Check all variables
check_variable("Age", "Age", data)
check_variable("MeanPause", "MeanPause", data)
check_variable("MTLD", "MTLD", data)
check_variable("ConnectiveVariety", "ConnectiveVariety", data)
check_variable("TUnitLength", "TUnitLength", data)
check_variable("ArgumentOverlap", "ArgumentOverlap", data)
check_variable("DependencyCosSim", "DependencyCosSim", data)

#group 1
# These are already well-behaved
data$Age_z <- as.vector(scale(data$Age))
data$ConnectiveVariety_z <- as.vector(scale(data$ConnectiveVariety))

# Verify
cat("Age_z range:", range(data$Age_z), "\n")
cat("ConnectiveVariety_z range:", range(data$ConnectiveVariety_z), "\n")

#group 2: Log + winsorize + z-score (same as timing variables)
# MeanPause - clearly needs transformation (max 17.19 SD!)
meanpause_log <- log(data$MeanPause)
lower_mp <- quantile(meanpause_log, 0.01, na.rm = TRUE)
upper_mp <- quantile(meanpause_log, 0.99, na.rm = TRUE)
data$MeanPause_log_win <- pmin(pmax(meanpause_log, lower_mp), upper_mp)
data$MeanPause_z <- as.vector(scale(data$MeanPause_log_win))

# MTLD - borderline but for consistency, transform it
mtld_log <- log(data$MTLD)
lower_mtld <- quantile(mtld_log, 0.01, na.rm = TRUE)
upper_mtld <- quantile(mtld_log, 0.99, na.rm = TRUE)
data$MTLD_log_win <- pmin(pmax(mtld_log, lower_mtld), upper_mtld)
data$MTLD_z <- as.vector(scale(data$MTLD_log_win))

# TUnitLength - clearly needs transformation (max 11.34 SD!)
tunit_log <- log(data$TUnitLength)
lower_tu <- quantile(tunit_log, 0.01, na.rm = TRUE)
upper_tu <- quantile(tunit_log, 0.99, na.rm = TRUE)
data$TUnitLength_log_win <- pmin(pmax(tunit_log, lower_tu), upper_tu)
data$TUnitLength_z <- as.vector(scale(data$TUnitLength_log_win))

# ArgumentOverlap - needs transformation (max 8.37 SD!)
# Note: has zeros, so need to handle carefully
argov_log <- log(data$ArgumentOverlap + 0.001)  # Add small constant to handle zeros
lower_ao <- quantile(argov_log, 0.01, na.rm = TRUE)
upper_ao <- quantile(argov_log, 0.99, na.rm = TRUE)
data$ArgumentOverlap_log_win <- pmin(pmax(argov_log, lower_ao), upper_ao)
data$ArgumentOverlap_z <- as.vector(scale(data$ArgumentOverlap_log_win))

# Check results
cat("\n=== MeanPause_z ===\n")
cat("Range:", range(data$MeanPause_z), "\n")
cat("% > |3| SD:", round(100 * sum(abs(data$MeanPause_z) > 3) / length(data$MeanPause_z), 2), "%\n")

cat("\n=== MTLD_z ===\n")
cat("Range:", range(data$MTLD_z), "\n")
cat("% > |3| SD:", round(100 * sum(abs(data$MTLD_z) > 3) / length(data$MTLD_z), 2), "%\n")

cat("\n=== TUnitLength_z ===\n")
cat("Range:", range(data$TUnitLength_z), "\n")
cat("% > |3| SD:", round(100 * sum(abs(data$TUnitLength_z) > 3) / length(data$TUnitLength_z), 2), "%\n")

cat("\n=== ArgumentOverlap_z ===\n")
cat("Range:", range(data$ArgumentOverlap_z), "\n")
cat("% > |3| SD:", round(100 * sum(abs(data$ArgumentOverlap_z) > 3) / length(data$ArgumentOverlap_z), 2), "%\n")

#group3: Group 3: DependencyCosSim - Special case: the left tail (min goes to -8.24 SD). This is because it has zeros (likely representing no dependency or missing data)
# Option A: If zeros are meaningful (no dependency), just winsorize without log
dep_wins <- data$DependencyCosSim
lower_dep <- quantile(dep_wins, 0.01, na.rm = TRUE)
upper_dep <- quantile(dep_wins, 0.99, na.rm = TRUE)
data$DependencyCosSim_win <- pmin(pmax(dep_wins, lower_dep), upper_dep)
data$DependencyCosSim_z <- as.vector(scale(data$DependencyCosSim_win))

# Check
cat("\n=== DependencyCosSim_z ===\n")
cat("Range:", range(data$DependencyCosSim_z), "\n")
cat("% > |3| SD:", round(100 * sum(abs(data$DependencyCosSim_z) > 3) / length(data$DependencyCosSim_z), 2), "%\n")

