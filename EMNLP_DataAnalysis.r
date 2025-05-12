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

#correlation####
# Load CSV file
data <- read_csv("AllData.csv")
summary(data)

# Shapiro-Wilk Test for normality
# For Human scores
shapiro.test(data$Score[data$ScoreType == "Human"])
# For GPT4o scores
shapiro.test(data$Score[data$ScoreType == "GPT4o"])
# For Claude3.7 scores
shapiro.test(data$Score[data$ScoreType == "Claude3.7"])

# kendall correlation
cor_test<-cor.test(data$Score[data$ScoreType == "Human"], data$Score[data$ScoreType == "Claude3.7"], method = "kendall")
cor_test

cor_test<-cor.test(data$Score[data$ScoreType == "Human"], data$Score[data$ScoreType == "GPT4o"], method = "kendall")
cor_test

#regression modelling####
data <- read.csv("AllData.csv",stringsAsFactors = T)
summary(data)

#relevel the reference levels
data$Gender <- factor(data$Gender, levels = c("Non-binary","Women","Men"))
#data$Ethnicity <- factor(data$Ethnicity, levels = c("Mixed","Asian","Latino","White","African", "Native"))
data$Ethnicity <- factor(data$Ethnicity, levels = c("Native","African","White","Latino","Asian", "Mixed"))
data$Education <- factor(data$Education, levels = c("LowHS", "HS","Bach","Mas","Prof","PhD"))
data$ScoreType <- factor(data$ScoreType, levels = c("Human", "GPT4o","Claude3.7"))

data_scaled <- data
numeric_vars <- c("Age", "P_burst", "R_burst", "MeanPause","WithinWordPause","BetweenWordPause","MeanDeletion","ProportionInsertion","IntervalVariance")
data_scaled[numeric_vars] <- scale(data[numeric_vars])
summary(data_scaled)

#maximal model
model_scaled1 <- lmer(Score ~ ScoreType*(Age + Gender + Ethnicity + Nativeness + Education + Topic + P_burst+R_burst+ MeanPause+
+WithinWordPause+ BetweenWordPause+MeanDeletion+ProportionInsertion+IntervalVariance) + (1|ID), 
                     data = data_scaled)
summary(model_scaled1)

#final model after prunning
model_scaled2 <- lmer(Score ~ ScoreType*(Age + Gender + Ethnicity+Education + Nativeness + Topic + P_burst+R_burst+ MeanPause+
                                           +ProportionInsertion+IntervalVariance) + (1|ID), 
                      data = data_scaled)
summary(model_scaled2)
tab_model(model_scaled2) #for Appendix D

#visualize the effect sizes of all the interactions####
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

#visualise all the interaction effects####
# Get interaction effect
interact_effect <- Effect(c("ScoreType", "Age"), model_scaled2)
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

PrimeType1 <- Effect(c("Gender", "ScoreType"), model_scaled2) %>% 
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

## join plots for making Figure 2
combined_plot <- age + PrimeType1+
  plot_layout(ncol = 2, guides = "collect") & 
  theme(legend.position = "bottom",
        legend.title = element_blank())  # This hides the legend title

combined_plot <- combined_plot + plot_annotation(
  title = "",
  theme = theme(plot.title = element_text(hjust = 0.5))
)
combined_plot


PrimeType2<- Effect(c("Nativeness","ScoreType"), model_scaled2) %>% as.data.frame %>% droplevels
PrimeType2<-ggplot(PrimeType2, aes(Nativeness, fit, group=ScoreType, color=ScoreType)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width=0,
                position = position_dodge(.2)) +
  geom_line(linetype = "dotted", position=position_dodge(.2)) +
  geom_point(size=2.5, position=position_dodge(.2)) +
  theme_bw()+
  labs(y="Predicted essay score", x="Nativeness") +
  scale_color_manual(values = c("Human" = "darkred","GPT4o" = "darkblue", "Claude3.7" = "darkgreen")) +
  guides(shape=guide_legend(ncol=1,title.position = "top"))+
  theme(axis.text.x = element_text(size=11), axis.title = element_text(size=13))+
  theme(legend.position = "right",legend.title = element_text(size=11), legend.text = element_text(size=10))
PrimeType2

PrimeType4 <- Effect(c("Topic", "ScoreType"), model_scaled2) %>% 
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

## join plots for making Figure 3
combined_plot <- PrimeType2 + PrimeType4+
  plot_layout(ncol = 2, guides = "collect") & 
  theme(legend.position = "bottom",
        legend.title = element_blank())  # This hides the legend title

combined_plot <- combined_plot + plot_annotation(
  title = "",
  theme = theme(plot.title = element_text(hjust = 0.5))
)
combined_plot

PrimeType3<- Effect(c("Ethnicity","ScoreType"), model_scaled2) %>% as.data.frame %>% droplevels
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

# Get interaction effects for keystroke indices
interact_effect <- Effect(c("ScoreType", "P_burst"), model_scaled2)
df_effect <- as.data.frame(interact_effect)
# Plot using ggplot2
p6<-ggplot(df_effect, aes(x = P_burst, y = fit, color = ScoreType)) +
  geom_line(linewidth = 0.5) +
  scale_color_manual(values = c("Human" = "darkred","GPT4o" = "darkblue", "Claude3.7" = "darkgreen")) +
  scale_fill_manual(values = c("Human" = "darkred","GPT4o" = "darkblue", "Claude3.7" = "darkgreen")) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = ScoreType), alpha = 0.2, color = NA) +
  labs(title = "",
       x = "mean_p_burst_length", y = "") +
  theme_bw()
p6

interact_effect <- Effect(c("ScoreType", "R_burst"), model_scaled2)
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
       x = "interval_variance", y = "") +
  theme_bw()
p10

## join plots for making Figure 6
combined_plot <- p6 + p7 + p8 + p9 + p10 +
  plot_layout(ncol = 2, guides = "collect") &
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        plot.margin = margin(0, 0, 0, 0))  # Reduce margins around plots

combined_plot <- combined_plot + plot_annotation(
  title = "",
  theme = theme(plot.title = element_text(hjust = 0.5),
                plot.margin = margin(0, 0, 0, 0))  # Reduce annotation margins
)

# If still see blank space, try:
combined_plot & theme(plot.background = element_blank(),
                      panel.spacing = unit(0.5, "lines"))  # Reduce spacing between panels
combined_plot

