

# load libraries
library(tidyverse)
library(labelled)
library(reshape2)
library(coin)
library(rstatix)
library(gmodels)
library(moments)

#  Load data
df <- read_csv("./data/data_numeric.csv")

df <- df %>% 
  rename(test = Q11,
         n_40hz_dizziness = n_40hz_rating_1,
         n_40hz_headache = n_40hz_rating_2,
         n_40hz_nausea = n_40hz_rating_3,
         n_40hz_relaxed = n_40hz_rating_4,
         n_40hz_mood = n_40hz_rating_5,
         n_40hz_focus = n_40hz_rating_6,
         nil_40hz_dizziness = no_info_40hz_rating_1,
         nil_40hz_headache = no_info_40hz_rating_2,
         nil_40hz_nausea = no_info_40hz_rating_3,
         nil_40hz_relaxed = no_info_40hz_rating_4,
         nil_40hz_mood = no_info_40hz_rating_5,
         nil_40hz_focus = no_info_40hz_rating_6,
         p_40hz_dizziness = ps_40hz_rating_1,
         p_40hz_headache = ps_40hz_rating_2,
         p_40hz_nausea = ps_40hz_rating_3,
         p_40hz_relaxed = ps_40hz_rating_4,
         p_40hz_mood = ps_40hz_rating_5,
         p_40hz_focus = ps_40hz_rating_6,
         pb_40hz_dizziness = pb_40Hz_ratings_1,
         pb_40hz_headache = pb_40Hz_ratings_2,
         pb_40hz_nausea = pb_40Hz_ratings_3,
         pb_40hz_relaxed = pb_40Hz_ratings_4,
         pb_40hz_mood = pb_40Hz_ratings_5,
         pb_40hz_focus = pb_40Hz_ratings_6,
         dizziness_brown = brown_ratings_1,
         headache_brown = brown_ratings_2,
         nausea_brown = brown_ratings_3,
         relaxed_brown = brown_ratings_4,
         mood_brown = brown_ratings_5,
         focus_brown = brown_ratings_6,
         nocebo_40hz = `Q60_Page Submit`,
         nocebo_brown = `Q40_Page Submit`,
         nil_40hz = `Q80_Page Submit`, 
         nil_brown = `Q84_Page Submit`, 
         placebo_40hz = `Q47_Page Submit`,
         placebo_brown = `Q63_Page Submit`,
         blocker_40hz = `Q136_Page Submit`,  
         blocker_brown = `Q142_Page Submit`,
         expectancy_prior = prior_expectancy,
         expectancy_40hz = `40hz_expectancy`,
         expectancy_brown = brown_expectancy,
         expectancy_post = post_expectancy,
         orange = Q28)

# Convert from character to numeric
df <- df %>% 
  mutate(across(test:age, as.numeric))

glimpse(df)

# filter for exclusion criteria
df <- df[-65, ] %>% 
  filter(test == 4,
         age != "NA",
         gender != "NA",
         SC0 == 1,
         nocebo_40hz >= 120 | nil_40hz >= 120 | placebo_40hz >= 120 | blocker_40hz >= 120 | nocebo_brown >= 120 | nil_brown >= 120 | placebo_brown >= 120 | blocker_brown >= 120)

# Transform gender to factor with named levels
df$gender <- factor(df$gender)
levels(df$gender) <- c("male", "female", "other")

# Create conditions variable from the four conditions (placebo, nocebo, placebo blocker, and no info)
df$condition_nocebo <- ifelse(df$nocebo_40hz > 1, "Nocebo", NA) 
df$condition_placebo <- ifelse(df$placebo_40hz > 1, "Placebo", NA)
df$condition_nil <- ifelse(df$nil_40hz > 1, "Unwarned", NA)
df$condition_blocker <- ifelse(df$blocker_40hz > 1, "Blocker", NA)

df <- df %>% 
  mutate(condition = coalesce(df$condition_blocker, df$condition_nil, df$condition_nocebo, df$condition_placebo)) 

df$condition <- as.factor(df$condition)

df$condition <- factor(df$condition, levels=c("Unwarned", "Nocebo", "Placebo", "Blocker"))

levels(df$condition)

# Merge 40hz scores via side effect (brownian scores already combined).
df <- df %>% 
  mutate(dizziness_40hz = (coalesce(n_40hz_dizziness, pb_40hz_dizziness, p_40hz_dizziness, nil_40hz_dizziness)-1),
         headache_40hz = (coalesce(n_40hz_headache, pb_40hz_headache, p_40hz_headache, nil_40hz_headache)-1),
         nausea_40hz = (coalesce(n_40hz_nausea, pb_40hz_nausea, p_40hz_nausea, nil_40hz_nausea)-1),
         relaxed_40hz = (coalesce(n_40hz_relaxed, pb_40hz_relaxed, p_40hz_relaxed, nil_40hz_relaxed)-1),
         mood_40hz = (coalesce(n_40hz_mood, pb_40hz_mood, p_40hz_mood, nil_40hz_mood)-1),
         focus_40hz = (coalesce(n_40hz_focus, pb_40hz_focus, p_40hz_focus, nil_40hz_focus)-1))

# Sum scores for 40hz positive
df$sum_40hz_positive <- df %>% 
  dplyr::select(relaxed_40hz, mood_40hz, focus_40hz) %>% 
  rowSums()

# Sum scores for brownian positive
df$sum_brown_positive <- df %>% 
  dplyr::select(relaxed_brown, mood_brown, focus_brown) %>% 
  rowSums()

# Sum scores for 40hz negative
df$sum_40hz_negative <- df %>% 
  dplyr::select(dizziness_40hz, nausea_40hz, headache_40hz) %>% 
  rowSums()

# Sum scores for brown negative
df$sum_brown_negative <- df %>% 
  dplyr::select(dizziness_brown, nausea_brown, headache_brown) %>% 
  rowSums()

names(df)

# Select relevant columns
df <- df %>% 
  dplyr::select(condition,
                sum_40hz_positive:sum_brown_negative,
                expectancy_prior, expectancy_40hz, expectancy_brown, expectancy_post,
                gender, age)

head(df)


# Figures -----------------------------------------------------------------

# Libraries
library(gghalves)
library(ggpubr)
library(papaja)

# Placebo figure
df.fig1 <- df %>% 
  dplyr::select(condition, sum_40hz_positive) %>% 
  dplyr::filter(condition %in% c("Unwarned", "Nocebo", "Placebo"))

levels(df.fig1$condition) <- c("Control", "Control", "Placebo", "Blocker")

df.fig1 <- pivot_longer(df.fig1, !condition, names_to = "symptom", values_to = "score")

ggplot(df.fig1, aes(condition, score, fill = condition)) +
  geom_half_violin(width = .5, colour = "white", fill = "grey") +
  geom_half_boxplot(side = "r", width = .3, alpha = .8) + scale_fill_manual(values = c("#FFFFFF", "#009E73")) +
  stat_compare_means(comparisons = list(c("Control", "Placebo")),label = "p.signif") +
  labs(x = "", y = "Positive symptom scores (Noise 1)") +
  theme_apa(base_size = 16) +
  guides(fill = FALSE) +
  scale_y_continuous(limits = c(0, 24), breaks = seq(0, 24, by = 6))


# Nocebo figure
df.fig2 <- df %>% 
  dplyr::select(condition, sum_brown_negative) %>% 
  dplyr::filter(condition %in% c("Nocebo", "Unwarned"))

ggplot(df.fig2, aes(condition, sum_brown_negative, fill = condition)) +
  geom_half_violin(width = .5, colour = "white", fill = "grey") +
  geom_half_boxplot(side = "r", width = .3, alpha = .8) + scale_fill_manual(values = c("#FFFFFF", "#0072B2")) +
  stat_compare_means(comparisons = list(c("Nocebo", "Unwarned")), label = "p.signif", label.y = 20) +
  labs(x = "", y = "Negative symptom scores (Noise 2)") +
  theme_apa(base_size = 16) +
  guides(fill = FALSE) +
  scale_y_continuous(limits = c(0, 24), breaks = seq(0, 24, by = 6))

# Blocker placebo effect (exploratory)
df.fig3 <- df %>% 
  dplyr::select(condition, sum_40hz_positive) %>% 
  dplyr::filter(condition %in% c("Unwarned", "Nocebo", "Blocker"))

levels(df.fig3$condition) <- c("Control", "Control", "Placebo", "Blocker")

ggplot(df.fig3, aes(condition, sum_40hz_positive, fill = condition)) +
  geom_half_violin(width = .5, colour = "white", fill = "grey") +
  geom_half_boxplot(side = "r", width = .3, alpha = .8) + scale_fill_manual(values = c("#FFFFFF", "#D55E00")) +
  stat_compare_means(comparisons = list(c("Control", "Blocker")),label = "p.signif") +
  labs(x = "", y = "Positive symptom scores (Noise 1)") +
  theme_apa(base_size = 16) +
  guides(fill = FALSE) +
  scale_y_continuous(limits = c(0, 24), breaks = seq(0, 24, by = 6))

# Placebo nocebo blocker (main comparison)
df.fig4 <- df %>% 
  dplyr::select(condition, sum_brown_negative) %>% 
  dplyr::filter(condition %in% c("Nocebo", "Placebo", "Blocker"))

ggplot(df.fig4, aes(condition, sum_brown_negative, fill = condition)) +
  geom_half_violin(width = .5, colour = "white", fill = "grey") +
  geom_half_boxplot(side = "r", width = .3, alpha = .8) + scale_fill_manual(values = c("#0072B2", "#009E73", "#D55E00")) +
  stat_compare_means(label.x = 2, label.y = 23, size = 6) +
  labs(x = "", y = "Negative symptom scores (Noise 2)") +
  theme_apa(base_size = 16) +
  guides(fill = FALSE) +
  scale_y_continuous(limits = c(0, 24), breaks = seq(0, 24, by = 6))

# Positive expectations
df.exp1 <- df %>% 
  dplyr::select(condition, expectancy_40hz) %>% 
  dplyr::filter(condition %in% c("Blocker", "Unwarned", "Nocebo"))

levels(df.exp1$condition) <- c("Control", "Control", "Placebo", "Blocker")

df.fig5 <- df %>% 
  dplyr::select(condition, expectancy_40hz)

levels(df.fig5$condition) <- c("Control", "Control", "Placebo", "Blocker")

ggplot(df.fig5, aes(condition, expectancy_40hz, fill = condition)) +
  geom_half_violin(width = .5, colour = "white", fill = "grey") +
  geom_half_boxplot(width = .3, side = "r", alpha = .8) + scale_fill_manual(values = c("#FFFFFF", "#009E73", "#D55E00")) +
  stat_compare_means(comparisons = list(c("Control", "Placebo"), c("Control", "Blocker")), label = "p.signif") +
  labs(x = "", y = "Positive expectation scores (Noise 1)") +
  theme_apa(base_size = 16)+
  guides(fill = FALSE)+
  scale_y_continuous(limits = c(0, 12), breaks = seq(0, 10, by = 2))

ggplot(df, aes(expectancy_40hz, sum_40hz_positive))+
  geom_jitter()+
  geom_smooth(method = "lm")+
  theme_apa(base_size = 16)+
  labs(x = "Positive expectations (Noise 1)", y = "Positive symptom score (Noise 1)")+
  scale_y_continuous(limits = c(0, 24), breaks = seq(0, 24, by = 6))+
  scale_x_continuous(breaks = seq(0, 10, by = 2))+
  stat_cor(method = "kendall", r.accuracy = 0.01, label.x = 4, label.y = 22, size = 6)

# Negative expectations
df.exp2 <- df %>% 
  dplyr::select(condition, expectancy_brown) %>% 
  dplyr::filter(condition %in% c("Blocker", "Unwarned"))

df.fig6 <- df %>% 
  dplyr::select(condition, expectancy_brown)

ggplot(df.fig6, aes(condition, expectancy_brown, fill = condition)) +
  geom_half_violin(width = .5, colour = "white", fill = "grey") +
  geom_half_boxplot(width = .3, side = "r", alpha = .8) + scale_fill_manual(values = c("#FFFFFF", "#0072B2", "#009E73", "#D55E00")) +
  stat_compare_means(comparisons = list(c("Unwarned", "Nocebo"), c("Unwarned", "Placebo"), c("Unwarned", "Blocker")), label = "p.signif") +
  labs(x = "", y = "Negative expectation scores (Noise 2)") +
  theme_apa(base_size = 16)+
  guides(fill = FALSE)+
  scale_y_continuous(limits = c(0, 14), breaks = seq(0, 10, by = 2))

ggplot(df, aes(expectancy_brown, sum_brown_negative))+
  geom_jitter()+
  geom_smooth(method = "lm")+
  theme_apa(base_size = 16)+
  labs(x = "Negative expectations (Noise 2)", y = "Negative symptom score (Noise 2)")+
  scale_y_continuous(limits = c(0, 24), breaks = seq(0, 24, by = 6))+
  scale_x_continuous(breaks = seq(0, 10, by = 2))+
  stat_cor(method = "kendall", r.accuracy = 0.01, label.x = 4, label.y = 22, size = 6)
