# library
library(dplyr)
library(ggplot2)
library(tidyverse)
library(caret)
library (skimr)
library(scales)
library(corrplot)
library(RColorBrewer)
library(caTools)
library(e1071)
library(class)
library(ElemStatLearn)
library(randomForest)



# Cleansing and EDA
df_water_potability <- read.csv("C:/Users/ramad/Downloads/water_quality/water_potability.csv")

str(df_water_potability)

#ph: pH of 1. water (0 to 14).
#Hardness: Capacity of water to precipitate soap in mg/L.
#Solids: Total dissolved solids in ppm.
#Chloramines: Amount of Chloramines in ppm.
#Sulfate: Amount of Sulfates dissolved in mg/L.
#Conductivity: Electrical conductivity of water in μS/cm.
#Organic_carbon: Amount of organic carbon in ppm.
#Trihalomethanes: Amount of Trihalomethanes in μg/L.
#Turbidity: Measure of light emiting property of water in NTU.
#Potability: Indicates if water is safe for human consumption. Potable - 1 and Not potable - 0


# Cleansing and EDA
df_water_potability$Potability <- as.factor(df_water_potability$Potability)
str(df_water_potability)

summary(dataset_water_potality)

#check percentage missing values
df_water_potability %>% group_by(Potability) %>%  skim() %>%
  filter(n_missing != 0) %>%
  as_tibble() %>%
  select(skim_variable, n_missing, complete_rate, Potability) %>%
  mutate(missing_rate = round(abs(complete_rate - 1) * 100, 1)) %>%
  ggplot(aes(
    x = fct_reorder(skim_variable, n_missing),
    y = missing_rate,
    fill = skim_variable,
    label = paste0(missing_rate, "%")
  )) +
  geom_col(width = .6) +
  geom_text(
    size = 4,
    hjust = 1.2,
    vjust = 0.25,
    col = "white"
  ) +
  coord_flip() +
  facet_wrap(vars(Potability)) +
  theme(aspect.ratio = .7) +
  theme(
    legend.position = "none",
    strip.background = element_rect(fill="#94246D"),
    strip.text = element_text(color = "white", face = "bold", size = 12)
  ) +
  scale_y_continuous(label = label_percent(scale = 1)) +
  scale_fill_manual(values = c("#D41C64",
                               "#4B6FB5",
                               "#6C3996")) +
  labs(
    title = "Berapa persentase missing value di setiap fitur?",
    subtitle = "Plot, Missing Data distribution VS Target Variable",
    x = NULL,
    y = NULL
  )


# fill NA values with Median
df_water_potability <- df_water_potability %>% mutate(across(where(is.numeric), ~replace_na(., median(., na.rm=TRUE))))

summary(df_water_potability)

# distribution potability
df_water_potability %>%
  select(Potability) %>%
  count(Potability) %>% mutate(percent = paste0(round(n / sum(n) * 100), "%"), 2) %>%
  ggplot(aes(
    x = Potability,
    y = n,
    label = percent,
    fill = Potability
  )) +
  geom_col() +
  geom_text(vjust = -0.2, color = "#7C4EA8") +
  scale_fill_manual(values = c("#E4652E", "#0E8A41")) +
  labs(
    title = "Potability distribution",
    subtitle = "Plot, Column Plot, Potability distribution",
    caption = "Data source: Kaggle.com, Water Quality",
    x = NULL,
    y = NULL,
    fill = NULL
  )

# box plot
df_water_potability %>%
  pivot_longer(cols = -Potability, names_to = "feature") %>%
  ggplot(aes(x = feature, y = value)) +
  geom_boxplot(aes(fill = Potability)) +
  facet_wrap(vars(feature), ncol = 3, scales = "free") +
  scale_color_manual(values = c("#E4652E", "#0E8A41")) +
  scale_fill_manual(values = c("#E4652E", "#0E8A41")) +
  theme(
    legend.position = "right",
    strip.background = element_rect(fill = "#0B2D5B"),
    strip.text = element_text(color = "white", face = "bold", size = 8)
  ) +
  labs(
    title = "Distribusi Variabel Target di Setiap Fitur",
    subtitle = "Plot, Box Plot",
    x = NULL,
    y = NULL,
    fill = NULL,
    color = NULL
  )

# outlier with Histogram
df_water_potability %>%
  pivot_longer(cols = -Potability, names_to = "feature") %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 30, aes(fill = feature)) +
  facet_wrap(vars(feature, Potability), ncol = 4, scales = "free") +
  scale_fill_brewer(palette = "Paired") +
  theme(
    legend.position = "none",
    strip.background = element_rect(fill = "#0B2D5B"),
    strip.text = element_text(color = "white", face = "bold", size = 8)
  ) +
  labs(
    title = "Distribusi Histogram di Setiap Fitur",
    x = NULL,
    y = NULL
  )

# correlation plot
corrplot(
  cor(df_water_potability[, -10]),
  type = "lower",
  method = "circle",
  number.cex = .9,
  order = "alphabet",
  tl.col = "#00796B",
  tl.srt = 25,
  col = brewer.pal(n = 9, name = "Purples"),
  title  = "\nCorrelation Plot of Water Potability Data "
)

ggpairs(
  df_water_potability,
  aes(color = Potability),
  columns = 1:9,
  lower = list(continuous = wrap(
    "smooth",
    alpha = 0.2,
    size = 0.5,
    color = "#DE942E"
  )),
  diag = list(continuous = "barDiag"),
  upper = list(continuous = wrap("cor", size = 4))
) +
  scale_color_manual(values = c("#1F5736", "#E94046")) +
  scale_fill_manual(values = c("#1F5736", "#E94046")) +
  theme(
    axis.text = element_text(size = 8),
    panel.background = element_rect(fill = "white"),
    strip.background = element_rect(fill = "white"),
    strip.background.x = element_rect(colour = "black"),
    strip.background.y = element_rect(colour = "black"),
    strip.text = element_text(color = "black", face = "bold", size = 8)
  ) +
  labs(
    title = "Pair Plot dari Varibale Target: Potability",
    subtitle = "Pair Plot, scatter plot, Histogram and Correlation coefficient",
    x = NULL,
    y = NULL
  )

view(df_water_potability)


# splitting the dataset
set.seed(123)
split = sample.split(df_water_potability$Potability, SplitRatio = 0.8)
training_set = subset(df_water_potability, split == TRUE)
test_set = subset(df_water_potability, split == FALSE)

# Scaling
training_set[, 1:9] = scale(training_set[, 1:9])
test_set[, 1:9] = scale(test_set[, 1:9])

#---------Random Forest-------
set.seed(123)
classifierRF_1 <- randomForest(Potability ~ .,
                               data=training_set,
                               ntree= 100)
classifierRF_1

set.seed(123)
classifierRF_2 <- randomForest(Potability ~ .,
                               data=training_set,
                               ntree= 500)
classifierRF_2

set.seed(123)
classifierRF_3 <- randomForest(Potability ~ .,
                               data=training_set,
                               ntree= 1000)
classifierRF_3

#prediction on test-dataset and make confusion matrix
predicted_outcomes_rf_1 <- predict(classifierRF_1, test_set)

rf1_confm <- confusionMatrix(predicted_outcomes_rf_1, test_set$Potability, positive='1')
rf1_confm

predicted_outcomes_rf_2 <- predict(classifierRF_2, test_set)

rf2_confm <- confusionMatrix(predicted_outcomes_rf_2, test_set$Potability, positive='1')
rf2_confm

predicted_outcomes_rf_3 <- predict(classifierRF_3, test_set)
rf3_confm  <- confusionMatrix(predicted_outcomes_rf_3, test_set$Potability, positive='1')
rf3_confm

plot(classifierRF_3)



