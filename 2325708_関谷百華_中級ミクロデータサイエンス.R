# 準備

data_2 <- read.csv("npb_Batting_2.csv")
data_4 <- read.csv("npb_Batting_4.csv")

colnames(data_2)[colnames(data_2) == "Player ID"] <- "Player.ID"
colnames(data_4)[colnames(data_4) == "Player ID"] <- "Player.ID"

data2_filtered <- subset(data_2, Year >= 1999 & Year <= 2009)
data4_filtered <- subset(data_4, Year >= 1999 & Year <= 2009)

data24 <- merge(data2_filtered, data4_filtered, by = c("Player.ID", "Year"), all = TRUE)

data_1 <- read.csv("npb_Batting_1.csv")
data_3 <- read.csv("npb_Batting_3.csv")

colnames(data_1)[colnames(data_1) == "Player ID"] <- "Player.ID"
colnames(data_3)[colnames(data_3) == "Player ID"] <- "Player.ID"

data_3 <- subset(data_3, select = -AVG)

data1_filtered <- subset(data_1, Year >= 2010 & Year <= 2021)
data3_filtered <- subset(data_3, Year >= 2010 & Year <= 2021)

data13 <- merge(data1_filtered, data3_filtered, by = c("Player.ID", "Year"), all = TRUE)

data1234 <- rbind(data24, data13)

# 必須内容
# 1変数

library(ggplot2)

ggplot(data1234, aes(x = Height)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "blue", alpha = 0.5) +
  geom_density(color = "red") +
  labs(title = "身長の分布", x = "身長", y = "密度")

summary(data1234$Height)


# 2変数
data1234$BA <- ifelse(data1234$AB > 0, data1234$H / data1234$AB, NA)

ggplot(data1234, aes(x = G, y = BA)) +
  geom_point(alpha = 0.7) +
  labs(title = "試合数と打率の関係", x = "試合数", y = "打率")

ggplot(data1234, aes(x = Position, y = BA)) +
  geom_boxplot() +
  labs(title = "ポジションごとの打率分布", x = "ポジション", y = "打率")


# 3変数

model <- lm(OBP ~ X2B + HR + BB, data = data1234)
summary(model)

plot(model$fitted.values, model$residuals, xlab = "予測値", ylab = "残差",
     main = "残差プロット")
abline(h = 0, col = "red", lty = 2)


#時系列の変遷についての折れ線グラフ

library(dplyr)
library(ggplot2)

agg_data_corrected <- data1234 %>%
  group_by(Year) %>%
  summarise(
    Total_OBP = sum(H + BB, na.rm = TRUE) / sum(AB + BB + SF, na.rm = TRUE)
  )

ggplot(agg_data_corrected, aes(x = Year, y = Total_OBP)) +
  geom_line() +
  geom_point() +
  labs(title = "年ごとの出塁率の推移", x = "年", y = "出塁率")


#予測の手法（線形回帰）

library(caret)
data_pred <- read.csv("npb_batting_prediction.csv")
train_data <- subset(data1234, Year >= 1999 & Year <= 2021)

data_pred$HR <- NA  

features <- c("G", "PA", "AB", "BB", "SO", "X2B", "X3B")  
target <- "HR" 

lm_model <- lm(as.formula(paste(target, "~", paste(features, collapse = "+"))), data = train_data)

summary(lm_model)

predictions <- predict(lm_model, newdata = data_pred)

data_pred$Predicted_HR <- predictions

mse <- mean((predict(lm_model, newdata = train_data) - train_data$HR)^2, na.rm = TRUE)
cat("トレーニングデータでの平均二乗誤差(MSE):", mse, "\n")


# 発展内容
# データ視覚化のために自分で考えた手法

ggplot(data1234, aes(x = Position, y = BA, fill = Position)) +
  geom_violin(trim = FALSE) +
  labs(title = "ポジションごとの打率分布", x = "ポジション", y = "打率") +
  theme_minimal()


# 機械学習のアルゴリズムの異なる手法

install.packages("randomForest")
library(randomForest)

train_data <- subset(data1234, Year >= 1999 & Year <= 2021)

data_pred$HR <- NA  

features <- c("G", "PA", "AB", "BB", "SO", "X2B", "X3B")
target <- "HR"  

set.seed(42)
rf_model <- randomForest(
  as.formula(paste(target, "~", paste(features, collapse = "+"))),
  data = train_data,
  ntree = 100,
  importance = TRUE
)

print(rf_model)

varImpPlot(rf_model, main = "ランダムフォレストの重要変数 (本塁打数)")

rf_predictions <- predict(rf_model, newdata = data_pred)

data_pred$Predicted_HR <- rf_predictions

train_predictions <- predict(rf_model, newdata = train_data)
mse <- mean((train_predictions - train_data$HR)^2, na.rm = TRUE)
cat("トレーニングデータでの平均二乗誤差(MSE):", mse, "\n")
