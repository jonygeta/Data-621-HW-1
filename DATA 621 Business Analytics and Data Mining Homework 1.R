## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = FALSE, message = FALSE,warning = FALSE)



## ------------------------------------------------------------------------
library(ggcorrplot)
library(pander)
library(tidyverse)
library(PerformanceAnalytics)
library(Amelia)
library(caret)
library(mice)


## ------------------------------------------------------------------------
training <- read_csv("moneyball-training-data.csv" )
training <- training[,2:ncol(training)]
evaluation <- read_csv("moneyball-evaluation-data.csv")
colId <- evaluation$INDEX
evaluation <- evaluation[,2:ncol(evaluation)]


## ------------------------------------------------------------------------
glimpse(training)


## ------------------------------------------------------------------------
pander(summary(training), split.table=120)


## ------------------------------------------------------------------------
for(col in colnames(training)){
  boxplot(training[,col],ylab = col)
}



## ------------------------------------------------------------------------
chart.Correlation(training)


## ------------------------------------------------------------------------
missmap(training)

## ------------------------------------------------------------------------
corr <- cor(training, use="complete.ob")
ggcorrplot(corr, hc.order = TRUE, type = "lower",
   lab = TRUE)


## ------------------------------------------------------------------------
imputed_Data <- mice(training, m=1, maxit = 50, method = 'pmm', seed = 500)
complete_data <- complete(imputed_Data,1)

imputed_Data_eval <- mice(evaluation, m=1, maxit = 50, method = 'pmm', seed = 500)
complete_data_evaluation <- complete(imputed_Data_eval,1)


## ------------------------------------------------------------------------
training$flag <- 0
training$flag[rowMeans(training) > 0] <- 1
table(training$flag)


## ------------------------------------------------------------------------
# TEAM_BATTING_H 0-1200,1200-2000,2000-3000
b <- c(-Inf, 1200, 2000, Inf)
names <- c("Low", "Medium", "High")
complete_data$TEAM_BATTING_H.cat <- cut(complete_data$TEAM_BATTING_H, breaks = b, labels = names)
complete_data_evaluation$TEAM_BATTING_H.cat <- cut(complete_data_evaluation$TEAM_BATTING_H, breaks = b, labels = names)




## ------------------------------------------------------------------------
complete_data$stolen <- complete_data$TEAM_BASERUN_CS + complete_data$TEAM_BASERUN_SB
complete_data_evaluation$stolen <- complete_data_evaluation$TEAM_BASERUN_CS + complete_data_evaluation$TEAM_BASERUN_SB

## ------------------------------------------------------------------------
# drop TEAM_BATTING_H
complete_data <- complete_data %>% 
  select(-TEAM_BATTING_H,-TEAM_BASERUN_CS, -TEAM_BASERUN_SB)
# drop TEAM_BATTING_H
evaluation <- evaluation %>% 
  select(-TEAM_BATTING_H,-TEAM_BASERUN_CS, -TEAM_BASERUN_SB)


## ------------------------------------------------------------------------
model1 <- lm(TARGET_WINS~., data = complete_data)
pander(summary(model1))


## ------------------------------------------------------------------------
model2 <- lm(TARGET_WINS~.-TEAM_PITCHING_HR-TEAM_PITCHING_BB-TEAM_PITCHING_SO, data = complete_data)
pander(summary(model2))


## ------------------------------------------------------------------------
model3 <- lm(TARGET_WINS~TEAM_BATTING_H.cat+TEAM_BATTING_2B +TEAM_BATTING_HR + TEAM_BATTING_BB+
              TEAM_PITCHING_H+TEAM_PITCHING_HR, data = complete_data )
pander(summary(model3))


## ------------------------------------------------------------------------
mse <- numeric(3)
mse[[1]] <- mean((complete_data$TARGET_WINS - predict(model1))^2)
mse[[2]] <- mean((complete_data$TARGET_WINS - predict(model2))^2)
mse[[3]] <- mean((complete_data$TARGET_WINS - predict(model3))^2)
mse


## ------------------------------------------------------------------------
 summary(model1)$adj.r.squared
 summary(model2)$adj.r.squared
 summary(model3)$adj.r.squared


## ------------------------------------------------------------------------
 summary(model1)$fstatistic
 summary(model2)$fstatistic
 summary(model3)$fstatistic


## ------------------------------------------------------------------------
par(mfrow=c(2,2))
plot(model1)
par(mfrow=c(2,2))
plot(model2)
par(mfrow=c(2,2))
plot(model3)


## ------------------------------------------------------------------------
complete_data_evaluation$PredictedWins <- predict(model2, complete_data_evaluation)
complete_data_evaluation$PredictedWins

## ------------------------------------------------------------------------
df <- data.frame("Index"=colId,"Predicted wins"= complete_data_evaluation$PredictedWins)
write.csv(df,"predictions.csv")

