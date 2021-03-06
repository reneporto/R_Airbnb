library(caret)
library(ggplot2)
library(ROCR)

## QUEST�O 2 ##

#1
data <- read.csv("D:\\MESTRADO\\Projeto\\Quest�o 2\\cleaned.csv", header=TRUE, 
                 sep=",", dec=",") %>% mutate_if(is.character, as.numeric) %>% mutate_if(is.integer, as.factor)
set.seed(100)

#2
train <- data[sample(1:nrow(data),round(nrow(data)*0.8)),]

#3
teste <- data[-(sample(1:nrow(data),round(nrow(data)*0.8))),]

# MODELO REGRESS�O LOG�STICA

#4
model<-glm(RainTomorrow ~ ., data = train, family="binomial", maxit=100)

#5
model_res<-predict(model,teste,type="response")

#6
previsao<-prediction(model_res,teste$RainTomorrow)

#7
desemp<-performance(previsao,measure = "tpr",x.measure="fpr")

#8
plot(desemp)

#9
auc <- performance(previsao,measure="auc")@y.values[[1]]

#10
auc


