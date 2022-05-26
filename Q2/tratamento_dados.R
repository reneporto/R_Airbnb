
## QUESTÃO 2 SUPLEMENTAR ##

library(VIM)
install.packages('ggplot2')
library(ggplot2)
library(corrplot)
# data <- read.csv("D:\\MESTRADO\\Projeto\\Questão 2\\weatherAUS.csv",
#                  header=TRUE, sep=",", dec=",")
# 
# write.csv(subdata3, "D:\\MESTRADO\\Projeto\\Questão 2\\subdata.csv", row.names = FALSE)
# 
# subdata <- subset(data, Location =='Cobar')
# 
# View(subdata)
# 
# ### UMA DAS COISAS QUE SE PODE FAZER COM A CIÊNCIA DE DADOS NO R É TRATAR O DATASET
# # ATRAVÉS DE DIVERSAS FORMAS DIFERENTES. UMA DAS FORMAS QUE EXISTE É A UTILIZAÇÃO
# # DO kNN, o k-nearest neighbour, para tratar os missing values ###
# 
# subdata_knn <- kNN(subdata, variable = c("Evaporation","Sunshine", "WindGustSpeed",
#                                       "WindSpeed9am", "WindSpeed3pm","Humidity9am",
#                                       "Humidity3pm", "Pressure9am", "Pressure3pm",
#                                       "Cloud9am", "Cloud3pm", "Temp9am",
#                                       "Temp3pm"), k = 23) # k=sqrt(534)=23
# 
# boxplot(dataNA$MinTemp,
#         border = "black",
#         horizontal = TRUE,
#         notch = FALSE)
# 
# #View(subdata_knn)
# subdata2 <- na.omit(subdata_knn)
# #View(subdata2)
# subdata3 <- subdata2[-c(1,2,22,24:36)]
# 
# #View(subdata3)
# View(subdata3)
# 
# ##CHECK FOR OUTLIERS##
# 
# subdata3$MinTemp <- as.numeric(subdata3$MinTemp)
# boxplot(subdata3$MinTemp,
#         border = "black",
#         horizontal = TRUE,
#         notch = FALSE)
# 
# subdata3$MaxTemp <- as.numeric(subdata3$MaxTemp)
# boxplot(subdata3$MaxTemp,
#         border = "black",
#         horizontal = TRUE,
#         notch = FALSE)
# 
# ##AQUI HÁ
# subdata3$Rainfall <- as.numeric(subdata3$Rainfall)
# boxplot(subdata3$Rainfall,
#         border = "black",
#         horizontal = TRUE,
#         notch = FALSE)
# hist(subdata3$Rainfall,main = "Rainfall",
#      xlab = "X",
#      ylab = "y",
#      border = "black")
# 
# 
# ##AQUI HÁ
# subdata3$Evaporation <- as.numeric(subdata3$Evaporation)
# boxplot(subdata3$Evaporation,
#         border = "black",
#         horizontal = TRUE,
#         notch = FALSE)
# 
# ##AQUI HÁ
# subdata3$Sunshine <- as.numeric(subdata3$Sunshine)
# boxplot(subdata3$Sunshine,
#         border = "black",
#         horizontal = TRUE,
#         notch = FALSE)
# 
# ##AQUI HÁ
# subdata3$WindGustSpeed <- as.numeric(subdata3$WindGustSpeed)
# boxplot(subdata3$WindGustSpeed,
#         border = "black",
#         horizontal = TRUE,
#         notch = FALSE)
# 
# 
# ##AQUI HÁ
# subdata3$WindSpeed9am <- as.numeric(subdata3$WindSpeed9am)
# boxplot(subdata3$WindSpeed9am,
#         border = "black",
#         horizontal = TRUE,
#         notch = FALSE)
# 
# ##AQUI HÁ
# subdata3$WindSpeed3pm <- as.numeric(subdata3$WindSpeed3pm)
# boxplot(subdata3$WindSpeed3pm,
#         border = "black",
#         horizontal = TRUE,
#         notch = FALSE)
# 
# subdata3$Humidity9am <- as.numeric(subdata3$Humidity9am)
# boxplot(subdata3$Humidity9am,
#         border = "black",
#         horizontal = TRUE,
#         notch = FALSE)
# 
# ##AQUI HÁ
# subdata3$Humidity3pm <- as.numeric(subdata3$Humidity3pm)
# boxplot(subdata3$Humidity3pm,
#         border = "black",
#         horizontal = TRUE,
#         notch = FALSE)
# 
# ##AQUI HÁ
# subdata3$Pressure9am <- as.numeric(subdata3$Pressure9am)
# boxplot(subdata3$Pressure9am,
#         border = "black",
#         horizontal = TRUE,
#         notch = FALSE)
# 
# ##AQUI HÁ
# subdata3$Pressure3pm <- as.numeric(subdata3$Pressure3pm)
# boxplot(subdata3$Pressure3pm,
#         border = "black",
#         horizontal = TRUE,
#         notch = FALSE)
# 
# subdata3$Cloud9am <- as.numeric(subdata3$Cloud9am)
# boxplot(subdata3$Cloud9am,
#         border = "black",
#         horizontal = TRUE,
#         notch = FALSE)
# 
# subdata3$Cloud3pm <- as.numeric(subdata3$Cloud3pm)
# boxplot(subdata3$Cloud3pm,
#         border = "black",
#         horizontal = TRUE,
#         notch = FALSE)
# 
# subdata3$Temp9am <- as.numeric(subdata3$Temp9am)
# boxplot(subdata3$Temp9am,
#         border = "black",
#         horizontal = TRUE,
#         notch = FALSE)
# 
# subdata3$Temp3pm <- as.numeric(subdata3$Temp3pm)
# boxplot(subdata3$Temp3pm,
#         border = "black",
#         horizontal = TRUE,
#         notch = FALSE)
# 
# 
# 
# df2<- data.frame(subdata3)
# 
# library(dplyr)
# subdata3 %>% 
#   mutate(RainTomorrow = ifelse(as.character(RainTomorrow) == "No", "0", as.character(RainTomorrow)))
# subdata3 %>% 
#   mutate(RainTomorrow = ifelse(as.character(RainTomorrow) == "Yes", "1", as.character(RainTomorrow)))
# 
# subdata3$RainTomorrow[subdata3$RainTomorrow=="No"] <- 0
# subdata3$RainTomorrow[subdata3$RainTomorrow=="Yes"] <- 1
# 
# View(subdata3)
# write.csv(subdata3, "D:\\MESTRADO\\Projeto\\Questão 2\\subdata.csv", row.names = FALSE)
# 
# 
# ## ELIMINANDO SIMPLESMENTEN OS NAs
# 
# # subdata_final <- na.omit(subdata)
# # 
# # duplicated(subdata_final, fromLast=TRUE)
# # 
# # write.csv(subdata_final, "D:\\MESTRADO\\Projeto\\Questão 2\\subdata.csv", row.names = FALSE)
# 
# 



####### FICHEIRO JÁ LIMPO ENCONTRADO NO KAGGLE #######
##################################################################################
dataCleaned <- read.csv("D:\\MESTRADO\\Projeto\\Questão 2\\cleaned_weatherAUS.csv", header=TRUE, sep=",", dec=",")
subdataCleaned <- subset(dataCleaned, Location =='Cobar')
#View(dataCleaned)
really_cleaned <- subdataCleaned[-c(1,2,3,21,23)]
#View(really_cleaned)
really_cleaned$RainTomorrow[really_cleaned$RainTomorrow=="No"] <- 0
really_cleaned$RainTomorrow[really_cleaned$RainTomorrow=="Yes"] <- 1
#View(really_cleaned)
data_reallycleaned <- really_cleaned[-c(4,6,7)]
#View(data_reallycleaned)
library(dplyr)
data1 <- data_reallycleaned %>% mutate_if(is.character, as.numeric)
#str(data1)
data1$RainTomorrow <- as.factor(data1$RainTomorrow)
str(data1)
write.csv(data_reallycleaned, "D:\\MESTRADO\\Projeto\\Questão 2\\cleaned.csv", row.names = FALSE)
# really_cleaned$RainTomorrow[really_cleaned$RainTomorrow==0] <- "No"
# really_cleaned$RainTomorrow[really_cleaned$RainTomorrow==1] <- "Yes"
##################################################################################


# nrow(data1)
# data1$RainTomorrow <- as.numeric(data1$RainTomorrow)
# View(data1)
# data1$RainTomorrow[data1$RainTomorrow=="1"] <- 0
# data1$RainTomorrow[data1$RainTomorrow=="2"] <- 1
# hist(data1$RainTomorrow,
#      border = "black",
#      horizontal = TRUE,
#      notch = FALSE)




