

################################################################################
# Tema:   Previsão do score atribuído por reviewers em alojamentos locais
################################################################################
# Alunos: Guilherme Mendonca  - 82575   - gasvm@iscte-iul.pt
#         Marta Neves         - 88660   - msmns@iscte-iul.pt
#         René Porto          - 101597  - rapfr@iscte-iul.pt
################################################################################
# Nome:       accomodation_model.R
# Descriacao: Esse script faz o modelo
#
# Como usar:  Atualizar as variaveis "caminho_in" com o local do ficheiro 
#             "accom_clean.csv" resultante do script "accomodation_cleaning.R" 
################################################################################

  library(rvest)
  library(stringr)
  library(ggplot2)
  library(plyr)
  library(corrplot)
  library(car)
  library(caret)
  library(partykit)
  library(rpart)
  library(rpart.plot)
  library(ipred)
  library(gbm)
  library(fastDummies)
  library(dplyr)
  library(neuralnet)
  library(Metrics)


caminho_in <- "C:\\Users\\Rene Porto\\Documents\\Trabalho Airbnb\\accom_clean.csv"

accom <- read.csv(caminho_in, header=TRUE, sep=",", dec=",")

colnames(accom) <- str_replace(colnames(accom), "accom.", "")

# Converter para factor
accom$superhost <- as.factor(accom$superhost) 
accom$studio <- as.factor(accom$studio) 
accom$kitchen <- as.factor(accom$kitchen) 
accom$wifi <- as.factor(accom$wifi) 
accom$tv <- as.factor(accom$tv) 
accom$elevator <- as.factor(accom$elevator) 
accom$washer <- as.factor(accom$washer) 
accom$dryer <- as.factor(accom$dryer) 
accom$ac <- as.factor(accom$ac) 
accom$balcony <- as.factor(accom$balcony) 
accom$luggage_do <- as.factor(accom$luggage_do) 
accom$hair_dryer <- as.factor(accom$hair_dryer) 
accom$pet <- as.factor(accom$pet) 
accom$fire_ext <- as.factor(accom$fire_ext) 
accom$first_aid <- as.factor(accom$first_aid) 
accom$breakfast <- as.factor(accom$breakfast) 
accom$parking <- as.factor(accom$parking) 
accom$smoking <- as.factor(accom$smoking) 
accom$property_type <- as.factor(accom$property_type)
accom$bath_private <- as.factor(accom$bath_private)

################################################################################
#                             Exploração
################################################################################

summary(accom)

################################################################################

accom$score <- as.numeric(accom$score)

#Target: Score
boxplot(accom$score,
        main = "Score das Acomodações",
        xlab = "Score",
        border = "black",
        horizontal = TRUE,
        notch = FALSE) 

hist(accom$score,
        main = "Score das Acomodações",
        xlab = "Score",
        border = "black",
        horizontal = TRUE,
        notch = FALSE)


################################################################################

#Reviews
boxplot(accom$nreviews,
        main = "Nº de reviews para cada acomodação",
        xlab = "Nºreviews",
        col = "#4ADCD2",
        border = "black",
        horizontal = TRUE,
        notch = FALSE) 

hist(accom$nreviews,
     main = "Nº de reviews para cada acomodação",
     xlab = "Nºreviews",
     col = "#4ADCD2",
     ylab = "Observações",
     border = "black")

#logaritimizar a variavel review
accom$lognreviews <- log(accom$nreviews)

boxplot(accom$lognreviews,
        main = "Log. nº de reviews para cada acomodação",
        xlab = "Nºreviews",
        col = "#4ADCD2",
        border = "black",
        horizontal = TRUE,
        notch = FALSE)

hist(accom$lognreviews,
        main = "Log. nº de reviews para cada acomodação",
        xlab = "Nºreviews",
        col = "#4ADCD2",
        border = "black",
        horizontal = TRUE,
        notch = FALSE)

# Apos a logaritimização da variavel, não temos mais outliers

################################################################################

#Price
boxplot(accom$price,
        main = "Preço das Acomodações em Euros ",
        xlab = "Preço",
        col = "#4ADCD2",
        border = "black",
        horizontal = TRUE,
        notch = FALSE) 

hist(accom$price,
     main = "Preço das Acomodações em Euros",
     xlab = "Preço",
     ylab = "Observações",
     col = "#4ADCD2",
     border = "black")

marca_i <- summary(accom$price)[2] - 1.5*IQR(accom$price)
marca_i

marca_s <- summary(accom$price)[5] + 1.5*IQR(accom$price)
marca_s

caps <- quantile(accom$price, probs=c(.05, .95), na.rm = T)
caps

length(accom$price[accom$price < marca_i])
#0

length(accom$price[accom$price > marca_s])
#199

#logaritimizar a variavel price
accom$logprice <- log(accom$price)


boxplot(accom$logprice,
        main = "Log preço das Acomodações em Euros ",
        xlab = "Preço",
        col = "#4ADCD2",
        border = "black",
        horizontal = TRUE,
        notch = FALSE) 

hist(accom$logprice,
     main = "Log preço das Acomodações em Euros",
     xlab = "Preço",
     ylab = "Observações",
     col = "#4ADCD2",
     border = "black")

marca_i <- summary(accom$logprice)[2] - 1.5*IQR(accom$logprice)
marca_i

marca_s <- summary(accom$logprice)[5] + 1.5*IQR(accom$logprice)
marca_s

length(accom$logprice[accom$logprice < marca_i])
#5

length(accom$logprice[accom$logprice > marca_s])
#24

# Substituir os outliers superiores
caps <- quantile(accom$logprice, probs=c(.05, .95), na.rm = T)
caps

accom$logprice[accom$logprice < marca_i] <- caps[1]
accom$logprice[accom$logprice > marca_s] <- caps[2]

boxplot(accom$logprice,
        main = "Log preço das Acomodações em Euros ",
        xlab = "Preço",
        col = "#4ADCD2",
        border = "black",
        horizontal = TRUE,
        notch = FALSE) 

hist(accom$logprice,
     main = "Log preço das Acomodações em Euros",
     xlab = "Preço",
     ylab = "Observações",
     border = "black")



################################################################################

#Guest
boxplot(accom$guest,
        main = "Nº de hóspedes em cada acomodação",
        xlab = "Nº de hóspedes",
        col = "#4ADCD2",
        border = "black",
        horizontal = TRUE,
        notch = FALSE) 

hist(accom$guest,
     main = "Nº de hóspedes em cada acomodação",
     xlab = "Nº de hóspedes",
     ylab = "Observações",
     border = "black")

length(accom$guest[accom$guest > marca_s])
#154


################################################################################

#bedroom
boxplot(accom$bedroom,
        main = "Nº quartos em cada acomodação",
        xlab = "Nº de quartos",
        col = "#4ADCD2",
        border = "black",
        horizontal = TRUE,
        notch = FALSE) 

hist(accom$bedroom,
     main = "Nº quartos em cada acomodação",
     xlab = "Nº de quartos",
     ylab = "Observações",
     border = "black")


length(accom$bedroom[accom$bedroom > marca_s])
#528


################################################################################

#bed
boxplot(accom$bed,
        main = "Nº de camas em cada acomodação",
        xlab = "Nº de camas",
        col = "#4ADCD2",
        border = "black",
        horizontal = TRUE,
        notch = FALSE)


hist(accom$bed,
     main = "Nº de camas em cada acomodação",
     xlab = "Nº de camas",
     ylab = "Observações",
     border = "black")


length(accom$bed[accom$bed > marca_s])
#211


################################################################################

#nrbath
boxplot(accom$nrbath,
        main = "Nº de casas de banho em cada acomodação",
        xlab = "Nº de casas de banho",
        col = "#4ADCD2",
        border = "black",
        horizontal = TRUE,
        notch = FALSE) 

hist(accom$nrbath,
     main = "Nº de casas de banho em cada acomodação",
     xlab = "Nº de casas de banho",
     ylab = "Observações",
     border = "black")

#545

################################################################################

#property_type


summary(accom)

length(accom[,1])

################################################################################
# CORRELAÇÃO DE PEARSON

df <- data.frame(
        accom$property_type,
        accom$score,
        accom$lognreviews,
        accom$logprice,
        accom$guest,
        accom$bedroom,
        accom$bed,
        accom$nrbath,
        accom$superhost,
        accom$studio,
        accom$kitchen,
        accom$wifi,
        accom$tv,
        accom$elevator,
        accom$washer,
        accom$dryer,
        accom$ac,
        accom$balcony,
        accom$luggage_do,
        accom$hair_dryer,
        accom$pet,
        accom$fire_ext,
        accom$first_aid,
        accom$breakfast,
        accom$parking,
        accom$smoking,
        accom$bath_private)

colnames(df) <- str_replace(colnames(df), "accom.", "")

summary(df)

#corrplot
correlation <- cor(df[,2:8])

#Correlation matrix
round(correlation, 3)

par(oma = c(0, 0, 0, 0)) # space around for text
corrplot.mixed(correlation,
               title = "Correlação entre todas váriaveis numéricas",
               mar=c(0,0,5,0),
               order = "hclust", #order of variables
               tl.pos = "lt", #text left + top
               upper = "circle"
)
plot(df[,2:8], pch = 19, lower.panel = NULL)
#talvez retirar:
#       logprice
#       logbed
#       logbedroom

df2 <- data.frame(
        accom$property_type,
        accom$score,
        accom$lognreviews,
        accom$guest,
        accom$nrbath,
        accom$superhost,
        accom$studio,
        accom$kitchen,
        accom$wifi,
        accom$tv,
        accom$elevator,
        accom$washer,
        accom$dryer,
        accom$ac,
        accom$balcony,
        accom$luggage_do,
        accom$hair_dryer,
        accom$pet,
        accom$fire_ext,
        accom$first_aid,
        accom$breakfast,
        accom$parking,
        accom$smoking,
        accom$bath_private)

colnames(df2) <- str_replace(colnames(df2), "accom.", "")

summary(df2)

#corrplot
correlation2 <- cor(df2[,2:5])

#Correlation matrix
round(correlation2, 3)

par(oma = c(0, 0, 0, 0)) # space around for text
corrplot.mixed(correlation2,
               title = "Correlação entre as váriaveis numéricas não correlacionadas",
               mar=c(0,0,5,0) ,
               order = "hclust", #order of variables
               tl.pos = "lt", #text left + top
               upper = "circle"
)


################################################################################
#                             Regressão Linear
################################################################################

#Multipla

summary(df2)

model_mult <- lm(score ~ ., data = df2)

summary(model_mult)
#R-squared:  0.2619

#Retirar:
# nrbath - 0.696504
# tv - 0.252805

df3 <- data.frame(
        accom$property_type,
        accom$score,
        accom$lognreviews,
        accom$guest,
        accom$superhost,
        accom$studio,
        accom$kitchen,
        accom$wifi,
        accom$elevator,
        accom$washer,
        accom$dryer,
        accom$ac,
        accom$balcony,
        accom$luggage_do,
        accom$hair_dryer,
        accom$pet,
        accom$fire_ext,
        accom$first_aid,
        accom$breakfast,
        accom$parking,
        accom$smoking,
        accom$bath_private)

colnames(df3) <- str_replace(colnames(df3), "accom.", "")

summary(df3)

model_mult2 <- lm(score ~ ., data = df3)

summary(model_mult2)
# Adjusted R-squared:  0.2622 


# Retirar:
# wifi - 0.646100
# elevator - 0.642684

df4 <- data.frame(
        accom$property_type,
        accom$score,
        accom$lognreviews,
        accom$guest,
        accom$superhost,
        accom$studio,
        accom$kitchen,
        accom$washer,
        accom$dryer,
        accom$ac,
        accom$balcony,
        accom$luggage_do,
        accom$hair_dryer,
        accom$pet,
        accom$fire_ext,
        accom$first_aid,
        accom$breakfast,
        accom$parking,
        accom$smoking,
        accom$bath_private)

colnames(df4) <- str_replace(colnames(df4), "accom.", "")

summary(df4)

model_mult3 <- lm(score ~ ., data = df4)

summary(model_mult3)
# Adjusted R-squared:  0.2626

# Retirar:
# dryer - 0.487956
# luggage_do - 0.360042

df5 <- data.frame(
        accom$property_type,
        accom$score,
        accom$lognreviews,
        accom$guest,
        accom$superhost,
        accom$studio,
        accom$kitchen,
        accom$washer,
        accom$ac,
        accom$balcony,
        accom$hair_dryer,
        accom$pet,
        accom$fire_ext,
        accom$first_aid,
        accom$breakfast,
        accom$parking,
        accom$smoking,
        accom$bath_private)

colnames(df5) <- str_replace(colnames(df5), "accom.", "")

summary(df5)

model_mult4 <- lm(score ~ ., data = df5)

summary(model_mult4)
# Adjusted R-squared: 0.2625

# pet - 0.394721
# fire_ext - 0.368615    
# first_aid - 0.986357

df6 <- data.frame(
        accom$property_type,
        accom$score,
        accom$lognreviews,
        accom$guest,
        accom$superhost,
        accom$studio,
        accom$kitchen,
        accom$washer,
        accom$ac,
        accom$balcony,
        accom$hair_dryer,
        accom$breakfast,
        accom$parking,
        accom$smoking,
        accom$bath_private)

colnames(df6) <- str_replace(colnames(df6), "accom.", "")

summary(df6)

model_mult5 <- lm(score ~ ., data = df6)

summary(model_mult5)
# Adjusted R-squared: 0.2624


# Divisão aleatória dos dados: Treino (70%) e Teste (30%)
set.seed(123)
index_1<-sample(1:nrow(df6),round(nrow(df6)*0.7))
train_1<-df6[index_1,]
teste_1<-df6[-index_1,]

model_mult5 <- lm(score ~ ., data = train_1)

RMSE_train<-sigma(model_mult5)
RMSE_train
# Erro Quadrático Médio - 0.2893199

MSE_train<-sigma(model_mult5)^2
MSE_train
# Raiz Quadrada do Erro Quadrático Médio - 0.08370603

MAE_train<-mean(abs(resid(model_mult5)))
MAE_train
# Cálculo do Erro Absoluto Médio - 0.205435

estimativas<-predict(model_mult5,teste_1)

tabela<-data.frame(VReais=teste_1$score,VPrevistos=estimativas)

tabela$error<-with(tabela,teste_1$score-estimativas)

tabela

MSE_teste<-with(tabela,mean(error^2))
MSE_teste
# Erro Quadrático Médio - 0.08181217

RMSE_teste<-sqrt(MSE_teste)
RMSE_teste
# Raiz Quadrada do Erro Quadrático - 0.2860283


MAE_teste<-with(tabela,mean(abs(error)))
MAE_teste
# Erro Absoluto Médio - 0.2116132

plot(tabela$VReais,
     tabela$VPrevistos,
     main = "Score das estadias em Hong Kong: Previstos vs Reais", 
     xlab = "Reais",
     ylab = "Previstos")
abline(0, 1, col="red")


################################################################################
#Multipla + Validação Cruzada
set.seed(123)
index_1<-sample(1:nrow(df6),round(nrow(df6)*0.7))
train_1<-df6[index_1,]
teste_1<-df6[-index_1,]


MR.control<-trainControl(method="cv",number=10)

model_mult_vc <- train(score ~ ., data=train_1,method="lm",trControl=MR.control)

model_mult_vc

estimativas_vc<-predict(model_mult_vc,teste_1)

tabela_vc<-data.frame(VReais=teste_1$score,VPrevistos=estimativas_vc)

tabela_vc$error<-with(tabela_vc,teste_1$score-estimativas_vc)

tabela_vc

MSE_teste_vc<-with(tabela_vc,mean(error^2))
MSE_teste_vc
# Erro Quadrático Médio - 0.08181217

RMSE_teste_vc<-sqrt(MSE_teste)
RMSE_teste_vc
# Raiz Quadrada do Erro Quadrático - 0.2860283

MAE_teste_vc<-with(tabela_vc,mean(abs(error)))
MAE_teste_vc
# Erro Absoluto Médio - 0.2116132

tabela_vc

#Representa Graficamente o melhor modelo
plot(tabela_vc$VReais,
     tabela_vc$VPrevistos,
     main = "Score das estadias em Hong Kong: Previstos vs Reais", 
     xlab = "Reais",
     ylab = "Previstos")
abline(0, 1, col="red")


# Reparamos que o MAE e RMSE são iguais entre os modelos regressão linear multipla e
# e de validação cruzada.


################################################################################
#                             Arvore de Decisão
################################################################################

set.seed(123)
index_1<-sample(1:nrow(df6),round(nrow(df6)*0.7))
train_1<-df6[index_1,]
teste_1<-df6[-index_1,]

model_tree<-rpart(formula = score~.,
                  data=train_1,method="anova",control=rpart.control(xval = 10))

model_tree_party<-as.party(model_tree)
model_tree_party
rpart.plot(model_tree,yesno=TRUE)
printcp(model_tree)
plotcp(model_tree)

#Importância das variáveis
model_tree$variable.importance

#Prever
model_tree_previsao<-predict(model_tree,teste_1,type="vector")
plot(teste_1$score,
     model_tree_previsao,
     main="Árvore model_tree: Previstos vs Reais",
     xlab="Reais",
     ylab="Previstos")
abline(0,1, col="red")

tabela<-data.frame(VReais=teste_1$score,VPrevistos=model_tree_previsao)
tabela$error<-with(tabela,teste_1$score-model_tree_previsao)
tabela

model_tree_rmse<-RMSE(pred=model_tree_previsao,obs=teste_1$score)
model_tree_rmse
# RMSE - 0.2956115

model_tree_mae<-MAE(pred=model_tree_previsao,obs=teste_1$score)
model_tree_mae
# MAE - 0.2151844

#Podar

model_tree_prune <- prune(model_tree, cp=0.025)

printcp(model_tree_prune)
plotcp(model_tree_prune)
model_tree_party<-as.party(model_tree_prune)
model_tree_party
rpart.plot(model_tree_prune,yesno=TRUE)

model_tree_prune_previsao<-predict(model_tree_prune,teste_1,type="vector")

plot(teste_1$score, 
     model_tree_prune_previsao,
     main="Árvore model_tree_prune: Previstos vs Reais",
     xlab="Reais",
     ylab="Previstos")
abline(0,1, col="red")
model_tree_prune$variable.importance

tabela <- data.frame(VReais=teste_1$score,VPrevistos=model_tree_prune_previsao)

tabela$error<-with(tabela,teste_1$score-model_tree_prune_previsao)
tabela

model_tree_prune_rmse<-RMSE(pred=model_tree_prune_previsao,obs=teste_1$score)
model_tree_prune_rmse
# RMSE - 0.2904529

model_tree_prune_mae<-MAE(pred=model_tree_prune_previsao,obs=teste_1$score)
model_tree_prune_mae
# MAE - 0.2168058

################################################################################
#Bagging

cv.control<-trainControl(method="cv",number=10,savePredictions="final")

summary(train_1)

model_bag<-train(score ~.,
                 data=train_1,
                 method="treebag",
                 nbagg=100,
                 metric="RMSE",
                 tuneLength=5,
                 trControl=cv.control)

#substituir o nbagg por 200,300,e 500

model_bag
#   RMSE       Rsquared   MAE      
#   0.2931773  0.2525777  0.2080819

model_bag_previsao<-predict(model_bag,teste_1)

plot(teste_1$score,
     model_bag_previsao,
     main="Classificação prevista com a árvore Bagging - Hipótese 2: Previstos vs Reais",
     xlab="Reais",
     ylab="Previstos")
abline(0,1, col="red")


tabela<-data.frame(VReais=teste_1$score,VPrevistos=model_bag_previsao)

tabela$error<-with(tabela,teste_1$score-model_bag_previsao)

tabela


model_bag_rmse<-RMSE(pred=model_bag_previsao,obs=teste_1$score)
model_bag_rmse
# 0.2875564

model_bag_mae<-MAE(pred=model_bag_previsao,obs=teste_1$score)
model_bag_mae
# 0.2106205

plot(varImp(model_bag),
        main="Importância das Variáveis com o modelo de árvore obtido com o método Bagging")


################################################################################
#Boosting

cv.control<-trainControl(method="cv",number=10,savePredictions="final")
#Final
tune_gbm<-expand.grid(interaction.depth=2,n.trees=1000,shrinkage=0.02,n.minobsinnode=20)

# Tentativa: tune_gbm<-expand.grid(interaction.depth=2,n.trees=1000,shrinkage=0.02,n.minobsinnode=10)
# Tentativa: tune_gbm<-expand.grid(interaction.depth=2,n.trees=500,shrinkage=0.05,n.minobsinnode=15)
# Tentativa: tune_gbm<-expand.grid(interaction.depth=2,n.trees=500,shrinkage=0.05,n.minobsinnode=5)
# Tentativa: tune_gbm<-expand.grid(interaction.depth=2,n.trees=300,shrinkage=0.05,n.minobsinnode=5)

model_boosting<-train(score ~.,data=train_1,method="gbm",trControl=cv.control,tuneGrid=tune_gbm)

model_boosting
model_boosting_previsao<-predict(model_boosting,teste_1)

plot(teste_1$score,
     model_boosting_previsao,main="Árvore obtida com método boosting: Previstos vs reais",
     xlab="Reais", ylab="Previstos")
abline(0,1, col="red")

tabela<-data.frame(VReais=teste_1$score,VPrevistos=model_boosting_previsao)

tabela$error<-with(tabela,teste_1$score-model_boosting_previsao)

tabela

model_boosting_rmse<-RMSE(pred=model_boosting_previsao,obs=teste_1$score)

model_boosting_rmse
# 0.2864961

model_boosting_mae<-MAE(pred=model_boosting_previsao,obs=teste_1$score)

model_boosting_mae
# 0.2087796

plot(varImp(model_boosting),
     main="Importância das Variáveis com o modelo de árvore obtido com gbm")

################################################################################
# Florestas Aleatórias

cv.control<-trainControl(method="cv",number=10,savePredictions="final")

model_forest<-train(score ~.,
                    data=train_1,
                    method="ranger",
                    metric="RMSE",
                    tuneLength=5,
                    importance = 'impurity',
                    trControl=cv.control)

model_forest

model_forest_previsao<-predict(model_forest,teste_1)

plot(teste_1$score,model_forest_previsao,main="Árvore obtida com método Florestas Aleatórias: Previstos vs
Reais",xlab="Reais",ylab="Previstos")

abline(0,1, col="red")

tabela<-data.frame(VReais=teste_1$score,VPrevistos=model_forest_previsao)

tabela$error<-with(tabela,teste_1$score-model_forest_previsao)

tabela

model_forest_rmse<-RMSE(pred=model_forest_previsao,obs=teste_1$score)
model_forest_rmse
# 0.2835549

model_forest_mae<-MAE(pred=model_forest_previsao,obs=teste_1$score)
model_forest_mae
# 0.2028045

plot(varImp(model_forest),
     main="Importância das Variáveis com o modelo de árvore obtido com o método Florestas Aleatórias")


################################################################################
#                             Regressão Neuronal
################################################################################

df7 <- df6

df7$property_type <- as.factor(str_replace(df7$property_type, " ", "_"))

# property_type - dummy
df7 <- dummy_cols(df7, select_columns='property_type')

# score - newValue = originalValue -  minimumValue
                        #_____________________________
#                       maximumValue - minimumValue

# Nos tinhamos duas possibilidades para tratar essa variavel, uma delas era
# dividir pelo valor maximo da variavel, o problema dessa abordagem é que não
# conseguimos pegar toda vantagem do intervalo da variavel, a não ser que 
# tivessemos valore proximos de zero, resolver isso vamos usar a seguinte 
# função sugerida no livro Just Enought R bla bla bla:
# nreviews - newValue = originalValue -  minimumValue
                        #_____________________________
#                       maximumValue - minimumValue
# 

df7$score2 <- (df7$score - min(df7$score)) / (max(df7$score) - min(df7$score))

df7$lognreviews2 <- (df7$lognreviews - min(df7$lognreviews)) / (max(df7$lognreviews) - min(df7$lognreviews))

df7$guest2 <- (df7$guest - min(df7$guest)) / (max(df7$guest) - min(df7$guest))

df7 <- df7 %>% mutate_if(is.factor, as.numeric)

summary(df7)

df8 <- data.frame(
        df7$property_type_condo,
        df7$property_type_home,
        df7$property_type_private_room,
        df7$property_type_serviced_apartment,
        df7$property_type_shared_room,
        df7$score2,
        df7$lognreviews2,
        df7$guest2,
        df7$superhost,
        df7$studio,
        df7$kitchen,
        df7$washer,
        df7$ac,
        df7$balcony,
        df7$hair_dryer,
        df7$breakfast,
        df7$parking,
        df7$smoking,
        df7$bath_private)


df8 <- df8 %>% mutate_if(is.factor, as.numeric)

colnames(df8) <- str_replace(colnames(df8), "df7.", "")

summary(df8)


index_1<-sample(1:nrow(df8),round(nrow(df8)*0.7))
train_1<-df8[index_1,]
teste_1<-df8[-index_1,]


nn1 <- neuralnet(score2 ~ . , 
                 data=train_1, 
                 hidden=c(1,20), 
                 linear.output=FALSE, 
                 threshold =0.01, 
                 algorithm = "backprop", 
                 learningrate = 0.001, 
                 stepmax = 1e7)

#plot(nn1, rep = "best")

nn2 <- neuralnet(score2 ~ . , 
                 data=train_1, 
                 hidden=c(1,30), 
                 linear.output=FALSE, 
                 threshold =0.01, 
                 algorithm = "backprop", 
                 learningrate = 0.001, 
                 stepmax = 1e7)

#plot(nn2, rep = "best")

nn3 <- neuralnet(score2 ~ . , 
                 data=train_1, 
                 hidden=c(1,50), 
                 linear.output=FALSE, 
                 threshold =0.01, 
                 algorithm = "backprop", 
                 learningrate = 0.001, 
                 stepmax = 1e7)

#plot(nn3, rep = "best")

nn4 <- neuralnet(score2 ~ . , 
                 data=train_1, 
                 hidden=c(10,10), 
                 linear.output=FALSE, 
                 threshold =0.01, 
                 algorithm = "backprop", 
                 learningrate = 0.001, 
                 stepmax = 1e7)

#plot(nn4, rep = "best")

nn5 <- neuralnet(score2 ~ . , 
                 data=train_1, 
                 hidden=c(20,20), 
                 linear.output=FALSE, 
                 threshold =0.01, 
                 algorithm = "backprop", 
                 learningrate = 0.001, 
                 stepmax = 1e7)


#plot(nn5, rep = "best")


nn1.results <- predict(nn1, teste_1)
nn2.results <- predict(nn2, teste_1)
nn3.results <- predict(nn3, teste_1)
nn4.results <- predict(nn4, teste_1)
nn5.results <- predict(nn5, teste_1)

actual <- teste_1$score2

result1 = rmse(actual, nn1.results)
result1_2 = mape(actual, nn1.results)

result2 = rmse(actual, nn2.results)
result2_2 = mape(actual, nn2.results)

result3 = rmse(actual, nn3.results)
result4_2 = mape(actual, nn3.results)

result4 = rmse(actual, nn4.results)
result4_2 = mape(actual, nn4.results)

result5 = rmse(actual, nn5.results)
result5_2 = mape(actual, nn5.results)

result1
result2
result3
result4
result5

# result2 - O melhor: 0.1114887

plot((nn2.results), type = "b", frame = FALSE, pch = 19, 
     col = "red", xlab = "x", ylab = "y")
# Add a second line
lines(actual, pch = 18, col = "blue", type = "b", lty = 2)
## Add a legend to the plot
legend("topleft", legend=c("real", "Prediction nnet"),
       col=c("red", "blue"), lty = 1:2, cex=0.8)


plot(teste_1$score2, nn2.results, pch=16, ylab = "predicted score NN", xlab = "real score")
abline(0,1, col='red')


#Vamos remover algumas variáveis e correr novamente a rede neuronal

df9 <- data.frame(
        df7$property_type_condo,
        df7$property_type_home,
        df7$property_type_private_room,
        df7$property_type_serviced_apartment,
        df7$property_type_shared_room,
        df7$score2,
        df7$lognreviews2,
        df7$guest2,
        df7$superhost,
        df7$kitchen,
        df7$breakfast,
        df7$smoking)


df9 <- df9 %>% mutate_if(is.factor, as.numeric)

df9 <- df9 %>% mutate_if(is.character, as.numeric)

colnames(df9) <- str_replace(colnames(df9), "df7.", "")

summary(df9)

index_1_2<-sample(1:nrow(df9),round(nrow(df9)*0.7))
train_1_2<-df9[index_1,]
teste_1_2<-df9[-index_1,]


nn1_2 <- neuralnet(score2 ~ . , 
                   data=train_1_2, 
                   hidden=c(1,20), 
                   linear.output=FALSE, 
                   threshold =0.01, 
                   algorithm = "backprop", 
                   learningrate = 0.001, 
                   stepmax = 1e7)

#plot(nn1_2, rep = "best")

nn2_2 <- neuralnet(score2 ~ . , 
                   data=train_1_2, 
                   hidden=c(1,30), 
                   linear.output=FALSE, 
                   threshold =0.01, 
                   algorithm = "backprop", 
                   learningrate = 0.001, 
                   stepmax = 1e7)

#plot(nn2_2, rep = "best")

nn3_2 <- neuralnet(score2 ~ . , 
                   data=train_1_2, 
                   hidden=c(1,50), 
                   linear.output=FALSE, 
                   threshold =0.01, 
                   algorithm = "backprop", 
                   learningrate = 0.001, 
                   stepmax = 1e7)

#plot(nn3_2, rep = "best")

nn4_2 <- neuralnet(score2 ~ . , 
                   data=train_1_2, 
                   hidden=c(10,10), 
                   linear.output=FALSE, 
                   threshold =0.01, 
                   algorithm = "backprop", 
                   learningrate = 0.001, 
                   stepmax = 1e7)

#plot(nn4_2, rep = "best")

nn5_2 <- neuralnet(score2 ~ . , 
                   data=train_1_2, 
                   hidden=c(20,20), 
                   linear.output=FALSE, 
                   threshold =0.01, 
                   algorithm = "backprop", 
                   learningrate = 0.001, 
                   stepmax = 1e7)


#plot(nn5_2, rep = "best")


nn1_2.results <- predict(nn1_2, teste_1_2)
nn2_2.results <- predict(nn2_2, teste_1_2)
nn3_2.results <- predict(nn3_2, teste_1_2)
nn4_2.results <- predict(nn4_2, teste_1_2)
nn5_2.results <- predict(nn5_2, teste_1_2)

actual <- teste_1_2$score2

result1_2 = rmse(actual, nn1_2.results)
result1_2_2 = mape(actual, nn1_2.results)

result2_2 = rmse(actual, nn2_2.results)
result2_2_2 = mape(actual, nn2_2.results)

result3_2 = rmse(actual, nn3_2.results)
result4_2_2 = mape(actual, nn3_2.results)

result4_2 = rmse(actual, nn4_2.results)
result4_2_2 = mape(actual, nn4_2.results)

result5_2 = rmse(actual, nn5_2.results)
result5_2_2 = mape(actual, nn5_2.results)

result1_2
result2_2
result3_2
result4_2
result5_2

# > result1
# [1] 0.1114887
# > result2
# [1] 0.1114224
# > result3
# [1] 0.1271665
# > result4
# [1] 0.1357598
# > result5
# [1] 0.1293559

# > result1_2
# [1] 0.1174681
# > result2_2
# [1] 0.1173443
# > result3_2
# [1] 0.1173191
# > result4_2
# [1] 0.1157172
# > result5_2
# [1] 0.132551


# Escolhemos o mais simples (com menos variaveis e hidden=c(1,30)), 
# pois a diferença é pouco
# > result2_2
# [1] 0.1173443


#Representação dos pesos generalizados
gwplot(nn2_2, selected.covariate= "superhost")
gwplot(nn2_2, selected.covariate="property_type_shared_room")
gwplot(nn2_2, selected.covariate="property_type_private_room")
gwplot(nn2_2, selected.covariate="score2")
gwplot(nn2_2, selected.covariate="breakfast")
gwplot(nn2_2, selected.covariate="guest2")
gwplot(nn2_2, selected.covariate="smoking")
gwplot(nn2_2, selected.covariate="kitchen")
gwplot(nn2_2, selected.covariate="lognreviews2")
gwplot(nn2_2, selected.covariate="property_type_shared_room")

plot((nn2_2.results), type = "b", frame = FALSE, pch = 19, 
     col = "red", xlab = "x", ylab = "y")
lines(actual, pch = 18, col = "blue", type = "b", lty = 2)
legend("topleft", legend=c("real", "Prediction nnet"),
       col=c("red", "blue"), lty = 1:2, cex=0.8)


plot(teste_1_2$score2, nn2_2.results, pch=16, ylab = "predicted score NN", xlab = "real score")
abline(0,1, col='red')

resume <- data.frame( rbind(
        c("Regressão Linear - Multipla", RMSE_teste_vc),
        c("Regressão Linear - Multipla (Validação Cruzada)", RMSE_teste_vc),
        c("Árvore de Decisão", model_tree_prune_rmse),
        c("Árvore de Decisão - Bagging", model_bag_rmse),
        c("Árvore de Decisão - Boosting", model_boosting_rmse),
        c("Árvore de Decisão - Florestas Aleatórias", model_forest_rmse),
        c("Rede Neuronal", result2_2)))

resume

#                     Regressão Linear - Multipla 0.286028261123184
# Regressão Linear - Multipla (Validação Cruzada) 0.286028261123184
#                               Árvore de Decisão 0.290452916909531
#                     Árvore de Decisão - Bagging 0.288735645777365
#                    Árvore de Decisão - Boosting 0.287419100188278
#        Árvore de Decisão - Florestas Aleatórias 0.288836492255406
#                                   Rede Neuronal 0.117344312509187


# Melhor modelo: 
# Rede Neuronal, com hidden = 30 e com a scopo reduzido de variáveis
resume[resume$X2 == min(resume$X2),]

# Rede Neuronal 0.117344312509187




