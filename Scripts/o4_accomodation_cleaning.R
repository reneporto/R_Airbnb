
################################################################################
# Tema:   Previsão do score atribuído por reviewers em alojamentos locais
################################################################################
# Alunos: Guilherme Mendonca  - 82575   - gasvm@iscte-iul.pt
#         Marta Neves         - 88660   - msmns@iscte-iul.pt
#         René Porto          - 101597  - rapfr@iscte-iul.pt
################################################################################
# Nome:       accomodation_cleaning.R
# Descriacao: Esse script faz a transformação, limpeza e uma exploração 
#             preliminar dos valores extraidos via web scrapping
#
# Como usar:  Atualizar as variaveis "caminho_in" com o local do ficheiro 
#             "accom_var.csv" resultante do script "accomodation_variables.R" 
#             e a variavel "caminho_out" com o local onde deseja guardar o 
#             ficheiro "accom_clean.csv", por padrao esta sendo salvo no "C:\".
################################################################################

library(rvest)
library(stringr)
library(ggplot2)
library(dplyr)

caminho_in <- "C:\\Users\\Rene Porto\\Documents\\Trabalho Airbnb\\accom_var.csv"
caminho_out <- "C:\\Users\\Rene Porto\\Documents\\Trabalho Airbnb\\accom_clean.csv"


accom <- read.csv(caminho_in, header=TRUE, sep=",", dec=",")

#Tratamento de dados: Tipo de Acomodação
property_type_txt <- str_extract(accom$title, ".*by")
property_type_txt <- str_remove(property_type_txt, " hosted by")
property_type_txt <- str_replace(property_type_txt, "Entire rental unit*", "home")
property_type_txt <- str_replace(property_type_txt, "Entire place*", "home")
property_type_txt <- ifelse(str_detect(property_type_txt, "in"),
                            str_extract(property_type_txt, ".* in"),
                            property_type_txt)
property_type_txt <- str_remove(property_type_txt, " in")
property_type_txt <- str_remove(property_type_txt, "Entire ")
property_type_txt <- str_remove(property_type_txt, " unit")
property_type_txt <- str_remove(property_type_txt, "Abby")
property_type_txt <- str_remove(property_type_txt, "w")
property_type_txt <- str_remove(property_type_txt, " rental")
property_type_txt <- str_remove(property_type_txt, " casa particular")
property_type_txt <- str_remove(property_type_txt, "Libby")
property_type_txt <- str_remove(property_type_txt, "Nebby")
property_type_txt <- str_remove(property_type_txt, "Noby")
property_type_txt <- str_remove(property_type_txt, "Ruby")
property_type_txt <- str_remove(property_type_txt, "kezhan")
property_type_txt <- str_remove(property_type_txt, "stay")
property_type_txt <- str_replace(property_type_txt, "home/apt", "home")
property_type_txt <- str_replace(property_type_txt, "Houseboat", "boat")
property_type_txt <- str_replace(property_type_txt, "residential home", "home")
property_type_txt <- str_replace(property_type_txt, "tonhouse", "home")
property_type_txt <- ifelse(property_type_txt == "Room", 
                            str_replace(property_type_txt, 
                                        "Room", 
                                        "private room"), 
                            property_type_txt) 
property_type_txt <- str_trim(property_type_txt)
property_type_txt <- str_to_lower(property_type_txt)
accom$property_type <- property_type_txt

accom$property_type <- ifelse(is.na(accom$property_type), 
                              ifelse(str_detect(accom$title, "condo"), "condo", NA), 
                              accom$property_type)

accom$property_type <- ifelse(is.na(accom$property_type), 
                              ifelse(str_detect(accom$title, "Tiny house"), "tiny house", NA), 
                              accom$property_type)


# Converter score para numeric
accom$score <- as.numeric(accom$score)

# Temos 1715 nreviews nulos
length(accom[(is.na(accom$nreviews)),1])

# Eliminar nreviews nulos
accom <- accom[(!is.na(accom$nreviews)),]

# Substituir os scores nulos por 0
accom$score <- ifelse(is.na(accom$score),0, accom$score)

# Temos 28 price nulos
length(accom[(is.na(accom$price)),1])

# Eliminar price nulos
accom <- accom[(!is.na(accom$price)),]

accom$nrbath <- ifelse(accom$bath_shared>0,accom$bath_shared, accom$bath)
accom$bath_private <- as.factor(ifelse(accom$bath_shared>0,0, 1))


#Analise: Tipo Acomodação
aggregate(accom$url, by=list(accom$property_type), FUN=length)
resume_type_accom <- data.frame(aggregate(accom$url, by=list(accom$property_type), FUN=length))
resume_type_accom$perc <- round(((resume_type_accom$x / length(accom$url)) * 100), digits=2)

resume_type_accom2 <- rbind(
  resume_type_accom,
  c("Other",
    sum(resume_type_accom[resume_type_accom$perc < 5, 2]),
    sum(resume_type_accom[resume_type_accom$perc < 5, 3])))

resume_type_accom2 <- resume_type_accom2[!(resume_type_accom2$perc < 1),]

resume_type_accom2

#Graficos
data <- data.frame(
  group=resume_type_accom2$Group.1,
  value=as.numeric(resume_type_accom2$x)
)

data <- data %>% 
  arrange(desc(group)) %>%
  mutate(prop = value / sum(data$value) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )

ggplot(data, aes(x="", y=prop, fill=group)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() +
  scale_fill_discrete(name = "Accomodation type") +
  ggtitle("Tipo de acomodações (%)") +
  geom_text(aes(y = ypos, label = round(prop, digits=2)), color = "white", size=4) +
  theme(plot.title = element_text(hjust = 0.5, size=20))

resume_type_accom

property_type_del <- resume_type_accom[resume_type_accom$x <= 100, 1]


length(accom[,1])
# 2623

# Apagar os property_type com poucos registos
accom <- accom[!accom$property_type %in% property_type_del,]

length(accom[,1])
# 2485

# Tiramos os scores = 0
accom <- accom[(accom$score!=0),]

length(accom[,1])
# 2214

summary(accom)
#Como temos o mesmo valor para a variavel heating e free_cancel, decidimos retirar

accom_data <- data.frame(
  accom$property_type,
  accom$studio,
  accom$score,
  accom$nreviews,
  accom$price,
  accom$guest,
  accom$bedroom,
  accom$bed,
  accom$nrbath,
  accom$bath_private,
  accom$superhost,
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
  accom$smoking
)

write.csv(accom_data, caminho_out, row.names = FALSE)
