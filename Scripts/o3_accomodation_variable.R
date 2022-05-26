
################################################################################
# Tema:   Previsão do score atribuído por reviewers em alojamentos locais
################################################################################
# Alunos: Guilherme Mendonca  - 82575   - gasvm@iscte-iul.pt
#         Marta Neves         - 88660   - msmns@iscte-iul.pt
#         René Porto          - 101597  - rapfr@iscte-iul.pt
################################################################################
# Nome:       accomodation_variables.R
# Descriacao: Esse script faz o web scrapping das variaveis das acomodações 
#             e faz uma pré-limpeza dos dados antes das criações dos modelos
#
# Requisitos: Java (JRE)
#             R Selenium
#             Firefox - Versão 93.0 (64-bit)
#
# Como usar:  Atualizar as variaveis "caminho_accom_in" com o local do ficheiro 
#             "url.csv" resultante do script "url_search.R" e a variavel
#             "caminho_out" com o local onde deseja guardar o ficheiro 
#             "accom_var.csv", por padrao esta sendo salvo no "C:\".
################################################################################

#To do
#Coloca a porta do drive dinamicamente ou liberar a porta
#Pegar os valores obrigatorios
#Salvar URL's em ficheiros csv
#Remover quartos duplicados

library(RSelenium)
library(rvest)
library(stringr)

caminho_accom_in <- "C:\\Users\\Rene Porto\\Documents\\Trabalho Airbnb\\accom.csv"

caminho_accom_out <- "C:\\Users\\Rene Porto\\Documents\\Trabalho Airbnb\\accom_var.csv"

url_airbnb_city <- read.csv(caminho_accom_in, header=TRUE, sep=",", dec=",")

url_airbnb_city <- url_airbnb_city[,1]

length(url_airbnb_city)

nr_accom <- str_extract(str_extract(url_airbnb_city, ".{20}check."), "\\d+")

remove_duplicate <- data.frame(cbind(
  nr_accom,
  url_airbnb_city))

remove_duplicate$nr_accom[duplicated(remove_duplicate$nr_accom)] <- ""

url_airbnb_city_aux<-remove_duplicate[!(remove_duplicate$nr_accom==""),]

length(url_airbnb_city_aux[,2])

url_airbnb_city <- url_airbnb_city_aux[,2]

url <- vector()
type_acom <- vector()
studio <- vector()
score <- vector()
nreviews <- vector()
price <- vector()
kitchen <- vector()
wifi <- vector()
tv <- vector()
elevator <- vector()
washer <- vector()
dryer <- vector()
ac <- vector()
balcony <- vector()
luggage_do <- vector()
hair_dryer <- vector()
guest <- vector()
bed <- vector()
bedroom <- vector()
bath <- vector()
bath_shared <- vector()
superhost <- vector()
title <- vector()
pet <- vector()
fire_ext <- vector()
first_aid <- vector()
breakfast <- vector()
heating <- vector()
parking <- vector()
smoking <- vector()
free_cancel <- vector()

driver <- rsDriver(browser="firefox", port=4594L, verbose=F)
remDr <- driver[["client"]]

for (k in 1:length(url_airbnb_city)) {
#for (k in 1:3) {
  
  url <- c(url, paste0(url_airbnb_city[k], "&locale=en"))
  
  print(paste0(url_airbnb_city[k], "&locale=en"))
  
  remDr$navigate(paste0(url_airbnb_city[k], "&locale=en"))
  Sys.sleep(5)
  
  scroll <- NULL
  scroll_count <- 0
  while (is.null(scroll)) {
    scroll <- 
      tryCatch(
        {remDr$findElement("css", "body")},
        error = function(e){NULL})
    
    scroll$sendKeysToElement(list(key = "end"))
    
    if (scroll_count == 3) {
      break()
    }
  }
  
  Sys.sleep(3)

  page_source <- remDr$getPageSource()[[1]]
  page_html <- read_html(page_source)
  
  ##############################################################################
    
    type_acomTxt <- html_elements(page_html, 
                                  xpath = "//div[@class='_1qsawv5']//text()")[1]
    
    type_acom <- c(type_acom, ifelse(!is.null(as.character(type_acomTxt)),as.character(type_acomTxt), "NULL")) 
    
    print(paste("type_acom -",type_acom[k]))
    
    page_source <- remDr$getPageSource()[[1]]
    page_html <- read_html(page_source)
    
  ##############################################################################
  
  scoreTxt <- 
    str_extract_all(paste(page_html, collapse=" "), 
                    "\"Rated [0-9]\\.[0-9][0-9]|\"Rated [0-9]\\.[0-9]")
  
    score_pre <- as.numeric(substring(scoreTxt[[1]][1], nchar("\"Rated ")+1))
  
  score <- c(score, ifelse(!is.null(score_pre), score_pre, "NULL"))
  
  print(paste("score -",score[k]))
  ##############################################################################
  
  nrevsTxt <- 
    str_extract_all(paste(page_html, collapse=" "), 
                    "[0-9]+ reviews\"")
  nreviews_pre <- as.integer(substring(nrevsTxt[[1]][1], 1,
                           nchar(nrevsTxt[[1]][1])-nchar(" reviews\"")))
  
  nreviews <- c(nreviews, ifelse(!is.null(nreviews_pre), nreviews_pre, "NULL"))
  
  print(paste("nreviews -",nreviews[k]))
  ##############################################################################
  
  priceTxt <- html_elements(page_html, 
                                xpath = "//span[@class='_tyxjp1']//text()")[1]
  price_pre <- as.numeric(str_remove(priceTxt, "???"))
  price <- c(price, ifelse(!is.null(price_pre),price_pre, "NULL"))
  
  print(paste("price -",price[k]))
  
  
  #priceTxt <- 
  #  str_extract_all(paste(page_html, collapse=" "), 
  #                  "\"???[0-9]|\"???[0-9][0-9]|\"???[0-9][0-9][0-9]|\"???[0-9][0-9][0-9][0-9]|\"???[0-9][0-9][0-9][0-9][0-9]")
  #price <- c(price, priceTxt)
  #
  #print(paste("price -",price[k]))
  ##############################################################################
  
  guestTxt <- str_extract_all(paste(page_html, collapse=" "), 
                              '([0-9]\\sguest.|[0-9][0-9]\\sguest.)')
  guestNr <- as.integer(substr(guestTxt[[1]][1], 1, 2))
  guest <- c(guest, ifelse(is.na(guestNr),1, guestNr))
  
  print(paste("guest -",guest[k]))
  ##############################################################################
  
  bedroomTxt <- 
    str_extract_all(paste(page_html, collapse=" "), 
                    '([0-9]\\sbedroom.|[0-9][0-9]\\sbedroom.)')
  bedroomNr <- as.integer(substr(bedroomTxt[[1]][1], 1, 2))
  bedroom <- c(bedroom, ifelse(is.na(bedroomNr),1, bedroomNr))
  
  print(paste("bedroom -",bedroom[k]))
  ##############################################################################
  
  bathTxt <- 
    str_extract_all(paste(page_html, collapse=" "), 
                    '([0-9]\\sbath.|[0-9][0-9]\\sbath.)')
  bathNr <- as.integer(substr(bathTxt[[1]][1], 1, 2))
  bath <- c(bath, ifelse(is.na(bathNr),1, bathNr))
  
  print(paste("bath -",bath[k]))
  ##############################################################################
  
  bath_sharedTxt <- 
    str_extract_all(paste(page_html, collapse=" "), 
                    '([0-9]\\sshared\\sbath.|[0-9][0-9]\\sshared\\sbath.)')
  bath_sharedNr <- as.integer(substr(bath_sharedTxt[[1]][1], 1, 2))
  bath_shared <- c(bath_shared, ifelse(is.na(bath_sharedNr),0, bath_sharedNr))
  
  print(paste("bath_shared -",bath_shared[k]))
  ##############################################################################

  bedTxt <- 
    str_extract_all(
      paste(page_html, collapse=" "),
      '((?![0-9]\\sbedroom.|[0-9][0-9]\\sbedroom.)[0-9]\\sbed|[0-9][0-9]\\sbed)')
  
  bedNr <- as.integer(substr(bedTxt[[1]][1], 1, 2))
  bed <- c(bed, ifelse(is.na(bedNr),1, bedNr))
  
  print(paste("bed -",bed[k]))
  ##############################################################################
  
  superhost_grid <- html_elements(page_html, 
                              xpath = "//div[@class='_1qdp1ym']//text()")
  superhost <-
    c(superhost, 
      ifelse(sum(ifelse(str_detect(superhost_grid, "Superhost"),1,0))>=1,1,0))
  
  print(paste("superhost -",superhost[k]))
  ##############################################################################
  
  title_txt <- html_elements(page_html, 
                                xpath = "//h2[@class='_14i3z6h']//text()")[1]
  title <- c(title, 
             ifelse(!is.null(as.character(title_txt)), 
                    as.character(title_txt), "NULL"))
  
  print(paste("title -",title[k]))
  
  ##############################################################################
  
  studio <- 
    c(studio, ifelse(sum(ifelse(str_detect(page_html, "Studio"),1,0))>=1,1,0))
  
  print(paste("studio -",studio[k]))
  
  ##############################################################################
  ##############################################################################
  
  offer_grid <- html_elements(page_html, 
                                xpath = "//div[@class='_1byskwn']//text()")
  
  ##############################################################################
  
  kitchen <- 
    c(kitchen, 
      ifelse(sum(ifelse(str_detect(offer_grid, "Kitchen"),1,0))>=1,1,0))
  
  print(paste("kitchen -",kitchen[k]))
  ##############################################################################
  
  wifi <- 
    c(wifi, ifelse(sum(ifelse(str_detect(offer_grid, "Wifi"),1,0))>=1,1,0))
  
  print(paste("wifi -",wifi[k]))
  ##############################################################################
  
  tv <- 
    c(tv, 
      ifelse(sum(ifelse(str_detect(offer_grid, "TV"),1,0))>=1,1,0))
  
  print(paste("tv -",tv[k]))
  ##############################################################################
  
  elevator <- 
    c(elevator, 
      ifelse(sum(ifelse(str_detect(offer_grid, "Elevator"),1,0))>=1,1,0))
  
  print(paste("elevator -",elevator[k]))
  ##############################################################################
  
  washer <- 
    c(washer, 
      ifelse(sum(ifelse(str_detect(offer_grid, "Washer"),1,0))>=1,1,0))
  
  print(paste("washer -",washer[k]))
  ##############################################################################
  
  dryer <- 
    c(dryer, 
      ifelse(sum(ifelse(str_detect(offer_grid, "Dryer"),1,0))>=1,1,0))
  
  print(paste("dryer -",dryer[k]))
  ##############################################################################
  
  ac <- 
    c(ac, 
      ifelse(
        sum(ifelse(str_detect(offer_grid, "Air conditioning"),1,0))>=1,1,0))
  
  print(paste("ac -",ac[k]))
  ##############################################################################
  
  balcony <- 
    c(balcony, 
      ifelse(
        sum(ifelse(str_detect(offer_grid, "Patio or balcony"),1,0))>=1,1,0))
  
  print(paste("balcony -",balcony[k]))
  ##############################################################################
  
  luggage_do <- 
    c(luggage_do, 
      ifelse(
        sum(ifelse(
          str_detect(offer_grid, "Luggage dropoff allowed"),1,0))>=1,1,0))
  
  print(paste("luggage_do -",luggage_do[k]))
  ##############################################################################
  
  hair_dryer <- 
    c(hair_dryer, 
      ifelse(sum(ifelse(str_detect(offer_grid, "Hair dryer"),1,0))>=1,1,0))
  
  print(paste("hair_dryer -",hair_dryer[k]))
  ##############################################################################
  
  pet <- 
    c(pet, 
      ifelse(sum(ifelse(str_detect(offer_grid, "Pets allowed"),1,0))>=1,1,0))
  
  print(paste("pet -",pet[k]))
  ##############################################################################
  
  fire_ext <- 
    c(fire_ext, 
      ifelse(
        sum(ifelse(str_detect(offer_grid, "Fire extinguisher"),1,0))>=1,1,0))
  
  print(paste("fire_ext -",fire_ext[k]))
  ##############################################################################
  
  first_aid <- 
    c(first_aid, 
      ifelse(sum(ifelse(str_detect(offer_grid, "First aid kit"),1,0))>=1,1,0))
  
  print(paste("first_aid -",first_aid[k]))
  ##############################################################################
  
  breakfast <- 
    c(breakfast, 
      ifelse(sum(ifelse(str_detect(offer_grid, "Breakfast"),1,0))>=1,1,0))
  
  print(paste("breakfast -",breakfast[k]))
  ##############################################################################
  
  heating <- 
    c(heating, 
      ifelse(sum(ifelse(str_detect(offer_grid, "Central heating"),1,0))>=1,1,0))
  
  print(paste("heating -",heating[k]))
  ##############################################################################
  
  parking <- 
    c(parking, 
      ifelse(sum(
        ifelse(str_detect(offer_grid, "Free parking on premises"),1,0))>=1,1,0))
  
  print(paste("parking -",parking[k]))
  ##############################################################################
  
  smoking <- 
    c(smoking, 
      ifelse(sum(ifelse(str_detect(offer_grid, "Smoking allowed"),1,0))>=1,1,0))
  
  print(paste("smoking -",smoking[k]))
  ##############################################################################
  
  free_cancel <- 
    c(free_cancel, 
      ifelse(sum(
        ifelse(str_detect(offer_grid, "Free cancellation"),1,0))>=1,1,0))
  
  
  ##############################################################################
  print("---------------------------------------------------------------------")

}

df <- data.frame(cbind(
  url,
  type_acom,
  score,
  nreviews,
  price,
  guest,
  bedroom,
  bath,
  bath_shared,
  bed,
  superhost,
  title,
  studio,
  kitchen,
  wifi,
  tv,
  elevator,
  washer,
  dryer,
  ac,
  balcony,
  luggage_do,
  hair_dryer,
  pet,
  fire_ext,
  first_aid,
  breakfast,
  heating,
  parking,
  smoking,
  free_cancel
))

write.csv(df, caminho_accom_out, row.names = FALSE)

print("Valores length:")
print("--")
print(paste("type_acom -",length(type_acom)))
print(paste("score -",length(score)))
print(paste("nreviews -",length(nreviews)))
print(paste("price -",length(price)))
print(paste("guest -",length(guest)))
print(paste("bedroom -",length(bedroom)))
print(paste("bath -",length(bath)))
print(paste("bath_shared -",length(bath_shared)))
print(paste("bed -",length(bed)))
print(paste("superhost -",length(superhost)))
print(paste("title -",length(title)))
print(paste("studio -",length(studio)))
print(paste("kitchen -",length(kitchen)))
print(paste("wifi -",length(wifi)))
print(paste("tv -",length(tv)))
print(paste("elevator -",length(elevator)))
print(paste("washer -",length(washer)))
print(paste("dryer -",length(dryer)))
print(paste("ac -",length(ac)))
print(paste("balcony -",length(balcony)))
print(paste("luggage_do -",length(luggage_do)))
print(paste("hair_dryer -",length(hair_dryer)))
print(paste("pet -",length(pet)))
print(paste("fire_ext -",length(fire_ext)))
print(paste("first_aid -",length(first_aid)))
print(paste("breakfast -",length(breakfast)))
print(paste("heating -",length(heating)))
print(paste("parking -",length(parking)))
print(paste("smoking -",length(smoking)))
print(paste("free_cancel -",length(free_cancel)))


