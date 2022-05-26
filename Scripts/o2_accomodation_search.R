
################################################################################
# Trabalho: Airbnb - Hong Kong
################################################################################
# Tema:   Previsão do score atribuído por reviewers em alojamentos locais
################################################################################
# Alunos: Guilherme Mendonca  - 82575   - gasvm@iscte-iul.pt
#         Marta Neves         - 88660   - msmns@iscte-iul.pt
#         René Porto          - 101597  - rapfr@iscte-iul.pt
################################################################################
# Nome:       accomodation_search.R
# Descriacao: Esse script tem o objetivo de buscar URL's das acomodacoes de
#             Hong Kong usando o ficheiro "url.csv" extraido no script 
#             url_search.R, e criar o ficheiro "accom.csv".
#
# Requisitos: Java (JRE)
#             R Selenium
#             Firefox - Versão 93.0 (64-bit)
#
# Como usar:  Atualizar as variaveis "caminho_in" com o local do ficheiro 
#             "url.csv" resultante do script "url_search.R" e a variavel
#             "caminho_out" com o local onde deseja guardar o ficheiro 
#             "accom.csv", por padrao esta sendo salvo no "C:\".
################################################################################

library(RSelenium)
library(rvest)
library(stringr)

caminho_in <- "C:\\Users\\Rene Porto\\Documents\\Trabalho Airbnb\\url.csv"
caminho_out <- "C:\\Users\\Rene Porto\\Documents\\Trabalho Airbnb\\accom.csv"

url_airbnb_city <- read.csv(caminho_in, header=TRUE, sep=",", dec=",")
url_airbnb_city

driver <- rsDriver(browser="firefox", port=4590L, verbose=F)
remDr <- driver[["client"]]

accomodationURL <- vector()

# Este ciclo percorre todas as URL's de pesquisa, paginas de acomodações
# e armazena a URL das acomodações
for (r in 1:length(url_airbnb_city[1:length(url_airbnb_city[,]),])){
  print(paste("URL -",r))
  print(Sys.time())
  print(paste("Qtd acomodacoes salvas -", length(accomodationURL)))
  for (i in seq(from=0,to=280,by=20)) {
    print(paste0(url_airbnb_city[r,],"&items_offset=",i))
    
    webElem <-NULL
    var_count <- 0
    while(is.null(webElem) || length(webElem) == 0){
      var_count <- var_count + 1

      remDr$navigate(paste0(url_airbnb_city[r,],"&items_offset=",i))

      Sys.sleep(5)
      
      if (var_count > 1 && var_count < 3){
        Sys.sleep(5)
      }
      else if (var_count >= 3) {
        break()
      }
      
      var_count2 <- 0
      
      while(is.null(webElem) || 
            (length(webElem) >= 0 && 
             length(webElem) < 20)) {
        var_count2 <- var_count2 + 1
        Sys.sleep(1)
        webElem <- 
          tryCatch(
            {remDr$findElements(
              using = "xpath", 
              '//div[@itemprop="itemListElement"]/meta[@itemprop="url"]')},
            error = function(e){NULL})
        
        if (var_count2 == 3) {
          break()
        }
        
      }
      print(paste("Qtd acomodacoes -", length(webElem)))
      
      for (j in 1:length(webElem)) {
        accomodationURL <-
          tryCatch(
            {c(accomodationURL, 
               paste0("https://",
                      webElem[[j]]$getElementAttribute("content")[[1]]))},
            error = function(e){accomodationURL})
      }
    }
  }
}

write.csv(accomodationURL, caminho_out, row.names = FALSE)
