
################################################################################
# Trabalho: Airbnb - Hong Kong
################################################################################
# Tema:   Previsão do score atribuído por reviewers em alojamentos locais
################################################################################
# Alunos: Guilherme Mendonca  - 82575   - gasvm@iscte-iul.pt
#         Marta Neves         - 88660   - msmns@iscte-iul.pt
#         René Porto          - 101597  - rapfr@iscte-iul.pt
################################################################################
# Nome:       url_search.R
# Descriacao: Esse script tem o objetivo de criar URL's de busca concatenando
#
# Como usar:  Atualizar a variavel "caminho" para um ficheiro de sua escolha,
#             por padrao esta sendo salvo no "C:\"
################################################################################

caminho <- "C:\\Users\\Rene Porto\\Documents\\Trabalho Airbnb\\url.csv"

url <- "https://www.airbnb.com/s/Hong-Kong/homes?locale=en"

checkin <- "&checkin="
checkout <- "&checkout="

checkin_list <- vector()
checkout_list <- vector()
dates_list <- vector()

first_checkin <- as.Date("2022-01-01")
first_checkout <- first_checkin + 5

# Escolhemos esse período, pois conseguimos buscar ao menos uma semana 
# de todos meses, assim evitando a ausência de alguma acomodação que esteja 
# reservada, alem de passar por todas as estações do ano.

for (i in seq(from=0, to=70, by=5)){
  checkin_list <- c(checkin_list, paste0(checkin,first_checkin + i))
  checkout_list <- c(checkout_list, paste0(checkout, first_checkout + i))
  
  first_checkin <- first_checkin + 20
  first_checkout <- first_checkout + 20
  
}
dates_list <- c(dates_list, paste0(checkin_list, checkout_list))
dates_list

pricemin <- "&price_min="
pricemax <- "&price_max="
prices_list <- vector()

for(i in seq(from=0, to=500, by=100)){
  prices_list <- 
    c(prices_list, paste0(pricemin, 
                          as.character(i+1), 
                          pricemax, 
                          as.character(i+100)))
}

# Incluir acomodações com valores maiores que 601
prices_list <- c(prices_list, paste0(pricemin, "311"))
prices_list

url <- c(paste0(url,dates_list))

url_search <- vector()

for (j in 1:length(url)){
  for (k in 1:length(prices_list)){
    url_search <- c(url_search, paste0(url[j], prices_list[k]))
  }
}

url_search
write.csv(url_search, caminho, row.names = FALSE)
