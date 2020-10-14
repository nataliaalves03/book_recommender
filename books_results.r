library(tidyverse)
library(rvest)
library(DT)


##Função para obter as informações do livro no site Book Finder

getItemInfo = function(isbn){

  url_site = "https://www.bookfinder.com/search/?author=&title=&lang=en&new_used=*&destination=br&currency=BRL&binding=*&keywords=&minprice=&maxprice=&publisher=&min_year=&max_year=&mode=advanced&st=sr&ac=qr&isbn="
  url_item = paste(url_site, isbn, sep = "")
  
  html_page = read_html(url_item)
  
  title = html_page %>% html_node("#describe-isbn-title") %>% html_text()
  author = html_page %>% html_node("span[itemprop='author']") %>% html_text()
  image = html_page %>% html_node("#coverImage") %>% html_attr("src")
  
  info = data.frame(title = title, author = author, image = image, isbn = isbn)
  
  return(info)
}

getItensInfo = function(itens){
  df = tibble(title = NA, author = NA, image = NA, isbn = NA)
  for(item in itens){
    info = getItemInfo(item)
    df = df %>% add_row(info)
    print(item)
  }
  return(drop_na(df))
}


##Plot recomendações

plotRecomendacoes = function(itens_info){
  itens_info %>% mutate(image = paste('<img height="100" src="', image, '"></img>', sep="")) %>% 
    mutate(description = paste('<b>',title,'</b><br/>por ',author, sep="")) %>%
    select(image, description) %>% 
    datatable(class = "hover row-border", escape = FALSE, 
              options = list(dom = 't',scrollX = TRUE, autoWidth = TRUE),
              colnames = c("Imagem","Livro"))
}



##########################################

##Recomendações geradas pelo modelo


# usuario ge6bad6218898ef2dc8cb61197af4178d

itens_user1 = c("1432842323", "1503942791", "0545582881", "0141331976", "197413198x")
itens_info1 = getItensInfo(itens_user1) 
plotRecomendacoes(itens_info1)

# usuario gf05c07b444138f3c21b4925fa0669edf

itens_user2 = c("1503951634", "0399151575", "1987987527", "0140569324", "0002318350")
itens_info2 = getItensInfo(itens_user2) 
plotRecomendacoes(itens_info2)

# usuario gf5807b417b6b71410574f74180789c34

itens_user3 = c("0425281280", "146101123x", "1772633550", "1772633828", "1772633895")
itens_info3 = getItensInfo(itens_user3)
plotRecomendacoes(itens_info3)

# usuario gfd052a1b283acd2855750390223a6495

itens_user4 = c("1986794784", "0099740915", "1983641723", "0399593489", "1635550904")
itens_info4 = getItensInfo(itens_user4)
plotRecomendacoes(itens_info4)



######################

##Cold Start

itens_user_cold = c("1683247353", "1503943372", "0316225908", "0425284689", 
                    "0999048287", "0312577230", "0996135693", "0375969020",
                    "0141378247", "1503954064", "1732144303", "1101885963", 
                    "0062498533", "1978098758", "0312577222", "1503943380",
                    "194379605x", "0062654195", "0399593489", "0091944244")

itens_info_cold = getItensInfo(itens_user_cold) 

cold_recomendacoes = sample_n(itens_info_cold, 5)
cold_recomendacoes

plotRecomendacoes(cold_recomendacoes)
