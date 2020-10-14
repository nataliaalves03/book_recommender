#install.packages("tidyverse")
#install.packages("jsonlite")
#install.packages("recommenderlab")
library(tidyverse)
library(jsonlite)
library(recommenderlab)


##fonte dos datasets

#Amazon
#info: https://nijianmo.github.io/amazon/index.html
#reviews: http://deepyeti.ucsd.edu/jianmo/amazon/categoryFilesSmall/Books.csv

#Goodreads
#info: https://sites.google.com/eng.ucsd.edu/ucsdbookgraph/home
#books: https://drive.google.com/uc?id=1LXpK1UfqtP89H1tYy0pBGHjYk8IhigUK
#reviews: https://drive.google.com/uc?id=1pQnXa7DWLdeUpvUFsKusYzwbA5CAAZx7


########################################################

##reduzir Goodreads json

#books

con_in <- gzcon(file("goodreads_books.json.gz", "rb"))
con_out <- file("goods_books.txt", open = "wb")
stream_in(con_in, handler = function(df){
  df2 <- data.frame(book_id = df$book_id, asin = df$asin, isbn = df$isbn)
  stream_out(df2, con_out, pagesize = 1000)
}, pagesize = 5000)
close(con_out)


#reviews

con_in <- gzcon(file("goodreads_reviews_dedup.json.gz", "rb"))
con_out <- file("goods_reviews.txt", open = "wb")
stream_in(con_in, handler = function(df){
  df2 <- data.frame(user = df$user_id, book_id = df$book_id, 
                    rating = df$rating, timestamp = df$date_added)
  stream_out(df2, con_out, pagesize = 1000)
}, pagesize = 5000)
close(con_out)



########################################################

##Goodreads json reduzido para csv

#books

dados_good_books = lapply(readLines(file("goods_books.txt","r")), fromJSON)
dados_good_books[[1]]

m <- matrix(unlist(dados_good_books),byrow=TRUE,ncol=length(dados_good_books[[1]]))
df_good_books = as.data.frame(m)
colnames(df_good_books) <- c("book_id","asin","isbn")

dim(df_good_books)
head(df_good_books)

write_csv(df_good_books, "goods_books.csv")


#reviews

dados_good_reviews = lapply(readLines(file("goods_reviews.txt","r")), fromJSON)
dados_good_reviews[[1]]

m2 <- matrix(unlist(dados_good_reviews),byrow=TRUE,ncol=length(dados_good_reviews[[1]]))
df_good_reviews = as.data.frame(m2)
colnames(df_good_reviews) <- c("user","book_id","rating","timestamp")

dim(df_good_reviews)
head(df_good_reviews)

write_csv(df_good_reviews, "goods_reviews.csv")



########################################################

##merge goodreads books reviews by book_id

df_goods = merge(df_good_reviews, df_good_books, by="book_id", all.x = T)

dim(df_goods)
head(df_goods)

#write_csv(df_goods, "goods_merge.csv")


########################################################


##cria a coluna item com ASIN ou ISBN
df_goods$item = ifelse(!is.na(df_goods$asin), df_goods$asin, df_goods$isbn)

head(df_goods)

##cria tuplas de ratings do Goodreads
df_goods = df_goods[,c(2,7,3,4)]


#verifica se há registros sem código do item ou do usuario
dim(is.na(df_goods$item))
dim(is.na(df_goods$user))

#verifica se há itens com código ASIN/ISBN incorreto
incorrect_item  = df_goods %>% filter(nchar(df_goods$item) != 10)
dim(incorrect_item)
df_goods = df_goods %>% filter(nchar(df_goods$item) == 10)

#verifica o intervalo de ratings 1-5
incorrect_rating  = df_goods %>% filter(df_goods$rating == 0)
dim(incorrect_rating)
df_goods = df_goods %>% filter(df_goods$rating > 0)
summary(df_goods$rating)

#formata data e hora
df_goods = df_goods %>% 
  mutate(timestamp = parse_datetime(substring(timestamp, 5), "%b %d %H:%M:%S %z %Y"))

#adiciona prefixo g no user
df_goods = df_goods %>% mutate(user = paste("g",user,sep = ""))


head(df_goods)
glimpse(df_goods)

#write_csv(df_goods, "goods_ratings.csv")



########################################################


#estatísticas
dim(df_goods)
summary(df_goods)
hist(df_goods$rating, breaks = "sturges", 
     main="Histograma - Ratings Goodreads",
     ylab = "Frequência", xlab = "Ratings")


#cria matriz de ratings
mr_goods = as(df_goods, "realRatingMatrix")
mr_goods


hist(rowCounts(mr_goods), breaks = 100, 
     main="Histograma - Número de ratings por usuário Goodreads",
     ylab = "Frequência", xlab = "Quantidade de ratings")

hist(colCounts(mr_goods), breaks = 100, 
     main="Histograma - Número de ratings por item Goodreads",
     ylab = "Frequência", xlab = "Quantidade de ratings")

#ratings por usuário
summary(rowCounts(mr_goods))

#ratings por item
summary(colCounts(mr_goods))


########################################################

##dados Amazon

dados_amazon = read.csv("Books.csv", header=F, sep=",", dec=".",
                        col.names = c("item","user","rating","timestamp"))

head(dados_amazon)
summary(dados_amazon)


#verifica se há registros sem código do item ou do usuario
dim(filter(dados_amazon, is.na(user)))
dim(filter(dados_amazon, is.na(item)))

#verifica se há itens com código ASIN/ISBN incorreto
incorrect_item  = dados_amazon %>% filter(nchar(dados_amazon$item) != 10)
dim(incorrect_item)

#verifica o intervalo de ratings 1-5
dados_amazon = dados_amazon %>% filter(rating > 0)
summary(dados_amazon)

#formata data e hora
dados_amazon = dados_amazon %>% 
  mutate(timestamp = as.POSIXct(timestamp, origin="1970-01-01"))

#adiciona prefixo a no user
dados_amazon = dados_amazon %>% mutate(user = paste("a",user,sep = ""))

#ordena colunas
dados_amazon = data.frame(user = dados_amazon$user,
                          item = dados_amazon$item, 
                          rating = dados_amazon$rating,
                          timestamp = dados_amazon$timestamp)

head(dados_amazon)
glimpse(dados_amazon)

write_csv(dados_amazon, "amazon_ratings.csv")


############


#estatísticas
dim(dados_amazon)
summary(dados_amazon)
hist(dados_amazon$rating, breaks = "sturges", 
     main="Histograma - Ratings Amazon",
     ylab = "Frequência", xlab = "Ratings")


#cria matriz de ratings
mr_amazon = as(dados_amazon, "realRatingMatrix")
mr_amazon


hist(rowCounts(mr_amazon), breaks = 100, 
     main="Histograma - Número de ratings por usuário Amazon",
     ylab = "Frequência", xlab = "Quantidade de ratings")

hist(colCounts(mr_amazon), breaks = 100, 
     main="Histograma - Número de ratings por item Amazon",
     ylab = "Frequência", xlab = "Quantidade de ratings")


#ratings por usuário
summary(rowCounts(mr_amazon))

#ratings por item
summary(colCounts(mr_amazon))


########################################################

#merge Amazon e Goodreads

dados_books = rbind(df_goods, dados_amazon)

write_csv(dados_books, "dados_books.csv")


#apenas reviews de out/2017 a out/2018

min(df_goods$timestamp) #none
max(df_goods$timestamp) #2017-11-04
min(dados_amazon$timestamp) #1996-05-19
max(dados_amazon$timestamp) #2018-10-01

dados_ano = dados_books %>% filter(timestamp >= as.POSIXct("2017-10-01"))

min(dados_ano$timestamp)
max(dados_ano$timestamp)
summary(dados_ano)
dim(dados_ano)

#write_csv(dados_ano, "dados_books_2017_2018.csv")



#remove a coluna timestamp
dados_ano$timestamp = NULL

#ordenar usuarios e itens
dados_books_sort = arrange(dados_ano, user, item)

#remove reviews duplicadas (mesmo user e item)
dados_books_sort = dados_books_sort %>% distinct(user, item, .keep_all = TRUE)

#write_csv(dados_books_sort, "dados_books_2017_2018_sort.csv")



#padroniza código do usuário e do item para letras minúsculas
dados_books_sub = dados_books_sort %>% mutate(user = str_to_lower(trimws(user)), 
                                              item = str_to_lower(trimws(item)))

#users com o minimo de 5 reviews
min_users = dados_books_sub %>% group_by(user) %>% mutate(n = n()) %>% 
  filter(n >= 5) %>% select(user) %>% distinct()

dados_books_sub = dados_books_sub %>% filter(user %in% min_users$user)

#verifica se todos users e itens tem pelo menos 1 review
dados_books_sub = dados_books_sub %>% group_by(user, item) %>% 
  mutate(n = n()) %>% filter(n == 1) %>% ungroup()

dados_books_sub$n = NULL
dados_books_sub$timestamp = NULL

dim(min_itens)
dim(min_users)
dim(dados_books_sub)


dim(dados_books_sub)
head(dados_books_sub)
tail(dados_books_sub)

#dataset final para o modelo de recomendação
write_csv(dados_books_sub, "dados_books_2017_2018_min5.csv")


