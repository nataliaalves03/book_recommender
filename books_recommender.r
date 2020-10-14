library(tidyverse)
library(recommenderlab)

## Entrada dos dados

dados_books = read_csv("dados_books_2017_2018_min5.csv")

head(dados_books)
dim(dados_books)
summary(dados_books)
hist(dados_books$rating, breaks = "sturges", 
     main="Histograma - Ratings",
     ylab = "Frequência", xlab = "Ratings")


##Amostragem

set.seed(2020)
user_ids = unique(dados_books$user)
amostra_user = sample(user_ids, round(0.02 * length(user_ids)))

dados_books_amostra = dados_books %>% filter(user %in% amostra_user)

#verifica se todos users e itens tem pelo menos 1 review
dados_books_amostra = dados_books_amostra %>% group_by(user, item) %>% 
  mutate(n = n()) %>% filter(n == 1) %>% ungroup()
dados_books_amostra$n = NULL
dados_books_amostra = as.data.frame(dados_books_amostra)

dim(dados_books_amostra)


##Matriz de ratings

mr_dados = as(dados_books_amostra, "realRatingMatrix")
mr_dados

summary(rowCounts(mr_dados))
summary(colCounts(mr_dados))


##Avaliação de algoritmos TopN

evalScheme = evaluationScheme(mr_dados, method = "cross", k = 5, 
                              train = 0.8, given = 5, goodRating = 4)
evalScheme

nlists = c(1,5,10,15,20)

#modelos possíveis
recommenderRegistry$get_entry("Popular", dataType = "realRatingMatrix")

models = list(
  'Random' = list(name="RANDOM", param = NULL),
  'Popular' = list(name="POPULAR", param = NULL))

results = evaluate(evalScheme, models, type="topNList", n = nlists)
results

models2 = list('ALS' = list(name="ALS", param = NULL),
               'ALS imp' = list(name="ALS_implicit", param = NULL))
results2 = evaluate(evalScheme, models2, type="topNList", n = nlists)
results2



#plot ROC - True Positive x False Positive
plot(results, annotate = c(1,2), legend = "topleft")

#plot Precision x Recall
plot(results, "prec/rec", annotate = c(1,2), legend = "topright")


#plot média dos runs com ggplot

avg_conf_matr <- function(results) {
  tmp <- results %>% getConfusionMatrix() %>% as.list() 
  as.data.frame(Reduce("+",tmp) / length(tmp)) %>% 
    mutate(n = c(1,5,10,15,20)) %>%
    select('n', 'precision', 'recall', 'TPR', 'FPR') 
}

results_tbl <- results %>% map(avg_conf_matr) %>% enframe() %>% unnest(cols = c(value))
results_tbl2 <- results2 %>% map(avg_conf_matr) %>% enframe() %>% unnest(cols = c(value))

results_tbl_all = rbind(results_tbl, results_tbl2)

#write_csv(results_tbl_all, "resultados.csv")

results_tbl_all %>%
  ggplot(aes(FPR, TPR, colour = fct_reorder2(as.factor(name), FPR, TPR))) +
  geom_line() + geom_label(aes(label = n))  +
  labs(title = "Curvas ROC", colour = "Modelo") +
  theme_grey(base_size = 14)

results_tbl_all %>%
  ggplot(aes(recall, precision, colour = fct_reorder2(as.factor(name), FPR, TPR))) +
  geom_line() + geom_label(aes(label = n))  +
  labs(title = "Precision x Recall", colour = "Modelo") +
  theme_grey(base_size = 14)




############################

##Modelo de recomendação final

dim(mr_dados)

trainData = mr_dados[1:2600,]
testData = mr_dados[2601:2604,]


modelRec = Recommender(trainData, "ALS_implicit")
predict = predict(modelRec, testData, n = 5)

#recomendações por usuário
as(predict, "list")



############################

##Cold start - produtos mais populares

mr_dados_cold = as(as.data.frame(dados_books), "realRatingMatrix")
mr_dados_cold

modelRec_cold = Recommender(mr_dados_cold, "Popular")
predict_cold = predict(modelRec_cold, mr_dados_cold[1,], n = 20)

#recomendações Cold Start
as(predict_cold, "list")

