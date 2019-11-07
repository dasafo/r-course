install.packages(c("twitteR", "RColorBrewer", "plyr",
                   "ggplot2", "devtools", "httr"))
require(devtools)

install_url("https://cran.r-project.org/src/contrib/Archive/Rstem/Rstem_0.4-1.tar.gz")
install_url("https://cran.r-project.org/src/contrib/Archive/slam/slam_0.1-37.tar.gz")
install_url("https://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.2.tar.gz")

library(slam)
library(sentiment)
library(twitteR)

api_key <- "Je2Hm6os35YTUpUWESCI02AYW"
api_secret <- "H8KdNKNzK9vKvD9UbJYlqs6Jzf0cFB4lFoa7eO8yu96oU1vz7J"
access_token <- "754714491875684352-N78RIjTDhXDZfAULlbLmLa7hiuLk78y"
access_token_secret <- "3XcegwAyVUBDtyaTa40qdXtkzrtJjHSAwVsbroQBjfuzz"

setup_twitter_oauth(api_key, api_secret,
                    access_token, access_token_secret)

tweets <- searchTwitter("mena", n = 1500, lang = "es")

texts <- sapply(tweets, function(x) x$getText())

clean.data <- function(text){
  #eliminar re-tweets y @ del texto original
  text = gsub("(RT|VIA)((?:\\b\\W*@\\w+)+)", "", text) 
  #?:\\b\\W*@\\w+ es tal vez espacio en blanco, o caracter especial @, o bien nada
  # con "" le decimos que entonces elimine dichas condiciones y nos quedamos con el texto original
  text = gsub("@\\w+", "", text)
  
  #eliminar signos de puntuación y dígitos del 0 al 9
  text = gsub("[[:punct:]]", "", text)
  text = gsub("[[:digit:]]", "", text)
  
  #eliminar links html, tabulaciones y espacios adicionales
  text = gsub("http\\w+","",text)
  text = gsub("[ \t]{2,}", "", text)
  text = gsub("^\\s+|\\s+$", "", text)
}

texts <- clean.data(texts)

head(texts)

handle.error <- function(x){
  #crear el valor omitido
  y = NA
  #try_catch error
  try_error <- tryCatch(tolower(x), error=function(e) e)
  #si no hay error
  if(!inherits(try_error, "error"))
    y = tolower(x)
  #devolvemos el resultado
  return(y)
}

texts = sapply(texts, handle.error)

head(texts)

texts <- texts[!is.na(texts)]

names(texts) <- NULL


##analisis de sentimiento

class_emo <- classify_emotion(texts, algorithm = "bayes", prior = 1)

head(class_emo)

emotion <- class_emo[,7]
emotion[is.na(emotion)]<-"unknown"
head(emotion)

#analisis de la polaridad

class_pol <- classify_polarity(texts, algorithm = "bayes")

head(class_pol)

polarity <- class_pol[,4]

sent_df <- data.frame(text = texts, 
                      emotion = emotion, polarity = polarity, stringsAsFactors = F)

sent_df <- within(sent_df, emotion <- factor(emotion, levels = names(sort(table(emotion), decreasing= T))))


library(RColorBrewer)
library(ggplot2)

ggplot(sent_df, aes(x=emotion))+
  geom_bar(aes(y = ..count.., fill=emotion))+
  scale_fill_brewer(palette = "Set2")+
  labs(x="Categorías de emocion", y = "Número de Tweets")+
  labs(title = "Análisis de Sentimiento acerca de Machine Learning")


ggplot(sent_df, aes(x=polarity))+
  geom_bar(aes(y = ..count.., fill = polarity))+
  scale_fill_brewer(palette = "Set3")+
  labs(x="Categorías de polaridad", y = "Número de Tweets")+
  labs(title="Análisis de Sentimiento acerca de Machine Learning")
