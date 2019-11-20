#Analisis de redes con Twitter

library(twitteR)
library(igraph)
library(dplyr)

api_key <- "Je2Hm6os35YTUpUWESCI02AYW"
api_secret <- "H8KdNKNzK9vKvD9UbJYlqs6Jzf0cFB4lFoa7eO8yu96oU1vz7J"
access_token <- "754714491875684352-N78RIjTDhXDZfAULlbLmLa7hiuLk78y"
access_token_secret <- "3XcegwAyVUBDtyaTa40qdXtkzrtJjHSAwVsbroQBjfuzz"


setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)

all_tweets <- searchTwitter("Unidas Podemos", n = 2000)

all_tweets <- twListToDF(all_tweets)

#sample_tweets <- all_tweets[1:200,]

sample_tweets <- all_tweets

split_tweets <- split(sample_tweets, sample_tweets$isRetweet)

retweets <- mutate(split_tweets[['TRUE']],
                   sender = substr(text, 5, regexpr(":",text)-1))

edge_list <- as.data.frame(cbind(sender = tolower(retweets$sender),
                                 receiver = tolower(retweets$screenName)))

edge_list <- count(edge_list, sender, receiver)


tweets_graph <- graph_from_data_frame(d=edge_list, directed = T)
save(tweets_graph, file="../data/tema9/tweets-from-UnidasPodemos.Rdata")


plot(tweets_graph, 
     #layout=layout.fruchterman.reingold,
     vertex.color = "blue",
     vertex.size = degree(tweets_graph, mode = "in"),
     vertex.label = NA,
     edge.arrow.size = 0.4,
     edge.arrow.width = 0.5,
     edge.width = edge_attr(tweets_graph)$n,
     edge.color=hsv(h=.9, s=1, v=.7, alpha = 0.5),
     main="Tweets sobre UP")


library(devtools)
devtools::install_github("analyxcompany/ForceAtlas2")
library(ForceAtlas2)

force_layout <- layout.forceatlas2(tweets_graph,
                                   iterations = 200,
                                   plotstep = 20) 

plot(tweets_graph, 
     layout = force_layout,
     vertex.color = "blue",
     vertex.size = degree(tweets_graph, mode = "in"),
     vertex.label = NA,
     edge.arrow.size = 0.5,
     edge.arrow.width = 0.5,
     edge.width = edge_attr(tweets_graph)$n,
     edge.color=hsv(h=.9, s=1, v=.7, alpha = 0.5),
     main="Tweets sobre Monetización")



write.graph(tweets_graph,
            file="../data/tema9/unidaspodemos.graphml",
            format= "graphml")
