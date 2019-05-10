#install.packages(c("rjson", "bit64", "httr"))

#Network:  https://rpubs.com/kateto/netviz

#library(devtools)
#install_github("twitteR", username="geoffjentry")
library(twitteR)
library(dplyr)
library(qdap)
library(networkD3)
library(igraph)
library(stringr)


setup_twitter_oauth("Utmrq4PjJ0WxhZA2A2ZKZ6WLP", 
                    "RAm9egBmB86dLiMSkqrw3NCZpp1YzY5YRCgmXSqdbT2XpleJMr", 
                    access_token="1008192867477553153-sYeFvvAEUFYfPaDxjih7LrLCRhHIN1", 
                    access_secret="N8OpNdwGDvRr2bm0GJy92ollEJ1z2x9QPtr2Jb0y2iQUN")


searchTwitter("iphone")

#collect recent 1000 tweets
alltweets <- twListToDF(searchTwitter("paxel", n=1000, lang=NULL,since=NULL, until=NULL,locale=NULL, geocode=NULL, sinceID=NULL, maxID=NULL,resultType=NULL, retryOnRateLimit=120))

#save tweets as .csv
write.csv(alltweets, "alltweets.csv")

#Network analysis and visualization is computationally intensive. We will use only the first 500 tweets for demo.

#alltweets <- read.csv("alltweets.csv")
alltweets<-alltweets[1:500,]

#split the data into two sets; one for retweet network and the other for mention network.

#create an edge-list for retweet network
sp = split(alltweets, alltweets$isRetweet)
rt = mutate(sp[['TRUE']], sender = substr(text, 5, regexpr(':', text) - 1))
el = as.data.frame(cbind(sender = tolower(rt$sender), receiver = tolower(rt$screenName)))
el = count(el, sender, receiver) 
el[1:5,] #show the first 5 edges in the edgelist

rt_graph <- graph_from_data_frame(d=el, directed=T)

################
#Visualisation##
###############

glay = layout.fruchterman.reingold(rt_graph) 
plot(rt_graph)

glay = layout.fruchterman.reingold(rt_graph)
par(bg="black", mar=c(1,1,1,1))
plot(rt_graph, layout=glay,
     vertex.color="gray25",
     vertex.size=(degree(rt_graph, mode = "in")), #sized by in-degree centrality
     vertex.label = NA,
     edge.arrow.size=0.8,
     edge.arrow.width=0.5,
     edge.width=edge_attr(rt_graph)$n/10, #sized by edge weight
     edge.color=hsv(h=.95, s=1, v=.7, alpha=0.5))
title("Retweet Network #Messi", cex.main=1, col.main="white")


glay = layout.fruchterman.reingold(rt_graph)
par(bg="white", mar=c(1,1,1,1))
plot(rt_graph, layout=glay, main = "Retweet Network",
     vertex.color="orange",
     vertex.size=(degree(rt_graph, mode = "in")), #sized by in-degree centrality
     vertex.label.family="sans",
     vertex.shape="circle",  #can also try "square", "rectangle", etc. More in igraph manual
     vertex.label.color=hsv(h=0.1, s=0.1, v=0.1, alpha=0.5),
     vertex.label.cex=(degree(rt_graph, mode = "in"))/2, #sized by in-degree centrality
     edge.arrow.size=0.8,
     edge.arrow.width=0.5,
     edge.width=edge_attr(rt_graph)$n/10, #sized by edge weight
     edge.color=hsv(h=.5, s=0.5, v=.7, alpha=0.6))


#wc <- cluster_walktrap(rt_graph)
#members <- membership(wc)
#d3_rt <- igraph_to_networkD3(rt_graph, group = members)

#forceNetwork(Links = d3_rt$links, Nodes = d3_rt$nodes, 
#             Source = 'source', Target = 'target', 
#             NodeID = 'name', Group = 'group', linkWidth = 1,
#             linkColour = "#afafaf", fontSize=12, zoom=T, legend=F,
#             opacity = 0.8, charge=-300, 
#             width = 600, height = 400)

################
###Statistcs####
################

ecount(rt_graph) #the number of edges
vcount(rt_graph) #the number of vertices
E(rt_graph)[1:50] #list the first 50 edges
V(rt_graph)[1:50] #list the first 50 vertex ids

#Calculate density:The proportion of present edges from all possible edges in the network.
edge_density(rt_graph, loops=F) #for an undirected network
ecount(rt_graph)/(vcount(rt_graph)*(vcount(rt_graph)-1)) #for a directed network
#Calculate centralization
centr_degree(rt_graph, mode = c("in"), loops = TRUE,normalized = TRUE)$centralization
#Calculate transitivity:the probability that the neighbors of a vertex are connected. 
transitivity(rt_graph, type="local")
#Find Cluster
cliques(rt_graph)[1:10]
largest_cliques(rt_graph)[1:20] #list only 20 vertices in that cluster
#Betweenness, Individual Importance in the Network
bt <- sort(betweenness(rt_graph, directed = F, weights = NA), decreasing = T)
bt[1:50]
#Closeness
cc <- sort(closeness(rt_graph, mode="all", weights = NA), decreasing = T)
cc[1:20]
#eigenvector
ec <- eigen_centrality(rt_graph, directed = T, weights = NA)
sort(ec$vector)[1:20]

as <- authority_score(rt_graph, weights = NA)$vector
sort(as,decreasing = T)[1:20]

hs <- hub_score(rt_graph, weights = NA)$vector
sort(hs,decreasing = T)[1:20]

#Calculate in-degree centrality
indegree <- sort(degree(rt_graph,mode = "in"),decreasing = TRUE)
indegree[1:20] #show the top vertices by in-degree 

#Calculate out-degree
outdegree <- sort(degree(rt_graph,mode = "out"),decreasing = TRUE)
outdegree[1:20] #show the top vertices by out-degree
