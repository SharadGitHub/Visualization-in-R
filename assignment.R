
library(ggplot2)
library(ggmap)

statesmap = map_data("state")

str(statesmap)
table(statesmap$region)
table(statesmap$subregion)

ggplot(statesmap, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "white", color = "black")

polling = read.csv("PollingImputed.csv", na.strings = c(""," ", NA))

str(polling)

Train = subset(polling, Year < 2012)

Test = subset(polling, Year == 2012)

mod2 = glm(Republican~ SurveyUSA + DiffCount, data = Train, family = "binomial")
TestPrediction = predict(mod2, newdata = Test, type= "response")

TestPredictionBinary = as.numeric(TestPrediction>0.5)

predictionDataFrame = data.frame(TestPrediction, TestPredictionBinary, Test$State)

str(predictionDataFrame)
mean(predictionDataFrame$TestPrediction)
table(predictionDataFrame$TestPredictionBinary)

predictionDataFrame$region = tolower(predictionDataFrame$Test.State)

predictionMap = merge(statesmap, predictionDataFrame, by="region")


predictionMap = predictionMap[order(predictionMap$order),]

ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary))+
  geom_polygon(color = "black") +
  scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1),labels= c("Democrat", "Republican")  ,name = "Prediction 2012")



ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPrediction))+ 
  geom_polygon(color = "black", linetype=3, alpha=0.3) +
  scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1),
                    labels = c("Democrat", "Republican"), name = "Prediction 2012")




ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPrediction))+ 
  geom_polygon(color = "black") +
  scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1),
                      labels = c("Democrat", "Republican"), name = "Prediction 2012")+borders( database = "state",colour= "grey50")


########## assignment2   #######
edges = read.csv("edges.csv", na.strings = c("", " ", NA))

users = read.csv("users.csv", na.strings = c("", " ", NA))

edges
users
sort(unique(edges$V1, edges$V2))
sort(unique(users$id))
users[!is.na(users$school), ]

install.packages("igraph")
library(igraph)
g= graph.data.frame(edges, FALSE, users)

plot(g, vertex.size=5, vertex.label=NA)

### users with more than 9 friends
friends =degree(g)
subset(friends, friends>9)


V(g)$size= degree(g)/2+2
plot(g, vertex.label=NA)

### setting color
V(g)$color= "black"
V(g)$color[V(g)$locale == "A"]= "red"
V(g)$color[V(g)$locale == "B"]= "gray"

plot(g, vertex.label=NA)

install.packages("rgl")
library(rgl)
rglplot(g, vertex.label=NA)



############ assignment 3

tweets = read.csv("tweets.csv", stringsAsFactors = F)

library(tm)
corpus= Corpus(VectorSource(tweets$Tweet)) 
corpus = tm_map(corpus, tolower)
corpus= tm_map(corpus, PlainTextDocument)

corpus= tm_map(corpus, removePunctuation)

corpus= tm_map(corpus, removeWords,c("apple", stopwords("english")))

frequencies = DocumentTermMatrix(corpus)

allTweets = as.data.frame(as.matrix(frequencies))

ncol(allTweets)
View(allTweets)


library(wordcloud)

wordcloud(colnames(allTweets[1:2000]), colSums(allTweets[1:2000]), 
          scale = c(3, .5), colors = brewer.pal(9, "Reds")[5:9],random.color = T ,random.order  = F, rot.per = 0.5)

##choose colors from here
display.brewer.all()


#### wordcloud
rm(words)
library(tm)
words= read.csv("words.csv", stringsAsFactors = F)

corpus= Corpus(VectorSource(words$V1)) 
corpus = tm_map(corpus, tolower)
corpus= tm_map(corpus, PlainTextDocument)

frequencies = DocumentTermMatrix(corpus)

allwordss = as.data.frame(as.matrix(frequencies))

library(wordcloud)
wordcloud(colnames(allwordss), colSums(allwordss), 
          scale = c(2.5, .2), min.freq = 1 ,colors = brewer.pal(9, "Reds")[5:9],
          random.color = T, random.order = T ,rot.per = 0.5)


