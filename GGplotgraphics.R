# 1

library(ggplot2)
library(ggmap)
library(maps)
statesMap = map_data("state")
str(statesMap)
table(statesMap$group)
ggplot(statesMap, aes(x = long, y = lat, group = group)) + geom_polygon(fill = "white", color = "black") + coord_map("mercator")
polling = read.csv("PollingImputed.csv")
Train = subset(polling, Year == 2004 | Year == 2008)
Test = subset(polling, Year == 2012)
mod2 = glm(Republican~SurveyUSA+DiffCount, data=Train, family="binomial")
TestPrediction = predict(mod2, newdata=Test, type="response")
TestPredictionBinary = as.numeric(TestPrediction > 0.5)
predictionDataFrame = data.frame(TestPrediction, TestPredictionBinary, Test$State)
View(predictionDataFrame)
table(TestPredictionBinary)
predictionDataFrame$region = tolower(predictionDataFrame$Test.State)
predictionMap = merge(statesMap, predictionDataFrame, by = "region")
predictionMap = predictionMap[order(predictionMap$order),]
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary)) + geom_polygon(color = "black")
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary))+ geom_polygon(color = "black") + scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")

#2
parole = read.csv("parole.csv")
str(parole)
parole$male = as.factor(parole$male)
parole$state = as.factor(parole$state)
parole$crime = as.factor(parole$crime)
table(parole$male, parole$violator) #table(parole$male[parole$violator ==1])
ggplot(data = parole, aes(x = age)) + geom_histogram()
ggplot(data = parole, aes(x = age)) + geom_histogram(binwidth=5)
ggplot(data = parole, aes(x = age)) + geom_histogram(binwidth=5, color="blue")
ggplot(data = parole, aes(x = age)) + geom_histogram(binwidth = 5) + facet_grid(male ~ .)
ggplot(data = parole, aes(x = age, fill = male)) + geom_histogram(binwidth = 5)
ggplot(data = parole, aes(x = age, fill = male)) + geom_histogram(binwidth = 5)
ggplot(data = parole, aes(x = age, fill = male)) + geom_histogram(binwidth = 5, position="identity",alpha=0.5)
ggplot(data = parole, aes(x = time.served)) + geom_histogram()
ggplot(data = parole, aes(x = time.served)) + geom_histogram(binwidth=1)
ggplot(data = parole, aes(x = time.served)) + geom_histogram(binwidth=0.1)
ggplot(data = parole, aes(x = time.served)) + geom_histogram(binwidth=1)+facet_grid(.~crime)
ggplot(data = parole, aes(x = time.served, fill=crime)) + geom_histogram(binwidth=1, position="identity", alpha=0.5)

# 3
edges = read.csv("edges.csv")
users = read.csv("users.csv")
table(users$locale)
library(igraph)
g=graph.data.frame(edges,FALSE,users)
plot(g, vertex.size=5, vertex.label=NA)
degree(g)

V(g)$size = degree(g)/2+2
plot(g, vertex.label=NA)
V(g)$color = "black"
V(g)$color[V(g)$gender == "A"] = "red"
V(g)$color[V(g)$gender == "B"] = "gray"
plot(g, vertex.label=NA)
V(g)$color[V(g)$school == "A"] = "red"
V(g)$color[V(g)$school == "B"] = "gray"
plot(g, vertex.label=NA)
V(g)$color[V(g)$school == "AB"] = "gray"
plot(g, vertex.label=NA)
V(g)$color = "black"
V(g)$color[V(g)$locale == "A"] = "red"
V(g)$color[V(g)$locale == "B"] = "gray"
plot(g, vertex.label=NA)

#4
tweets = read.csv("tweets.csv", stringsAsFactors=FALSE)
library(tm)
library(SnowballC)
corpus = Corpus(VectorSource(tweets$Tweet))
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, removePunctuation) 
corpus = tm_map(corpus, removeWords, stopwords("english"))
frequencies = DocumentTermMatrix(corpus)
allTweets= as.data.frame(as.matrix(frequencies))
install.packages("wordcloud")
library(wordcloud)
ncol(allTweets)
wordcloud(names(allTweets),colSums(allTweets),c(2,0.25))
corpus = tm_map(corpus, removeWords, c("apple", stopwords("english")))
frequencies = DocumentTermMatrix(corpus)
allTweets= as.data.frame(as.matrix(frequencies))
wordcloud(names(allTweets),colSums(allTweets),c(2,0.25))