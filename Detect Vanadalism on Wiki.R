packg<-c("tm","caTools")
install.packages("tm")
install.packages("caTools")
install.packages("rpart")
install.packages("rpart.plot")
library("tm")
library("caTools")
library("rpart")
library("rpart.plot")

##-------------------Data loading------------------------------##
wiki<- read.csv("wiki.csv",stringsAsFactors = FALSE)
str(wiki)
#Change variable type of vandal
wiki$Vandal<-as.factor(wiki$Vandal)

#Check the number of records for each level of o/p
table(wiki$Vandal) #the data set is balanced

##----------------------Words Added----------------------------------##
# Create a corpus of all text in added
corpus<-Corpus(VectorSource(wiki$Added))
#Text to lowercase
corpus<-tm_map(corpus,tolower)
#Remove punctuations from the text
corpus<-tm_map(corpus,removePunctuation)
#Remove stopwords(the,a,is...etc.)
corpus<-tm_map(corpus,removeWords,stopwords("english"))
#Stemming eg-sent,send-->sen
corpus<-tm_map(corpus,stemDocument)
inspect(corpus[1:10])

##Command R to treat the preprocessed text as text documents
corpus = tm_map(corpus, PlainTextDocument)
#convert the corpus to term matrix.
dtm<-DocumentTermMatrix(corpus)
dtm

##----------------------------Removed----------------------------------##
corpus<-Corpus(VectorSource(wiki$Removed))
corpus<-tm_map(corpus,tolower)
corpus<-tm_map(corpus,removePunctuation)
corpus<-tm_map(corpus,removeWords,stopwords("english"))

corpus<-tm_map(corpus,stemDocument)

corpus = tm_map(corpus, PlainTextDocument)
dtm2<-DocumentTermMatrix(corpus)


--------------------------------------------------------------------------
#removing sparse terms
dtm<-removeSparseTerms(dtm,0.997)
sparsedtm<-dtm
#Convert the final term document to data frame 
wordsadded<-as.data.frame(as.matrix(sparsedtm))
#Change the column names, suffixing with added
colnames(wordsadded)<-paste("A",colnames(wordsadded))

dtm2<-removeSparseTerms(dtm2,0.997)
sparsedtm2<-dtm2
wordsremoved<-as.data.frame(as.matrix(sparsedtm2))
colnames(wordsremoved)<-paste("R",colnames(wordsremoved))

str(wordsremoved)
#------------------------Preparing model ready dataset-------------
#Combining the added and removed dataframes
wikiWords = cbind(wordsadded,wordsremoved)
#Appending the dependent and independent variables to the bag of words
wikiWords$vandal<-wiki$Vandal
str(wiki)
wikiWords$Minor<-wiki$Minor
wikiWords$Loggedin<-wiki$Loggedin

##-----------------HTTP detection------------------------------------
# webaddresses might have been removed w
wikiWords$added_http = ifelse(grepl("http",wiki$Added,fixed=TRUE), 1, 0)
wikiWords$removed_http = ifelse(grepl("http",wiki$Removed,fixed=TRUE), 1, 0)
table(added_HTTP)

#----------adding count of words added and removed---------------------
wikiWords$NumWordsAdded = rowSums(as.matrix(dtm))

wikiWords$NumWordsRemoved = rowSums(as.matrix(dtm2))

str(wikiWords)

##---------------Sampling data into training and test sets--------------
set.seed(123)
spl<-sample.split(wikiWords$vandal,SplitRatio=0.75)
wikitrain<-subset(wikiWords,spl==TRUE)
wikitest<-subset(wikiWords,spl==FALSE)

table(wikitest$vandal)
table(wikitrain$vandal)
5461+1361
1546/6822

515/(515+454)
#----------------------MODELING-----------------------------------------
#Building decision tree
wikicart<-rpart(vandal ~ .,data = wikitrain,method="class")
pred<-predict(wikicart,newdata = wikitest,type = "class")
table(wikitest$vandal,pred)

#Accuracy
(451+238)/nrow(wikitest)#=> 0.711

prp(wikicart)




