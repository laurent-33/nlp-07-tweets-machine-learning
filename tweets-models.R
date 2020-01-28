library(ggplot2)
library(RTextTools)
library(openxlsx)
library(stringi)
library(dplyr)
library(stringi)
library(tm)
library(caret)

source('create_matrix_fixed.R')
set.seed(123)

path_xlsx = "data_tweets.xlsx"

tweets = data.frame()
for(i in 1:3){
  temp_df = read.xlsx(path_xlsx, sheet = i)
  tweets = rbind(tweets, temp_df)
  rm(temp_df)
}

#tweets <- data.frame(tweets)
tweets['index'] <- 1:nrow(tweets) #rownames(tweets)

colnames(tweets)

set.seed(123)

balance_tonalite <- function(data){
  triAplat = table(data$tonalite)
  twee_cl1 = filter(data, tonalite == "1")
  twee_cl2 = filter(data, tonalite == "2")
  twee_cl3 = filter(data, tonalite == "3")
  
  size = min(c(nrow(twee_cl1),nrow(twee_cl2),nrow(twee_cl3)))
  
  twee_cl1 = twee_cl1[sample(nrow(twee_cl1), size), ]
  twee_cl2 = twee_cl2[sample(nrow(twee_cl2), size), ]
  twee_cl3 = twee_cl3[sample(nrow(twee_cl3), size), ]
  
  tweets = rbind(twee_cl1,twee_cl2,twee_cl3)
  return(tweets)
}

tweets_balanced = balance_tonalite(tweets)

dim(tweets_balanced)
rownames(tweets_balanced) <- c(tweets_balanced$index)

# tweets <- tweets %>%
#     select(-'index')

# set.kRp.env(TT.cmd="~/Devel/TreeTagger/cmd/tree-tagger-french", lang="fr")
# lemmatization <- function(texts){
#     #print(length(texts))
#     out_texts <- vector(mode="character", length = length(texts))
#     for( i in nrow(texts)){
#         tags = treetag(file = texts[i], format = "obj", stopwords=stopwords)
#         words <- data.frame(token = tags[,'token'], lemma = tags[,'lemma'], stop = tags[,'stop']) %>%
#             filter(stop == FALSE & lemma != "<unknown>" & lemma !="@card@") #%>%
#             #select(lemma)
#         print(words$lemma)
#         out_texts[i] = paste(words$lemma, " ")
#         }
#     print(length(out_texts))
#     return(list(out_texts))
# }

stopwords = stopwords = c(tm::stopwords("fr"),"ratp","sncf", "à", "ter")

remove_stop_words <- function(texts){
  documents = Corpus(VectorSource(texts))
  return(tm_map(documents, removeWords, stopwords)$content)
}

clean <- function(tweets){
  tweets_clean <- tweets %>%
    mutate(tonalite = as.factor(tonalite)) %>%
    mutate(tweet = gsub('([@#]\\S*\\s?)', '', tweet)) %>% #enleve mentions et hashtags
    mutate(tweet = gsub('(http(s)[^\\s]*\\s?)', '', tweet)) %>% #enleve liens html
    mutate(tweet = gsub("[^\x01-\x7F]", "", tweet)) %>% #remove emoticon
    mutate(tweet = gsub('[[:punct:]]', ' ', tweet)) %>% #enleve ponctuation
    #mutate(tweet = lemmatization(tweet)) %>%
    mutate(tweet = remove_stop_words(tweet)) %>% #enleve stopwords
    mutate(tweet = tolower(tweet)) %>% #met en minuscule
    mutate(tweet = stri_trans_general(tweet, "ASCII"))#converti les accents
  return(tweets_clean)
}

tweets_clean <- clean(tweets_balanced)

#lemmatization(tweets$tweet[1])

split_tweets <- function(tweets){
  tweets_shuffled = tweets[sample(1:nrow(tweets), nrow(tweets)),]
  n_train = round(nrow(tweets_shuffled)*0.75)
  train_set = tweets_shuffled[1:n_train, ]
  test_set = tweets_shuffled[(n_train+1):nrow(tweets_shuffled), ]
  rownames(train_set) <- c(train_set$index)
  rownames(test_set) <- c(test_set$index)
  train_set <- train_set %>%
    select(-'index')
  test_set <- test_set %>%
    select(-'index')
  return(list('train' = train_set, 'test' = test_set))
}

split <- split_tweets(tweets_clean)
train_set = split$train
test_set = split$test

###prop.tables
fullprop <- prop.table(table(tweets_clean$tonalite, useNA = "ifany"))
trainprop <- prop.table(table(train_set$tonalite, useNA = "ifany"))
testprop <- prop.table(table(test_set$tonalite, useNA = "ifany"))

# ggplot(score, aes(1:3)) +
#   geom_bar(aes(y = t(as.vector(fullprop))), color = "blue")
  # geom_bar(aes(y = trainprop), color = "red") +
  # geom_bar(aes(y = testprop), color = "red") +
  # xlab('Tonalité') +
  # ylab('Distribution')

prop.table(table(tweets_clean$ironie, useNA = "ifany"))
prop.table(table(train_set$ironie, useNA = "ifany"))
prop.table(table(test_set$ironie, useNA = "ifany"))

prop.table(table(tweets_clean$pub_presse, useNA = "ifany"))
prop.table(table(train_set$pub_presse, useNA = "ifany"))
prop.table(table(test_set$pub_presse, useNA = "ifany"))

my_matrix <- function(data, trainMatrix=NULL){
  return(create_matrix_fixed(data, language = "french", minDocFreq = 1,
                             maxDocFreq = Inf, minWordLength = 3, maxWordLength = Inf, 
                             ngramLength = 2, originalMatrix = trainMatrix, removeNumbers = TRUE, 
                             removePunctuation = TRUE, removeSparseTerms = 0,
                             removeStopwords = TRUE, stemWords = TRUE,
                             stripWhitespace = TRUE, toLower = TRUE, weighting = weightTf))
}

train_test_matrix <- function(train_set, test_set){
  matrix_train <- my_matrix(train_set$tweet)
  matrix_test <- my_matrix(test_set$tweet, trainMatrix = matrix_train)
  return(list('train' = matrix_train, 'test' = matrix_test))
}

train_test_container <- function(matrix_train, matrix_test,
                                 labels_train, labels_test){
  container_train = create_container(matrix_train, labels_train,
                                     trainSize=1:length(labels_train),
                                     testSize=NULL, virgin = TRUE)
  container_test = create_container(matrix_test, labels_test, trainSize=NULL,
                                    testSize=1:length(labels_test), virgin = FALSE)
  return(list('train' = container_train, 'test' = container_test))
}

# création de la matrice de termes
matrix <- train_test_matrix(train_set, test_set)

# Création du container d'entraînement pour le modèle
container <- train_test_container(matrix$train, matrix$test,
                                  train_set$tonalite, test_set$tonalite)

# Génération du modèle
model <- train_model(container$train, "MAXENT", l1_regularizer = 1)

# cost_matrix
cost_matrix = c(1,0.25,0,0.25,1,0.25,0,0.25,1)
dim(cost_matrix) <- c(3,3)
cost_matrix

evaluate <- function(model, container, true_output){
  results <- classify_model(container, model)
  conf.matrix = confusionMatrix(results[,1], true_output)
  table = conf.matrix$table
  accuracy = as.numeric(conf.matrix$overall[1])
  score = sum(conf.matrix$table * cost_matrix)/length(true_output)
  return(list('table' = table, 'accuracy' = accuracy, 'score' = score,
              'prediction' = results[,1], 'confidence' = results[,2]))
}

# Efficacité
eval.train <- evaluate(model, container$train, train_set$tonalite)
eval.test <- evaluate(model, container$test, test_set$tonalite)

eval.train$score
eval.test$score

# Affichage des erreurs
compare = data.frame(original = tweets[rownames(test_set),'tweet'],
                     words=test_set$tweet, pred=results[,1],
                     true=test_set$tonalite)
errors <- compare %>%
  subset(pred!=true)

big_errors <- compare %>%
  subset(abs(as.integer(pred)-as.integer(true))==2)

head(errors)
head(big_errors)

# liste des features
container$train@column_names

# conversion matrice pour être utilisée
library(SparseM)
test <- as.matrix(container$train@classification_matrix)

# affichage d'un tweet avec feature voulue
train_set[which(test[, 901] == 1), 'tweet']

# Scores obtenus
# SVM(radial) -> train 0.7860 / test 0.6822
# SVM_poly -> 0.4184 / 0.4113
# SVM_linear -> 0.9986 / 0.6416
# SVM_sigmo -> 0.7520 / 0.6827
# SVM_0.9 -> 0.6028 / 0.5758
# SVM_0.8 -> 0.6028 / 0.5758

# GLMNET -> 0.9600 / 0.6844

# MAXENT -> 0.9989 / 0.6489
# MAXENT_1 -> 0.8651 / 0.6856
# MAXENT_2 -> 0.9499 / 0.6853
# BOOSTING -> 0.5968 / 0.5805
# TREE -> 0.5142 / 0.5153
# BAGGING -> 0.8038 / 0.6383

# SVM test 0.58 avec lemm / 0.55 sans lemm (4% dataset)
# SVM test 0.65 avec lemm / 0.60 sans lemm (10% dataset)
# SVM test 0.68 avec lemm / 0.60 sans lemm (100% dataset)


# learning curve
points_number = 20
score.train = vector(length = points_number)
score.test = vector(length = points_number)

for(i in 1:points_number){
  subdata = train_set[1:(i/points_number*dim(train_set)[1]),]
  matrix <- train_test_matrix(subdata, test_set)
  container <- train_test_container(matrix$train, matrix$test,
                                    subdata$tonalite,
                                    test_set$tonalite)
  model <- train_model(container$train, "MAXENT", l1_regularizer = 1)
  eval.train <- evaluate(model, container$train, subdata$tonalite)
  eval.test <- evaluate(model, container$test, test_set$tonalite)
  score.train[i] <- eval.train$score
  score.test[i] <- eval.test$score
}

score <- data.frame('train' = score.train, 'test' = score.test)
plot(score$train)
lines()

ggplot(score, aes(1:points_number)) +
  geom_line(aes(y = train), color = "blue") +
  geom_line(aes(y = test), color = "red") +
  xlab('Training dataset size') +
  ylab('Performance')
