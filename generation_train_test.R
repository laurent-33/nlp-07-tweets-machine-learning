library(openxlsx)

path_xlsx = "data_tweets.xlsx"

tweets = data.frame()
for(i in 1:3){
  temp_df = read.xlsx(path_xlsx, sheet = i)
  tweets = rbind(tweets, temp_df)
  rm(temp_df)
}

#shuffle
set.seed(123)
tweets_shuffled = tweets[sample(1:nrow(tweets), nrow(tweets)),]

#treats different values
tweets_shuffled$ironie = ifelse((is.na(tweets_shuffled$ironie) | tweets_shuffled$ironie!= 1), 0, tweets_shuffled$ironie)
tweets_shuffled$pub_presse = ifelse((is.na(tweets_shuffled$pub_presse) | tweets_shuffled$pub_presse!= 1), 0, tweets_shuffled$pub_presse)


#train - test
n_train = round(nrow(tweets_shuffled)*0.75)
train_set = tweets_shuffled[1:n_train, ]
test_set = tweets_shuffled[(n_train+1):nrow(tweets_shuffled), ]


###prop.tables

prop.table(table(tweets_shuffled$tonalite, useNA = "ifany"))
prop.table(table(train_set$tonalite, useNA = "ifany"))
prop.table(table(test_set$tonalite, useNA = "ifany"))

prop.table(table(tweets_shuffled$ironie, useNA = "ifany"))
prop.table(table(train_set$ironie, useNA = "ifany"))
prop.table(table(test_set$ironie, useNA = "ifany"))

prop.table(table(tweets_shuffled$pub_presse, useNA = "ifany"))
prop.table(table(train_set$pub_presse, useNA = "ifany"))
prop.table(table(test_set$pub_presse, useNA = "ifany"))
