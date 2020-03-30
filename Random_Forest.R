##################################
# 目的 : ランダムフォレスト実行部
# ソースコード名 : RandomForest.R
# バージョン : SSI(1)
# 最終変更日時 : 2017.06.08, 17:12
# 作成者 : Taniguchi Hidetaka
##################################

library('randomForest')

train.y.factor <- as.factor(train.y)

randomforest.res <- randomForest(train.x, train.y.factor, importance=TRUE, ntree=300)

# 分類にとくに寄与した特徴の可視化
# varImpPlot(randomforest.res)

predictions <- predict(randomforest.res, test.x, type="class")

#################

# スパムテストデータの通し番号
spam_from <- 1
spam_to <- length(test.y[which(test.y==1)])

rr.predictions.spam_results <- predictions[spam_from:spam_to]
rr.predictions.spam_results <- as.numeric(rr.predictions.spam_results)
test_class_spam <- test_df.x$ZSpecies[spam_from:spam_to]
test_class_spam <- as.numeric(test_class_spam)
rr.spam_accuracy <- length(rr.predictions.spam_results[which(rr.predictions.spam_results == test_class_spam)]) / length(rr.predictions.spam_results)

# ハムテストデータの通し番号
ham_from <- (spam_to + 1)
ham_to <- (spam_to +length(test.y[which(test.y==0)]))

rr.predictions.ham_results <- predictions[ham_from:ham_to]
rr.predictions.ham_results <- as.numeric(rr.predictions.ham_results)
test_class_ham <- test_df.x$ZSpecies[ham_from:ham_to]
test_class_ham <- as.numeric(test_class_ham)
rr.ham_accuracy <- length(rr.predictions.ham_results[which(rr.predictions.ham_results == test_class_ham)]) / length(rr.predictions.ham_results)

(rr.spam_accuracy + rr.ham_accuracy) / 2

# rr.result <- length(rr.predictions[which(rr.predictions == test_df.x$ZSpecies)]) / length(rr.predictions)

result_RR[[cnt_j]] <- c(rr.spam_accuracy, rr.ham_accuracy)

print("Random Forest")
print(rr.spam_accuracy)
print(rr.ham_accuracy)

 