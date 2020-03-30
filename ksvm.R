##################################
# 目的 : SVMの別バージョンの実行プログラム. 
# ソースコード名 : ksvm.R
# バージョン : SR(1)
# 最終変更日時 : 2017.06.08, 16:57
# 作成者 : Taniguchi Hidetaka
##################################
# パッケージの挙動がおかしいため、実験には使っていない
##################################

library("kernlab")

ksvm.kernel_type <- "rbfdot"
ksvm.fit <- ksvm(ZSpecies~., data=train_df.x, scaled=FALSE, kernel=ksvm.kernel_type, kpar = "automatic", C=1, gamma=1)
ksvm.predictions <- predict(ksvm.fit, test_df.x)

# スパムテストデータの通し番号
spam_from <- 1
spam_to <- length(test.y[which(test.y==1)])

ksvm.predictions.spam_results <- ksvm.predictions[spam_from:spam_to]
test_class_spam <- test_df.x$ZSpecies[spam_from:spam_to]
ksvm.spam_accuracy <- length(ksvm.predictions.spam_results[which(ksvm.predictions.spam_results == test_class_spam)]) / length(ksvm.predictions.spam_results)

# ハムテストデータの通し番号
ham_from <- (spam_to + 1)
ham_to <- (spam_to +length(test.y[which(test.y==0)]))

ksvm.predictions.ham_results <- ksvm.predictions[ham_from:ham_to]
test_class_ham <- test_df.x$ZSpecies[ham_from:ham_to]
ksvm.ham_accuracy <- length(ksvm.predictions.ham_results[which(ksvm.predictions.ham_results == test_class_ham)]) / length(ksvm.predictions.ham_results)

(ksvm.spam_accuracy + ksvm.ham_accuracy) / 2

# ksvm.result <- length(ksvm.predictions[which(ksvm.predictions == test_df.x$ZSpecies)]) / length(ksvm.predictions)

result_KSVM[[cnt_j]] <- c(ksvm.spam_accuracy, ksvm.ham_accuracy)

print(paste("ksvm:", ksvm.kernel_type, sep=""))
print(ksvm.spam_accuracy)
print(ksvm.ham_accuracy) 

 