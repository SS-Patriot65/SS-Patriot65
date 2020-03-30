##################################
# 目的 : SVM実行プログラム. また, NNなどで使うデータフレームをここで作る. 
# ソースコード名 : chapter12.R
# バージョン : SR(1)
# 最終変更日時 : 2017.06.08, 16:57
# 作成者 : Taniguchi Hidetaka
##################################

library('e1071')

# Eleventh code snippet
# load(file.path('dtm.RData'))

##########################

training_data_mix.docs <- c(spam.docs, easyham.docs)
training_data_mix.docs <- sort(training_data_mix.docs)

head(training_data_mix.docs)

{
if(CORPUS_NAME == "pizza") {
	# 全データから、NBの教師データとして使われたものと同じファイル名のもののみを抽出する
	training_dtm <- dtm[which(is.na(match(rownames(dtm), training_data_mix.docs))==FALSE),]
	head(sort(rownames(training_dtm)))
}

else {
	# 全データから、NBの教師データとして使われたものと同じファイル名のもののみを抽出する
	training_dtm <- dtm[which(is.na(match(dtm$X, training_data_mix.docs))==FALSE),]
	# head(sort(rownames(training_dtm)))
}
}

# 教師データ数
length(training_dtm[,1])

# 読み込まれた全データ数
length(dtm[,1])

# 全データ（hardhamを除く）には、スパム481通、イージーハム2412通の順に格納されている。
# このため、yに先頭の500通のメールのスパムクラスとして格納し、その他はハムクラスとする
y <- c(1:length(dtm[,1]))

for(i in 1 : length(y))　{
	{if(i <= corpus_length_spam)　{
			y[i] <- 1
	}
	else　{
			y[i] <- 0
	}}
}

##########
test_data_mix.docs <- c(spam2.docs, easyham2.docs)
test_data_mix.docs <- sort(test_data_mix.docs)

{
if(CORPUS_NAME == "pizza") {
	# 全データの中から、教師データに含まれなかったものを更に抽出し、テストデータとする
	test_dtm <- dtm[which(is.na(match(rownames(dtm), test_data_mix.docs))==FALSE),]
	# head(sort(rownames(test_dtm)))
}

else {
	# 全データの中から、教師データに含まれなかったものを更に抽出し、テストデータとする
	test_dtm <- dtm[which(is.na(match(dtm$X, test_data_mix.docs))==FALSE),]
	# head(sort(rownames(test_dtm)))
}
}

# テストデータ数
length(test_dtm[,1])

###########

{
if(CORPUS_NAME == "pizza") {
	# 教師データ名（ファイル名） * 単語数の行列を作る
	train.x <- dtm[which(is.na(match(rownames(dtm), rownames(training_dtm)))==FALSE), 3:ncol(training_dtm)]

	# 教師データ数 * 教師データのクラスのベクトルを作る
	train.y <- y[which(is.na(match(rownames(dtm), rownames(training_dtm)))==FALSE)]
	names(train.y) <- rownames(training_dtm)

	# テストデータ名（ファイル名） * 単語数の行列を作る
	test.x <- dtm[which(is.na(match(rownames(dtm), rownames(test_dtm)))==FALSE), 3:ncol(test_dtm)]

	# テストデータ数 * テストデータのクラスのベクトルを作る
	test.y <- y[which(is.na(match(rownames(dtm), rownames(test_dtm)))==FALSE)]
	names(test.y) <- rownames(test_dtm)
}

else {
	# 教師データ名（ファイル名） * 単語数の行列を作る
	train.x <- dtm[which(is.na(match(dtm$X, training_dtm$X))==FALSE), 3:ncol(training_dtm)]

	# 教師データ数 * 教師データのクラスのベクトルを作る
	train.y <- y[which(is.na(match(dtm$X, training_dtm$X))==FALSE)]
	names(train.y) <- training_dtm$X

	# テストデータ名（ファイル名） * 単語数の行列を作る
	test.x <- dtm[which(is.na(match(dtm$X, test_dtm$X))==FALSE), 3:ncol(test_dtm)]

	# テストデータ数 * テストデータのクラスのベクトルを作る
	test.y <- y[which(is.na(match(dtm$X, test_dtm$X))==FALSE)]
	names(test.y) <- test_dtm$X
}
}

head(sort(rownames(test.x)))

# is.na(match(names(dtm[,1]), names(test.y))==TRUE)
# which(match(names(dtm[,1]), names(train.y))==TRUE)

# rm(dtm)

# 読み込んだ教師データの数(スパム)
length(train.y[which(train.y==1)])
# 読み込んだ教師データの数(ハム)
length(train.y[which(train.y==0)])

# 読み込んだテストデータの数(スパム)
length(test.y[which(test.y==1)])
# 読み込んだテストデータの数(ハム)
length(test.y[which(test.y==0)])

###########################################

train_df.x <- data.frame(train.x)
train_df.x$ZSpecies <- train.y
names(train_df.x$ZSpecies) <- "ZSpecies"

test_df.x <- data.frame(test.x)
test_df.x$ZSpecies <- test.y
names(test_df.x$ZSpecies) <- "ZSpecies"

train_df.x$ZSpecies <- cut(train_df.x$ZSpecies, c(0, 0.5, 1.0), labels=c("ham", "spam"), right=FALSE)
test_df.x$ZSpecies <- cut(test_df.x$ZSpecies, c(0, 0.5, 1.0), labels=c("ham", "spam"), right=FALSE)

svm.kernel_type <- "radial"

# SVM, 線形カーネル
svm.fit <- svm(ZSpecies~., data=train_df.x, scale=FALSE, kernel=svm.kernel_type, cost=0.1)

# Seventeenth code snippet
svm.predictions <- predict(svm.fit, test_df.x)

svm.predictions.spam_results <- svm.predictions[1:length(test.y[which(test.y==1)])]
test_class_spam <- test_df.x$ZSpecies[1:length(test.y[which(test.y==1)])]
svm.spam_accuracy <- length(svm.predictions.spam_results[which(svm.predictions.spam_results == test_class_spam)]) / length(svm.predictions.spam_results)

svm.predictions.ham_results <- svm.predictions[(length(test_class_spam)+1):length(test.y[which(test.y==0)])]
test_class_ham <- test_df.x$ZSpecies[(length(test_class_spam)+1):length(test.y[which(test.y==0)])]
svm.ham_accuracy <- length(svm.predictions.ham_results[which(svm.predictions.ham_results == test_class_ham)]) / length(svm.predictions.ham_results)

# 
svm.result <- length(svm.predictions[which(svm.predictions == test_df.x$ZSpecies)]) / length(svm.predictions)

result_SVM[[cnt_j]] <- c(svm.spam_accuracy, svm.ham_accuracy)

print(paste("svm:", svm.kernel_type, sep=""))
print(svm.spam_accuracy)
print(svm.ham_accuracy)

 