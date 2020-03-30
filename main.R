##################################
# 目的 : mainファイル. これを実行すると実験が開始される. 結果の保存もこのプログラムで行う. 
# ソースコード名 : main.R
# バージョン : SR(2)
# 最終変更日時 : 2017.06.11, 19:19
# 作成者 : Taniguchi Hidetaka
##################################

library('tm')
library('ggplot2')

setwd("C:\\Users\\V\\Desktop\\spam_classifier\\src")
(getwd())

# 利用するコーパス名
CORPUS_NAME <- "ling"
df_skew <- FALSE

# NB, LSNB, eLSNBで特徴数を限定する場合の量
if(df_skew == TRUE) {
	df_skewed_number <- 10
}

# 何回試行するか
ITERATION <- 6

# 各回で、何度プログラムを回すか
LOOP_COUNTER <- 50

# 各ファイルから、ある単語がN回以上観測された場合、単語文章行列に加える
MIN_DOC_FREQ_N <- 2

corpus_length_spam <- -1
corpus_length_ham <- -1

if(CORPUS_NAME == "enron") {
	corpus_length_spam <- 1496
	corpus_length_ham <- 4360
}

if(CORPUS_NAME == "ling") {
	corpus_length_spam <- 470
	corpus_length_ham <- 2400
}

if(CORPUS_NAME == "sa") {
	corpus_length_spam <- 492
	corpus_length_ham <- 2442
}

# テストデータの数
NUMBER_OF_TEST_DATA_HAM <- (corpus_length_ham / 2)
NUMBER_OF_TEST_DATA_SPAM <- (corpus_length_spam / 2)

# NUMBER_OF_TEST_DATA_HAM <- 2
# NUMBER_OF_TEST_DATA_SPAM <- 2

# 事前確率
PRIOR_NB_SPAM <- 0.500
PRIOR_NB_HAM <- (1.000 - PRIOR_NB_SPAM)

# 未知語に対する確率設定
ccc <- 1e-6

# Set the global paths
source('path.R')

# NBの結果
result_NB <- list()
result_LSNB <- list()
result_LSNB_New <- list()

result_SVM <- list()
result_KSVM <- list()
result_NN <- list()
result_LoRe <- list()
result_RR <- list()

cnt_i <- 1
cnt_j <- 1

# データの間隔
HAM_SPARSITY <- 40

# 使用するハムデータの初期値
HAM_FROM <- 40

# ハムに対して、スパムのデータをどれだけ使うか
SPAM_RATIO <- (1/1)

# Ham 固定用
# SPAM_SPARSITY <- 40
# SPAM_FROM <- 40
# HAM_RATIO <- (1/1)

for(cnt_i in 1:ITERATION)
{
	# 学習に使う教師データの数を更新する
 	# NUM_SUPERVISE_DATASETS_EASYHAM <- HAM_FROM + (HAM_SPARSITY * (cnt_i-1))
	# NUM_SUPERVISE_DATASETS_SPAM <- (NUM_SUPERVISE_DATASETS_EASYHAM * SPAM_RATIO)

	# Spam 固定用
 	NUM_SUPERVISE_DATASETS_EASYHAM <- HAM_FROM + (HAM_SPARSITY * (cnt_i-1))
	NUM_SUPERVISE_DATASETS_SPAM <- 50

	# Ham 固定用
 	# NUM_SUPERVISE_DATASETS_EASYHAM <- 50
	# NUM_SUPERVISE_DATASETS_SPAM <- SPAM_FROM + (SPAM_SPARSITY * (cnt_i-1))

	# ワンショット
	# NUM_SUPERVISE_DATASETS_EASYHAM <- 1
	# NUM_SUPERVISE_DATASETS_SPAM <- 1

	# コーパス作成用
	# NUM_SUPERVISE_DATASETS_EASYHAM <- corpus_length_ham - 2
	# NUM_SUPERVISE_DATASETS_SPAM <- corpus_length_spam - 2

	NUM_SUPERVISE_DATASETS_TOTAL <- (NUM_SUPERVISE_DATASETS_EASYHAM + NUM_SUPERVISE_DATASETS_SPAM)

	# プログラム実行部
	for(cnt_j in 1:LOOP_COUNTER)
	{
		# 分類器宣言
		source('Classifier.R')
		source('Classifier_LS.R')
	
		# 教師データの読み込みと、単語文章行列の生成
		source('Training.R')

		# 分類器の再設定
		source('Test.R')

		# LSの再設定
		source('New_NB_Classifier.R')
		source('New_LS.R')
		source('Test_LS.R')

		{
		if(CORPUS_NAME == "sa") { 
			source('Classifier_LSNB_New_for_SA.R')
		}

		else {
			source('Classifier_LSNB_New.R')
		}
		}

		source('Test_LS_New.R')

		if(CORPUS_NAME != "pizza") {
			if(cnt_i == 1 && cnt_j == 1) {
				source('Read_dtm.R')
			}
		}

		else {
			source('Read_dtm.R')
		}

		# サポートベクターマシン(SVM)
		source("chapter12.R")
		print("--------------")

		# サポートベクターマシン(KSVM)
		source("ksvm.R")
		print("--------------")

		# ニューラルネットワーク(NN)
		source("NNET.R")
		print("--------------")

		# ロジスティック回帰(LoRe)
		source("Logistic_Regression.R")
		print("--------------")

		# ランダムフォレスト(RR)
		source("Random_Forest.R")
		print("--------------")

		# クリア関数
		source('Clear.R')

		print("--------------")
	}

	# NBの結果を記録する
	result_matrix_NB <- data.frame(result_NB)
	result_matrix_NB <- t(result_matrix_NB)
	result_NB_name <- paste(CORPUS_NAME, "_results//NB//", CORPUS_NAME, "_NB_", "h", NUM_SUPERVISE_DATASETS_EASYHAM, "_", "s", NUM_SUPERVISE_DATASETS_SPAM, "(hp", PRIOR_NB_HAM, ",hs", PRIOR_NB_SPAM, ")", ".csv", sep="")
	write.table(result_matrix_NB, result_NB_name, sep=",", col.names=F, row.names=F, append=T)

	# LSNBの結果を記録する
	result_matrix_LSNB <- data.frame(result_LSNB)
	result_matrix_LSNB <- t(result_matrix_LSNB)
	result_LSNB_name <- paste(CORPUS_NAME, "_results//LSNB//", CORPUS_NAME,"_LSNB_", "h", NUM_SUPERVISE_DATASETS_EASYHAM, "_", "s", NUM_SUPERVISE_DATASETS_SPAM, "(hp", PRIOR_NB_HAM, ",hs", PRIOR_NB_SPAM, ")", ".csv", sep="")
	write.table(result_matrix_LSNB, result_LSNB_name, sep=",", col.names=F, row.names=F, append=T)

	# LSNB_Newの結果を記録する
	result_matrix_LSNB_New <- data.frame(result_LSNB_New)
	result_matrix_LSNB_New <- t(result_matrix_LSNB_New)
	result_LSNB_New_name <- paste(CORPUS_NAME, "_results//LSNB_New//", CORPUS_NAME, "_LSNB_New_", "h", NUM_SUPERVISE_DATASETS_EASYHAM, "_", "s", NUM_SUPERVISE_DATASETS_SPAM, "(hp", PRIOR_NB_HAM, ",hs", PRIOR_NB_SPAM, ")", ".csv", sep="")
	write.table(result_matrix_LSNB_New, result_LSNB_New_name, sep=",", col.names=F, row.names=F, append=T)

	# SVMの結果を記録する
	result_matrix_SVM <- data.frame(result_SVM)
	result_matrix_SVM <- t(result_matrix_SVM)
	result_SVM_name <- paste(CORPUS_NAME, "_results//SVM//", CORPUS_NAME, "_SVM_", "h", NUM_SUPERVISE_DATASETS_EASYHAM, "_", "s", NUM_SUPERVISE_DATASETS_SPAM, "(hp", PRIOR_NB_HAM, ",hs", PRIOR_NB_SPAM, ")", ".csv", sep="")
	write.table(result_matrix_SVM, result_SVM_name, sep=",", col.names=F, row.names=F, append=T)

	# KSVMの結果を記録する
	result_matrix_KSVM <- data.frame(result_KSVM)
	result_matrix_KSVM <- t(result_matrix_KSVM)
	result_KSVM_name <- paste(CORPUS_NAME, "_results//KSVM//", CORPUS_NAME, "_KSVM_", "h", NUM_SUPERVISE_DATASETS_EASYHAM, "_", "s", NUM_SUPERVISE_DATASETS_SPAM, "(hp", PRIOR_NB_HAM, ",hs", PRIOR_NB_SPAM, ")", ".csv", sep="")
	write.table(result_matrix_KSVM, result_KSVM_name, sep=",", col.names=F, row.names=F, append=T)

	# NNの結果を記録する
	result_matrix_NN <- data.frame(result_NN)
	result_matrix_NN <- t(result_matrix_NN)
	result_NN_name <- paste(CORPUS_NAME, "_results//NN//", CORPUS_NAME, "_NN_", "h", NUM_SUPERVISE_DATASETS_EASYHAM, "_", "s", NUM_SUPERVISE_DATASETS_SPAM, "(hp", PRIOR_NB_HAM, ",hs", PRIOR_NB_SPAM, ")", ".csv", sep="")
	write.table(result_matrix_NN, result_NN_name, sep=",", col.names=F, row.names=F, append=T)

	# LoReの結果を記録する
	result_matrix_LoRe <- data.frame(result_LoRe)
	result_matrix_LoRe <- t(result_matrix_LoRe)
	result_LoRe_name <- paste(CORPUS_NAME, "_results//LoRe//", CORPUS_NAME, "_LoRe_", "h", NUM_SUPERVISE_DATASETS_EASYHAM, "_", "s", NUM_SUPERVISE_DATASETS_SPAM, "(hp", PRIOR_NB_HAM, ",hs", PRIOR_NB_SPAM, ")", ".csv", sep="")
	write.table(result_matrix_LoRe, result_LoRe_name, sep=",", col.names=F, row.names=F, append=T)

	# RRの結果を記録する
	result_matrix_RR <- data.frame(result_RR)
	result_matrix_RR <- t(result_matrix_RR)
	result_RR_name <- paste(CORPUS_NAME, "_results//RR//", CORPUS_NAME, "_RR_", "h", NUM_SUPERVISE_DATASETS_EASYHAM, "_", "s", NUM_SUPERVISE_DATASETS_SPAM, "(hp", PRIOR_NB_HAM, ",hs", PRIOR_NB_SPAM, ")", ".csv", sep="")
	write.table(result_matrix_RR, result_RR_name, sep=",", col.names=F, row.names=F, append=T)
}

 