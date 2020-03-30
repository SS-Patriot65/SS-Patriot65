##################################
# 目的 : クリア関数. 毎ループごとに開放したい変数が存在するが, ここでは「開放したくない変数」を定義し, それ以外の変数を全て開放する. 
# ソースコード名 : Clear.R
# バージョン : SR(2)
# 最終変更日時 : 2017.06.11, 18:51
# 作成者 : Taniguchi Hidetaka
##################################

# プログラムのクリア部分、ここで全ての変数を解放する

# 宣言された全ての変数の名前を記録する
rm_list <- ls()

# ここに"開放しない変数の名前"を指定する
rm_list <- rm_list[rm_list != c("rm_list")]

rm_list <- rm_list[rm_list != c("CORPUS_NAME")]
rm_list <- rm_list[rm_list != c("df_skew")]
rm_list <- rm_list[rm_list != c("df_skewed_number")]

rm_list <- rm_list[rm_list != c("ITERATION")]
rm_list <- rm_list[rm_list != c("LOOP_COUNTER")]
rm_list <- rm_list[rm_list != c("HAM_SPARSITY")]
rm_list <- rm_list[rm_list != c("HAM_FROM")]
rm_list <- rm_list[rm_list != c("HAM_RATIO")]
rm_list <- rm_list[rm_list != c("SPAM_SPARSITY")]
rm_list <- rm_list[rm_list != c("SPAM_FROM")]
rm_list <- rm_list[rm_list != c("SPAM_RATIO")]
rm_list <- rm_list[rm_list != c("MIN_DOC_FREQ_N")]

rm_list <- rm_list[rm_list != c("PRIOR_NB_SPAM")]
rm_list <- rm_list[rm_list != c("PRIOR_NB_HAM")]
rm_list <- rm_list[rm_list != c("PRIOR_LSNB_SPAM")]
rm_list <- rm_list[rm_list != c("PRIOR_LSNB_HAM")]
rm_list <- rm_list[rm_list != c("PRIOR_eLSNB_SPAM")]
rm_list <- rm_list[rm_list != c("PRIOR_eLSNB_HAM")]
rm_list <- rm_list[rm_list != c("ccc")]

rm_list <- rm_list[rm_list != c("spam.path")]
rm_list <- rm_list[rm_list != c("easyham.path")]
rm_list <- rm_list[rm_list != c("hardham.path")]

# 結果はもちろん消さない
rm_list <- rm_list[rm_list != c("result_NB")]
rm_list <- rm_list[rm_list != c("result_LSNB")]
rm_list <- rm_list[rm_list != c("result_LSNB_New")]
rm_list <- rm_list[rm_list != c("result_SVM")]
rm_list <- rm_list[rm_list != c("result_KSVM")]
rm_list <- rm_list[rm_list != c("result_NN")]
rm_list <- rm_list[rm_list != c("result_LoRe")]
rm_list <- rm_list[rm_list != c("result_RR")]

rm_list <- rm_list[rm_list != c("cnt_i")]
rm_list <- rm_list[rm_list != c("cnt_j")]

rm_list <- rm_list[rm_list != c("dtm")]

rm_list <- rm_list[rm_list != c("corpus_length_ham")]
rm_list <- rm_list[rm_list != c("corpus_length_spam")]

rm_list <- rm_list[rm_list != c("NUM_SUPERVISE_DATASETS_EASYHAM")]
rm_list <- rm_list[rm_list != c("NUM_SUPERVISE_DATASETS_SPAM")]
rm_list <- rm_list[rm_list != c("NUM_SUPERVISE_DATASETS_TOTAL")]

rm_list <- rm_list[rm_list != c("NUMBER_OF_TEST_DATA_HAM")]
rm_list <- rm_list[rm_list != c("NUMBER_OF_TEST_DATA_SPAM")]

# テストデータは不変なので消さない
rm_list <- rm_list[rm_list != c("easyham2.docs")]
rm_list <- rm_list[rm_list != c("hardham2.docs")]
rm_list <- rm_list[rm_list != c("spam2.docs")]

# 一部を除く全ての変数を解放する
rm(list=rm_list)

 