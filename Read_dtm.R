##################################
# 目的 : SR 用プログラム
# ソースコード名 : Read_dtm.R
# バージョン : SR(1)
# 最終変更日時 : 2017.05.31, 11:36
# 作成者 : Taniguchi Hidetaka
##################################

{
if(CORPUS_NAME == "pizza") {
	load(file.path('dtm.RData'))
}

else {
	dtm <- read.csv(paste(CORPUS_NAME, ".csv", sep=""))
	# dtm <- read.csv(paste("tmp.csv", sep=""))
}
}

 