##################################
# 目的 : 教師データおよびテストデータが存在するディレクトのパスを定義する. 
# ソースコード名 : path.R
# バージョン : SR(1)
# 最終変更日時 : 2017.06.08, 16:58
# 作成者 : Taniguchi Hidetaka
##################################

# ファイルのある相対アドレスを指定する
spam.path <- file.path(paste(CORPUS_NAME, "_data", sep=""), "spam")
easyham.path <- file.path(paste(CORPUS_NAME, "_data", sep=""), "ham")

if(CORPUS_NAME == "sa") {
	hardham.path <- file.path("data", "hard_ham")
	hardham.docs <- dir(hardham.path)
	hardham.docs <- hardham.docs[which(hardham.docs != "cmds")]
}
 