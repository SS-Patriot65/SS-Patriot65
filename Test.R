##################################
# 目的 : ナイーブベイズの実行部 
# ソースコード名 : test.R
# バージョン : SR(1)
# 最終変更日時 : 2017.06.08, 16:56
# 作成者 : Taniguchi Hidetaka
##################################

# 教師データに含まれないデータを抽出する 
spam2.docs <- dir(spam.path)[is.na(match(dir(spam.path), spam.docs))==TRUE]
tmp <- sample(length(spam2.docs), NUMBER_OF_TEST_DATA_SPAM)
spam2.docs <- spam2.docs[tmp]
spam2.docs <- spam2.docs[which(is.na(match(spam2.docs, "cmds"))==TRUE)]

easyham2.docs <- dir(easyham.path)[is.na(match(dir(easyham.path), easyham.docs))==TRUE]
tmp <- sample(length(easyham2.docs), NUMBER_OF_TEST_DATA_HAM)
easyham2.docs <- easyham2.docs[tmp]
easyham2.docs <- easyham2.docs[which(is.na(match(easyham2.docs, "cmds"))==TRUE)]

# Classify them all!
easyham2.class <- suppressWarnings(lapply(easyham2.docs,
                                   function(p)
                                   {
                                     spam.classifier(file.path(easyham.path, p))
                                   }))

spam2.class <- suppressWarnings(lapply(spam2.docs,
                                function(p)
                                {
                                  spam.classifier(file.path(spam.path, p))
                                }))

# Create a single, final, data frame with all of the classification data in it
easyham2.matrix <- do.call(rbind, easyham2.class)
easyham2.final <- cbind(easyham2.matrix, "EASYHAM")

spam2.matrix <- do.call(rbind, spam2.class)
spam2.final <- cbind(spam2.matrix, "SPAM")

class.matrix <- rbind(easyham2.final, spam2.final)
class.df <- data.frame(class.matrix, stringsAsFactors = FALSE)
names(class.df) <- c("Pr.SPAM" ,"Pr.HAM", "Class", "Type")
class.df$Pr.SPAM <- as.numeric(class.df$Pr.SPAM)
class.df$Pr.HAM <- as.numeric(class.df$Pr.HAM)
class.df$Class <- as.logical(as.numeric(class.df$Class))
class.df$Type <- as.factor(class.df$Type)

get.results <- function(bool.vector)
{
  results <- c(length(bool.vector[which(bool.vector == FALSE)]) / length(bool.vector),
               length(bool.vector[which(bool.vector == TRUE)]) / length(bool.vector))
  return(results)
}

# Save results as a 2x3 table
easyham2.col <- get.results(subset(class.df, Type == "EASYHAM")$Class)
spam2.col <- get.results(subset(class.df, Type == "SPAM")$Class)

class.res <- rbind(easyham2.col, spam2.col)
colnames(class.res) <- c("NOT SPAM", "SPAM")
print(class.res)

result_NB[[cnt_j]] <- c(class.res[1,1], class.res[2,2])

# 教師データに使用したファイルの数の表示
length(easyham.docs)
length(spam.docs)

# 教師データの情報を記録する。ここでファイル名を指定
# Log_name_Spam <- paste("spam_df_", cnt_j, ".csv", sep="")
# Log_name_Easy_Ham <- paste("easyham_df_", cnt_j, ".csv", sep="")

# write.csv(spam.df, file.path("data", Log_name_Spam), row.names = FALSE)
# write.csv(easyham.df, file.path("data", Log_name_Easy_Ham), row.names = FALSE)
