##################################
# 目的 : LSNBの実行部 
# ソースコード名 : Test_LS.R
# バージョン : SR(1)
# 最終変更日時 : 2017.06.08, 16:56
# 作成者 : Taniguchi Hidetaka
##################################

# Classify them all!(LS版)
easyham2.class_LSNB <- suppressWarnings(lapply(easyham2.docs,
                                   function(p)
                                   {
                                     spam.classifier_LSNB(file.path(easyham.path, p))
                                   }))

spam2.class_LSNB <- suppressWarnings(lapply(spam2.docs,
                                function(p)
                                {
                                  spam.classifier_LSNB(file.path(spam.path, p))
                                }))

# Create a single, final, data frame with all of the classification data in it
easyham2.matrix_LSNB <- do.call(rbind, easyham2.class_LSNB)
easyham2.final_LSNB <- cbind(easyham2.matrix_LSNB, "EASYHAM")

spam2.matrix_LSNB <- do.call(rbind, spam2.class_LSNB)
spam2.final_LSNB <- cbind(spam2.matrix_LSNB, "SPAM")

class.matrix_LSNB <- rbind(easyham2.final_LSNB, spam2.final_LSNB)
class.df_LSNB <- data.frame(class.matrix_LSNB, stringsAsFactors = FALSE)
names(class.df_LSNB) <- c("Pr.SPAM" ,"Pr.HAM", "Class_LSNB", "Type")
class.df_LSNB$Pr.SPAM <- as.numeric(class.df_LSNB$Pr.SPAM)
class.df_LSNB$Pr.HAM <- as.numeric(class.df_LSNB$Pr.HAM)
class.df_LSNB$Class <- as.logical(as.numeric(class.df_LSNB$Class))
class.df_LSNB$Type <- as.factor(class.df_LSNB$Type)

get.results <- function(bool.vector)
{
  results <- c(length(bool.vector[which(bool.vector == FALSE)]) / length(bool.vector),
               length(bool.vector[which(bool.vector == TRUE)]) / length(bool.vector))
  return(results)
}

# Save results as a 2x3 table
easyham2.col_LSNB <- get.results(subset(class.df_LSNB, Type == "EASYHAM")$Class)
spam2.col_LSNB <- get.results(subset(class.df_LSNB, Type == "SPAM")$Class)

class.res_LSNB <- rbind(easyham2.col_LSNB, spam2.col_LSNB)
colnames(class.res_LSNB) <- c("NOT SPAM", "SPAM")
print(class.res_LSNB)

result_LSNB[[cnt_j]] <- c(class.res_LSNB[1,1], class.res_LSNB[2,2])