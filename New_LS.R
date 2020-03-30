##################################
# 目的 : LSの実行部. 
# ソースコード名 : New_LS.R
# バージョン : SR(1)
# 最終変更日時 : 2017.06.08, 16:56
# 作成者 : Taniguchi Hidetaka
##################################

calculate.LS <- function(as.a, as.b, as.c, as.d)
{
  bias.neg <- as.a * as.c/(as.a + as.c)
  bias.pos <- as.b * as.d/(as.b + as.d)

  return((as.a + bias.pos)/(as.a + bias.pos + as.b + bias.neg))
}

spam.occurrence_LS <- sapply(1:nrow(spam.df),
	function(i){
	  idx <- match(spam.df$term[i], easyham.df$term)

	  as.a <- spam.df$countPos[i] * spam.df$density[i]

	  as.b <- ifelse(is.na(idx), 
			  spam.df$countNeg[i] * ccc, 
			  spam.df$countNeg[i] * easyham.df$density[idx])

	  as.c <- ifelse(is.na(idx),
                    ccc * spam.df$density[i],
			  easyham.df$countPos[idx] * spam.df$density[i])

	  as.d <- ifelse(is.na(idx),
                    (1.0 - ccc) * ccc,
                    easyham.df$countNeg[idx] * easyham.df$density[idx])

	  calculate.LS(as.a, as.b, as.c, as.d)
        }
)

easyham.occurrence_LS <- sapply(1:nrow(easyham.df),
	function(i){
	  idx <- match(easyham.df$term[i], spam.df$term)

	  as.a <- easyham.df$countPos[i] * easyham.df$density[i]

	  as.b <- ifelse(is.na(idx), 
			  easyham.df$countNeg[i] * ccc, 
			  easyham.df$countNeg[i] * spam.df$density[idx])

	  as.c <- ifelse(is.na(idx),
                    ccc * easyham.df$density[i],
			  spam.df$countPos[idx] * easyham.df$density[i])

	  as.d <- ifelse(is.na(idx),
                    (1.0 - ccc) * ccc,
                    spam.df$countNeg[idx] * spam.df$density[idx])

	  calculate.LS(as.a, as.b, as.c, as.d)
})

spam.df <- transform(spam.df,
                     occurrence_LS = spam.occurrence_LS)

spam.df$occurrence_LS[which(is.na(spam.df$occurrence_LS))] <- ccc


easyham.df <- transform(easyham.df,
                     occurrence_LS = easyham.occurrence_LS)

easyham.df$occurrence_LS[which(is.na(easyham.df$occurrence_LS))] <- ccc

 