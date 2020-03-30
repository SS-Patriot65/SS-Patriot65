##################################
# 目的 : LSの定義部. 
# ソースコード名 : LS.R
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

	  as.a <- spam.df$countPos[i]

	  as.b <- spam.df$countNeg[i]

	  as.c <- ifelse(is.na(idx),
                      ccc,
                      easyham.df$countPos[idx])

	  as.d <- ifelse(is.na(idx),
                      ccc,
                      easyham.df$countNeg[idx])

	  calculate.LS(as.a, as.b, as.c, as.d)
        }
)

easyham.occurrence_LS <- sapply(1:nrow(easyham.df),
	function(i){
	  idx <- match(easyham.df$term[i], spam.df$term)

	  as.a <- easyham.df$countPos[i]

	  as.b <- easyham.df$countNeg[i]

	  as.c <- ifelse(is.na(idx),
                      ccc,
                      spam.df$countPos[idx])

	  as.d <- ifelse(is.na(idx),
                      ccc,
                      spam.df$countNeg[idx])

	  calculate.LS(as.a, as.b, as.c, as.d)
})

spam.df <- transform(spam.df,
                     occurrence_LS = spam.occurrence_LS)

easyham.df <- transform(easyham.df,
                     occurrence_LS = easyham.occurrence_LS)