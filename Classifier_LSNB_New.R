##################################
# 目的 : eLSNBの実行部. 
# ソースコード名 : Classifier_LSNB_New.R
# バージョン : SR(1)
# 最終変更日時 : 2017.06.08, 16:57
# 作成者 : Taniguchi Hidetaka
##################################

# LSNB
classify_LSNB_New.email <- function(path, training.df, counterpart.df, prior, c = ccc)
{
  msg <- get.msg(path)
  msg.tdm <- get.tdm(msg)
  msg.matrix <- as.matrix(msg.tdm)
  msg.freq <- rowSums(msg.matrix)

  # 1e-323を下回るとアンダーフローしてしまうので、その前に下限値を決めておく
  MIN_DEC <- 40

  match.probs <- 1.0
  num_not_match <- 0

  # クラス1と共起する単語を抽出する
  msg.match_tr <- intersect(names(msg.freq), training.df$term)
  
  # クラス2と共起する単語を抽出する
  msg.match_cp <- intersect(names(msg.freq), counterpart.df$term)

  {
  # 2つのクラスのどちらでも共起しなかった場合、P(mail|class)=c^単語数
  if((length(msg.match_tr) + length(msg.match_cp)) < 1)
  {
    return(prior * c ^ (length(msg.freq)))
  }

  else
  {
    # pathから読み込んだファイル群を基に、データフレームを生成する
    msg.df <- data.frame(cbind(names(msg.freq),
                          as.numeric(msg.freq)),
                    stringsAsFactors = FALSE)
    names(msg.df) <- c("term", "frequency")
    msg.df$frequency <- as.numeric(msg.df$frequency)

    # pathから作ったデータフレームに含まれる単語を確認する
    for(i in 1:nrow(msg.df))
    {
      # クラス1と共起するかを調べ、共起すれば該当する物のtraining.df$termでの通し番号を記録
      idx_tr <- match(msg.df$term[i], training.df$term)
      idx_cp <- match(msg.df$term[i], counterpart.df$term)

	{
	# ある単語が2つのクラス両方で見つからなかった
      if(is.na(idx_tr) && is.na(idx_cp))
      {
	    num_not_match <- num_not_match + 1
      }

	# クラス1のみで見つかった
	else if(is.na(idx_tr)==F && is.na(idx_cp))
      {
          as.a <- training.df$countPos[idx_tr] * training.df$density[idx_tr]
	    as.b <- training.df$countNeg[idx_tr] * c
	    as.c <- c * c
	    as.d <- (1.0 - c) * training.df$density[idx_tr]

	    tmp_occ <- calculate.LS(as.a, as.b, as.c, as.d)
	    match.probs <- match.probs * tmp_occ
      }

	# クラス2のみで見つかった
	else if(is.na(idx_tr) && is.na(idx_cp)==F)
      {
          as.a <- c * c
	    as.b <- (1.0 - c) * counterpart.df$density[idx_cp]
	    as.c <- counterpart.df$countPos[idx_cp] * counterpart.df$density[idx_cp]
	    as.d <- counterpart.df$countNeg[idx_cp] * c

	    tmp_occ <- calculate.LS(as.a, as.b, as.c, as.d)
	    match.probs <- match.probs * tmp_occ
      }

	# 両方ともで見つかった
	else
	{
	    as.a <- training.df$countPos[idx_tr] * training.df$density[idx_tr]
	    as.b <- training.df$countNeg[idx_tr] * counterpart.df$density[idx_cp]
	    as.c <- counterpart.df$countPos[idx_cp] * counterpart.df$density[idx_cp]
	    as.d <- counterpart.df$countNeg[idx_cp] * training.df$density[idx_tr]

	    tmp_occ <- calculate.LS(as.a, as.b, as.c, as.d)
	    match.probs <- match.probs * tmp_occ
	}
	}
    }

    # アンダーフローを防ぐ 
    if(num_not_match >= (MIN_DEC+1))
    {
	  num_not_match = MIN_DEC
    }

    # print(prior * match.probs * (c ^ num_not_match))

    return_value <- prior * match.probs * (c ^ num_not_match)

    if(is.na(return_value) == TRUE)
    {
        return_value <- 1e-320
    }

    # print(return_value)

    if(return_value <= 1e-320)
    {
        return_value <- 1e-320
    }

    return(return_value)
  }
  }
}

# LSNBの実行部
spam.classifier_LSNB <- function(path)
{
  pr.spam <- classify_LSNB_New.email(path, spam.df, easyham.df, PRIOR_NB_SPAM)
  pr.ham <- classify_LSNB_New.email(path, easyham.df, spam.df, PRIOR_NB_HAM)
  return(c(pr.spam, pr.ham, ifelse(pr.spam > pr.ham, 1, 0)))
}
