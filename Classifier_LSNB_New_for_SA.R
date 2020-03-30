##################################
# 目的 : SpamAssassinを用いたときのeLSNBの実行部. 現状では必要ないかも. 
# ソースコード名 : Classifier_LSNB_New_for_SA.R
# バージョン : SR(1)
# 最終変更日時 : 2017.06.08, 16:59
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

  match.probs <- 0.0
  num_not_match <- 0

  # クラス1と共起する単語を抽出する
  msg.match_tr <- intersect(names(msg.freq), training.df$term)
  
  # クラス2と共起する単語を抽出する
  msg.match_cp <- intersect(names(msg.freq), counterpart.df$term)

  {
  # 2つのクラスのどちらでも共起しなかった場合、P(mail|class)=c^単語数
  if((length(msg.match_tr) + length(msg.match_cp)) < 1)
  {
    return(log(prior) + log(c) * (length(msg.freq)))
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
          as.a <- training.df$occurrence[idx_tr] * training.df$density[idx_tr]
	    as.b <- (1.0 - training.df$occurrence[idx_tr]) * c
	    as.c <- c * c
	    as.d <- (1.0 - c) * training.df$density[idx_tr]

	    if(as.a == 0.0) as.a <- c
	    if(as.b == 0.0) as.b <- c
	    if(as.c == 0.0) as.c <- c
	    if(as.d == 0.0) as.d <- c

	    tmp_occ <- calculate.LS(as.a, as.b, as.c, as.d)
	    match.probs <- match.probs + log(tmp_occ)
      }

	# クラス2のみで見つかった
	else if(is.na(idx_tr) && is.na(idx_cp)==F)
      {
          as.a <- c * c
	    as.b <- (1.0 - c) * counterpart.df$density[idx_cp]
	    as.c <- counterpart.df$occurrence[idx_cp] * counterpart.df$density[idx_cp]
	    as.d <- (1.0 - counterpart.df$occurrence[idx_cp]) * c

	    if(as.a == 0.0) as.a <- c
	    if(as.b == 0.0) as.b <- c
	    if(as.c == 0.0) as.c <- c
	    if(as.d == 0.0) as.d <- c

	    tmp_occ <- calculate.LS(as.a, as.b, as.c, as.d)
	    match.probs <- match.probs + log(tmp_occ)
      }

	# 両方ともで見つかった
	else
	{
	    as.a <- training.df$occurrence[idx_tr] * training.df$density[idx_tr]
	    as.b <- (1.0 - training.df$occurrence[idx_tr]) * counterpart.df$density[idx_cp]
	    as.c <- counterpart.df$countPos[idx_cp] * counterpart.df$density[idx_cp]
	    as.d <- (1.0 - counterpart.df$occurrence[idx_cp]) * training.df$density[idx_tr]

	    if(as.a == 0.0) as.a <- c
	    if(as.b == 0.0) as.b <- c
	    if(as.c == 0.0) as.c <- c
	    if(as.d == 0.0) as.d <- c

	    tmp_occ <- calculate.LS(as.a, as.b, as.c, as.d)
	    match.probs <- match.probs + log(tmp_occ)
	}
	}
    }

    # アンダーフローを防ぐ 
    if(num_not_match >= (MIN_DEC+1))
    {
	  num_not_match = MIN_DEC
    }

    return_value <- log(prior) + match.probs + (log(c) * num_not_match)
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
