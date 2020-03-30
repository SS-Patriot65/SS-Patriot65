##################################
# 目的 : ナイーブベイズの改良型の定義部. 
# ソースコード名 : New_nB_Classifier.R
# バージョン : SR(1)
# 最終変更日時 : 2017.06.08, 16:56
# 作成者 : Taniguchi Hidetaka
##################################

# Newナイーブベイズ
classify.email <- function(path, training.df, prior, c = ccc)
{
  # Here, we use many of the support functions to get the
  # email text data in a workable format
  msg <- get.msg(path)
  msg.tdm <- get.tdm(msg)
  msg.freq <- rowSums(as.matrix(msg.tdm))

  # Find intersections of words
  msg.match <- intersect(names(msg.freq), training.df$term)

  # Now, we just perform the naive Bayes calculation
  if(length(msg.match) < 1)
  {
    return(prior * c ^ (length(msg.freq)))
  }
  else
  {
    match.probs <- training.df$density[match(msg.match, training.df$term)] * training.df$occurrence[match(msg.match, training.df$term)]
    return_value <- prior * prod(match.probs) * c ^ (length(msg.freq) - length(msg.match))

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

# Newナイーブベイズ分類器の実行部
spam.classifier <- function(path)
{
  pr.spam <- classify.email(path, spam.df, PRIOR_NB_SPAM)
  pr.ham <- classify.email(path, easyham.df, PRIOR_NB_HAM)
  return(c(pr.spam, pr.ham, ifelse(pr.spam > pr.ham, 1, 0)))
}
