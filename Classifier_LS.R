##################################
# 目的 : LSNBの実行部.
# ソースコード名 : Classifier_LS.R
# バージョン : SR(1)
# 最終変更日時 : 2017.06.08, 16:57
# 作成者 : Taniguchi Hidetaka
##################################

# LSNB
classify_LSNB.email <- function(path, training.df, prior, c = ccc)
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
    # LSNBにdensityを加えた
    match.probs <- training.df$occurrence_LS[match(msg.match, training.df$term)]
    
    return(prior * prod(match.probs) * c ^ (length(msg.freq) - length(msg.match)))
  }
}

# LSNBの実行部
spam.classifier_LSNB <- function(path)
{
  pr.spam <- classify_LSNB.email(path, spam.df, PRIOR_NB_SPAM)
  pr.ham <- classify_LSNB.email(path, easyham.df, PRIOR_NB_HAM)
  return(c(pr.spam, pr.ham, ifelse(pr.spam > pr.ham, 1, 0)))
}