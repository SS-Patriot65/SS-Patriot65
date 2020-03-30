##################################
# 目的 : 学習データを読み込み, 特徴ベクトルを生成する 
# ソースコード名 : Training.R
# バージョン : SR(1)
# 最終変更日時 : 2017.06.08, 16:56
# 作成者 : Taniguchi Hidetaka
##################################

# Return a single element vector of just the email body
# This is a very simple approach, as we are only using 
# words as features
get.msg <- function(path)
{
  con <- file(path, open = "rt", encoding = "latin1")
  text <- readLines(con)
  # The message always begins after the first full line break
  msg <- text
  close(con)
  return(paste(msg, collapse = "\n"))
}

# Create a TermDocumentMatrix (TDM) from the corpus of SPAM email.
# The TDM control can be modified, and the sparsity level can be 
# altered.  This TDM is used to create the feature set used to do 
# train our classifier.
get.tdm <- function(doc.vec)
{
  control <- list(stopwords = TRUE,
                  removePunctuation = TRUE,
                  removeNumbers = TRUE,
                  minDocFreq = MIN_DOC_FREQ_N)
  doc.corpus <- Corpus(VectorSource(doc.vec))
  doc.dtm <- TermDocumentMatrix(doc.corpus, control)
  return(doc.dtm)
}

# This function takes a file path to an email file and a string, 
# the term parameter, and returns the count of that term in 
# the email body.
count.word <- function(path, term)
{
  msg <- get.msg(path)
  msg.corpus <- Corpus(VectorSource(msg))
  # Hard-coded TDM control
  control <- list(stopwords = TRUE,
                  removePunctuation = TRUE,
                  removeNumbers = TRUE)
  msg.tdm <- TermDocumentMatrix(msg.corpus, control)
  word.freq <- rowSums(as.matrix(msg.tdm))
  term.freq <- word.freq[which(names(word.freq) == term)]
  # We use ifelse here because term.freq = NA if nothing is found
  return(ifelse(length(term.freq) > 0, term.freq, 0))
}

# With all of our support functions written, we can perform the classification.
# First, we create document corpus for spam messages

# Get all the SPAM-y email into a single vector
spam.docs <- dir(spam.path)
spam.docs <- spam.docs[which(spam.docs != "cmds")]

# 教師データの数を絞る
# tmp <- floor(runif(NUM_SUPERVISE_DATASETS_SPAM, 1, length(spam.docs)))
tmp <- sample(length(spam.docs), NUM_SUPERVISE_DATASETS_SPAM)
tmp.docs <- spam.docs[tmp]
spam2.docs <- spam.docs[is.na(match(spam.docs, tmp.docs))]
spam.docs <- tmp.docs

all.spam <- sapply(spam.docs,
                   function(p) get.msg(file.path(spam.path, p)))

# Create a DocumentTermMatrix from that vector
spam.tdm <- get.tdm(all.spam)

# 教師データの特徴ベクトルを含んだデータフレームを生成する
spam.matrix <- as.matrix(spam.tdm)
spam.counts <- rowSums(spam.matrix)
spam.df <- data.frame(cbind(names(spam.counts),
                            as.numeric(spam.counts)),
                      stringsAsFactors = FALSE)
names(spam.df) <- c("term", "frequency")
spam.df$frequency <- as.numeric(spam.df$frequency)

# 各単語を含むファイルの数を記録する
spam.countPos <- sapply(1:nrow(spam.matrix),
                          function(i)
                          {
                            length(which(spam.matrix[i, ] > 0))
                          })

# 各単語を含まないファイルの数を記録する
spam.countNeg <- sapply(1:nrow(spam.matrix),
				  function(i)
				  {
				    ncol(spam.matrix) - spam.countPos[i]
				  })

spam.occurrence <- sapply(1:nrow(spam.matrix),
                          function(i)
                          {
                            length(which(spam.matrix[i, ] > 0)) / ncol(spam.matrix)
                          })

spam.density <- spam.df$frequency / sum(spam.df$frequency)

# データフレームに用語密度および共起頻度を追加
spam.df <- transform(spam.df,
                     density = spam.density,
			   countPos = spam.countPos,
			   countNeg = spam.countNeg,
                     occurrence = spam.occurrence)

###########################################

if(df_skew == TRUE) {
	spam_df_occurrence_order <- order(spam.df$frequency, decreasing = TRUE)
	spam_dist_skwed <- spam_df_occurrence_order[1:df_skewed_number]

	tmp_term <- spam.df$term[spam_dist_skwed]
	tmp_frequency <- spam.df$frequency[spam_dist_skwed]
	tmp_occurrence <- spam.df$occurrence[spam_dist_skwed]
	tmp_density <- spam.df$density[spam_dist_skwed]
	tmp_countPos <- spam.df$countPos[spam_dist_skwed]
	tmp_countNeg <- spam.df$countNeg[spam_dist_skwed]

	rm(spam.df)

	spam.df <- data.frame(tmp_term,tmp_frequency, tmp_occurrence, 
				    tmp_density, tmp_countPos, tmp_countNeg, stringsAsFactors = FALSE)
	names(spam.df) <- c("term", "frequency", "occurrence", "density", "countPos", "countNeg")
}
###########################################

# 同様の事をハムデータにも行う
easyham.docs <- dir(easyham.path)
easyham.docs <- easyham.docs[which(easyham.docs != "cmds")]

# 教師データの数を絞る 
tmp <- sample(length(easyham.docs), NUM_SUPERVISE_DATASETS_EASYHAM)
tmp.docs <- easyham.docs[tmp]
# 残りのデータをテストデータにする
easyham2.docs <- easyham.docs[is.na(match(easyham.docs, tmp.docs))]
easyham.docs <- tmp.docs

all.easyham <- sapply(easyham.docs[1:length(easyham.docs)],
                      function(p) get.msg(file.path(easyham.path, p)))

easyham.tdm <- get.tdm(all.easyham)

easyham.matrix <- as.matrix(easyham.tdm)
easyham.counts <- rowSums(easyham.matrix)
easyham.df <- data.frame(cbind(names(easyham.counts),
                               as.numeric(easyham.counts)),
                         stringsAsFactors = FALSE)
names(easyham.df) <- c("term", "frequency")
easyham.df$frequency <- as.numeric(easyham.df$frequency)

# 各単語を含むファイルの数を記録する
easyham.countPos <- sapply(1:nrow(easyham.matrix),
                          function(i)
                          {
                            length(which(easyham.matrix[i, ] > 0))
                          })

# 各単語を含まないファイルの数を記録する
easyham.countNeg <- sapply(1:nrow(easyham.matrix),
				  function(i)
				  {
				    ncol(easyham.matrix) - easyham.countPos[i]
				  })

easyham.occurrence <- sapply(1:nrow(easyham.matrix),
                            function(i)
                            {
                              length(which(easyham.matrix[i, ] > 0)) / ncol(easyham.matrix)
                            })
easyham.density <- easyham.df$frequency / sum(easyham.df$frequency)

easyham.df <- transform(easyham.df,
                        density = easyham.density,
			   	countPos = easyham.countPos,
			   	countNeg = easyham.countNeg,
                        occurrence = easyham.occurrence)

###########################################

if(df_skew == TRUE) {
	easyham_df_occurrence_order <- order(easyham.df$frequency, decreasing = TRUE)
	easyham_dist_skwed <- easyham_df_occurrence_order[1:df_skewed_number]

	tmp_term <- easyham.df$term[easyham_dist_skwed]
	tmp_frequency <- easyham.df$frequency[easyham_dist_skwed]
	tmp_occurrence <- easyham.df$occurrence[easyham_dist_skwed]
	tmp_density <- easyham.df$density[easyham_dist_skwed]
	tmp_countPos <- easyham.df$countPos[easyham_dist_skwed]
	tmp_countNeg <- easyham.df$countNeg[easyham_dist_skwed]

	rm(easyham.df)

	easyham.df <- data.frame(tmp_term, tmp_frequency, tmp_occurrence,
				    tmp_density, tmp_countPos, tmp_countNeg, stringsAsFactors = FALSE)
	names(easyham.df) <- c("term", "frequency", "occurrence", "density", "countPos", "countNeg")
}

###########################################

supervised_Ham_DirName <- paste(CORPUS_NAME, "_results//supervised_data//", "Ham=", NUM_SUPERVISE_DATASETS_EASYHAM, sep="")
dir.create(supervised_Ham_DirName, showWarnings = F)
supervised_Ham_FileName <- paste(supervised_Ham_DirName, "//", cnt_j, ".csv", sep="")
write.csv(easyham.df, supervised_Ham_FileName, row.names=F, quote=T)

supervised_Spam_DirName <- paste(CORPUS_NAME, "_results//supervised_data//", "Spam=", NUM_SUPERVISE_DATASETS_SPAM, sep="")
dir.create(supervised_Spam_DirName, showWarnings = F)
supervised_Spam_FileName <- paste(supervised_Spam_DirName, "//", cnt_j, ".csv", sep="")
write.csv(spam.df, supervised_Spam_FileName, row.names=F, quote=T)

 