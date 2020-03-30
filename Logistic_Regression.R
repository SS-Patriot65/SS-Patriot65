##################################
# 目的 : ロジスティック回帰の実行プログラム.  
# ソースコード名 : chapter12.R
# バージョン : SR(1)
# 最終変更日時 : 2017.06.08, 16:57
# 作成者 : Taniguchi Hidetaka
##################################

# Twelfth code snippet
library('glmnet')

train.x_for_LR <- as.matrix(train.x)

regularized.logit.fit <- glmnet(train.x_for_LR, train.y, family = c('binomial'), alpha=1)

# Thirteenth code snippet
lambdas <- regularized.logit.fit$lambda

performance <- data.frame()

# for (lambda in lambdas)
# {
#   predictions <- predict(regularized.logit.fit, test.x, s = lambda)
#   predictions <- as.numeric(predictions > 0)
#   mse <- mean(predictions != test.y)
#   performance <- rbind(performance, data.frame(Lambda = lambda, MSE = mse))
# }

s_results <- c()
h_results <- c()

i_lf <- 1

test.x_for_LR <- as.matrix(test.x)

# 学習データの数だけ判別を行い、その平均を最終成績とする
for (lambda in lambdas)
{
  predictions <- predict(regularized.logit.fit, test.x_for_LR, s = lambda)
  predictions <- as.numeric(predictions > 0)

  s_score_in_time_t <- 0
  h_score_in_time_t <- 0

  for (i in 1:NUMBER_OF_TEST_DATA_SPAM) {
    if(test.y[i] == predictions[i]) {
        s_score_in_time_t <- s_score_in_time_t + 1
    }
  }

  for (i in (NUMBER_OF_TEST_DATA_SPAM - 1):NUMBER_OF_TEST_DATA_HAM) {
    if(test.y[i] == predictions[i]) {
        h_score_in_time_t <- h_score_in_time_t + 1
    }
  }

  s_results[i_lf] <- s_score_in_time_t / NUMBER_OF_TEST_DATA_SPAM
  h_results[i_lf] <- h_score_in_time_t / NUMBER_OF_TEST_DATA_HAM

  i_lf <- i_lf + 1
}

# Fourteenth code snippet
# best.lambda <- with(performance, max(Lambda[which(MSE == min(MSE))]))

# Fifteenth code snippet
# mse <- with(subset(performance, Lambda == best.lambda), MSE)

s_results <- mean(s_results)
h_results <- mean(h_results)

result_LoRe[[cnt_j]] <- c(s_results, h_results)

print("Logistic Regression:")
print(s_results)
print(h_results)

 