##################################
# �ړI : �j���[�����l�b�g���[�N�̎��s��.
# �\�[�X�R�[�h�� : NNET.R
# �o�[�W���� : SR(1)
# �ŏI�ύX���� : 2017.06.08, 16:56
# �쐬�� : Taniguchi Hidetaka
##################################

# �t�B�[�h�t�H���[�h�j���[�����l�b�g���[�N

library("nnet")
library("base")

# nn.res <- nnet(ZSpecies~., data=train_df.x, size=2)

# ���ԑw�̐ݒ����\��
nn.res <- nnet(ZSpecies~., data=train_df.x, size=10, maxit = 100, MaxNWts = 40000)

nn.predictions <- predict(nn.res, test_df.x, type="class")

# �X�p���e�X�g�f�[�^�̒ʂ��ԍ�
spam_from <- 1
spam_to <- length(test.y[which(test.y==1)])

nn.predictions.spam_results <- nn.predictions[spam_from:spam_to]
test_class_spam <- test_df.x$ZSpecies[spam_from:spam_to]
nn.spam_accuracy <- length(nn.predictions.spam_results[which(nn.predictions.spam_results == test_class_spam)]) / length(nn.predictions.spam_results)

# �n���e�X�g�f�[�^�̒ʂ��ԍ�
ham_from <- (spam_to + 1)
ham_to <- (spam_to +length(test.y[which(test.y==0)]))

nn.predictions.ham_results <- nn.predictions[ham_from:ham_to]
test_class_ham <- test_df.x$ZSpecies[ham_from:ham_to]
nn.ham_accuracy <- length(nn.predictions.ham_results[which(nn.predictions.ham_results == test_class_ham)]) / length(nn.predictions.ham_results)

(nn.spam_accuracy + nn.ham_accuracy) / 2

# nn.result <- length(nn.predictions[which(nn.predictions == test_df.x$ZSpecies)]) / length(nn.predictions)

result_NN[[cnt_j]] <- c(nn.spam_accuracy, nn.ham_accuracy)

print("nn:")
print(nn.spam_accuracy)
print(nn.ham_accuracy)

# �����׏d
# coef(nn.res)
