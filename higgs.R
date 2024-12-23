data.higgs=read.csv('atlas-higgs-challenge-2014-v2.csv')

set.seed(123)

train = data.higgs[data.higgs$KaggleSet=="t",]
test = data.higgs[data.higgs$KaggleSet=="b",]


names(train)

orig.train=nrow(train)
orig.test=nrow(test)
orig.train
orig.test

train=subset(train, select=-c(Weight, KaggleWeight, EventId, KaggleSet))
test=subset(test, select=-c(Weight, KaggleWeight, EventId, KaggleSet))

train$Label = factor(train$Label)
test$Label = factor(test$Label)
#train$Label = as.numeric(train$Label == 's')
#test$Label = as.numeric(test$Label == 's')

# omit values
train[train == -999] <- NA
test[test == -999] <- NA

train <- na.omit(train)
test <- na.omit(test)

nrow(train)
nrow(train)/orig.train
nrow(test)
nrow(test)/orig.train

table(train$Label)
table(test$Label)

# use small set for data exploration

s.train = train[sample(nrow(train), 10000), ]
s.test = test[sample(nrow(test), 2000), ]

names(s.train)
s.train$PRI_jet_num = as.factor(s.train$PRI_jet_num)
#levels(s.train$PRI_jet_num) = c(0,1,2,3)

s.test$PRI_jet_num = as.factor(s.test$PRI_jet_num)
#levels(s.test$PRI_jet_num) = c(0,1,2,3)

no.label = subset(s.train, select=-c(Label, PRI_jet_num))
no.label.test = subset(s.test, select=-c(Label, PRI_jet_num))

#no.label = subset(s.train, select=-c(Label))
#no.label.test = subset(s.test, select=-c(Label))

### Intermezzo: predict mass

no.mass = subset(s.train, select=-c(Label, PRI_jet_num, DER_mass_MMC))
no.mass.test = subset(s.train, select=-c(Label, PRI_jet_num, DER_mass_MMC))
# normalization?

mod.mass = lm(log(s.train$DER_mass_MMC)~.^2, data=no.mass)
plot(mod.mass,1)

library(leaps)
#mod.step=step(mod.mass, direction='backward')
###

library(glmnet)

mod = glm(s.train$Label~ ., data=no.label, family=binomial)

summary(mod)

pred.prob=predict(mod, newdata=no.label.test, type="response")
pred = rep('b', length(pred.prob))
pred[pred.prob>.5]='s'

table(pred, s.test$Label)

perf.measure <- function(true.values, pred.values,  lab.pos = 1){
  #
  # compute the confusion matrix and number of units
  conf.matrix <- table(pred.values, true.values)
  n <- sum(conf.matrix)
  #
  # force the label of positives to be a character string
  lab.pos <- as.character(lab.pos)
  #
  # obtain the label of negatives
  lab <- rownames(conf.matrix)
  lab.neg <- lab[lab != lab.pos]
  if (length(unique(pred.values))==1){
    return(list(overall.ER=-1))
  }
  #
  # extract relevant quantities from the confusion matrix
  TP <- conf.matrix[lab.pos, lab.pos]
  TN <- conf.matrix[lab.neg, lab.neg]
  FP <- conf.matrix[lab.pos, lab.neg]
  FN <- conf.matrix[lab.neg, lab.pos]
  P     <- TP + FN
  N     <- FP + TN
  P.ast <- TP + FP
  #
  # compute the performance measures
  OER <- (FP+FN)/n
  PPV <- TP/P.ast
  TPR <- TP/P
  F1  <- 2*PPV*TPR/(PPV+TPR)
  TNR <- TN/N
  FPR <- FP/N
  return(list(overall.ER = OER, PPV=PPV, TPR=TPR, F1=F1, TNR=TNR, FPR=FPR))
}

perf.all=perf.measure(s.test$Label, pred, lab.pos='s')
perf.all


# for (i in names(no.label)) {
#   if ( min(subset(no.label, select = c(i)))>0 ) {
#     no.label[c(i)] = log(no.label[c(i)])
#   }
#   
# }

par(mfrow=c(6,5))
for (i in names(no.label)) {
  cat(i)
  hist(unlist( subset(no.label, select = c(i)), use.names=FALSE ), main=i, xlab='')
}
barplot(prop.table(table(s.train$PRI_jet_num)), main='PRI_jet_num')


normalization <- function(x){
  return( x/abs(max(x)))
}
# check normalization (we should keep track of the max values, to normalize the training set!)
no.label = as.data.frame(apply(no.label, 2, normalization))
no.label.test = as.data.frame(apply(no.label.test, 2, normalization))

par(mfrow=c(6,5))
for (i in names(no.label)) {
  cat(i)
  hist(unlist( subset(no.label, select = c(i)), use.names=FALSE ), main=i, xlab='')
}

#for (i in names(no.label)) {
#  cat(i)
#  hist(unlist( subset(no.label.test, select = c(i)), use.names=FALSE ), main=i, xlab='')
#}

par(mfrow=c(1,1))
library(corrplot)

corrplot(cor(no.label), method='color')

# find correlated variables
cor.table = cor(no.label)
cor.table[lower.tri(cor.table)] = 0
diag(cor.table) = 0
corrplot(abs(cor.table))
corr.val = abs(cor.table) > .8
corrplot(corr.val)

#plot correlated variables?
plot(no.label$DER_mass_MMC, no.label$DER_mass_vis, col=s.train$Label)
legend("topleft", legend=levels(s.train$Label), pch=16, col=unique(s.train$Label))
plot(no.label$DER_deltaeta_jet_jet, no.label$DER_prodeta_jet_jet, col=s.train$Label)

plot(no.label$DER_sum_pt, no.label$DER_PRI_met_sumet, col=s.train$Label)

plot(no.label$DER_sum_pt, no.label$PRI_jet_leading_pt, col=s.train$Label)

plot(no.label$DER_sum_pt, no.label$PRI_jet_all_pt, col=s.train$Label)

# drop correlated variables ? 

no.label.drop = as.data.frame( subset(no.label, select=-c(DER_mass_vis, DER_prodeta_jet_jet, PRI_met_sumet, PRI_jet_leading_pt, PRI_jet_all_pt)) )
no.label.test.drop = as.data.frame( subset(no.label.test, select=-c(DER_mass_vis, DER_prodeta_jet_jet, PRI_met_sumet, PRI_jet_leading_pt, PRI_jet_all_pt)) )


# my_glmnet <- function(formula, data, family, alpha){
#   # Put formula and data in lm()
#   model <- lm(formula= as.formula(formula), data= data)
#   # Get x and y
#   x <- model$model[ , -1]
#   y <- model$model[ , 1]
#   # Use x and y in glmnet
#   glmnet::glmnet(x, y, alpha= alpha, family=family) 
#   
#}

# Single variable predictors

library(glmnet)

for (i in names(no.label.drop)) {
  cat(i)
  cat('\n')
  mod.temp = glm(s.train$Label~., data=subset(no.label, select = c(i)) , family=binomial)
  pred.prob=predict(mod.temp, newdata=no.label.test.drop,type="response")
  pred = rep('b', length(pred.prob))
  pred[pred.prob>.5]='s'
  perf.temp=perf.measure(s.test$Label, pred, lab.pos='s')
  cat(perf.temp$overall.ER)
  cat('\n')
}

plot(no.label$DER_deltaeta_jet_jet, no.label$DER_lep_eta_centrality,col= s.train$Label)

mod = glm(s.train$Label~ ., data=no.label.drop, family=binomial)

summary(mod)
# diagnostic plots difficult to interpret for logistic regression.
plot(mod)

#pred=predict(mod, newdata=no.label.drop,type="response")

mod = glm(s.train$Label~ ., data=no.label.drop, family=binomial)
summary(mod)
plot(mod)

pred.prob=predict(mod, newdata=no.label.test.drop,type="response")
pred = rep('b', length(pred.prob))
pred[pred.prob>.5]='s'

perf.drop=perf.measure(s.test$Label, pred, lab.pos='s')
perf.drop
table(s.test$Label, pred)

library(pROC)
roc.out <- roc(s.test$Label, pred.prob, levels=c("b", "s"))

# different ways of plotting the ROC curve
plot(roc.out,  print.auc=TRUE) # check values on the x axis

coords(roc.out, 0.5)

coords(roc.out, seq(0.1, 0.9, by=0.01))
coords(roc.out, "best")

best.threshold = coords(roc.out, "best")$threshold

pred.prob=predict(mod, newdata=no.label.test.drop,type="response")
pred = rep('b', length(pred.prob))
pred[pred.prob>best.threshold]='s'

perf.best=perf.measure(s.test$Label, pred, lab.pos='s')
perf.best
table(s.test$Label, pred)


# cross validation on training set, works but with logistic loss?
library(boot)

# logistic loss

log.loss = function(true.labels, pred){
  s=- true.labels * log(pred) - (1-true.labels)*log(1-pred)
  return(mean(s))
}

mod = glm(s.train$Label~ ., data=no.label.drop, family=binomial)

dropped = no.label.drop
dropped$Label = s.train$Label

mod = glm(Label ~ ., data=dropped, family=binomial)

mod.cv = cv.glm(dropped, mod, cost=log.loss,K=10)

# subset selection
library(leaps)

library(bestglm)

# backwards step selection
backw = step(mod, direction='backward')


# glmnet 

X <- model.matrix(Label ~ ., data=dropped,)
X <- X[,-1]
y <- dropped$Label

grid <- 10^seq(1, -6, length=100)
ridge.mod <- glmnet(X, y, alpha=1, family = binomial, nfold=10, lambda=grid)

plot(ridge.mod)


# do the same for model w/ interactions!
# there are too many variables, only keep derivatives

library(glmnet)

der.data= no.label.drop[,1:11]
der.data.= no.label.test.drop[,1:11]

mod = glm(s.train$Label~ .:., data=der.data, family=binomial)

summary(mod)
# diagnostic plots difficult to interpret for logistic regression.
plot(mod)

#pred=predict(mod, newdata=no.label.drop,type="response")


pred.prob=predict(mod, newdata=no.label.test.drop,type="response")
pred = rep('b', length(pred.prob))
pred[pred.prob>.5]='s'

perf.drop=perf.measure(s.test$Label, pred, lab.pos='s')
perf.drop
table(s.test$Label, pred)


library(pROC)
roc.out <- roc(s.test$Label, pred.prob, levels=c("b", "s"))

# different ways of plotting the ROC curve
plot(roc.out,  print.auc=TRUE) # check values on the x axis

#coords(roc.out, seq(0.1, 0.9, by=0.01))
coords(roc.out, "best")

best.threshold = coords(roc.out, "best")$threshold

pred.prob=predict(mod, newdata=no.label.test.drop,type="response")
pred = rep('b', length(pred.prob))
pred[pred.prob>best.threshold]='s'

perf.best=perf.measure(s.test$Label, pred, lab.pos='s')
perf.best
table(s.test$Label, pred)


der.data.v1 = der.data[-7913,]
label.v1 = s.train$Label[-7913]

mod = glm(label.v1~ .:., data=der.data.v1, family=binomial)

summary(mod)
# diagnostic plots difficult to interpret for logistic regression.
plot(mod)

#pred=predict(mod, newdata=no.label.drop,type="response")


pred.prob=predict(mod, newdata=no.label.test.drop,type="response")
pred = rep('b', length(pred.prob))
pred[pred.prob>.5]='s'

perf.drop=perf.measure(s.test$Label, pred, lab.pos='s')
perf.drop
table(s.test$Label, pred)

higgs.pred= function (mod, newdata, type='response', threshold=.5){
  pred.prob=predict(mod, newdata=no.label.test.drop,type="response")
  pred = rep('b', length(pred.prob))
  return(pred[pred.prob>threshold]='s')
}
