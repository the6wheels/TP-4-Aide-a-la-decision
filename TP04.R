


library(xlsx)
breast.app <- read.xlsx(file="breast.xls",sheetIndex=1,header=T)
breast.test <- read.xlsx(file="breast.xls",sheetIndex=2,header=T)



library(rpart)
arbre.1 <- rpart(classe ~ ., data=breast.app, method="class")
print(arbre.1)
plot(arbre.1)
text(arbre.1)












library(rpart.plot)
rpart.plot(arbre.1)

pred.classe <- predict(arbre.1, newdata = breast.test, type = "class")
print(summary(pred.classe))

mc <- table(breast.test$classe,pred.classe)
print(mc)

erreur <- (mc[2,1]+mc[1,2])/sum(mc)
print(erreur)

parametres <- rpart.control(minsplit = 50, minbucket = 20)
arbre.2 <- rpart(classe ~ ., data=breast.app, method = "class", control = "parametres")
print(arbre.2)

library(tree)

param.3 <- tree.control(nobs=nrow(breast.app), mincut=20, minsize=50)
breast.app$classe <- as.factor(breast.app$classe)
arbre.3 <- tree(classe ~ ., data=breast.app, control=param.3)
print(arbre.3)

print(nrow(breast.app))

library(party)
param.4 <- ctree_control(minsplit=20,minbucket=10)
arbre.4 <- ctree(classe ~ ., data = breast.app, control=param.4)
plot(arbre.4)



breast.app <- breast.app[-1, ]
print(breast.app)
is.na(breast.app)
any(is.na(breast.app))



