#Random Forest
#-----
set.seed(382)
ind <- sample(2, nrow(ControlBaseline.fframe), replace = T, prob = c(0.6, 0.4)) 
ControlBaseline.trainsetf <- ControlBaseline.fframe[ind == 1, ]
ControlBaseline.testsetf <- ControlBaseline.fframe[ind == 2, ]

ind <- sample(2, nrow(ControlBaseline.TVframe), replace = T, prob = c(0.6, 0.4)) 
ControlBaseline.trainsetTV <- ControlBaseline.TVframe[ind == 1, ]
ControlBaseline.testsetTV <- ControlBaseline.TVframe[ind == 2, ]

ControlBaseline.rff <- randomForest(getNonRejectedFormula(ControlBaseline.fboruta), data = ControlBaseline.trainsetf, preProcess = c("center","scale"))
ControlBaseline.pf <- predict(ControlBaseline.rff,ControlBaseline.testsetf)
confusionMatrix(ControlBaseline.pf,ControlBaseline.testsetf$Age)

ControlBaseline.rfTV <- randomForest(getNonRejectedFormula(ControlBaseline.TVboruta), data = ControlBaseline.trainsetTV, preProcess = c("center","scale"))
ControlBaseline.pTV <- predict(ControlBaseline.rfTV,ControlBaseline.testsetTV)
confusionMatrix(ControlBaseline.pTV,ControlBaseline.testsetTV$Age)

ControlBaseline.rff.importance <- varImp(ControlBaseline.rff, scale=FALSE)
ControlBaseline.rff.importance <- ControlBaseline.rff.importance/sum(ControlBaseline.rff.importance)
ControlBaseline.rfTV.importance <- varImp(ControlBaseline.rfTV, scale=FALSE)
ControlBaseline.rfTV.importance <- ControlBaseline.rfTV.importance/sum(ControlBaseline.rfTV.importance)

ControlBaseline.froc<-multiclass.roc(ControlBaseline.testsetf$Age,as.numeric(ControlBaseline.pf))
ControlBaseline.froc$auc #87.5

ControlBaseline.TVroc<-multiclass.roc(ControlBaseline.testsetTV$Age,as.numeric(ControlBaseline.pTV))
ControlBaseline.TVroc$auc #94.44

#Control Hypercapnia 
set.seed(382)
ind <- sample(2, nrow(ControlHyper.fframe), replace = T, prob = c(0.6, 0.4)) 
ControlHyper.trainsetf <- ControlHyper.fframe[ind == 1, ]
ControlHyper.testsetf <- ControlHyper.fframe[ind == 2, ]

ind <- sample(2, nrow(ControlHyper.TVframe), replace = T, prob = c(0.6, 0.4)) 
ControlHyper.trainsetTV <- ControlHyper.TVframe[ind == 1, ]
ControlHyper.testsetTV <- ControlHyper.TVframe[ind == 2, ]

ControlHyper.rff <- randomForest(getNonRejectedFormula(ControlHyper.fboruta), data = ControlHyper.trainsetf, preProcess = c("center","scale"))
ControlHyper.pf <- predict(ControlHyper.rff,ControlHyper.testsetf)
confusionMatrix(ControlHyper.pf,ControlHyper.testsetf$Age)

ControlHyper.rfTV <- randomForest(getNonRejectedFormula(ControlHyper.TVboruta), data = ControlHyper.trainsetTV, preProcess = c("center","scale"))
ControlHyper.pTV <- predict(ControlHyper.rfTV,ControlHyper.testsetTV)
confusionMatrix(ControlHyper.pTV,ControlHyper.testsetTV$Age)

ControlHyper.rff.importance <- varImp(ControlHyper.rff, scale=FALSE)
ControlHyper.rff.importance <- ControlHyper.rff.importance/sum(ControlHyper.rff.importance)
ControlHyper.rfTV.importance <- varImp(ControlHyper.rfTV, scale=FALSE)
ControlHyper.rfTV.importance <- ControlHyper.rfTV.importance/sum(ControlHyper.rfTV.importance)

ControlHyper.froc<-multiclass.roc(ControlHyper.testsetf$Age,as.numeric(ControlHyper.pf))
ControlHyper.froc$auc #0.9

ControlHyper.TVroc<-multiclass.roc(ControlHyper.testsetTV$Age,as.numeric(ControlHyper.pTV))
ControlHyper.TVroc$auc #100

#Gradient Boosted
#-----
#Control Baseline
set.seed(382)
ind <- sample(2, nrow(ControlBaseline.fframe), replace = T, prob = c(0.6, 0.4)) 
ControlBaseline.trainsetf <- ControlBaseline.fframe[ind == 1, ]
ControlBaseline.testsetf <- ControlBaseline.fframe[ind == 2, ]

ind <- sample(2, nrow(ControlBaseline.TVframe), replace = T, prob = c(0.6, 0.4)) 
ControlBaseline.trainsetTV <- ControlBaseline.TVframe[ind == 1, ]
ControlBaseline.testsetTV <- ControlBaseline.TVframe[ind == 2, ]

grid <- expand.grid(n.trees = 500, interaction.depth=2, shrinkage=0.01, n.minobsinnode=2)
ctrl <- trainControl(method = "repeatedcv",number = 10, repeats = 10, allowParallel = T)
registerDoParallel(detectCores()-1)
ControlBaseline.GBMModelf <- train(getNonRejectedFormula(ControlBaseline.fboruta),data = ControlBaseline.trainsetf, method = "gbm", trControl = ctrl, tuneGrid= grid, preProcess = c("center","scale"))
confusionMatrix(ControlBaseline.GBMModelf)
ControlBaseline.gpmpf <-predict(ControlBaseline.GBMModelf,ControlBaseline.testsetf)
confusionMatrix(ControlBaseline.gpmpf,ControlBaseline.testsetf$Age)

roc_gbm_testf <- multiclass.roc(response = ControlBaseline.testsetf$Age, predictor =as.numeric(ControlBaseline.gpmpf))
roc_gbm_testf$auc #77.08

grid <- expand.grid(n.trees = 500, interaction.depth=2, shrinkage=0.01, n.minobsinnode=2)
ctrl <- trainControl(method = "repeatedcv",number = 10, repeats = 10, allowParallel = T)
registerDoParallel(detectCores()-1)
ControlBaseline.GBMModelTV <- train(getNonRejectedFormula(ControlBaseline.TVboruta),data = ControlBaseline.trainsetTV, method = "gbm", trControl = ctrl, tuneGrid= grid, preProcess = c("center","scale"))
confusionMatrix(ControlBaseline.GBMModelTV)
ControlBaseline.gpmp <-predict(ControlBaseline.GBMModelTV,ControlBaseline.testsetTV)
confusionMatrix(ControlBaseline.gpmp,ControlBaseline.testsetTV$Age)

roc_gbm_testTV <- multiclass.roc(response = ControlBaseline.testsetTV$Age, predictor =as.numeric(ControlBaseline.gpmp))
roc_gbm_testTV$auc #83.33

summary(ControlBaseline.GBMModelf)
summary(ControlBaseline.GBMModelTV)



#Control Hyper 
set.seed(382)
ind <- sample(2, nrow(ControlHyper.fframe), replace = T, prob = c(0.6, 0.4)) 
ControlHyper.trainsetf <- ControlHyper.fframe[ind == 1, ]
ControlHyper.testsetf <- ControlHyper.fframe[ind == 2, ]

ind <- sample(2, nrow(ControlHyper.TVframe), replace = T, prob = c(0.6, 0.4)) 
ControlHyper.trainsetTV <- ControlHyper.TVframe[ind == 1, ]
ControlHyper.testsetTV <- ControlHyper.TVframe[ind == 2, ]

grid <- expand.grid(n.trees = 500, interaction.depth=2, shrinkage=0.01, n.minobsinnode=1)
ctrl <- trainControl(method = "repeatedcv",number = 10, repeats = 10, allowParallel = T)
registerDoParallel(detectCores()-1)
ControlHyper.GBMModelf <- train(getNonRejectedFormula(ControlHyper.fboruta),data = ControlHyper.trainsetf, method = "gbm", trControl = ctrl, tuneGrid= grid, preProcess = c("center","scale"))
confusionMatrix(ControlHyper.GBMModelf)
ControlHyper.gpmpf <-predict(ControlHyper.GBMModelf,ControlHyper.testsetf)
confusionMatrix(ControlHyper.gpmpf,ControlHyper.testsetf$Age)

roc_gbm_testfhyp <- multiclass.roc(response = ControlHyper.testsetf$Age, predictor =as.numeric(ControlHyper.gpmpf))
roc_gbm_testfhyp$auc #0.9

grid <- expand.grid(n.trees = 500, interaction.depth=2, shrinkage=0.01, n.minobsinnode=1)
ctrl <- trainControl(method = "repeatedcv",number = 10, repeats = 10, allowParallel = T)
registerDoParallel(detectCores()-1)
ControlHyper.GBMModelTV <- train(getNonRejectedFormula(ControlHyper.TVboruta),data = ControlHyper.trainsetTV, method = "gbm", trControl = ctrl, tuneGrid= grid, preProcess = c("center","scale"))
confusionMatrix(ControlHyper.GBMModelTV)
ControlHyper.gpmp <-predict(ControlHyper.GBMModelTV,ControlHyper.testsetTV)
confusionMatrix(ControlHyper.gpmp,ControlHyper.testsetTV$Age)

roc_gbm_testTVhyp <- multiclass.roc(response = ControlHyper.testsetTV$Age, predictor =as.numeric(ControlHyper.gpmp))
roc_gbm_testTVhyp$auc #0.775


summary(ControlHyper.GBMModelf)
summary(ControlHyper.GBMModelTV)

#Support Vector
#-----
set.seed(382)
ind <- sample(2, nrow(ControlBaseline.fframe), replace = T, prob = c(0.6, 0.4)) 
ControlBaseline.trainsetf <- ControlBaseline.fframe[ind==1, ]
ControlBaseline.testsetf <- ControlBaseline.fframe[ind == 2, ]

ind <- sample(2, nrow(ControlBaseline.TVframe), replace = T, prob = c(0.6, 0.4)) 
ControlBaseline.trainsetTV <- ControlBaseline.TVframe[ind == 1, ]
ControlBaseline.testsetTV <- ControlBaseline.TVframe[ind == 2, ]

ControlBaseline.SVmodf <- svm(getNonRejectedFormula(ControlBaseline.fboruta), data=ControlBaseline.trainsetf, method = "C-classification", kernal = "radial", gamma=0.1, cost=10, preProcess = c("center","scale"))
summary(ControlBaseline.SVmodf)
ControlBaseline.SVp <- predict(ControlBaseline.SVmodf, ControlBaseline.testsetf)
confusionMatrix(ControlBaseline.SVp, ControlBaseline.testsetf$Age)

roc_svm_testf <- multiclass.roc(response = ControlBaseline.testsetf$Age, predictor =as.numeric(ControlBaseline.SVp))
roc_svm_testf$auc #83.33

ControlBaseline.SVmodTV <- svm(getNonRejectedFormula(ControlBaseline.TVboruta), data=ControlBaseline.trainsetTV, method = "C-classification", kernal = "radial", gamma=0.1, cost=10, preProcess = c("center","scale"))
summary(ControlBaseline.SVmodTV)
ControlBaseline.SVp <- predict(ControlBaseline.SVmodTV, ControlBaseline.testsetTV)
confusionMatrix(ControlBaseline.SVp, ControlBaseline.testsetTV$Age)

ControlBaseline.modelf <- fit(getNonRejectedFormula(ControlBaseline.fboruta), ControlBaseline.fframe, model = "svm")
ControlBaseline.varimpf <- Importance(ControlBaseline.modelf, ControlBaseline.trainsetf,method="sensv")
ControlBaseline.varimpf$imp
ControlBaseline.modelTV <- fit(getNonRejectedFormula(ControlBaseline.TVboruta), ControlBaseline.TVframe, model = "svm")
ControlBaseline.varimpTV <- Importance(ControlBaseline.modelTV, ControlBaseline.trainsetTV,method="sensv")
ControlBaseline.varimpTV$imp

roc_svm_testTV <- multiclass.roc(response = ControlBaseline.testsetTV$Age, predictor =as.numeric(ControlBaseline.SVp))
roc_svm_testTV$auc #88.89

set.seed(382)
ind <- sample(2, nrow(ControlHyper.fframe), replace = T, prob = c(0.6, 0.4)) 
ControlHyper.trainsetf <- ControlHyper.fframe[ind==1, ]
ControlHyper.testsetf <- ControlHyper.fframe[ind == 2, ]

ind <- sample(2, nrow(ControlHyper.TVframe), replace = T, prob = c(0.6, 0.4)) 
ControlHyper.trainsetTV <- ControlHyper.TVframe[ind == 1, ]
ControlHyper.testsetTV <- ControlHyper.TVframe[ind == 2, ]

ControlHyper.SVmodf <- svm(getNonRejectedFormula(ControlHyper.fboruta), data=ControlHyper.trainsetf, method = "C-classification", kernal = "radial", gamma=0.1, cost=10, preProcess = c("center","scale"))
summary(ControlHyper.SVmodf)
ControlHyper.SVp <- predict(ControlHyper.SVmodf, ControlHyper.testsetf)
confusionMatrix(ControlHyper.SVp, ControlHyper.testsetf$Age)

roc_svm_testfHyper <- multiclass.roc(response = ControlHyper.testsetf$Age, predictor =as.numeric(ControlHyper.SVp))
roc_svm_testfHyper$auc #0.9

ControlHyper.SVmodTV <- svm(getNonRejectedFormula(ControlHyper.TVboruta), data=ControlHyper.trainsetTV, method = "C-classification", kernal = "radial", gamma=0.1, cost=10, preProcess = c("center","scale"))
summary(ControlHyper.SVmodTV)
ControlHyper.SVp <- predict(ControlHyper.SVmodTV, ControlHyper.testsetTV)
confusionMatrix(ControlHyper.SVp, ControlHyper.testsetTV$Age)

roc_svm_testTVHyper <- multiclass.roc(response = ControlHyper.testsetTV$Age, predictor =as.numeric(ControlHyper.SVp))
roc_svm_testTVHyper$auc #0.9

ControlHyper.modelf <- fit(getNonRejectedFormula(ControlHyper.fboruta), ControlHyper.fframe, model = "svm")
ControlHyper.varimpf <- Importance(ControlHyper.modelf, ControlHyper.trainsetf,method="sensv")
ControlHyper.varimpf$imp
ControlHyper.modelTV <- fit(getNonRejectedFormula(ControlHyper.TVboruta), ControlHyper.TVframe, model = "svm")
ControlHyper.varimpTV <- Importance(ControlHyper.modelTV, ControlHyper.trainsetTV,method="sensv")
ControlHyper.varimpTV$imp



