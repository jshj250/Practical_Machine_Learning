library(caret)
training <- read.csv('pml-training.csv',header=T,dec='.',na.strings=c('NA', ''))
testing  <- read.csv('pml-testing.csv', header=T,dec='.',na.strings=c('NA', ''))

element_name   <- names(as.data.frame(t(na.omit(t(training)))))
clean_training <- training[, element_name]
clean_testing  <- testing [, element_name[-length(element_name)]]
clean_training <- clean_training[, -1]
clean_testing  <- clean_testing [, -1]

set.seed(998)
inTraining  <- createDataPartition(clean_training$classe, p = 0.75, list = FALSE)
cv_training <- clean_training[ inTraining, ]
cv_testing  <- clean_training[-inTraining, ]

set.seed(825)
fitControl <- trainControl(method = "repeatedcv", number = 2, repeats = 2, verboseIter=T)
gbm_Fit <- train(classe ~ ., data = cv_training, method = "gbm", trControl = fitControl, verbose = TRUE)

trellis.par.set(caretTheme())
plot(gbm_Fit, metric = "Accuracy")
trellis.par.set(caretTheme())
plot(gbm_Fit, metric = "Kappa")

set.seed(825)
fitControl <- trainControl(method = "none", classProbs = TRUE, verboseIter=T)
gbm_Fit_CV <- train(classe ~ ., data = cv_training, method = "gbm", trControl = fitControl, 
                    verbose = TRUE, tuneGrid = gbm_Fit$bestTune)
pred  <- predict(gbm_Fit_CV, newdata = cv_testing)
obs   <-  cv_testing$classe
metric <- defaultSummary(data = data.frame(obs=obs, pred=pred))

set.seed(825)
fitControl <- trainControl(method = "none", classProbs = TRUE, verboseIter=T)
gbm_Fit_Final <- train(classe ~ ., data = clean_training, method = "gbm", trControl = fitControl, 
                       verbose = TRUE, tuneGrid = gbm_Fit$bestTune)
pred  <- predict(gbm_Fit_Final, newdata = clean_testing)
