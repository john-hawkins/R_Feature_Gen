# #############################################
# RUN A SET OF STANDARD MODELS WITH DEFAULTS
# #############################################
require(randomForest)
require(gbm)
require(xgboost)
require(mltools)
require(data.table)
require(magrittr)

runGBM_pipeline_01 <- function(train, valid, target, features, metric='AUC') {

    formula     <- getFormula(target, features)
    mod.gbm	<- gbm(formula, data=train, n.trees=1000, shrinkage=0.05, interaction.depth=3, bag.fraction = 0.5, cv.folds = 3)
    best.iter <- gbm.perf(mod.gbm, method="cv")

    gbm.preds    <- predict(mod.gbm, valid, n.trees=100, type="response")
    realvals    <- as.numeric(as.character(valid[,target]))
    aucval      <- auc_roc(gbm.preds, realvals)

    # Return a list with the model and metric
    return( c(mod=mod.gbm, auc=aucval) )
}


runRF_pipline_01 <- function(train, valid, target, features, metric='AUC') {
    formula     <- getFormula(target, features)
    mod.rf      <- randomForest(formula, data=train, importance=TRUE)
    rf.preds    <- predict(mod.rf, valid, type='prob')
    realvals    <- as.numeric(as.character(valid[,target])) 
    aucval      <- auc_roc(rf.preds[,2], realvals)

    # Return a list with the model and metric
    return( c(mod=rf.mod, auc=aucval) )
}


runGLM_pipeline_01 <- function(train, valid, target, features, metric='AUC') {
    formula     <- getFormula(target, features)
    mod.glm     <- glm(formula, data=train, family=binomial)
    glm.pred    <- predict(mod.glm, valid, type="response")
    realvals    <- as.numeric(as.character(valid[,target]))
    aucval      <- auc_roc(glm.preds, realvals)
    return( c(mod=mod.glm, auc=aucval) )
}


runXGB_pipeline_01 <- function(train, valid, target, features, metric='AUC') {
    trn.mtx <- getXgboostMatrix( train, features, target )
    vd.mtx <- getXgboostMatrix( valid, features, target )

    mod.xgb     <- xgboost(data = trn.mtx, nrounds=100, max.depth=15, objective='binary:logistic')
    xgb.pred    <- predict(mod.xgb, vd.mtx)
    realvals    <- as.numeric(as.character(valid[,target]))
    aucval      <- auc_roc(xgb.pred, realvals)
    return( c(mod=mod.xgb, auc=aucval) ) 
}


# ###############################################################################
#  --------------- UTILITY FUNCTIONS --------------------------------------------
# ###############################################################################

getFormula <- function(target, features) {
    as.formula(paste(target, paste(features, collapse=" + "), sep=" ~ "))
}

replaceNAsFromFormula  	<- function( df, formu, rplcmnt) {
    test <- as.character(formu)
    test2 <- strsplit(test[[3]], split='\\s*\\+\\s*')	
    print(paste("We have", length(test2[[1]]), " elements")) 
    replaceNAs(df, test2[[1]], rplcmnt)
}

replaceNAs  	<- function(df, cols, rplcmnt) {
    for (j in cols) set(df, which(is.na(df[[j]])), j, rplcmnt)
}


# #################################################
# generate a training matrix for xgboost
# ###############################################
    
getXgboostMatrix <- function( df, feats, target ) {

  # cast data frame to a data table
  dt <- data.table(df)
    
  xgb.DMatrix(
    data = data.matrix( dt[,
             feats[[1]],
             with = FALSE]),
    label = dt[, get(target)],
    missing = NA
  ) 
    
}

getXgboostMatrixFromFormula <- function( df, formula ) {
  temp <- as.character(formula)
  target <- temp[[2]]
  feats <- gsub( "\\n| ", "", temp[[3]]) %>%  strsplit( ., "\\+" )
  getXgboostMatrix(df, feats, target) 
}   
   

