#' @importFrom FNN knnx.index
imputeFromPred <- function(
    pred
  , modelType
  , valueSelector
  , meanMatchCandidates
  , prior
  , priorPreds
)
{
  
  # pred - The output from the model of the samples you want to impute
  # modelType - Classification or Regression
  # valueSelector - meanMatch or value
  # meanMatchCandidates - Integer
  # prior - Unaltered values of original nonmissing data
  # priorPreds model predictions associated with prior
  
  if (valueSelector == "value") {
    return(pred)
  } else {
    if (modelType == "Classification") {
      # Transform vector to 1 row matrix. Ranger returns a vector
      # if there is only 1 value to predict. Needs to be a matrix
      if ("numeric" %in% class(pred)) pred <- t(pred)
      lvls <- colnames(pred)
      return(apply(pred,MARGIN=1,function(x) sample(lvls,prob=x,size=1)))
    } else if (modelType == "Regression") {
      # For each prediction of a missing value, find the closest values in the
      # predictions for the non-missing values.
      nearest <- knnx.index(priorPreds,pred,k=meanMatchCandidates)
      nearest <- prior[apply(nearest,MARGIN=1,function(x) sample(x,size=1))]
      return(nearest)
    }
  }
}
