## ----amputeData,message=FALSE-------------------------------------------------
require(miceRanger)
set.seed(1)

# Load data
data(iris)

# Ampute the data. iris contains no missing values by default.
ampIris <- amputeData(iris,perc=0.25)

miceObj <- miceRanger(
    ampIris
  , m=6
  , verbose=FALSE
)

## ----plotDistributions--------------------------------------------------------
plotDistributions(miceObj,vars='allNumeric')

## ----plotCorrelations---------------------------------------------------------
plotCorrelations(miceObj,vars='allNumeric')

## ----plotVarConvergence-------------------------------------------------------
plotVarConvergence(miceObj,vars='allNumeric')

## ----plotModelError-----------------------------------------------------------
plotModelError(miceObj,vars='allNumeric')

## ----plotVarImportance--------------------------------------------------------
plotVarImportance(miceObj)

## ----plotImputationVariance, fig.height = 4, fig.width = 8--------------------
plotImputationVariance(miceObj,ncol=2,widths=c(5,3))

