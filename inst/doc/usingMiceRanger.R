## ----amputeData,message=FALSE-------------------------------------------------
require(miceRanger)
set.seed(1)

# Load data
data(iris)

# Ampute the data. iris contains no missing values by default.
ampIris <- amputeData(iris,perc=0.25)
head(ampIris,10)

## ----simpleMice,message=FALSE-------------------------------------------------
# Perform mice, return 6 datasets. 
seqTime <- system.time(
  miceObj <- miceRanger(
      ampIris
    , m=6
    , returnModels = TRUE
    , verbose=FALSE
  )
)

## ----parMice,message=FALSE----------------------------------------------------
library(doParallel)

# Set up back ends.
cl <- makeCluster(2)
registerDoParallel(cl)

# Perform mice 
parTime <- system.time(
  miceObjPar <- miceRanger(
      ampIris
    , m=6
    , parallel = TRUE
    , verbose = FALSE
  )
)
stopCluster(cl)
registerDoSEQ()

## ----parFaster----------------------------------------------------------------
perc <- round(1-parTime[[3]]/seqTime[[3]],2)*100
print(paste0("The parallel process ran ",perc,"% faster using 2 R back ends."))

## ----addToMice----------------------------------------------------------------
miceObj <- addIterations(miceObj,iters=2,verbose=FALSE)
miceObj <- addDatasets(miceObj,datasets=1,verbose=FALSE)

## ----customSetup--------------------------------------------------------------
v <- list(
  Sepal.Width = c("Sepal.Length","Petal.Width","Species")
  , Sepal.Length = c("Sepal.Width","Petal.Width")
  , Species = c("Sepal.Width")
)
pmm <- c(
    Sepal.Width = "meanMatch"
  , Sepal.Length = "value"
  , Species = "meanMatch"
)

miceObjCustom <- miceRanger(
    ampIris
  , vars = v
  , valueSelector = pmm
  , verbose=FALSE
)

## -----------------------------------------------------------------------------
newDat <- amputeData(iris)
newImputed <- impute(newDat,miceObj,verbose=FALSE)

## ----completeData-------------------------------------------------------------
dataList <- completeData(miceObj)
head(dataList[[1]],10)

## ----impAccuracy,message=FALSE,echo=FALSE,fig.height = 6,warning=FALSE--------
require(ggplot2)
require(ggpubr)

plotVars <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")

plotList <- lapply(
    plotVars
  , function(x) {
    missIndx <- is.na(ampIris[,get(x)])
    impVsAmp <- data.table(
      originalData = iris[missIndx,x]
      , imputedData = dataList[[1]][missIndx,get(x)]
      , Species = iris[missIndx,]$Species
    )
    return(
      ggscatter(impVsAmp,x="originalData",y="imputedData",add="reg.line",size = 0) +
        geom_point(data=impVsAmp,aes(x=originalData,y=imputedData,color=Species)) +
        stat_cor(label.x = min(impVsAmp$originalData), label.y = max(impVsAmp$imputedData)*0.9+0.1*min(impVsAmp$imputedData)) +
        xlab(paste0("Original ",x)) +
        ylab(paste0("Imputed ",x))
    )
  }  
)
arranged <- ggarrange(
    plotlist = plotList
  , common.legend = TRUE
)
annotate_figure(
    arranged
  , top=text_grob(
        "Original Data Compared to Imputed Value"
      , face = "bold"
      , size = 14
    )
)

