params <-
list(EVAL = TRUE)

## ---- SETTINGS-knitr, include=FALSE-------------------------------------------
stopifnot(require(knitr))
opts_chunk$set(
  eval = if (isTRUE(exists("params"))) params$EVAL else FALSE
)

## ----eval=TRUE,echo=FALSE,fig.align='center',out.height='462px',out.width='851px'----
knitr::include_graphics("MICEalgorithm.png")

## ----eval=TRUE,echo=FALSE,fig.align='center',out.height='278px',out.width='793px'----
knitr::include_graphics("PMM.png")

## ----skewedData, fig.height = 8, fig.width = 8--------------------------------
library(data.table)
library(miceRanger)

# random uniform variable
nrws <- 1000
dat <- data.table(Uniform_Variable = runif(nrws))

# slightly bimodal variable correlated with Uniform_Variable
dat$Close_Bimodal_Variable <- sapply(
    dat$Uniform_Variable
  , function(x) sample(c(rnorm(1,-2),rnorm(1,2)),prob=c(x,1-x),size=1)
) + dat$Uniform_Variable

# very bimodal variable correlated with Uniform_Variable
dat$Far_Bimodal_Variable <- sapply(
    dat$Uniform_Variable
  , function(x) sample(c(rnorm(1,-3),rnorm(1,3)),prob=c(x,1-x),size=1)
)

# Highly skewed variable correlated with Uniform_Variable
dat$Skewed_Variable <- exp((dat$Uniform_Variable*runif(nrws)*3)) + runif(nrws)*3

# Integer variable correlated with Close_Bimodal_Variable and Uniform_Variable
dat$Integer_Variable <- round(dat$Uniform_Variable + dat$Close_Bimodal_Variable/3 + runif(nrws)*2)

# Ampute the data.
ampDat <- amputeData(dat,0.2)

# Plot the original data
plot(dat)

## -----------------------------------------------------------------------------
mrMeanMatch <- miceRanger(ampDat,valueSelector = "meanMatch",verbose=FALSE)
mrModelOutput <- miceRanger(ampDat,valueSelector = "value",verbose=FALSE)

## ----eval=TRUE,echo=FALSE,fig.align='center',out.width='800px'----------------
knitr::include_graphics("mmEffectsFarBimodal.png")

## ----eval=TRUE,echo=FALSE,fig.align='center',out.width='800px'----------------
knitr::include_graphics("mmEffectsSkewed.png")

## ----eval=TRUE,echo=FALSE,fig.align='center',out.width='800px'----------------
knitr::include_graphics("mmEffectsInteger.png")

