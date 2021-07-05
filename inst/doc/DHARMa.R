## ----global_options, include=FALSE--------------------------------------------
knitr::opts_chunk$set(fig.width=8.5, fig.height=5.5, fig.align='center', warning=FALSE, message=FALSE, cache = T)

## ---- echo = F, message = F---------------------------------------------------
library(DHARMa)
set.seed(123)

## ---- echo = F, fig.width=8, fig.height=3.5-----------------------------------
library(lme4)

overdispersedData = createData(sampleSize = 250, overdispersion = 0, quadraticFixedEffects = -2, family = poisson())
fittedModelOverdispersed <- glmer(observedResponse ~ Environment1 + (1|group) , family = "poisson", data = overdispersedData)

plotConventionalResiduals(fittedModelOverdispersed)

testData = createData(sampleSize = 250, intercept = 0, overdispersion = 0, family = poisson(), randomEffectVariance = 0)
fittedModel <- glmer(observedResponse ~ Environment1 + (1|group) , family = "poisson", data = testData)
plotConventionalResiduals(fittedModel)


## ---- eval = F----------------------------------------------------------------
#  install.packages("DHARMa")

## -----------------------------------------------------------------------------
library(DHARMa)
citation("DHARMa")

## -----------------------------------------------------------------------------
testData = createData(sampleSize = 250)
fittedModel <- glmer(observedResponse ~ Environment1 + (1|group) , 
                     family = "poisson", data = testData)

## ---- results = "hide", fig.show='hide'---------------------------------------
testDispersion(fittedModel)

## -----------------------------------------------------------------------------
simulationOutput <- simulateResiduals(fittedModel = fittedModel, plot = F)

## ---- results = "hide"--------------------------------------------------------
residuals(simulationOutput)

## ---- eval = F----------------------------------------------------------------
#  residuals(simulationOutput, quantileFunction = qnorm, outlierValues = c(-7,7))

## -----------------------------------------------------------------------------
plot(simulationOutput)

## ---- eval = F----------------------------------------------------------------
#  plotQQunif(simulationOutput) # left plot in plot.DHARMa()
#  plotResiduals(simulationOutput) # right plot in plot.DHARMa()

## ---- eval = F----------------------------------------------------------------
#  plotResiduals(simulationOutput, form = YOURPREDICTOR)

## ---- eval = F----------------------------------------------------------------
#  plotResiduals(simulationOutput, form = testData$group)

## ---- eval = F----------------------------------------------------------------
#  ?simulateResiduals

## ---- eval= F-----------------------------------------------------------------
#  simulationOutput <- simulateResiduals(fittedModel = fittedModel, refit = T)

## ---- eval= F-----------------------------------------------------------------
#  simulationOutput <- simulateResiduals(fittedModel = fittedModel, n = 250, use.u = T)

## ---- eval= F-----------------------------------------------------------------
#  simulationOutput = recalculateResiduals(simulationOutput, group = testData$group)

## ---- eval = F----------------------------------------------------------------
#  simulationOutput$randomState

## -----------------------------------------------------------------------------
testData = createData(sampleSize = 200, overdispersion = 1.5, family = poisson())
fittedModel <- glm(observedResponse ~  Environment1 , family = "poisson", data = testData)

simulationOutput <- simulateResiduals(fittedModel = fittedModel)
plot(simulationOutput)

## -----------------------------------------------------------------------------
testData = createData(sampleSize = 500, intercept=0, fixedEffects = 2, overdispersion = 0, family = poisson(), roundPoissonVariance = 0.001, randomEffectVariance = 0)
fittedModel <- glmer(observedResponse ~ Environment1 + (1|group) , family = "poisson", data = testData)

simulationOutput <- simulateResiduals(fittedModel = fittedModel)
plot(simulationOutput)

## ----overDispersionTest, echo = T, fig.width=4.5, fig.height=4.5--------------
testDispersion(simulationOutput)

## -----------------------------------------------------------------------------
testData = createData(sampleSize = 500, intercept = 2, fixedEffects = c(1), overdispersion = 0, family = poisson(), quadraticFixedEffects = c(-3), randomEffectVariance = 0, pZeroInflation = 0.6)

par(mfrow = c(1,2))
plot(testData$Environment1, testData$observedResponse, xlab = "Envrionmental Predictor", ylab = "Response")
hist(testData$observedResponse, xlab = "Response", main = "")

## -----------------------------------------------------------------------------
fittedModel <- glmer(observedResponse ~ Environment1 + I(Environment1^2) + (1|group) , family = "poisson", data = testData)

simulationOutput <- simulateResiduals(fittedModel = fittedModel)
plot(simulationOutput)

## ---- fig.width=4, fig.height=4-----------------------------------------------
testZeroInflation(simulationOutput)

## -----------------------------------------------------------------------------
library(glmmTMB)
fittedModel <- glmmTMB(observedResponse ~ Environment1 + I(Environment1^2) + (1|group), ziformula = ~1 , family = "poisson", data = testData)
summary(fittedModel)

simulationOutput <- simulateResiduals(fittedModel = fittedModel)
plot(simulationOutput)

## ---- fig.width=4.5, fig.height=4.5-------------------------------------------
countOnes <- function(x) sum(x == 1)  # testing for number of 1s
testGeneric(simulationOutput, summary = countOnes, alternative = "greater") # 1-inflation

## -----------------------------------------------------------------------------
testData = createData(sampleSize = 500, intercept = 0, overdispersion = function(x){return(rnorm(length(x), sd = 2 * abs(x)))}, family = poisson(), randomEffectVariance = 0)
fittedModel <- glm(observedResponse ~ Environment1 , family = "poisson", data = testData)

simulationOutput <- simulateResiduals(fittedModel = fittedModel)
plot(simulationOutput)

## ---- eval = F----------------------------------------------------------------
#  testQuantiles(simulationOutput)

## ---- eval = F----------------------------------------------------------------
#  testCategorical(simulationOutput, catPred = testData$group)

## -----------------------------------------------------------------------------
testData = createData(sampleSize = 500, intercept = 0, overdispersion = function(x){return(rnorm(length(x), sd = 2*abs(x)))}, family = poisson(), randomEffectVariance = 0)
fittedModel <- glmer(observedResponse ~ Environment1 + (1|group) + (1|ID), family = "poisson", data = testData)

# plotConventionalResiduals(fittedModel)

simulationOutput <- simulateResiduals(fittedModel = fittedModel)
plot(simulationOutput)

## ---- eval = F----------------------------------------------------------------
#  simulationOutput$scaledResiduals

## -----------------------------------------------------------------------------
testData = createData(sampleSize = 200, intercept = 1, fixedEffects = c(1,2), overdispersion = 0, family = poisson(), quadraticFixedEffects = c(-3,0))
fittedModel <- glmer(observedResponse ~ Environment1 + Environment2 + (1|group) , family = "poisson", data = testData)
simulationOutput <- simulateResiduals(fittedModel = fittedModel)
# plotConventionalResiduals(fittedModel)
plot(simulationOutput, quantreg = T)
# testUniformity(simulationOutput = simulationOutput)

## -----------------------------------------------------------------------------
par(mfrow = c(1,2))
plotResiduals(simulationOutput, testData$Environment1)
plotResiduals(simulationOutput, testData$Environment2)

## ---- fig.width=4.5, fig.height=4.5-------------------------------------------
testData = createData(sampleSize = 100, family = poisson(), spatialAutocorrelation = 5)
fittedModel <- glmer(observedResponse ~ Environment1 + (1|group), data = testData, family = poisson() )
simulationOutput <- simulateResiduals(fittedModel = fittedModel)
testSpatialAutocorrelation(simulationOutput = simulationOutput, x = testData$x, y= testData$y)

## ---- echo = F----------------------------------------------------------------
data = structure(list(N_parasitized = c(226, 689, 481, 960, 1177, 266, 
46, 4, 884, 310, 19, 4, 7, 1, 3, 0, 365, 388, 369, 829, 532, 
5), N_adult = c(1415, 2227, 2854, 3699, 2094, 376, 8, 1, 1379, 
323, 2, 2, 11, 2, 0, 1, 1394, 1392, 1138, 719, 685, 3), density.attack = c(216.461273226486, 
214.662143448767, 251.881252132684, 400.993643475831, 207.897856251888, 
57.0335141562012, 6.1642552100285, 0.503930659141302, 124.673812637575, 
27.3764667492035, 0.923453215863429, 0.399890030241684, 0.829818131526174, 
0.146640466903247, 0.216795117773948, 0.215498663908284, 110.635445098884, 
91.3766566822467, 126.157080458047, 82.9699108890686, 61.0476207779938, 
0.574539291305784), Plot = structure(c(1L, 1L, 1L, 1L, 1L, 1L, 
1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L, 3L
), .Label = c("1", "2", "3", "4"), class = "factor"), PY = c("p1y82", 
"p1y83", "p1y84", "p1y85", "p1y86", "p1y87", "p1y88", "p1y89", 
"p2y86", "p2y87", "p2y88", "p2y89", "p2y90", "p2y91", "p2y92", 
"p2y93", "p3y88", "p3y89", "p3y90", "p3y91", "p3y92", "p3y93"
), Year = c(82, 83, 84, 85, 86, 87, 88, 89, 86, 87, 88, 89, 90, 
91, 92, 93, 88, 89, 90, 91, 92, 93), ID = 1:22), .Names = c("N_parasitized", 
"N_adult", "density.attack", "Plot", "PY", "Year", "ID"), row.names = c("p1y82", 
"p1y83", "p1y84", "p1y85", "p1y86", "p1y87", "p1y88", "p1y89", 
"p2y86", "p2y87", "p2y88", "p2y89", "p2y90", "p2y91", "p2y92", 
"p2y93", "p3y88", "p3y89", "p3y90", "p3y91", "p3y92", "p3y93"
), class = "data.frame")

data$logDensity = log10(data$density.attack+1)

## ---- fig.width=5, fig.height=5-----------------------------------------------
plot(N_parasitized / (N_adult + N_parasitized ) ~ logDensity, 
     xlab = "Density", ylab = "Proportion infected", data = data)

## -----------------------------------------------------------------------------
mod1 <- glm(cbind(N_parasitized, N_adult) ~ logDensity, data = data, family=binomial)
simulationOutput <- simulateResiduals(fittedModel = mod1)
plot(simulationOutput)

## -----------------------------------------------------------------------------
mod2 <- glmer(cbind(N_parasitized, N_adult) ~ logDensity + (1|ID), data = data, family=binomial)
simulationOutput <- simulateResiduals(fittedModel = mod2)
plot(simulationOutput)

## -----------------------------------------------------------------------------
mod3 <- glmer(cbind(N_parasitized, N_adult) ~ logDensity + I(logDensity^2) + (1|ID), data = data, family=binomial)
simulationOutput <- simulateResiduals(fittedModel = mod3)
plot(simulationOutput)

## -----------------------------------------------------------------------------
anova(mod2, mod3)

## -----------------------------------------------------------------------------
AIC(mod2) 
AIC(mod3)

## -----------------------------------------------------------------------------
m1 <- glm(SiblingNegotiation ~ FoodTreatment*SexParent + offset(log(BroodSize)), data=Owls , family = poisson)
res <- simulateResiduals(m1)
plot(res)

## -----------------------------------------------------------------------------
m2 <- glmer(SiblingNegotiation ~ FoodTreatment*SexParent + offset(log(BroodSize)) + (1|Nest), data=Owls , family = poisson)
res <- simulateResiduals(m2)
plot(res)

## -----------------------------------------------------------------------------
m3 <- glmmTMB(SiblingNegotiation ~ FoodTreatment*SexParent + offset(log(BroodSize)) + (1|Nest), data=Owls , family = nbinom1)

res <- simulateResiduals(m3, plot = T)
par(mfrow = c(1,3))
plotResiduals(res, Owls$FoodTreatment)
testDispersion(res)
testZeroInflation(res)

## -----------------------------------------------------------------------------
m4 <- glmmTMB(SiblingNegotiation ~ FoodTreatment*SexParent + offset(log(BroodSize)) + (1|Nest), ziformula = ~ FoodTreatment + SexParent,  data=Owls , family = nbinom1)

res <- simulateResiduals(m4, plot = T)
par(mfrow = c(1,3))
plotResiduals(res, Owls$FoodTreatment)
testDispersion(res)
testZeroInflation(res)

## -----------------------------------------------------------------------------
m5 <- glmmTMB(SiblingNegotiation ~ FoodTreatment*SexParent + offset(log(BroodSize)) + (1|Nest), dispformula = ~ FoodTreatment , ziformula = ~ FoodTreatment + SexParent,  data=Owls , family = nbinom1)

res <- simulateResiduals(m4, plot = T)
par(mfrow = c(1,3))
plotResiduals(res, Owls$FoodTreatment)
testDispersion(res)
testZeroInflation(res)

## -----------------------------------------------------------------------------
testData = createData(sampleSize = 500, overdispersion = 0, fixedEffects = 5, family = binomial(), randomEffectVariance = 3, numGroups = 25)
fittedModel <- glm(observedResponse ~ 1, family = "binomial", data = testData)

simulationOutput <- simulateResiduals(fittedModel = fittedModel)

## -----------------------------------------------------------------------------
plot(simulationOutput, asFactor = T)

## -----------------------------------------------------------------------------
plotResiduals(simulationOutput, testData$Environment1, quantreg = T)

## -----------------------------------------------------------------------------
par(mfrow = c(1,2))
testDispersion(simulationOutput)

simulationOutput = recalculateResiduals(simulationOutput , group = testData$group)
testDispersion(simulationOutput)

## -----------------------------------------------------------------------------
n = 1000
p = runif(n, 0.1,0.9) # true probabilities
obs = rbinom(n,1,p)

## -----------------------------------------------------------------------------
fit<-glm(obs ~ 1, family = "binomial") # wrong model, assumes equal probabilities
library(DHARMa)
res<-simulateResiduals(fit, plot = T) # nothing to see

## -----------------------------------------------------------------------------
res2 <- recalculateResiduals(res, group = rep(1:20, each = 10))
plot(res2) 

## -----------------------------------------------------------------------------
grouping = cut(p, breaks = quantile(p, seq(0,1,0.02)))
res3 <- recalculateResiduals(res, group = grouping)
plot(res3)

## -----------------------------------------------------------------------------
set.seed(123)

# created data and fit with a missing predictor (Environment2)

testData = createData(sampleSize = 500, overdispersion = 0, fixedEffects = c(1,3), family = binomial(), randomEffectVariance = 3, numGroups = 50)
fittedModel <- glm(observedResponse ~ Environment1, family = "binomial", data = testData)

res <- simulateResiduals(fittedModel = fittedModel)
plot(res)

# grouping according to RE produces overdispersion because RE is missing in the model 

res2 = recalculateResiduals(res , group = testData$group)
plot(res2)

# grouping according to random factor does not produce an effect, wrt. to this the model has correct dispersion

grouping = as.factor(sample.int(50, 500, replace = T))

res2 = recalculateResiduals(res , group = grouping)
plot(res2)

# grouping according to response doesn't create a pattern

x = predict(fittedModel)
grouping = cut(x, breaks = quantile(x, seq(0,1,0.02)))
res2 = recalculateResiduals(res , group = grouping)
plot(res2)

# grouping according to missing variable creates pattern, because wrt. to this variable, the model is overdispersed

x = testData$Environment2
grouping = cut(x, breaks = quantile(x, seq(0,1,0.02)))
res2 = recalculateResiduals(res , group = grouping)
plot(res2)

# grouping according to space does not create a pattern, because there is no missing spatial predictor

x = testData$x
grouping = cut(x, breaks = quantile(x, seq(0,1,0.02)))
res3 = recalculateResiduals(res , group = grouping)
plot(res3)

## ---- eval = T----------------------------------------------------------------
testData = createData(sampleSize = 200, overdispersion = 0.5, family = poisson())
fittedModel <- glm(observedResponse ~ Environment1, family = "poisson", data = testData)

simulatePoissonGLM <- function(fittedModel, n){
  pred = predict(fittedModel, type = "response")
  nObs = length(pred)
  sim = matrix(nrow = nObs, ncol = n)
  for(i in 1:n) sim[,i] = rpois(nObs, pred)
  return(sim)
}

sim = simulatePoissonGLM(fittedModel, 100)

DHARMaRes = createDHARMa(simulatedResponse = sim, observedResponse = testData$observedResponse, 
             fittedPredictedResponse = predict(fittedModel), integerResponse = T)
plot(DHARMaRes, quantreg = F)

