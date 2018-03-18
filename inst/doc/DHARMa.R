## ---- echo = F-----------------------------------------------------------
library(DHARMa)
set.seed(123)

## ----global_options, include=FALSE---------------------------------------
knitr::opts_chunk$set(fig.width=7, fig.height=4.5, fig.align='center', warning=FALSE, message=FALSE, cache = F)

## ---- echo = F, fig.width=8, fig.height=3.5------------------------------
library(lme4)

overdispersedData = createData(sampleSize = 250, overdispersion = 0, quadraticFixedEffects = -2, family = poisson())
fittedModelOverdispersed <- glmer(observedResponse ~ Environment1 + (1|group) , family = "poisson", data = overdispersedData)

plotConventionalResiduals(fittedModelOverdispersed)


testData = createData(sampleSize = 250, intercept = 0, overdispersion = 0, family = poisson(), randomEffectVariance = 0)
fittedModel <- glmer(observedResponse ~ Environment1 + (1|group) , family = "poisson", data = testData)

plotConventionalResiduals(fittedModel)


## ---- eval = F-----------------------------------------------------------
#  install.packages("DHARMa")

## ------------------------------------------------------------------------
library(DHARMa)
citation("DHARMa")

## ------------------------------------------------------------------------
simulationOutput <- simulateResiduals(fittedModel = fittedModel, n = 250)

## ---- eval = F-----------------------------------------------------------
#  simulationOutput$scaledResiduals

## ---- eval = F-----------------------------------------------------------
#  simulationOutput$scaledResidualsNormal

## ------------------------------------------------------------------------
plotSimulatedResiduals(simulationOutput = simulationOutput)

## ---- eval = F-----------------------------------------------------------
#  plotResiduals(YOURPREDICTOR, simulationOutput$scaledResiduals)

## ------------------------------------------------------------------------
testUniformity(simulationOutput = simulationOutput)

## ---- eval = F-----------------------------------------------------------
#  ?simulateResiduals

## ---- eval= F------------------------------------------------------------
#  simulationOutput <- simulateResiduals(fittedModel = fittedModel, refit = T)

## ---- eval= F------------------------------------------------------------
#  simulationOutput <- simulateResiduals(fittedModel = fittedModel, n = 250, use.u = T)

## ------------------------------------------------------------------------
simulationOutput <- simulateResiduals(fittedModel = fittedModel, seed = NA)
testUniformity(simulationOutput)
simulationOutput <- simulateResiduals(fittedModel = fittedModel, seed = NA)
testUniformity(simulationOutput)

## ---- eval = F-----------------------------------------------------------
#  simulationOutput$randomState

## ---- eval = F-----------------------------------------------------------
#  res = createDHARMa(scaledResiduals = posteriorPredictiveSimulations, simulatedResponse = medianPosteriorPredictions, observedResponse = observations, integerResponse = ?)

## ------------------------------------------------------------------------
set.seed(123)

## ---- eval = F-----------------------------------------------------------
#  sessionInfo()

## ------------------------------------------------------------------------
testData = createData(sampleSize = 500, overdispersion = 2, family = poisson())
fittedModel <- glmer(observedResponse ~ Environment1 + (1|group) , family = "poisson", data = testData)

simulationOutput <- simulateResiduals(fittedModel = fittedModel)
plotSimulatedResiduals(simulationOutput = simulationOutput)

## ------------------------------------------------------------------------
testData = createData(sampleSize = 500, intercept=0, fixedEffects = 2, overdispersion = 0, family = poisson(), roundPoissonVariance = 0.001, randomEffectVariance = 0)
fittedModel <- glmer(observedResponse ~ Environment1 + (1|group) , family = "poisson", data = testData)

summary(fittedModel)

# plotConventionalResiduals(fittedModel)

simulationOutput <- simulateResiduals(fittedModel = fittedModel)
plotSimulatedResiduals(simulationOutput = simulationOutput)
testUniformity(simulationOutput = simulationOutput)

## ----overDispersionTest, echo = T----------------------------------------
# Option 1
testOverdispersionParametric(fittedModel)

# Option 2
testOverdispersion(simulationOutput)

# Option 3
simulationOutput2 <- simulateResiduals(fittedModel = fittedModel, refit = T, n = 20)
testOverdispersion(simulationOutput2)

## ------------------------------------------------------------------------
testData = createData(sampleSize = 500, intercept = 2, fixedEffects = c(1), overdispersion = 0, family = poisson(), quadraticFixedEffects = c(-3), randomEffectVariance = 0, pZeroInflation = 0.6)

par(mfrow = c(1,2))
plot(testData$Environment1, testData$observedResponse, xlab = "Envrionmental Predictor", ylab = "Response")
hist(testData$observedResponse, xlab = "Response", main = "")

## ------------------------------------------------------------------------

fittedModel <- glmer(observedResponse ~ Environment1 + I(Environment1^2) + (1|group) , family = "poisson", data = testData)

simulationOutput <- simulateResiduals(fittedModel = fittedModel)
plotSimulatedResiduals(simulationOutput = simulationOutput)

## ---- fig.width=4, fig.height=4------------------------------------------

testZeroInflation(simulationOutput)

## ------------------------------------------------------------------------
testData = createData(sampleSize = 500, intercept = 0, overdispersion = function(x){return(rnorm(length(x), sd = 2*abs(x)))}, family = poisson(), randomEffectVariance = 0)
fittedModel <- glmer(observedResponse ~ Environment1 + (1|group), family = "poisson", data = testData)

simulationOutput <- simulateResiduals(fittedModel = fittedModel)
plotSimulatedResiduals(simulationOutput = simulationOutput)
testUniformity(simulationOutput = simulationOutput)

## ------------------------------------------------------------------------
testData = createData(sampleSize = 500, intercept = 0, overdispersion = function(x){return(rnorm(length(x), sd = 2*abs(x)))}, family = poisson(), randomEffectVariance = 0)
fittedModel <- glmer(observedResponse ~ Environment1 + (1|group) + (1|ID), family = "poisson", data = testData)

# plotConventionalResiduals(fittedModel)

simulationOutput <- simulateResiduals(fittedModel = fittedModel)
plotSimulatedResiduals(simulationOutput = simulationOutput)
testUniformity(simulationOutput = simulationOutput)

## ---- eval = F-----------------------------------------------------------
#  simulationOutput$scaledResiduals

## ------------------------------------------------------------------------
testData = createData(sampleSize = 200, intercept = 1, fixedEffects = c(1,2), overdispersion = 0, family = poisson(), quadraticFixedEffects = c(-3,0))
fittedModel <- glmer(observedResponse ~ Environment1 + Environment2 + (1|group) , family = "poisson", data = testData)
simulationOutput <- simulateResiduals(fittedModel = fittedModel)
# plotConventionalResiduals(fittedModel)
plotSimulatedResiduals(simulationOutput = simulationOutput, quantreg = T)
testUniformity(simulationOutput = simulationOutput)

## ------------------------------------------------------------------------
par(mfrow = c(1,2))
plotResiduals(testData$Environment1,  simulationOutput$scaledResiduals)
plotResiduals(testData$Environment2,  simulationOutput$scaledResiduals)

## ------------------------------------------------------------------------
testData = createData(sampleSize = 100, family = poisson(), temporalAutocorrelation = 5)

fittedModel <- glmer(observedResponse ~ Environment1 + (1|group), data = testData, family = poisson() )

simulationOutput <- simulateResiduals(fittedModel = fittedModel)

## ---- fig.width=4, fig.height=4------------------------------------------
testTemporalAutocorrelation(simulationOutput = simulationOutput, time = testData$time)
testTemporalAutocorrelation(simulationOutput = simulationOutput)

## ------------------------------------------------------------------------
testData = createData(sampleSize = 100, family = poisson(), spatialAutocorrelation = 5)

fittedModel <- glmer(observedResponse ~ Environment1 + (1|group), data = testData, family = poisson() )

simulationOutput <- simulateResiduals(fittedModel = fittedModel)


## ---- fig.width=4, fig.height=4------------------------------------------
testSpatialAutocorrelation(simulationOutput = simulationOutput, x = testData$x, y= testData$y)
testSpatialAutocorrelation(simulationOutput = simulationOutput)

## ---- echo = F-----------------------------------------------------------
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

## ---- fig.width=4, fig.height=4------------------------------------------
plot(N_parasitized / (N_adult + N_parasitized ) ~ logDensity, xlab = "Density", ylab = "Proportion infected", data = data)

## ------------------------------------------------------------------------
mod1 <- glm(cbind(N_parasitized, N_adult) ~ logDensity, data = data, family=binomial)
simulationOutput <- simulateResiduals(fittedModel = mod1)
plotSimulatedResiduals(simulationOutput = simulationOutput)

## ------------------------------------------------------------------------
testUniformity(simulationOutput = simulationOutput)

## ------------------------------------------------------------------------
testOverdispersion(simulationOutput = simulationOutput)

## ------------------------------------------------------------------------
mod2 <- glmer(cbind(N_parasitized, N_adult) ~ logDensity + (1|ID), data = data, family=binomial)
simulationOutput <- simulateResiduals(fittedModel = mod2)
plotSimulatedResiduals(simulationOutput = simulationOutput)


## ---- echo=F, cache = T--------------------------------------------------
altitude = rep(seq(0,1,len = 50), each = 20)
dataID = 1:1000
spatialCoordinate = rep(seq(0,30, len = 50), each = 20)

moisture = runif(1000, -1,1)
deadwood = runif(1000, -1,1)

# random effects + zeroinflation
plot = rep(1:50, each = 20)
year = rep(1:20, times = 50)

yearRandom = rnorm(20, 0, 1)
plotRandom = rnorm(50, 0, 1)
overdispersion = rnorm(1000, sd = 0.5)
zeroinflation = rbinom(1000,1,0.6)

beetles <- rpois(1000, exp( 0  + 12*altitude - 12*altitude^2 - 0.2 * moisture + deadwood 
#  + overdispersion   + plotRandom[plot]
 + yearRandom[year]) * zeroinflation )

data = data.frame(dataID, beetles, altitude, moisture, deadwood, plot, year, spatialCoordinate)

## ------------------------------------------------------------------------
par(mfrow = c(1,3))
plot(log10(beetles) ~ altitude + I(altitude) + moisture, data = data, main = "Beetle counts", xlab = "Altitude")

## ------------------------------------------------------------------------
mod <- glmer(beetles ~ altitude + I(altitude^2) + moisture + (1|plot) + (1|year), data = data, family=poisson, control = glmerControl(optCtrl = list(maxfun = 10000)))
simulationOutput <- simulateResiduals(fittedModel = mod)
plotSimulatedResiduals(simulationOutput = simulationOutput)
summary(mod)

## ---- fig.width=4, fig.height=4------------------------------------------
plotResiduals(data$deadwood, simulationOutput$scaledResiduals)

## ------------------------------------------------------------------------
mod <- glmer(beetles ~ altitude + I(altitude^2) + moisture + deadwood + (1|plot) + (1|year) , data = data, family=poisson, control = glmerControl(optCtrl = list(maxfun = 10000)))
simulationOutput <- simulateResiduals(fittedModel = mod)
plotSimulatedResiduals(simulationOutput = simulationOutput)
summary(mod)

## ------------------------------------------------------------------------
mod <- glmer(beetles ~ altitude + I(altitude^2) + moisture + deadwood + (1|plot) + (1|year) + (1|dataID) , data = data, family=poisson, control = glmerControl(optCtrl = list(maxfun = 10000)))
simulationOutput <- simulateResiduals(fittedModel = mod)
plotSimulatedResiduals(simulationOutput = simulationOutput)

## ---- fig.width=4, fig.height=4------------------------------------------
testZeroInflation(simulationOutput)

