data(pie)
obs <- ObsLulcRasterStack(x=pie,
                          pattern="lu",
                          categories=c(1,2,3),
                          labels=c("Forest","Built","Other"),
                          t=c(0,6,14))
ef <- ExpVarRasterList(x=pie, pattern="ef")

part <- partition(x=obs[[1]],
                  size=0.1, spatial=TRUE)
# extract training data
train.data <- getPredictiveModelInputData(obs=obs,
                                          ef=ef,
                                          cells=part[["train"]],
                                          t=0)

test.data <- getPredictiveModelInputData(obs=obs,
                                         ef=ef,
                                         cells=part[["test"]])
# fit models (note that a predictive model is required for each land use category)
forms <- list(Built~ef_001+ef_002+ef_003,
              Forest~ef_001+ef_002,
              Other~ef_001+ef_002)

# generalized linear model models
glm.models <- glmModels(formula=forms,
                        family=binomial,
                        data=train.data,
                        obs=obs)
# obtain demand scenario
dmd <- approxExtrapDemand(obs=obs, tout=0:14)

ordered.model <- OrderedModel(obs=obs,
                              ef=ef,
                              models=glm.models,
                              time=0:14,
                              demand=dmd,
                              order=c(2,1,3))

ordered.model.all <- allocate(ordered.model, stochastic=TRUE)


# ========================================= #
# Try with independent model predictions ####
# ========================================= #

bjt <- 
prd <- predict(object=model@models, newdata=newdata) 