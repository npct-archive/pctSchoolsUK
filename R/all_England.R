library(ggplot2)
library(dplyr)

# Aim: subset the data to a region

#setwd("/home/geoif/pct/pctSchoolsUK")
# Read in and pre-process the data
if(!exists("sld11"))
  source("R/preprocess-data.R")
  #source("R/analysis-sld.R")


# # Use the spatial dataframe with the full set of schools to do the spatial subsetting
# # (to get all of Leeds, not just LSOAs where there are Secondary schools)
# leeds_met_area = c("Leeds", "Wakefield","Kirklees","Calderdale","Bradford","Barnsley","Craven","Harrogate","Selby","York")
# summary(unique(sldfull_sp$`LA (name)`) %in% leeds_met_area)
# schools_leeds = sldfull_sp[sldfull_sp$`LA (name)` %in% leeds_met_area, ]
# #schools_leeds = sldfull_sp[sldfull_sp$`LA (name)`=="Leeds", ]
# schools_leeds = spTransform(schools_leeds, CRSobj = proj4string(las))
# las_leeds = las[schools_leeds,]
# cents_lsoa_leeds = cents_lsoa[schools_leeds,]

# #######################################################################
# ### JUST BRADFORD
# 
# cents_lsoa_brad = cents_lsoa[grepl("Bradford", cents_lsoa$LSOA11NM),]
# las_brad = las[cents_lsoa_brad,]
# schools_brad = schools[las_brad,]
# 
# # TODO: fix this - not working! # ILAN: This works for me! Weird..
# sel_flow_brad = gWithin(flow, las_brad, byid = TRUE)
# bbox_brad = stplanr::bb2poly(bb = bbox(las_brad))
# proj4string(bbox_brad) = proj4string(flow)
# flow_brad = flow[bbox_brad,]
# flow_brad = flow_brad[las_brad,]
# plot(las_brad); lines(flow_brad, pch='.', col="blue", cex=5); points(schools_brad, pch='.', col="red", cex=5)
# 
# 
# #######################################################################
# ### JUST CAMBRIDGE
# 
# 
# cents_lsoa_cam = cents_lsoa[grepl("\\<Cambridge\\>", cents_lsoa$LSOA11NM),]
# las_cam = las[cents_lsoa_cam,]
# schools_cam = schools[las_cam,]
# 
# # TODO: fix this - not working! # ILAN: This works for me! Weird..
# sel_flow_cam = gWithin(flow, las_cam, byid = TRUE)
# bbox_cam = stplanr::bb2poly(bb = bbox(las_cam))
# proj4string(bbox_cam) = proj4string(flow)
# flow_cam = flow[bbox_cam,]
# flow_cam = flow_cam[las_cam,]
# 
# #plot(las_cam); lines(flow_cam, pch='.', col="blue", cex=5); points(schools_cam, pch='.', col="red", cex=5)

#####################################################################
# BASELINE MODEL FITTING FOR ENGLAND

# See ?stplanr::route_cyclestreet for methods to save and use Cyclestreets API key

# READ IN THE ROUTES FROM THE ORIGINAL DATA SET WHERE WE MISIDENTIFIED THE LSOAS AS BEING 2011 WHEN THEY WERE IN FACT 2001
# THIS DOESN'T MATTER AS ALL THIS WILL DO IS THAT LINES WHICH DON'T MATCH WITH THE 2011 CENTROIDS WILL BE DROPPED SO THERE WILL BE A SLIGHT LOSS OF DATA
# WE USE THIS ONLY TO TRAIN THE MODEL, AND THIS HAS INTEGER FLOWS AS REQUIRED FOR THAT
# AT THE END WE WILL PREDICT ON THE CORRECTLY CONVERTED 2011 LSOAS WITH NON-INTEGER FLOWS (FROM LSOAS WHICH SPLIT 2001-->2011) AND THIS IS THE FINAL, CORRECT OUTPUT
if(!file.exists("private_data/rf_england_schools.Rds")){
  # , base_url="http://pct.cyclestreets.net/api/"
  fast_routes_england = line2route(l = flow, route_fun = route_cyclestreet, plan = "fastest", n_processes = 10, base_url="http://pct.cyclestreets.net/api/")
  saveRDS(fast_routes_england, "private_data/rf_england_schools.Rds")
  #quiet_routes_england = line2route(l = flow, route_fun = route_cyclestreet, plan = "quietest", n_processes = 10, base_url="http://pct.cyclestreets.net/api/")
  #saveRDS(quiet_routes_england, "private_data/rq_england_schools.Rds")
} else{
  fast_routes_england = readRDS("private_data/rf_england_schools.Rds")
  #quiet_routes_england = readRDS("private_data/rq_england_schools.Rds")
}


#plot(las); plot(flow, col="blue", add=T); plot(fast_routes_england, col="red", add = T)

# summary(fast_routes_england$length)
# summary(quiet_routes_england$length)
# 
# mean(fast_routes_england$length, na.rm=TRUE)
# sd(fast_routes_england$length, na.rm=TRUE)
# IQR(fast_routes_england$length, na.rm=TRUE)
# 
# mean(quiet_routes_england$length, na.rm=TRUE)
# sd(quiet_routes_england$length, na.rm=TRUE)
# IQR(quiet_routes_england$length, na.rm=TRUE)
# 
# boxplot(quiet_routes_england$length, fast_routes_england$length)
# 
# violindf1 = data.frame(Type="Quiet", Length=quiet_routes_england$length)
# violindf2 = data.frame(Type="Fast", Length=fast_routes_england$length)
# violindf = rbind(violindf1, violindf2)
# #violindf = data.frame(fast_routes_england)

# Mode = function(x) {
#   ux = unique(x)
#   ux[which.max(tabulate(match(x, ux)))]
# }

#hist(quiet_routes_england$length)
#hist(fast_routes_england$length)

#ggplot(violindf, aes(x=Type, y=Length)) + geom_violin(trim=T, fill="blue", alpha=0.4) + geom_boxplot(width=0.1, outlier.size = 2, outlier.color = "red", outlier.shape = 21) + stat_summary(fun.y=mean, geom="point", shape=18, color="blue", size=3) #+ ylim(0, 20000)#+ stat_summary(fun.data = mean_sdl, geom="pointrange", color="red", fun.args=list(mult=1))#+ stat_summary(fun.y=mean, geom="point")  + stat_summary(fun.y=median, geom="point", shape=23, color="red")#+ geom_hline(aes(yintercept=Mode(quiet_routes_england$length)), col="red")

names(fast_routes_england) = paste(names(fast_routes_england),"fast", sep = "_")
#names(quiet_routes_england) = paste(names(quiet_routes_england),"quiet", sep = "_")

#englandrf = cbind(flow, fast_routes_england, quiet_routes_england)#@data

englandrf = fast_routes_england
englandrf@data = cbind(flow@data, fast_routes_england@data)

englandrf$pcycle = englandrf$CYCLE/englandrf$TOTAL


englandrf$distance_fast = englandrf$length_fast / 1000
englandrf$gradient_fast = (englandrf$av_incline_fast * 100) - 0.97
#englandrf$distance_quiet = englandrf$length_quiet / 1000
#englandrf$gradient_quiet = (englandrf$av_incline_quiet * 100) - 0.97


nrow(englandrf)
sel = which(englandrf$distance_fast <= 15)
englandrf = englandrf[sel, ]
nrow(englandrf)


# Just smooth fits to the raw pcycle vs dist/grad data
#ggplot(englandrf@data, aes(distance_fast, pcycle)) + geom_point(alpha=0.3, size=1, shape=1, na.rm = T) + geom_smooth(na.rm = T) + xlim(0,15)
#ggplot(englandrf@data, aes(distance_quiet, pcycle)) + geom_point(alpha=0.3, size=1, shape=1, na.rm = T) + geom_smooth(na.rm = T) + xlim(0,15)

# QDF FACTOR
#englandrf$qdf = englandrf$distance_quiet/englandrf$distance_fast
#ggplot(englandrf@data, aes(qdf, pcycle)) + geom_point(alpha=0.3, size=1, shape=1, na.rm = T) #+ geom_smooth(na.rm = T)# + xlim(0,15)


# Keep the desire lines in these and the covariates and uptake predictions in englandrf matching up row by row
fast_routes_england = fast_routes_england[sel,]
#quiet_routes_england = quiet_routes_england[sel,]

isTRUE(nrow(englandrf) == nrow(fast_routes_england))
#isTRUE(nrow(englandrf) == nrow(quiet_routes_england))


#install.packages("Amelia")
#library(Amelia)
#missmap(englandrf@data)
#dropcols = c("error_quiet","error_fast")
dropcols = c("error_fast")
englandrf = englandrf[, !(names(englandrf) %in% dropcols)]


# Create the columns necessary for plotting bins of average values within each bin
#tapply(englandrf$pcycle, cut(englandrf$gradient_fast, breaks = seq(min(englandrf$gradient_fast), max(englandrf$gradient_fast))), length)
#tapply(englandrf$pcycle, cut(englandrf$gradient_fast, breaks = seq(-1, 8), labels=seq(0,8)), mean)
englandrf$gradbins = cut(englandrf$gradient_fast, breaks = seq(-1, 8), labels=seq(0,8))

#tapply(englandrf$pcycle, cut(englandrf$distance_fast, breaks = seq(min(englandrf$distance_fast), max(englandrf$distance_fast))), length)
#tapply(englandrf$pcycle, cut(englandrf$distance_fast, breaks = seq(0, 14)), mean)
#tapply(englandrf$pcycle, cut(englandrf$distance_fast, breaks = seq(0, 14), labels=seq(1,14)), mean)
englandrf$distbins = cut(englandrf$distance_fast, breaks = seq(0, 14), labels=seq(1,14))


# trainforanna_idx = sample(seq_len(nrow(meltdf)), size=floor(0.5*nrow(meltdf)))
# set.seed(5)
# trainforanna_idx = sample(seq_len(nrow(meltdf)), size=floor(0.5*nrow(meltdf)))
# trainforanna = meltdf[trainforanna_idx,]
# testforanna = meltdf[-trainforanna_idx,]
# readr::write_csv(trainforanna, "binary_50pc_england1.csv")
# readr::write_csv(testforanna, "binary_50pc_england2.csv")



##############################################################################
# SPLIT INTO TRAINING AND TEST SETS
# smp_size = floor(0.5*nrow(englandrf))
# set.seed(5)
# trainidx = sample(seq_len(nrow(englandrf)), size=smp_size)
# 
# traindf = englandrf[trainidx,]
# testdf = englandrf[-trainidx,]
##############################################################################


# TO USE THE FULL DATASET TO TRAIN
# Justification: point 3) here:  http://www.fharrell.com/2017/01/split-sample-model-validation.html
traindf = englandrf

traindf = traindf[complete.cases(traindf$CYCLE) & complete.cases(traindf$WALK) & complete.cases(traindf$CAR) & complete.cases(traindf$OTHER) & complete.cases(traindf$UNKNOWN) & complete.cases(traindf$distance_fast) & complete.cases(traindf$gradient_fast) & complete.cases(traindf$qdf), ]

#traindf = as.data.frame(traindf)
#names(traindf) = names(logitdf)

#if(!file.exists("private_data/meltdf_train.Rds")){
if(!file.exists("private_data/meltdf_full.Rds")){
  meltdf = data.frame(matrix(NA, nrow=sum(traindf$TOTAL), ncol=8))
  names(meltdf) = c("CYCLE","WALK","CAR","OTHER","UNKNOWN","distance_fast","gradient_fast","qdf")
  jstart = 1
  for(i in 1:nrow(traindf)){
    blocklen = 0
    if(traindf@data[i, "CYCLE"] > 0){
      blocklen = traindf@data[i, "CYCLE"]
      jend = jstart + blocklen - 1
      meltdf[jstart:jend, "CYCLE"] = 1
      meltdf[jstart:jend, "gradient_fast"] = traindf@data[i, "gradient_fast"]
      meltdf[jstart:jend, "distance_fast"] = traindf@data[i, "distance_fast"]
      meltdf[jstart:jend, "qdf"] = traindf@data[i, "qdf"]
      jstart = jend + 1
    }
    if(traindf@data[i, "WALK"] > 0){
      blocklen = traindf@data[i, "WALK"]
      jend = jstart + blocklen - 1
      meltdf[jstart:jend, "WALK"] = 1
      meltdf[jstart:jend, "gradient_fast"] = traindf@data[i, "gradient_fast"]
      meltdf[jstart:jend, "distance_fast"] = traindf@data[i, "distance_fast"]
      meltdf[jstart:jend, "qdf"] = traindf@data[i, "qdf"]
      jstart = jend + 1
    }
    if(traindf@data[i, "CAR"] > 0){
      blocklen = traindf@data[i, "CAR"]
      jend = jstart + blocklen - 1
      meltdf[jstart:jend, "CAR"] = 1
      meltdf[jstart:jend, "gradient_fast"] = traindf@data[i, "gradient_fast"]
      meltdf[jstart:jend, "distance_fast"] = traindf@data[i, "distance_fast"]
      meltdf[jstart:jend, "qdf"] = traindf@data[i, "qdf"]
      jstart = jend + 1
    }
    if(traindf@data[i, "OTHER"] > 0){
      blocklen = traindf@data[i, "OTHER"]
      jend = jstart + blocklen - 1
      meltdf[jstart:jend, "OTHER"] = 1
      meltdf[jstart:jend, "gradient_fast"] = traindf@data[i, "gradient_fast"]
      meltdf[jstart:jend, "distance_fast"] = traindf@data[i, "distance_fast"]
      meltdf[jstart:jend, "qdf"] = traindf@data[i, "qdf"]
      jstart = jend + 1
    }
    if(traindf@data[i, "UNKNOWN"] > 0){
      blocklen = traindf@data[i, "UNKNOWN"]
      jend = jstart + blocklen - 1
      meltdf[jstart:jend, "UNKNOWN"] = 1
      meltdf[jstart:jend, "gradient_fast"] = traindf@data[i, "gradient_fast"]
      meltdf[jstart:jend, "distance_fast"] = traindf@data[i, "distance_fast"]
      meltdf[jstart:jend, "qdf"] = traindf@data[i, "qdf"]
      jstart = jend + 1
    }
  }
  meltdf[is.na(meltdf)] = 0
  #saveRDS(meltdf, "private_data/meltdf_train.Rds")
  saveRDS(meltdf, "private_data/meltdf_full.Rds")
  } else{
    #meltdf = readRDS("private_data/meltdf_train.Rds")
    meltdf = readRDS("private_data/meltdf_full.Rds")
}

isTRUE(sum(meltdf[!is.na(meltdf$CYCLE), "CYCLE"]) == sum(traindf$CYCLE))
isTRUE(sum(meltdf[!is.na(meltdf$WALK), "WALK"]) == sum(traindf$WALK))
isTRUE(sum(meltdf[!is.na(meltdf$CAR), "CAR"]) == sum(traindf$CAR))

isTRUE((sum(meltdf[!is.na(meltdf$CAR), "CAR"]) + sum(meltdf[!is.na(meltdf$WALK), "WALK"]) + sum(meltdf[!is.na(meltdf$CYCLE), "CYCLE"]) + sum(meltdf[!is.na(meltdf$OTHER), "OTHER"]) + sum(meltdf[!is.na(meltdf$UNKNOWN), "UNKNOWN"])) == sum(traindf$TOTAL))
isTRUE(nrow(meltdf) == sum(traindf$TOTAL))

####################################################################################################
# Regularisation methods explored. Best performance was acheived for models with very small lambda,
#   therefore no regularisation is really required. LASSO suggested sqrt(distance_fast) and distance_fast^2
#   are the dominant features.

library(glmnet)
# NOTE THAT A "-1" OR "+0" IN THE FORMULA IS CRUCIAL AS OTHERWISE IT WILL ADD AN INTERCEPT, WHICH GLMNET WILL ITSELF LATER DO
# WITH QDF
#f = as.formula(~distance_fast+sqrt(distance_fast)+I(distance_fast^2)+gradient_fast+distance_fast:gradient_fast+sqrt(distance_fast):gradient_fast+I(distance_fast^(1/3))+qdf+0)
# WITHOUT QDF
#f = as.formula(~distance_fast+sqrt(distance_fast)+I(distance_fast^2)+gradient_fast+distance_fast:gradient_fast+sqrt(distance_fast):gradient_fast+I(distance_fast^(1/3))+0)
f = as.formula(~distance_fast+I(distance_fast^(2))+I(distance_fast^(1/2))+gradient_fast+distance_fast:gradient_fast+0)
x = model.matrix(f, meltdf)
# glmnet has standardize=T by default, so this is not required
y = as.matrix(meltdf$CYCLE, ncol=1)


#Cross-validation to select LASSO/Ridge/Elastic and do feature selection as well.
#library(doParallel)
#nodes <- detectCores()
#cl <- makeCluster(nodes)
#registerDoParallel(cl)

#if(!file.exists("private_data/CV_models_train.Rdata")){
if(!file.exists("private_data/CV_models_full.Rdata")){
  fitlasso = glmnet(x, y, family="binomial", alpha=1)
  fitridge = glmnet(x, y, family="binomial", alpha=0)
  fitelastic = glmnet(x, y, family="binomial", alpha=0.5)
  cvmods = c("fitlasso", "fitridge", "fitelastic")
  #foreach(i=seq(0,10,by=1), .packages = "glmnet") %dopar% {
  for(i in seq(0,10,by=1)){
    time1 = proc.time()
    print(paste("Alpha = ", i/10))
    assign(paste("fit",i,sep=""), cv.glmnet(x, y, type.measure = "auc", alpha=i/10, family="binomial", nfolds=3))
    print(proc.time() - time1)
    cvmods = c(cvmods, paste("fit",i,sep=""))
  }
  #save(list=cvmods, file="private_data/CV_models_train.Rdata")
  save(list=cvmods, file="private_data/CV_models_full.Rdata")
} else{
  #load(file="private_data/CV_models_train.Rdata")
  load(file="private_data/CV_models_full.Rdata")
}
#stopCluster(cl)

#devtools::install_github("cran/plotmo")

#plot(fitlasso, xvar="lambda", label=T)
plotmo::plot_glmnet(fitlasso)
plot(fitlasso, xvar="dev")
plot(fit10, main="LASSO")

#plot(fitridge, xvar="lambda", label=T)
plotmo::plot_glmnet(fitridge)
plot(fitridge, xvar="dev")
plot(fit0, main="Ridge")

#plot(fitelastic, xvar="lambda", label=T)
plotmo::plot_glmnet(fitelastic)
plot(fitelastic, xvar="dev")
plot(fit5, main="Elastic Net")



# To get the coefficients fit model with lambda value found by cross-validation
coef(fitelastic, s=fit5$lambda.min)
#coef(fit5, s="lambda.min") #these should be the same as above since fitelastic and fit5 are both alpha=0.5 (elastic net)
#coef(fitelastic, s=fit5$lambda.1se)


#################################################################################
# Fit a standard logistic regression, just for comparison
# glm requires the formula with the intercept in, and the response variable:
#f = as.formula(CYCLE~distance_fast+sqrt(distance_fast)+I(distance_fast^2)+gradient_fast+distance_fast:gradient_fast+sqrt(distance_fast):gradient_fast+I(distance_fast^(1/3)))
f_ordinary = as.formula(CYCLE~distance_fast+I(distance_fast^(2))+I(distance_fast^(1/2))+gradient_fast+distance_fast:gradient_fast)
glm(f_ordinary, family=binomial(link="logit"), data=meltdf)

# ##################################################################################################
# # Attempt at combinatorial optimisation for variable selection
# 
# genModels = function(response, predictors, n_models){
#   models_indicator = rbinom(n_models*length(predictors), size=1, prob=0.5)
#   models_indicator = matrix(models_indicator, nrow=n_models, byrow=T)
#   models_indicator = unique(models_indicator, MARGIN=1) # drop models with identical predictors
#   models_indicator = models_indicator[rowSums(models_indicator)!=0, ] #drop models with no predictors
#   comb_models = matrix(NA, nrow=nrow(models_indicator)*(1+length(predictors)), ncol=length(predictors))
#   counter = 1
#   for(i in 1:nrow(models_indicator)){
#     comb_models[counter,] = models_indicator[i,]
#     #print(paste("Base: ", comb_models[counter,]))
#     for(j in 1:length(predictors)){
#       counter = counter + 1
#       comb_models[counter,] = models_indicator[i,]
#       comb_models[counter, j] = ifelse(models_indicator[i, j] == 0, 1, 0)
#       #print(paste("variant: ", comb_models[counter, ]))
#     }
#     #print(counter)
#     counter = counter+1
#   }
#   comb_models
#   comb_models = unique(comb_models, MARGIN=1)
#   comb_models = comb_models[rowSums(comb_models)!=0, ] #drop models with no predictors
#   models = c()
#   for(i in 1:nrow(comb_models)){
#     models = c(models, paste(response, '~', paste(predictors[as.logical(comb_models[i,])], collapse='+')))
#   }
#   models
#   # Could use combn(predictors,2) to implement interaction and quadratic terms later
# }
# 
# genModels("CYCLE", c("distance_fast","gradient_fast"), n_models=10)
# 




# # ###################################################################################
# # THIS IS THE SECTION USED FOR MODEL SPECIFICATION/PREDICTOR SELECTION
# # Let's try fitting an Elastic Net model with every term we can think of and see if it gives
# #     us a decent subset of features to work with in a final model (ie doing feature selection)
# 
# smp_size = floor(0.15*nrow(meltdf))
# set.seed(5)
# trainidx = sample(seq_len(nrow(meltdf)), size=smp_size)
# 
# train_meltdf = meltdf[trainidx,]
# test_meltdf = meltdf[-trainidx,]
# 
# train_meltdf = train_meltdf[complete.cases(train_meltdf),]
# 
# # Can't use cube root or log terms for gradients because gradients can be zero or negative
# #f_feat = as.formula(~distance_fast+sqrt(distance_fast)+I(distance_fast^2)+I(distance_fast^3)+I(distance_fast^(1/3))+log(distance_fast)+gradient_fast+I(gradient_fast^2)+I(gradient_fast^3)+distance_fast:gradient_fast+0)
# # The model above gives the most significant predictors as log(dist), dist^1/3, grad, and dist. With a maximum AUC of 0.7
# # Let's see if we can get AUC 0.7 with less coefficients.
# f_feat = as.formula(~distance_fast+I(distance_fast^(2))+I(distance_fast^(1/2))+gradient_fast+0)
# # the linear and square/cube root terms are required to make the probability peak at low distances
# # this is the simplest model I could build with AUC ~0.7 and which fits the data (for both distance and gradient plots)
# 
# x_feat = model.matrix(f_feat, train_meltdf)
# y_feat = as.matrix(train_meltdf$CYCLE, ncol=1)
# feat_sel_mod = glmnet(x_feat, y_feat, alpha=0.5, family="binomial")
# 
# plotmo::plot_glmnet(feat_sel_mod)
# #plot(fitelastic, xvar="dev")
# #plot(feat_sel_mod, main="Elastic Net")
# 
# feat_sel_mod_cv = cv.glmnet(x_feat, y_feat, type.measure = "auc", alpha=0.5, family="binomial", nfolds=3)
# plot(feat_sel_mod_cv)
# coef(feat_sel_mod_cv, s="lambda.min")
# 
# #copytraindf = traindf
# smp_size = floor(0.15*nrow(traindf))
# set.seed(5)
# trainidx = sample(seq_len(nrow(traindf)), size=smp_size)
# copytraindf=traindf[trainidx,]
# 
# xfeatpred = model.matrix(f_feat, copytraindf)
# copytraindf$pcycle_pred = predict(feat_sel_mod, s=fit5$lambda.min, xfeatpred, type="response")
# 
# # Residual plot
# ggplot(copytraindf@data, aes(x=pcycle_pred)) + geom_point(aes(y=pcycle-pcycle_pred)) + geom_smooth(aes(y=pcycle-pcycle_pred, col="red")) #+ geom_smooth(aes(y=pcycle_pred)) + geom_histogram(aes(pcycle), binwidth = 1)
# # skewed because probabilities have a lower bound of 0
# 
# bw=0.001
# ggplot(copytraindf@data) + geom_histogram(aes(pcycle_pred), binwidth=bw, fill="blue", alpha=0.3) + geom_histogram(aes(pcycle), binwidth=bw, fill="red", alpha=0.3) + xlim(c(-.01,0.08))
# 
# ggplot(copytraindf@data, aes(x=distance_fast)) + geom_smooth(aes(y=pcycle, col="red")) + geom_smooth(aes(y=pcycle_pred))
# ggplot(copytraindf@data, aes(x=gradient_fast)) + geom_smooth(aes(y=pcycle, col="red")) + geom_smooth(aes(y=pcycle_pred))
# 
# ggplot(copytraindf@data, aes(x=distance_fast)) + geom_point(aes(y=pcycle)) + geom_smooth(aes(y=pcycle, col="red")) + geom_smooth(aes(y=pcycle_pred)) #+ xlim(c(0,5)) + ylim(c(0,0.5))
# ggplot(copytraindf@data, aes(x=gradient_fast)) + geom_point(aes(y=pcycle)) + geom_smooth(aes(y=pcycle, col="red")) + geom_smooth(aes(y=pcycle_pred))
# 
# 
# 
# ##################################################################################
# CHECK PERFORMANCE ON TEST SET

testnow = FALSE

if(testnow==TRUE){
  
  testdf = testdf[complete.cases(testdf$CYCLE) & complete.cases(testdf$WALK) & complete.cases(testdf$CAR) & complete.cases(testdf$OTHER) & complete.cases(testdf$UNKNOWN) & complete.cases(testdf$distance_fast) & complete.cases(testdf$gradient_fast) & complete.cases(testdf$qdf), ]
  
  #if(!file.exists("private_data/meltdf_test.Rds")){
    meltdf_test = data.frame(matrix(NA, nrow=sum(testdf$TOTAL), ncol=8))
    names(meltdf_test) = c("CYCLE","WALK","CAR","OTHER","UNKNOWN","distance_fast","gradient_fast","qdf")
    jstart = 1
    for(i in 1:nrow(testdf)){
      blocklen = 0
      if(testdf@data[i, "CYCLE"] > 0){
        blocklen = testdf@data[i, "CYCLE"]
        jend = jstart + blocklen - 1
        meltdf_test[jstart:jend, "CYCLE"] = 1
        meltdf_test[jstart:jend, "gradient_fast"] = testdf@data[i, "gradient_fast"]
        meltdf_test[jstart:jend, "distance_fast"] = testdf@data[i, "distance_fast"]
        meltdf_test[jstart:jend, "qdf"] = testdf@data[i, "qdf"]
        jstart = jend + 1
      }
      if(testdf@data[i, "WALK"] > 0){
        blocklen = testdf@data[i, "WALK"]
        jend = jstart + blocklen - 1
        meltdf_test[jstart:jend, "WALK"] = 1
        meltdf_test[jstart:jend, "gradient_fast"] = testdf@data[i, "gradient_fast"]
        meltdf_test[jstart:jend, "distance_fast"] = testdf@data[i, "distance_fast"]
        meltdf_test[jstart:jend, "qdf"] = testdf@data[i, "qdf"]
        jstart = jend + 1
      }
      if(testdf@data[i, "CAR"] > 0){
        blocklen = testdf@data[i, "CAR"]
        jend = jstart + blocklen - 1
        meltdf_test[jstart:jend, "CAR"] = 1
        meltdf_test[jstart:jend, "gradient_fast"] = testdf@data[i, "gradient_fast"]
        meltdf_test[jstart:jend, "distance_fast"] = testdf@data[i, "distance_fast"]
        meltdf_test[jstart:jend, "qdf"] = testdf@data[i, "qdf"]
        jstart = jend + 1
      }
      if(testdf@data[i, "OTHER"] > 0){
        blocklen = testdf@data[i, "OTHER"]
        jend = jstart + blocklen - 1
        meltdf_test[jstart:jend, "OTHER"] = 1
        meltdf_test[jstart:jend, "gradient_fast"] = testdf@data[i, "gradient_fast"]
        meltdf_test[jstart:jend, "distance_fast"] = testdf@data[i, "distance_fast"]
        meltdf_test[jstart:jend, "qdf"] = testdf@data[i, "qdf"]
        jstart = jend + 1
      }
      if(testdf@data[i, "UNKNOWN"] > 0){
        blocklen = testdf@data[i, "UNKNOWN"]
        jend = jstart + blocklen - 1
        meltdf_test[jstart:jend, "UNKNOWN"] = 1
        meltdf_test[jstart:jend, "gradient_fast"] = testdf@data[i, "gradient_fast"]
        meltdf_test[jstart:jend, "distance_fast"] = testdf@data[i, "distance_fast"]
        meltdf_test[jstart:jend, "qdf"] = testdf@data[i, "qdf"]
        jstart = jend + 1
      }
    }
    meltdf_test[is.na(meltdf_test)] = 0
    #saveRDS(meltdf_test, "private_data/meltdf_test.Rds")
#  } else{
#    meltdf_test = readRDS("private_data/meltdf_test.Rds")
#  }
  
  isTRUE(sum(meltdf_test[!is.na(meltdf_test$CYCLE), "CYCLE"]) == sum(testdf$CYCLE))
  isTRUE(sum(meltdf_test[!is.na(meltdf_test$WALK), "WALK"]) == sum(testdf$WALK))
  isTRUE(sum(meltdf_test[!is.na(meltdf_test$CAR), "CAR"]) == sum(testdf$CAR))
  
  isTRUE((sum(meltdf_test[!is.na(meltdf_test$CAR), "CAR"]) + sum(meltdf_test[!is.na(meltdf_test$WALK), "WALK"]) + sum(meltdf_test[!is.na(meltdf_test$CYCLE), "CYCLE"]) + sum(meltdf_test[!is.na(meltdf_test$OTHER), "OTHER"]) + sum(meltdf_test[!is.na(meltdf_test$UNKNOWN), "UNKNOWN"])) == sum(testdf$TOTAL))
  isTRUE(nrow(meltdf_test) == sum(testdf$TOTAL))
  
  
  test_xpred = model.matrix(f, meltdf_test)
  test_pred = predict(fitelastic, s=fit5$lambda.min, test_xpred, type="response")[,1]
  
  #install.packages("ROCR")
  pred_and_target = ROCR::prediction(test_pred, meltdf_test$CYCLE) #get it into form ROCR expects
  auc = ROCR::performance(pred_and_target, "auc")@y.values[[1]][1]
  print(auc)
  roc = ROCR::performance(pred_and_target, "tpr", "fpr")
  print(ROCR::plot(roc, colorize=TRUE))
  print(ROCR::plot(roc, col=rainbow(10), print.cutoffs.at=seq(0.01,0.07, by=0.01)))
}

###################################################################################

# model = glm(CYCLE~distance_fast+sqrt(distance_fast)+I(distance_fast^2)+gradient_fast+distance_fast:gradient_fast+sqrt(distance_fast):gradient_fast, family=binomial(link='logit'), data=meltdf, na.action = na.omit)
# summary(model)
# 
# # H0 : no significant difference between model and observed data (if p> 0.05)
# ResourceSelection::hoslem.test(meltdf$CYCLE, fitted(model))
# 
# traindf$pcycle_pred = predict(model, traindf@data, type="response")  #predict(model, list(wt=xvals), type="response")


# TO PREDICT ON THE FULL DATA SET
#traindf = englandrf


# NOW WE PREDICT ON THE ROUTE NETWORK LINES FROM THE (NON-INTEGER) ROUTE NETWORK FLOWS WITH THE CORRECT 2011 LSOAS

if(!file.exists("private_data/england_flows_lsoa2011.Rds")){
  source("R/lsoa2001_to_2011_conversion.R")
  predictflow = readRDS("private_data/england_flows_lsoa2011.Rds") 
} else{
  predictflow = readRDS("private_data/england_flows_lsoa2011.Rds") 
}

if(!file.exists("private_data/rf_england_schools_lsoa2011.Rds")){
  source("R/lsoa2001_to_2011_conversion.R")
  fast_routes_england_predict = readRDS("private_data/rf_england_schools_lsoa2011.Rds")
  #quiet_routes_england_predict = readRDS("private_data/rq_england_schools_lsoa2011.Rds")
} else{
  fast_routes_england_predict = readRDS("private_data/rf_england_schools_lsoa2011.Rds")
  #quiet_routes_england_predict = readRDS("private_data/rq_england_schools_lsoa2011.Rds")
}

names(fast_routes_england_predict) = paste(names(fast_routes_england_predict),"fast", sep = "_")
#names(quiet_routes_england_predict) = paste(names(quiet_routes_england_predict),"quiet", sep = "_")


#predictdf = cbind(predictflow@data, fast_routes_england_predict, quiet_routes_england_predict@data)#@data
#predictdf = cbind(predictflow@data, fast_routes_england_predict)#@data
#predictdf = cbind(predictflow@data, fast_routes_england_predict)

predictdf = fast_routes_england_predict
predictdf@data = cbind(predictflow@data, fast_routes_england_predict@data)


predictdf$pcycle = predictdf$CYCLE/predictdf$TOTAL


predictdf$distance_fast = predictdf$length_fast / 1000
predictdf$gradient_fast = (predictdf$av_incline_fast * 100) - 0.97
#predictdf$distance_quiet = predictdf$length_quiet / 1000
#predictdf$gradient_quiet = (predictdf$av_incline_quiet * 100) - 0.97


nrow(predictdf)
sel = which(predictdf$distance_fast <= 15)
predictdf = predictdf[sel, ]
nrow(predictdf)


# Just smooth fits to the raw pcycle vs dist/grad data
#ggplot(predictdf@data, aes(distance_fast, pcycle)) + geom_point(alpha=0.3, size=1, shape=1, na.rm = T) + geom_smooth(na.rm = T) + xlim(0,15)
#ggplot(predictdf@data, aes(distance_quiet, pcycle)) + geom_point(alpha=0.3, size=1, shape=1, na.rm = T) + geom_smooth(na.rm = T) + xlim(0,15)

# QDF FACTOR
#predictdf$qdf = predictdf$distance_quiet/predictdf$distance_fast
#ggplot(predictdf@data, aes(qdf, pcycle)) + geom_point(alpha=0.3, size=1, shape=1, na.rm = T) #+ geom_smooth(na.rm = T)# + xlim(0,15)

# Keep the desire lines in these and the covariates and uptake predictions in predictdf matching up row by row
fast_routes_england_predict = fast_routes_england_predict[sel,]
#quiet_routes_england_predict = quiet_routes_england_predict[sel,]

isTRUE(nrow(predictdf) == nrow(fast_routes_england_predict))
#isTRUE(nrow(predictdf) == nrow(quiet_routes_england_predict))


#install.packages("Amelia")
#library(Amelia)
#Amelia::missmap(predictdf@data)
#dropcols = c("error_quiet","error_fast")
dropcols = c("error_fast")
predictdf = predictdf[, !(names(predictdf) %in% dropcols)]

predictdf$gradbins = cut(predictdf$gradient_fast, breaks = seq(-1, 8), labels=seq(0,8))
predictdf$distbins = cut(predictdf$distance_fast, breaks = seq(0, 14), labels=seq(1,14))


# Predict on the dataframe of flows, not the binary-coded one.
# Gradient and distance_fast variables are the same for both, so this is fine.
xpred = model.matrix(f, predictdf)
predictdf$pcycle_pred = predict(fitelastic, s=fit5$lambda.min, xpred, type="response")[,1] #this gives a 1D matrix, so take first col
#predictdf$pcycle_pred = predict(fitelastic, s=fit5$lambda.1se, xpred, type="response")
#predictdf$pcycle_pred = predict(fitelasticpred, xpred, type="response")


# Frank Harrell discourages residual plots for logistic regression(!!)
# https://stats.stackexchange.com/questions/45050/diagnostics-for-logistic-regression/70583#70583
#residuals = data.frame(Predicted=c(predictdf$pcycle_pred), Residual=c(predictdf$pcycle-predictdf$pcycle_pred))
#ggplot(residuals, aes(x=Predicted, y=Residual)) + geom_point() + geom_hline(yintercept = 0, col="red") + stat_smooth(method="lm", linetype=2, se=FALSE)



datmoddistscatterplot = ggplot(predictdf@data, aes(x=distance_fast)) +
  geom_point(aes(y=pcycle), shape=1, alpha=0.3) +
  geom_smooth(aes(y=pcycle, colour="Observed")) +
  geom_smooth(aes(y=pcycle_pred, colour="Modelled")) +
  scale_color_manual("", breaks=c("Observed","Modelled"), values=c("blue","red")) +
  xlab("Distance (km, fastest route)") +
  ylab("% children cycling to school")

print(datmoddistscatterplot)
ggsave(plot=datmoddistscatterplot, filename = "paper/figures/data_model_scatter_distance.png")


datmodgradscatterplot = ggplot(predictdf@data, aes(x=gradient_fast)) +
  geom_point(aes(y=pcycle), shape=1, alpha=0.3) +
  geom_smooth(aes(y=pcycle, colour="Observed")) +
  geom_smooth(aes(y=pcycle_pred, colour="Modelled")) +
  scale_color_manual("", breaks=c("Observed","Modelled"), values=c("blue","red")) +
  xlab("Route gradient (average %, fastest route)") +
  ylab("% children cycling to school")

print(datmodgradscatterplot)
ggsave(plot=datmodgradscatterplot, filename="paper/figures/data_model_scatter_gradient.png")




#ggplot(predictdf@data, aes(x=distance_fast)) + geom_smooth(aes(y=pcycle, col="red")) + geom_smooth(aes(y=pcycle_pred))
datmoddistplot = ggplot(predictdf@data[!is.na(predictdf$distbins),], aes(x=distbins)) +
  stat_summary(aes(y=100*pcycle, group=1, colour="Observed"), fun.y=mean, geom="line", size=1) +
  stat_summary(aes(y=100*pcycle_pred, group=1, colour="Modelled"), fun.y=mean, geom="line", size=1) +
  scale_color_manual("", breaks=c("Observed","Modelled"), values=c("blue","red")) +
  xlab("Distance (km, fastest route)") +
  ylab("% children cycling to school")

print(datmoddistplot)
ggsave(plot=datmoddistplot, filename = "paper/figures/data_model_distance.png")


# https://stackoverflow.com/questions/33150915/ggplot-plotting-the-bins-on-x-axis-and-the-average-on-y-axis

#ggplot(predictdf@data, aes(x=gradient_fast)) + geom_smooth(aes(y=pcycle, col="red")) + geom_smooth(aes(y=pcycle_pred))
datmodgradplot = ggplot(predictdf@data[!is.na(predictdf$gradbins),], aes(x=gradbins)) +
  stat_summary(aes(y=100*pcycle, group=1, colour="Observed"), fun.y=mean, geom="line", size=1) +
  stat_summary(aes(y=100*pcycle_pred, group=1, colour="Modelled"), fun.y=mean, geom="line", size=1) +
  scale_color_manual("", breaks=c("Observed","Modelled"), values=c("blue","red")) +
  xlab("Route gradient (average %, fastest route)") +
  ylab("% children cycling to school")

print(datmodgradplot)
ggsave(plot=datmodgradplot, filename="paper/figures/data_model_gradient.png")






##############################################################################################

# Government scenario
predictdf$govtarget_slc = predictdf$CYCLE + (predictdf$pcycle_pred*predictdf$TOTAL)
# If GovTarget larger than number of commuters in flow, set to total number of commuters in flow
sel = predictdf$govtarget_slc > predictdf$TOTAL
if(sum(sel) > 0){
  #predictdf$govtarget_slc[sel,] = predictdf$TOTAL[sel,]
  predictdf@data[sel, "govtarget_slc"] = predictdf@data[sel, "TOTAL"]
}
predictdf$govtarget_sic = predictdf$govtarget_slc - predictdf$CYCLE
range(predictdf$govtarget_sic)

#Check that the GovTarget corresponds to a rough doubling of cycling numbers
sum(predictdf$CYCLE)
sum(predictdf$govtarget_slc)



#############################################################################################
# THE GODUTCH SCENARIO
# THE UNREGULARISED MODEL AGREED WITH ANNA'S ANSWER, SO THE DUTCHNESS COEFFICIENT FROM THEREGULARISED VERSION SHOULD BE SENSIBLE,
#   BUT WE ENDED UP USING A VALUE ANNA PROVIDED AS SHE MADE THE CASE THAT AGREEMENT WITH DATA WAS BETTER WITH THIS VALUE

ntsdf = readr::read_csv("private_data/School_NTS_170121.csv", col_types = "iiidddd")
# converting to factors is not required as model.matrix() will then convert to dummy variables, but they are already in that form
#ntsdf$netherlands = as.factor(ntsdf$netherlands)
#str(ntsdf)

f_dutch_ordinary = as.formula(cycle~trip_distraw_km+trip_distraw_kmsqrt+trip_distraw_kmsq+netherlands)
glm(f_dutch_ordinary, family=binomial(link="logit"), data=ntsdf, weight=hillweight)

library(glmnet)
f_dutch_reg = as.formula(~trip_distraw_km+trip_distraw_kmsqrt+trip_distraw_kmsq+netherlands+0)
x_dutch = model.matrix(f_dutch_reg, ntsdf)
y_dutch = as.matrix(ntsdf$cycle, ncol=1)
weights = ntsdf$hillweight
dutchelastic_cv = cv.glmnet(x_dutch, y_dutch, weights, type.measure = "auc", alpha=0.5, family="binomial", nfolds=10)

dutchelastic = glmnet(x_dutch, y_dutch, family="binomial", weights, alpha=0.5)

plotmo::plot_glmnet(dutchelastic)
#plot(dutchelastic, xvar="dev")
plot(dutchelastic_cv, main="Elastic Net")

dutchcoef = coef(dutchelastic_cv, s="lambda.min")
print(dutchcoef)
dutchnesscoef = dutchcoef[dutchcoef@Dimnames[[1]]=="netherlands",]
print(dutchnesscoef)

# GoDutch scenario

# PREVIOUS VALUES USED...
#predictdf$pred_dutch = boot::inv.logit(boot::logit(predictdf$pcycle_pred) + 4.838 + (0.9073*predictdf$distance_fast)  + (-1.924*sqrt(predictdf$distance_fast)))
#predictdf$pred_dutch = boot::inv.logit(boot::logit(predictdf$pcycle_pred) + 3.682 + (0.3044*predictdf$distance_fast))
#predictdf$pred_dutch = boot::inv.logit(boot::logit(predictdf$pcycle_pred) + 5.122)

# THE REGULARISED COEFFICIENT WE GOT, OR ANNA'S VALUE. WE OPTED FOR THE LATTER
#predictdf$pred_dutch = boot::inv.logit(boot::logit(predictdf$pcycle_pred) + dutchnesscoef) # 4.49885
predictdf$pred_dutch = boot::inv.logit(boot::logit(predictdf$pcycle_pred) + 4.912)
predictdf$dutch_slc = predictdf$pred_dutch*predictdf$TOTAL
sel = predictdf$dutch_slc > predictdf$TOTAL
if(sum(sel) > 0){
  #predictdf$dutch_slc[sel,] = predictdf$TOTAL[sel,]
  predictdf@data[sel, "dutch_slc"] = predictdf@data[sel, "TOTAL"]
}
sel = predictdf$dutch_slc < predictdf$CYCLE
if(sum(sel) > 0){
  #predictdf$dutch_slc[sel] = predictdf$CYCLE[sel]
  predictdf@data[sel, "dutch_slc"] = predictdf@data[sel, "CYCLE"]
}
predictdf$dutch_sic = predictdf$dutch_slc - predictdf$CYCLE
range(predictdf$dutch_sic)

#######################################################################################



#ggplot(predictdf@data, aes(x=distance_fast)) + geom_smooth(aes(y=CYCLE, col="red")) + geom_smooth(aes(y=pcycle_pred*TOTAL, col="blue")) + geom_smooth(aes(y=govtarget_slc, col="green")) + geom_smooth(aes(y=dutch_slc, col="orange"))
#ggplot(predictdf@data, aes(x=distance_fast)) + geom_point(aes(y=CYCLE)) + geom_smooth(aes(y=CYCLE, col="red")) + geom_smooth(aes(y=pcycle_pred*TOTAL, col="blue")) + geom_smooth(aes(y=govtarget_slc, col="green")) + geom_smooth(aes(y=dutch_slc, col="orange"))

predictdf$pred_govtarget = predictdf$govtarget_slc/predictdf$TOTAL




ggplotdf = data.frame(Distance=predictdf$distance_fast, Gradient=predictdf$gradient_fast, Observed=predictdf$pcycle, Model=predictdf$pcycle_pred, GovTarget=predictdf$pred_govtarget, GoDutch=predictdf$pred_dutch)
meltggplotdistdf = ggplotdf[c("Distance","Observed","GovTarget","GoDutch","Model")]
meltggplotdistdf = reshape2::melt(meltggplotdistdf, id.vars="Distance")
meltggplotgraddf = ggplotdf[c("Gradient","Observed","GovTarget","GoDutch","Model")]
meltggplotgraddf = reshape2::melt(meltggplotgraddf, id.vars="Gradient")

#ggplot(meltggplotgraddf, aes(x=Gradient, y=value, color=variable)) + geom_smooth()





meltggplotdistdf$distbins = cut(meltggplotdistdf$Distance, breaks = seq(-1, 8), labels=seq(0,8))
meltggplotgraddf$gradbins = cut(meltggplotgraddf$Gradient, breaks = seq(0, 14), labels=seq(1,14))

#ggplot(meltggplotdistdf, aes(x=Distance, y=value, color=variable)) + geom_smooth()
uptakesdistplot = ggplot(meltggplotdistdf[!is.na(meltggplotdistdf$distbins),]) +
  stat_summary(aes(x=distbins, y=100*value, color=variable, group=variable), fun.y=mean, geom="line", size=1) +
  xlab("Distance (km, fastest route)") + 
  ylab("% children cycling to school") +
  labs(colour="")

print(uptakesdistplot)
ggsave(plot=uptakesdistplot, filename = "paper/figures/uptake_distance.png")


uptakesgradplot = ggplot(meltggplotgraddf[!is.na(meltggplotgraddf$gradbins),]) +
  stat_summary(aes(x=gradbins, y=100*value, color=variable, group=variable), fun.y=mean, geom="line", size=1) +
  xlab("Route gradient (average %, fastest route)") + 
  ylab("% children cycling to school") +
  labs(colour="")

print(uptakesgradplot)
ggsave(plot=uptakesgradplot, filename = "paper/figures/uptake_gradient.png")


ggplot(predictdf@data, aes(distance_fast, pcycle)) + geom_point(alpha=0.5, size=1, shape=21, na.rm = T)

ggplot(predictdf@data, aes(distance_fast, gradient_fast)) + geom_point(shape=1, alpha=0.3)

ggplot(predictdf@data, aes(distance_fast, pcycle_pred)) + geom_point(shape=1, alpha=0.3) + geom_smooth() #+ geom_point(aes(y=pcycle))
ggplot(predictdf@data, aes(gradient_fast, pcycle_pred)) + geom_point(shape=1, alpha=0.3) + geom_smooth() #+ geom_point(aes(y=pcycle))


# make sure that the desire lines (and uptake projections), the fastest route network, and the quiet route network flows, all match up
isTRUE(nrow(predictdf) == nrow(fast_routes_england_predict))
#isTRUE(nrow(predictdf) == nrow(quiet_routes_england_predict))




# MAKE SURE DESIRE LINES AND ROUTE NETWORK FLOWS STILL MATCH UP
plot(predictflow[5,])
plot(fast_routes_england_predict[5,], col="red", add = T)
#plot(quiet_routes_england_predict[5,], col="blue", add = T)

#Amelia::missmap(predictdf@data)
#saveRDS(predictdf, "private_data/l_national.Rds")

# PLOT THE FINAL DESIRE LINES AND ROUTE NETWORK FLOWS (WHICH HAVE THE UPTAKE SCENARIO PROJECTIONS), TO MAKE SURE EVERYTHING A-OK
#plot(las); plot(predictflow, add=T,col="blue")
#plot(las); plot(predictdf, add=T,col="red")

saveRDS(predictdf, "private_data/uptake_scenarios_final.Rds")




#saveRDS(predictdf[sample(seq_len(nrow(predictdf)), size=1000),], "private_data/full_england_w_uptake_scenarios_1000.Rds")




###################################################################################################################################









# Saving the spatial dataframes for the Shiny app
# lads = readRDS("../pct-bigdata/england_lad_2011.Rds") ## 2001 LSOAs
# lads = spTransform(lads, CRSobj = proj4string(las))
# lads_leeds = lads[lads$name == "Leeds",]
# #plot(lads_leeds)
# sel_leeds = rgeos::gContains(lads_leeds, predictdf, byid = T)
# leeds_uptakes = predictdf[c(sel_leeds),]
# leeds_rf = fast_routes_england[c(sel_leeds),]
# leeds_rq = quiet_routes_england[c(sel_leeds),]
# isTRUE(nrow(leeds_uptakes) == nrow(leeds_rf))
# isTRUE(nrow(leeds_uptakes) == nrow(leeds_rq))
# plot(leeds_uptakes[666,])
# plot(leeds_rf[666,], add = T)
# plot(leeds_rq[666,], add = T)
# saveRDS(leeds_uptakes, "private_data/leeds_uptakes.Rds")
# saveRDS(leeds_rf, "private_data/leeds_rf.Rds")
# saveRDS(leeds_rq, "private_data/leeds_rq.Rds")
# 






###################################################################################################################################







# notcols = c("LSOA","LAESTAB_SPR11","URN","SCHOOLNAME_SPR11")
# dataforapp = predictdf[!(names(predictdf) %in% notcols)]
# names(dataforapp)
# 
# readr::write_csv(meltdf, "Binary-coded_full_England.csv")
# readr::write_csv(dataforapp@data, "Uptake_scenarios_route_network.csv")
# 
# saveRDS(dataforapp, "Data_to_be_subset_for_Shiny_app.Rds")








# ######################################################################
# # LEEDS
# 
# cents_lsoa_leeds = cents_lsoa[grepl("Leeds", cents_lsoa$LSOA11NM),]
# las_leeds = las[cents_lsoa_leeds,]
# schools_leeds = schools[las_leeds,]
# 
# # TODO: fix this - not working! # ILAN: This works for me! Weird..
# sel_flow_leeds = gWithin(flow, las_leeds, byid = TRUE)
# bbox_leeds = stplanr::bb2poly(bb = bbox(las_leeds))
# proj4string(bbox_leeds) = proj4string(flow)
# flow_leeds = flow[bbox_leeds,]
# flow_leeds = flow_leeds[las_leeds,]
# 
# #plot(las_leeds); lines(flow_leeds, pch='.', col="blue", cex=5); points(schools_leeds, pch='.', col="red", cex=5)
# 
# if(!file.exists("private_data/rf_leeds_schools.Rds") | !file.exists("private_data/rq_leeds_schools.Rds")){
#   fast_routes_leeds = line2route(l = flow_leeds, route_fun = route_cyclestreet, plan = "fastest", base_url="http://pct.cyclestreets.net/api/")
#   quiet_routes_leeds = line2route(l = flow_leeds, route_fun = route_cyclestreet, plan = "quietest", base_url="http://pct.cyclestreets.net/api/")
#   saveRDS(fast_routes_leeds, "private_data/rf_leeds_schools.Rds")
#   saveRDS(quiet_routes_leeds, "private_data/rq_leeds_schools.Rds")
# } else{
#   fast_routes_leeds = readRDS("private_data/rf_leeds_schools.Rds")
#   quiet_routes_leeds = readRDS("private_data/rq_leeds_schools.Rds")
# }
# 
# plot(las_leeds); plot(flow_leeds, col="blue", add=T); plot(fast_routes_leeds, col="red", add = T); plot(quiet_routes_leeds, col="green", add = T)
# 
# summary(fast_routes_leeds$length)
# summary(quiet_routes_leeds$length)
# 
# mean(fast_routes_leeds$length)
# sd(fast_routes_leeds$length)
# IQR(fast_routes_leeds$length)
# 
# mean(quiet_routes_leeds$length)
# sd(quiet_routes_leeds$length)
# IQR(quiet_routes_leeds$length)
# 
# boxplot(quiet_routes_leeds$length, fast_routes_leeds$length)
# 
# violindf1 = data.frame(Type="Quiet", Length=quiet_routes_leeds$length)
# violindf2 = data.frame(Type="Fast", Length=fast_routes_leeds$length)
# violindf = rbind(violindf1, violindf2)
# violindf
# 
# # Mode = function(x) {
# #   ux = unique(x)
# #   ux[which.max(tabulate(match(x, ux)))]
# # }
# 
# hist(quiet_routes_leeds$length)
# hist(fast_routes_leeds$length)
# 
# library(ggplot2)
# ggplot(violindf, aes(Type, Length)) + geom_violin(trim=T, fill="blue", alpha=0.4) + geom_boxplot(width=0.1, outlier.size = 2, outlier.color = "red", outlier.shape = 21) + stat_summary(fun.y=mean, geom="point", shape=18, color="blue", size=3) #+ ylim(0, 20000)#+ stat_summary(fun.data = mean_sdl, geom="pointrange", color="red", fun.args=list(mult=1))#+ stat_summary(fun.y=mean, geom="point")  + stat_summary(fun.y=median, geom="point", shape=23, color="red")#+ geom_hline(aes(yintercept=Mode(quiet_routes_leeds$length)), col="red")
# 
# # To plot the distance decay curves we need the route length from e.g. fast_routes_leeds
# #   but we also need the flow numbers from flow_leeds.
# # These two dataframes have the same nrow() and CycleStreets should have processed them 
# #   in order, so just do a cbind() to do this join
# 
# decaydf = cbind(flow_leeds, fast_routes_leeds)#@data
# decaydf$pcycle = decaydf$CYCLE/decaydf$TOTAL
# 
# ggplot(decaydf@data, aes(length, pcycle)) + geom_point(alpha=0.5, size=1, shape=21, na.rm = T) + geom_smooth(na.rm = T) + xlim(0,15000)
# 
# 
# ######################################################################################
# 
# 
# distance = decaydf$length / 1000
# gradient = (decaydf$av_incline * 100) - 0.97
# 
# logit_pcycle = -3.959 + (-0.5963 * distance) + (1.866 * sqrt(distance) ) + (0.00805 * distance^2) + (-0.271*gradient) 
#                 +(0.009394*distance*gradient) + (-0.05135*sqrt(distance)*gradient)
# decaydf$govtarget = boot::inv.logit(logit_pcycle) + decaydf$pcycle
# 
# decaydf$govtargetflow = floor(decaydf$govtarget * decaydf$TOTAL)
# 
# 
# # This looks like way more than a doubling of cycling to me! Something wrong here?
# #decaydf[decaydf$govtargetflow > decaydf$CYCLE, c("govtargetflow", "CYCLE")]@data
# sum(decaydf$CYCLE)
# sum(decaydf$govtargetflow)
# 
# sum(decaydf$govtargetflow)/sum(decaydf$TOTAL)
# 
# 
# 
# 
# 
# logitdf = cbind(decaydf@data, distance, gradient)  #decaydf[, c("distance", "gradient")]
# #logitdf = logitdf[, c("pcycle","distance","gradient")]
# 
# smp_size = floor(0.6*nrow(logitdf))
# set.seed(5)
# trainidx = sample(seq_len(nrow(logitdf)), size=smp_size)
# 
# predictdf = logitdf[trainidx,]
# testdf = logitdf[-trainidx,]
# 
# 
# predictdf = as.data.frame(predictdf)
# names(predictdf) = names(logitdf)
# 
# 
# 
# #modes = c("CYCLE","WALK","CAR","OTHER","UNKNOWN")
# #meltdf_minimal = predictdf[c(modes, "distance", "gradient")]
# 
# # head(meltdf)
# # indeces = rep(1:nrow(predictdf), predictdf[i])
# # indeces[1:100]
# # predictdf$TOTAL[1]
# # meltdf = meltdf[indeces,]
# 
# predictdf = predictdf[predictdf$TOTAL >= 10, ]
# predictdf = predictdf[predictdf$distance <= 30, ]
# 
# meltdf = data.frame(matrix(NA, nrow=sum(predictdf$TOTAL), ncol=7))
# names(meltdf) = c("CYCLE","WALK","CAR","OTHER","UNKNOWN","distance","gradient")
# jstart = 1
# for(i in 1:nrow(predictdf)){
#   #indeces_melt = which(indeces == i)
#   #cycle_index = indeces_melt[1:predictdf$CYCLE[i]]
#   #meltdf$CYCLE[cycle_index] = 1
#   #meltdf$CYCLE
#   #print(jstart)
#   blocklen = 0
#   if(predictdf[i, "CYCLE"] > 0){
#     blocklen = predictdf[i, "CYCLE"]
#     jend = jstart + blocklen - 1
#     meltdf[jstart:jend, "CYCLE"] = 1
#     meltdf[jstart:jend, "gradient"] = predictdf[i, "gradient"]
#     meltdf[jstart:jend, "distance"] = predictdf[i, "distance"]
#     jstart = jend + 1
#   }
#   if(predictdf[i, "WALK"] > 0){
#     # if(blocklen == 0)
#     #   blocklen = predictdf[i, "WALK"]
#     # else
#     #   blocklen = blocklen + predictdf[i, "WALK"]
#     blocklen = predictdf[i, "WALK"]
#     jend = jstart + blocklen - 1
#     meltdf[jstart:jend, "WALK"] = 1
#     meltdf[jstart:jend, "gradient"] = predictdf[i, "gradient"]
#     meltdf[jstart:jend, "distance"] = predictdf[i, "distance"]
#     jstart = jend + 1
#   }
#   if(predictdf[i, "CAR"] > 0){
#     blocklen = predictdf[i, "CAR"]
#     jend = jstart + blocklen - 1
#     meltdf[jstart:jend, "CAR"] = 1
#     meltdf[jstart:jend, "gradient"] = predictdf[i, "gradient"]
#     meltdf[jstart:jend, "distance"] = predictdf[i, "distance"]
#     jstart = jend + 1
#   }
#   if(predictdf[i, "OTHER"] > 0){
#     blocklen = predictdf[i, "OTHER"]
#     jend = jstart + blocklen - 1
#     meltdf[jstart:jend, "OTHER"] = 1
#     meltdf[jstart:jend, "gradient"] = predictdf[i, "gradient"]
#     meltdf[jstart:jend, "distance"] = predictdf[i, "distance"]
#     jstart = jend + 1
#   }
#   if(predictdf[i, "UNKNOWN"] > 0){
#     blocklen = predictdf[i, "UNKNOWN"]
#     jend = jstart + blocklen - 1
#     meltdf[jstart:jend, "UNKNOWN"] = 1
#     meltdf[jstart:jend, "gradient"] = predictdf[i, "gradient"]
#     meltdf[jstart:jend, "distance"] = predictdf[i, "distance"]
#     jstart = jend + 1
#   }
#   #print(jend)
# }
# 
# meltdf[is.na(meltdf)] = 0
# 
# isTRUE(sum(meltdf[!is.na(meltdf$CYCLE), "CYCLE"]) == sum(predictdf$CYCLE))
# isTRUE(sum(meltdf[!is.na(meltdf$WALK), "WALK"]) == sum(predictdf$WALK))
# isTRUE(sum(meltdf[!is.na(meltdf$CAR), "CAR"]) == sum(predictdf$CAR))
# 
# isTRUE((sum(meltdf[!is.na(meltdf$CAR), "CAR"]) + sum(meltdf[!is.na(meltdf$WALK), "WALK"]) + sum(meltdf[!is.na(meltdf$CYCLE), "CYCLE"]) + sum(meltdf[!is.na(meltdf$OTHER), "OTHER"]) + sum(meltdf[!is.na(meltdf$UNKNOWN), "UNKNOWN"])) == sum(predictdf$TOTAL))
# isTRUE(nrow(meltdf) == sum(predictdf$TOTAL))
# 
# 
# model = glm(CYCLE~distance+sqrt(distance)+I(distance^2)+gradient+distance:gradient+sqrt(distance):gradient, family=binomial(link='logit'), data=meltdf, na.action = na.omit)
# #model = glm(CYCLE~distance+gradient+distance:gradient, family=binomial(link='logit'), data=meltdf, na.action = na.omit)
# model
# summary(model)
# 
# # H0 : no significant difference between model and observed data (if p> 0.05)
# ResourceSelection::hoslem.test(meltdf$CYCLE, fitted(model))
# 
# #range(predictdf$distance)
# #xvals = predictdf$distance
# #pcycle_pred = boot::inv.logit(predict(model, predictdf, type="response"))  #predict(model, list(wt=xvals), type="response")
# pcycle_pred = predict(model, predictdf, type="response")  #predict(model, list(wt=xvals), type="response")
# 
# plotdf = cbind(predictdf, pcycle_pred)
# 
# ggplot(plotdf, aes(distance, pcycle)) + geom_point(alpha=0.5, size=1, shape=21, na.rm = T)
# 
# ggplot(plotdf, aes(x=distance)) + geom_smooth(aes(y=pcycle_pred), col="blue")
# ggplot(plotdf, aes(x=distance)) + geom_smooth(aes(y=pcycle), col="red")
# 
# 
# ggplot(plotdf, aes(x=distance)) + geom_smooth(aes(y=pcycle_pred), col="blue") + geom_smooth(aes(y=pcycle), col="red")
# 
# 
# #ggplot(plotdf, aes(distance, pcycle)) + geom_point(alpha=0.5, size=1, shape=21, na.rm = T) + geom_line(pcycle_pred)
# 
# ggplot(plotdf, aes(distance, gradient)) + geom_point()
# 
# ggplot(plotdf, aes(distance, pcycle_pred)) + geom_line() + geom_smooth() #+ geom_point(aes(y=pcycle))
# ggplot(plotdf, aes(gradient, pcycle_pred)) + geom_line() + geom_smooth() #+ geom_point(aes(y=pcycle))

#plot(logitdf$distance, logitdf$pcycle)
#lines(xvals, yvals)

#testdf = plotdf[order(plotdf$distance, plotdf$gradient),]
#persp(testdf$distance, testdf$gradient, testdf$pcycle)


#logit_pcycle_dutch = logit_pcycle + 2.499 -0.07384 * distance
#decaydf$godutch = boot::inv.logit(logit_pcycle_dutch)
#decaydf$`Go Dutch` = decaydf$godutch * decaydf$TOTAL
#lr_nograd = lr




# data(flowlines)
# l = flowlines[2:5,]
# fast_routes = line2route(l = l, route_fun = route_cyclestreet, plan = "fastest")
# quiet_routes = line2route(l = l, route_fun = route_cyclestreet, plan = "quietest")
# plot(l); plot(fast_routes, add = T); plot(quiet_routes, add = T)

# TODO: fix - giving this error: > flow_cam = flow[las_cam,]
# Error in RGEOSBinPredFunc(spgeom1, spgeom2, byid, func) : 
#   IllegalArgumentException: point array must contain 0 or >1 elements
#cents_lsoa_cam = cents_lsoa[grepl("Cambridge", cents_lsoa$LSOA11NM),]  #This is all of Cambridgeshire
# cents_lsoa_cam = cents_lsoa[grepl("\\<Cambridge\\>", cents_lsoa$LSOA11NM),] # This is exclusively Cambridge
# las_cam = las[cents_lsoa_cam,]
# schools_cam = schools[las_cam,]
# 
# flow_cam = flow[las_cam,]
# plot(las_cam); lines(flow_cam, pch='.', col="blue", cex=5); points(schools_cam, pch='.', col="red", cex=5)
# 
# 
# sld11[grepl("Cambridge",sld11$`LA (name)`), c("SchoolName","Phase", "PhaseOfEducation (name)")]
# sld11[grepl("Cambridge",sld11$`LA (name)`) & sld11$Phase=="Secondary", "SchoolName"]
# sld11[grepl("Cambridge",sld11$`LA (name)`) & sld11$`PhaseOfEducation (name)`=="Secondary", "SchoolName"]


#starttime <- proc.time()
#flow = od2line(flow = s_cut, zones = cents_lsoa, destinations = schools)
#proc.time() - starttime

#plot(las); lines(flow, col = 'blue')  

#points(cents_lsoa)
#points(schools, col = 'red', pch = 4)
