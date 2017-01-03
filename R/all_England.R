# Aim: subset the data to a region

#setwd("/home/geoif/pct/pctSchoolsUK")
# Read in and pre-process the data
if(!exists("sld11"))
  source("R/analysis-sld.R")

# Read in UK Local Authorities, subset Leeds only
#las = readRDS("../pct-bigdata/las.Rds") ## 2001 LSOAs
if(!file.exists("private_data/las_2011.Rds")){
  # Simplified LSOA boundaries
  las = shapefile("private_data/Lower_Layer_Super_Output_Areas_December_2011_Full_Clipped__Boundaries_in_England_and_Wales_SIMPLIFIED/Lower_Layer_Super_Output_Areas_December_2011_Full_Clipped__Boundaries_in_England_and_Wales.shp")
  # Full LSOA boundaries
  #las = shapefile("Lower_Layer_Super_Output_Areas_December_2011_Full_Clipped__Boundaries_in_England_and_Wales/Lower_Layer_Super_Output_Areas_December_2011_Full_Clipped__Boundaries_in_England_and_Wales.shp")
  las = spTransform(las, CRS("+init=epsg:4326"))
  saveRDS(las, "private_data/las_2011.Rds")
} else{
  las = readRDS("private_data/las_2011.Rds")
}

#sld_test = sld_sp
#sld_test@coords = sld_sp[(sld_sp@coords[,1] != 654558) & (sld_sp@coords[,2] != 653613),]

#leeds_la = las[las$NAME == "Leeds",]
#plot(leeds_la)
#plot(las)
# Match CRS of spatial data to that of Local Authorities data
#proj4string(sld_sp) = CRS("+init=epsg:27700")
#proj4string(sld_sp) = proj4string(las)
#sld_sp = spTransform(sld_sp, CRSobj = proj4string(las))
# Subset schools data to Leeds
#schools = sld_sp[leeds_la,]
#points(schools)

#schools = spTransform(schools, CRSobj = )
if(!file.exists("private_data/sld_england.Rds")){
  schools = spTransform(sld_sp, CRSobj = proj4string(las))
  saveRDS(schools, "private_data/sld_england.Rds")
} else{
  schools = readRDS("private_data/sld_england.Rds")
}
# subset all ods that go to these schools

# Get the centroids for UK LSOAs
#cents_lsoa = readRDS("../pct-bigdata/cents_lsoa.Rds")
if(!file.exists("private_data/cents_lsoa_2011.Rds")){
  cents_lsoa = shapefile("private_data/lower_layer_super_output_areas_(e+w)_2011_population_weighted_centroids_v2/LSOA_2011_EW_PWC.shp")
  #cents_lsoa = spTransform(cents_lsoa, CRS("+init=epsg:4326"))
  cents_lsoa = spTransform(cents_lsoa, CRSobj = proj4string(las))
  #proj4string(cents_lsoa) = proj4string(las)
  saveRDS(cents_lsoa, "private_data/cents_lsoa_2011.Rds")
} else{
  cents_lsoa = readRDS("private_data/cents_lsoa_2011.Rds")
}


# Match CRS of LSOAs to be consistent with las and schools

#plot(cents_lsoa)
#bbox(cents_lsoa)


#Subset centroids of LSOAs to Leeds only
#cents_lsoa = cents_lsoa[leeds_la,]

# subset the flow (Origin-Destination) data
# select only LSOA centroids in the Leeds-only centroid data
#sel_od = s11$LLSOA_SPR11 %in% cents_lsoa$LSOA11CD
#s = s11[sel_od,]

nrow(s11)
s = s11[!is.na(s11$LLSOA_SPR11),] # drop flows with no LSOA data
nrow(s)
s = s[s$URN_SPR11 %in% schools$URN,] # drop flows with no school data
nrow(s)

# we want to plot LSOA centroid --> school desire lines
# the stplanr::od2line() function needs matching column titles to join
# between the zones (origins), flows (origin-destination), and destination columns
# If no matching column names are found origin is matched to first column of flows
# and destination is matched to second column of flows.
# Rearrage our dataframes to match this
# origins (LSOAs) matched to flows on LSOA, flows matched to destinations (schools) on URN
names(cents_lsoa)
names(s)
names(schools)
names(cents_lsoa)[names(cents_lsoa)=="LSOA11CD"] <- "LSOA"
names(s)[names(s)=="LLSOA_SPR11"] <- "LSOA"
names(s)[names(s)=="URN_SPR11"] <- "URN"
#names(schools)[names(schools)=="LEA11_URN"] <- "URN"

#names(cents_lsoa)[1] = names(s)[1]
#names(schools)[2] = names(s)[3]

# Some LSOA centroids are wrong and are in the ocean. Remove the ones not within LSOAs
cents_lsoa = cents_lsoa[las,]
nrow(cents_lsoa)
schools = schools[las,]
nrow(schools[las,])

summary(s$LSOA %in% cents_lsoa$LSOA)
summary(s$URN %in% schools$URN)

s = s[s$LSOA %in% cents_lsoa$LSOA,]
s = s[s$URN %in% schools$URN,]

#nrow(cents_lsoa)
#nrow(schools)
#cents_lsoa = cents_lsoa[cents_lsoa$LSOA %in% s$LSOA,]
#schools = schools[schools$URN %in% s$URN,]
#nrow(cents_lsoa)
#nrow(schools)

#plot(las)
#points(cents_lsoa)
#points(schools, col = 'red', pch = 4)

summary(s$TOTAL)
nrow(s)
s_cut = s[s$TOTAL >= 10,]
nrow(s_cut)

# plot(las); plot(schools, col="red", pch='.', cex=2, add=T)
# remove year as 1st column
AcademicYear = schools$AcademicYear
schools$AcademicYear = NULL
schools@data = cbind(schools@data, AcademicYear)
rm(AcademicYear)

# Test matching
zone_code = s_cut$LSOA
origin_code = cents_lsoa$LSOA
dest_code = s_cut$URN
zone_code_d = schools$URN
summary(zone_code %in% origin_code)
summary(dest_code %in% zone_code_d)


if(!file.exists("private_data/england_flows.Rds")){
  starttime <- proc.time()
  flow = od2line(flow = s_cut, zones = cents_lsoa, destinations = schools,
                 zone_code = "LSOA", origin_code = "LSOA", dest_code = "URN",
                 zone_code_d = "URN")
  print(proc.time() - starttime)
  saveRDS(flow, "private_data/england_flows.Rds")
} else{
  flow = readRDS("private_data/england_flows.Rds")
}

unique(schools$`LA (name)`)

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
if(!file.exists("private_data/rf_england_schools.Rds") | !file.exists("private_data/rq_england_schools.Rds")){
  # , base_url="http://pct.cyclestreets.net/api/"
  fast_routes_england = line2route(l = flow, route_fun = route_cyclestreet, plan = "fastest", n_processes = 10, base_url="http://pct.cyclestreets.net/api/")
  saveRDS(fast_routes_england, "private_data/rf_england_schools.Rds")
  quiet_routes_england = line2route(l = flow, route_fun = route_cyclestreet, plan = "quietest", n_processes = 10, base_url="http://pct.cyclestreets.net/api/")
  saveRDS(quiet_routes_england, "private_data/rq_england_schools.Rds")
} else{
  fast_routes_england = readRDS("private_data/rf_england_schools.Rds")
  quiet_routes_england = readRDS("private_data/rq_england_schools.Rds")
}


#plot(las); plot(flow, col="blue", add=T); plot(fast_routes_england, col="red", add = T)

summary(fast_routes_england$length)
summary(quiet_routes_england$length)

mean(fast_routes_england$length, na.rm=TRUE)
sd(fast_routes_england$length, na.rm=TRUE)
IQR(fast_routes_england$length, na.rm=TRUE)

mean(quiet_routes_england$length, na.rm=TRUE)
sd(quiet_routes_england$length, na.rm=TRUE)
IQR(quiet_routes_england$length, na.rm=TRUE)

boxplot(quiet_routes_england$length, fast_routes_england$length)

violindf1 = data.frame(Type="Quiet", Length=quiet_routes_england$length)
violindf2 = data.frame(Type="Fast", Length=fast_routes_england$length)
violindf = rbind(violindf1, violindf2)
#violindf = data.frame(fast_routes_england)

# Mode = function(x) {
#   ux = unique(x)
#   ux[which.max(tabulate(match(x, ux)))]
# }

hist(quiet_routes_england$length)
hist(fast_routes_england$length)

library(ggplot2)
ggplot(violindf, aes(x=Type, y=Length)) + geom_violin(trim=T, fill="blue", alpha=0.4) + geom_boxplot(width=0.1, outlier.size = 2, outlier.color = "red", outlier.shape = 21) + stat_summary(fun.y=mean, geom="point", shape=18, color="blue", size=3) #+ ylim(0, 20000)#+ stat_summary(fun.data = mean_sdl, geom="pointrange", color="red", fun.args=list(mult=1))#+ stat_summary(fun.y=mean, geom="point")  + stat_summary(fun.y=median, geom="point", shape=23, color="red")#+ geom_hline(aes(yintercept=Mode(quiet_routes_england$length)), col="red")

names(fast_routes_england) = paste(names(fast_routes_england),"fast", sep = "_")
names(quiet_routes_england) = paste(names(quiet_routes_england),"quiet", sep = "_")

englandrf = cbind(flow, fast_routes_england, quiet_routes_england)#@data
englandrf$pcycle = englandrf$CYCLE/englandrf$TOTAL


#library(ggplot2)
ggplot(englandrf@data, aes(length_fast, pcycle)) + geom_point(alpha=0.5, size=1, shape=21, na.rm = T) + geom_smooth(na.rm = T) + xlim(0,15000)
ggplot(englandrf@data, aes(length_quiet, pcycle)) + geom_point(alpha=0.5, size=1, shape=21, na.rm = T) + geom_smooth(na.rm = T) + xlim(0,15000)

englandrf$distance_fast = englandrf$length_fast / 1000
englandrf$gradient_fast = (englandrf$av_incline_fast * 100) - 0.97
englandrf$distance_quiet = englandrf$length_quiet / 1000
englandrf$gradient_quiet = (englandrf$av_incline_quiet * 100) - 0.97

englandrf$qdf = englandrf$distance_quiet/englandrf$distance_fast

englandrf = englandrf[englandrf$distance_fast <= 15, ]

#install.packages("Amelia")
#library(Amelia)
#missmap(englandrf@data)

dropcols = c("error_quiet","error_fast")
englandrf = englandrf[, !(names(englandrf) %in% dropcols)]


#logitdf = cbind(englandrf@data, distance_fast, gradient_fast)  #decaydf[, c("distance_fast", "gradient_fast")]
#logitdf = logitdf[, c("pcycle","distance_fast","gradient_fast")]

smp_size = floor(0.6*nrow(englandrf))
set.seed(5)
trainidx = sample(seq_len(nrow(englandrf)), size=smp_size)

traindf = englandrf[trainidx,]
testdf = englandrf[-trainidx,]

traindf = traindf[complete.cases(traindf$CYCLE) & complete.cases(traindf$WALK) & complete.cases(traindf$CAR) & complete.cases(traindf$OTHER) & complete.cases(traindf$UNKNOWN) & complete.cases(traindf$distance_fast) & complete.cases(traindf$gradient_fast) & complete.cases(traindf$qdf), ]

#traindf = as.data.frame(traindf)
#names(traindf) = names(logitdf)

if(!file.exists("private_data/meltdf.Rds")){
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
  saveRDS(meltdf, "private_data/meltdf.Rds")
  } else{
    meltdf = readRDS("private_data/meltdf.Rds")
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
# 

pkgs = c("glmnet")
to_install = !pkgs %in% installed.packages()
if(sum(to_install) > 0){
  install.packages(pkgs[to_install])
}
lapply(X = pkgs, FUN = library, character.only = T)

library(glmnet)
f = as.formula(~distance_fast+sqrt(distance_fast)+I(distance_fast^2)+gradient_fast+distance_fast:gradient_fast+sqrt(distance_fast):gradient_fast+qdf+I(distance_fast^(1/3)))
x = model.matrix(f, meltdf)
# glmnet has standardize=T by default, so this is not required
y = as.matrix(meltdf$CYCLE, ncol=1)


#Cross-validation to select LASSO/Ridge/Elastic and do feature selection as well.
#library(doParallel)
#nodes <- detectCores()
#cl <- makeCluster(nodes)
#registerDoParallel(cl)
if(!file.exists("private_data/CV_models.Rdata")){
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
  save(list=cvmods, file="private_data/CV_models.Rdata")
} else{
  load(file="private_data/CV_models.Rdata")
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
plot(fitridge, xvar="dev")
plot(fit5, main="Elastic Net")



# To get the coefficients fit model with lambda value found by cross-validation
coef(fitelastic, s=fit5$lambda.min)
#coef(fitelastic, s=fit5$lambda.1se)




# BINNING
# bins = cut(traindf$distance_fast, seq(0.25, 15, by=0.5), include.lowest = TRUE)
# meanobs = tapply(traindf$pcycle, bins, mean)
# meanpred = tapply(traindf$pcycle_pred, bins, mean)
# 
# df <- quakes
# df$bins <- with(df, cut(depth, breaks = c(0,40, 120, 200, 280, 360, 440, 520, 600, 680)))
# df.plot <- ddply(df, .(bins), summarise, avg.mag = mean(mag))
# qplot(bins, avg.mag, data = df.plot)




###################################################################################

# model = glm(CYCLE~distance_fast+sqrt(distance_fast)+I(distance_fast^2)+gradient_fast+distance_fast:gradient_fast+sqrt(distance_fast):gradient_fast, family=binomial(link='logit'), data=meltdf, na.action = na.omit)
# summary(model)
# 
# # H0 : no significant difference between model and observed data (if p> 0.05)
# ResourceSelection::hoslem.test(meltdf$CYCLE, fitted(model))
# 
# traindf$pcycle_pred = predict(model, traindf@data, type="response")  #predict(model, list(wt=xvals), type="response")

# Predict on the dataframe of flows, not the binary-coded one.
# Gradient and distance_fast variables are the same for both, so this is fine.
xpred = model.matrix(f, traindf)
traindf$pcycle_pred = predict(fitelastic, s=fit5$lambda.min, xpred, type="response")
#traindf$pcycle_pred = predict(fitelastic, s=fit5$lambda.1se, xpred, type="response")
#traindf$pcycle_pred = predict(fitelasticpred, xpred, type="response")


# Frank Harrell discourages residual plots for logistic regression(!!)
# https://stats.stackexchange.com/questions/45050/diagnostics-for-logistic-regression/70583#70583
#residuals = data.frame(Predicted=c(traindf$pcycle_pred), Residual=c(traindf$pcycle-traindf$pcycle_pred))
#ggplot(residuals, aes(x=Predicted, y=Residual)) + geom_point() + geom_hline(yintercept = 0, col="red") + stat_smooth(method="lm", linetype=2, se=FALSE)


ggplot(traindf@data, aes(x=distance_fast)) + geom_smooth(aes(y=pcycle, col="red")) + geom_smooth(aes(y=pcycle_pred))
ggplot(traindf@data, aes(x=gradient_fast)) + geom_smooth(aes(y=pcycle, col="red")) + geom_smooth(aes(y=pcycle_pred))

ggplot(traindf@data, aes(x=distance_fast)) + geom_point(aes(y=pcycle)) + geom_smooth(aes(y=pcycle, col="red")) + geom_smooth(aes(y=pcycle_pred))
ggplot(traindf@data, aes(x=gradient_fast)) + geom_point(aes(y=pcycle)) + geom_smooth(aes(y=pcycle, col="red")) + geom_smooth(aes(y=pcycle_pred))



# Government scenario
traindf$govtarget_slc = traindf$CYCLE + (traindf$pcycle_pred*traindf$TOTAL)
# If GovTarget larger than number of commuters in flow, set to total number of commuters in flow
sel = traindf$govtarget_slc > traindf$TOTAL
if(sum(sel) > 0){
  traindf$govtarget_slc[sel,] = traindf$TOTAL[sel,]
}
traindf$govtarget_sic = traindf$govtarget_slc - traindf$CYCLE
range(traindf$govtarget_sic)

#Check that the GovTarget corresponds to a rough doubling of cycling numbers
sum(traindf$govtarget_slc)
sum(traindf$CYCLE)

# GoDutch scenario
#traindf$pred_dutch = boot::inv.logit(boot::logit(traindf$pcycle_pred) + 4.838 + (0.9073*traindf$distance_fast)  + (-1.924*sqrt(traindf$distance_fast)))
traindf$pred_dutch = boot::inv.logit(boot::logit(traindf$pcycle_pred) + 3.682 + (0.3044*traindf$distance_fast))
traindf$dutch_slc = traindf$pred_dutch*traindf$TOTAL
sel = traindf$dutch_slc > traindf$TOTAL
if(sum(sel) > 0){
  traindf$dutch_slc[sel,] = traindf$TOTAL[sel,]
}
sel = traindf$dutch_slc < traindf$CYCLE
if(sum(sel) > 0){
  traindf$dutch_slc[sel] = traindf$CYCLE[sel]
}
traindf$dutch_sic = traindf$dutch_slc - traindf$CYCLE
range(traindf$dutch_sic)





#ggplot(traindf@data, aes(x=distance_fast)) + geom_smooth(aes(y=CYCLE, col="red")) + geom_smooth(aes(y=pcycle_pred*TOTAL, col="blue")) + geom_smooth(aes(y=govtarget_slc, col="green")) + geom_smooth(aes(y=dutch_slc, col="orange"))
#ggplot(traindf@data, aes(x=distance_fast)) + geom_point(aes(y=CYCLE)) + geom_smooth(aes(y=CYCLE, col="red")) + geom_smooth(aes(y=pcycle_pred*TOTAL, col="blue")) + geom_smooth(aes(y=govtarget_slc, col="green")) + geom_smooth(aes(y=dutch_slc, col="orange"))

traindf$pred_govtarget = traindf$govtarget_slc/traindf$TOTAL

ggplotdf = data.frame(Distance=traindf$distance_fast, Gradient=traindf$gradient_fast, Observed=traindf$pcycle, Model=c(traindf$pcycle_pred), GovTarget=c(traindf$pred_govtarget), GoDutch=c(traindf$pred_dutch))
meltggplotdistdf = ggplotdf[c("Distance","Observed","GovTarget","GoDutch","Model")]
meltggplotdistdf = reshape2::melt(meltggplotdistdf, id.vars="Distance")
meltggplotgraddf = ggplotdf[c("Gradient","Observed","GovTarget","GoDutch","Model")]
meltggplotgraddf = reshape2::melt(meltggplotgraddf, id.vars="Gradient")

ggplot(meltggplotdistdf, aes(x=Distance, y=value, color=variable)) + geom_smooth()
ggplot(meltggplotgraddf, aes(x=Gradient, y=value, color=variable)) + geom_smooth()







ggplot(traindf@data, aes(distance_fast, pcycle)) + geom_point(alpha=0.5, size=1, shape=21, na.rm = T)

ggplot(traindf@data, aes(distance_fast, gradient_fast)) + geom_point()

ggplot(traindf@data, aes(distance_fast, pcycle_pred)) + geom_point() + geom_smooth() #+ geom_point(aes(y=pcycle))
ggplot(traindf@data, aes(gradient_fast, pcycle_pred)) + geom_point() + geom_smooth() #+ geom_point(aes(y=pcycle))











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
# traindf = logitdf[trainidx,]
# testdf = logitdf[-trainidx,]
# 
# 
# traindf = as.data.frame(traindf)
# names(traindf) = names(logitdf)
# 
# 
# 
# #modes = c("CYCLE","WALK","CAR","OTHER","UNKNOWN")
# #meltdf_minimal = traindf[c(modes, "distance", "gradient")]
# 
# # head(meltdf)
# # indeces = rep(1:nrow(traindf), traindf[i])
# # indeces[1:100]
# # traindf$TOTAL[1]
# # meltdf = meltdf[indeces,]
# 
# traindf = traindf[traindf$TOTAL >= 10, ]
# traindf = traindf[traindf$distance <= 30, ]
# 
# meltdf = data.frame(matrix(NA, nrow=sum(traindf$TOTAL), ncol=7))
# names(meltdf) = c("CYCLE","WALK","CAR","OTHER","UNKNOWN","distance","gradient")
# jstart = 1
# for(i in 1:nrow(traindf)){
#   #indeces_melt = which(indeces == i)
#   #cycle_index = indeces_melt[1:traindf$CYCLE[i]]
#   #meltdf$CYCLE[cycle_index] = 1
#   #meltdf$CYCLE
#   #print(jstart)
#   blocklen = 0
#   if(traindf[i, "CYCLE"] > 0){
#     blocklen = traindf[i, "CYCLE"]
#     jend = jstart + blocklen - 1
#     meltdf[jstart:jend, "CYCLE"] = 1
#     meltdf[jstart:jend, "gradient"] = traindf[i, "gradient"]
#     meltdf[jstart:jend, "distance"] = traindf[i, "distance"]
#     jstart = jend + 1
#   }
#   if(traindf[i, "WALK"] > 0){
#     # if(blocklen == 0)
#     #   blocklen = traindf[i, "WALK"]
#     # else
#     #   blocklen = blocklen + traindf[i, "WALK"]
#     blocklen = traindf[i, "WALK"]
#     jend = jstart + blocklen - 1
#     meltdf[jstart:jend, "WALK"] = 1
#     meltdf[jstart:jend, "gradient"] = traindf[i, "gradient"]
#     meltdf[jstart:jend, "distance"] = traindf[i, "distance"]
#     jstart = jend + 1
#   }
#   if(traindf[i, "CAR"] > 0){
#     blocklen = traindf[i, "CAR"]
#     jend = jstart + blocklen - 1
#     meltdf[jstart:jend, "CAR"] = 1
#     meltdf[jstart:jend, "gradient"] = traindf[i, "gradient"]
#     meltdf[jstart:jend, "distance"] = traindf[i, "distance"]
#     jstart = jend + 1
#   }
#   if(traindf[i, "OTHER"] > 0){
#     blocklen = traindf[i, "OTHER"]
#     jend = jstart + blocklen - 1
#     meltdf[jstart:jend, "OTHER"] = 1
#     meltdf[jstart:jend, "gradient"] = traindf[i, "gradient"]
#     meltdf[jstart:jend, "distance"] = traindf[i, "distance"]
#     jstart = jend + 1
#   }
#   if(traindf[i, "UNKNOWN"] > 0){
#     blocklen = traindf[i, "UNKNOWN"]
#     jend = jstart + blocklen - 1
#     meltdf[jstart:jend, "UNKNOWN"] = 1
#     meltdf[jstart:jend, "gradient"] = traindf[i, "gradient"]
#     meltdf[jstart:jend, "distance"] = traindf[i, "distance"]
#     jstart = jend + 1
#   }
#   #print(jend)
# }
# 
# meltdf[is.na(meltdf)] = 0
# 
# isTRUE(sum(meltdf[!is.na(meltdf$CYCLE), "CYCLE"]) == sum(traindf$CYCLE))
# isTRUE(sum(meltdf[!is.na(meltdf$WALK), "WALK"]) == sum(traindf$WALK))
# isTRUE(sum(meltdf[!is.na(meltdf$CAR), "CAR"]) == sum(traindf$CAR))
# 
# isTRUE((sum(meltdf[!is.na(meltdf$CAR), "CAR"]) + sum(meltdf[!is.na(meltdf$WALK), "WALK"]) + sum(meltdf[!is.na(meltdf$CYCLE), "CYCLE"]) + sum(meltdf[!is.na(meltdf$OTHER), "OTHER"]) + sum(meltdf[!is.na(meltdf$UNKNOWN), "UNKNOWN"])) == sum(traindf$TOTAL))
# isTRUE(nrow(meltdf) == sum(traindf$TOTAL))
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
# #range(traindf$distance)
# #xvals = traindf$distance
# #pcycle_pred = boot::inv.logit(predict(model, traindf, type="response"))  #predict(model, list(wt=xvals), type="response")
# pcycle_pred = predict(model, traindf, type="response")  #predict(model, list(wt=xvals), type="response")
# 
# plotdf = cbind(traindf, pcycle_pred)
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
