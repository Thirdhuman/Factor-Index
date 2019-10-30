#### Index of OZs #### 
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(zoo)
library(mice)
library(scales)
library(lattice)
library(psych)
library(GPArotation)
library(corrplot) #plotting correlation matrices
library(lavaan)  #for fitting structural equation models
library(semPlot)  #for automatically making diagrams 
library(TAM)

rm(list=ls());gc()

index_calcs=readxl::read_excel("~/Desktop/Welfare_Policy/Struggling_Regions/Index/test_index_df.xlsx")
# incarceration_calcs = readstata13::read.dta13("~/Desktop/Welfare_Policy/Struggling_Regions/Index/Additional Data/Index_Data/Incarceration/tract_outcomes_simple.dta")

afford.idx=readxl::read_excel("~/Desktop/Welfare_Policy/Struggling_Regions/Index/Additional Data/Index_Data/Development/Housing_Affordability_MedianFamily.xlsx")
afford.idx=as.data.frame(afford.idx)
names(afford.idx)[names(afford.idx) == 'Census Tract'] = 'FIPS'
names(afford.idx)[names(afford.idx) == 'Percent of income spent on housing by a median-income family hou'] = 'afford.idx'
glimpse(afford.idx)
afford.idx$afford.idx=as.numeric(afford.idx$afford.idx)
glimpse(afford.idx)
afford.idx=afford.idx[c("FIPS", 'afford.idx')]

afford.idx$FIPS= ifelse(
		 afford.idx$FIPS == "51515050100", "51019050100",
	ifelse(afford.idx$FIPS ==  "04019002701", "04019002704",
	ifelse(afford.idx$FIPS ==  "04019002903", "04019002906",
	ifelse(afford.idx$FIPS ==  "04019410501", "04019004118",
	ifelse(afford.idx$FIPS ==  "04019410502", "04019004121",
	ifelse(afford.idx$FIPS ==  "04019410503", "04019004125",
	ifelse(afford.idx$FIPS ==  "04019470400", "04019005200",
	ifelse(afford.idx$FIPS ==  "04019470500", "04019005300",
	ifelse(afford.idx$FIPS ==  "06037930401", "06037137000",
	ifelse(afford.idx$FIPS ==  "02270000100", "02158000100",
	ifelse(afford.idx$FIPS ==  "46113940500", "46102940500",
	ifelse(afford.idx$FIPS ==  "46113940800", "46102940800",
	ifelse(afford.idx$FIPS ==  "46113940900", "46102940900",
	ifelse(afford.idx$FIPS ==  "36053940101", "36053030101",
	ifelse(afford.idx$FIPS ==  "36053940102", "36053030102",
	ifelse(afford.idx$FIPS ==  "36053940103", "36053030103",
	ifelse(afford.idx$FIPS ==  "36053940200", "36053030200",
	ifelse(afford.idx$FIPS ==  "36053940300", "36053030300",
	ifelse(afford.idx$FIPS ==  "36053940401", "36053030401",
	ifelse(afford.idx$FIPS ==  "36053940403", "36053030403",
	ifelse(afford.idx$FIPS ==  "36053940600", "36053030600",
	ifelse(afford.idx$FIPS ==  "36053940700", "36053030402",
	ifelse(afford.idx$FIPS ==  "36065940000", "36065024800",
	ifelse(afford.idx$FIPS ==  "36065940100", "36065024700",
	ifelse(afford.idx$FIPS ==  "36065940200", "36065024900", afford.idx$FIPS)))))))))))))))))))))))))


mig.idx=read.csv("~/Desktop/Welfare_Policy/Struggling_Regions/Index/Additional Data/Index_Data/Migration/Mobility_2017.csv")
mig.idx=as.data.frame(mig.idx)
mig.idx$Geo_FIPS=str_pad(mig.idx$Geo_FIPS, 11, pad = "0")
mig.idx=mig.idx[c("Geo_FIPS", 'PCT_ACS17_5yr_B07204009')]
glimpse(mig.idx)
names(mig.idx)[names(mig.idx) == 'Geo_FIPS'] = 'FIPS'
names(mig.idx)[names(mig.idx) == 'PCT_ACS17_5yr_B07204009'] = 'mig.idx'

mig.idx$FIPS= ifelse(
		 mig.idx$FIPS == "51515050100", "51019050100",
	ifelse(mig.idx$FIPS ==  "04019002701", "04019002704",
	ifelse(mig.idx$FIPS ==  "04019002903", "04019002906",
	ifelse(mig.idx$FIPS ==  "04019410501", "04019004118",
	ifelse(mig.idx$FIPS ==  "04019410502", "04019004121",
	ifelse(mig.idx$FIPS ==  "04019410503", "04019004125",
	ifelse(mig.idx$FIPS ==  "04019470400", "04019005200",
	ifelse(mig.idx$FIPS ==  "04019470500", "04019005300",
	ifelse(mig.idx$FIPS ==  "06037930401", "06037137000",
	ifelse(mig.idx$FIPS ==  "02270000100", "02158000100",
	ifelse(mig.idx$FIPS ==  "46113940500", "46102940500",
	ifelse(mig.idx$FIPS ==  "46113940800", "46102940800",
	ifelse(mig.idx$FIPS ==  "46113940900", "46102940900",
	ifelse(mig.idx$FIPS ==  "36053940101", "36053030101",
	ifelse(mig.idx$FIPS ==  "36053940102", "36053030102",
	ifelse(mig.idx$FIPS ==  "36053940103", "36053030103",
	ifelse(mig.idx$FIPS ==  "36053940200", "36053030200",
	ifelse(mig.idx$FIPS ==  "36053940300", "36053030300",
	ifelse(mig.idx$FIPS ==  "36053940401", "36053030401",
	ifelse(mig.idx$FIPS ==  "36053940403", "36053030403",
	ifelse(mig.idx$FIPS ==  "36053940600", "36053030600",
	ifelse(mig.idx$FIPS ==  "36053940700", "36053030402",
	ifelse(mig.idx$FIPS ==  "36065940000", "36065024800",
	ifelse(mig.idx$FIPS ==  "36065940100", "36065024700",
	ifelse(mig.idx$FIPS ==  "36065940200", "36065024900", mig.idx$FIPS)))))))))))))))))))))))))

index_calcs$FIPS= ifelse(
		 index_calcs$FIPS == "51515050100", "51019050100",
	ifelse(index_calcs$FIPS ==  "04019002701", "04019002704",
	ifelse(index_calcs$FIPS ==  "04019002903", "04019002906",
	ifelse(index_calcs$FIPS ==  "04019410501", "04019004118",
	ifelse(index_calcs$FIPS ==  "04019410502", "04019004121",
	ifelse(index_calcs$FIPS ==  "04019410503", "04019004125",
	ifelse(index_calcs$FIPS ==  "04019470400", "04019005200",
	ifelse(index_calcs$FIPS ==  "04019470500", "04019005300",
	ifelse(index_calcs$FIPS ==  "06037930401", "06037137000",
	ifelse(index_calcs$FIPS ==  "02270000100", "02158000100",
	ifelse(index_calcs$FIPS ==  "46113940500", "46102940500",
	ifelse(index_calcs$FIPS ==  "46113940800", "46102940800",
	ifelse(index_calcs$FIPS ==  "46113940900", "46102940900",
	ifelse(index_calcs$FIPS ==  "36053940101", "36053030101",
	ifelse(index_calcs$FIPS ==  "36053940102", "36053030102",
	ifelse(index_calcs$FIPS ==  "36053940103", "36053030103",
	ifelse(index_calcs$FIPS ==  "36053940200", "36053030200",
	ifelse(index_calcs$FIPS ==  "36053940300", "36053030300",
	ifelse(index_calcs$FIPS ==  "36053940401", "36053030401",
	ifelse(index_calcs$FIPS ==  "36053940403", "36053030403",
	ifelse(index_calcs$FIPS ==  "36053940600", "36053030600",
	ifelse(index_calcs$FIPS ==  "36053940700", "36053030402",
	ifelse(index_calcs$FIPS ==  "36065940000", "36065024800",
	ifelse(index_calcs$FIPS ==  "36065940100", "36065024700",
	ifelse(index_calcs$FIPS ==  "36065940200", "36065024900", index_calcs$FIPS)))))))))))))))))))))))))



Index_Dat=list(mig.idx,afford.idx,index_calcs) %>% reduce(full_join, by = "FIPS")

Index_Dat$labor_index=scale(Index_Dat$labor_index)
Index_Dat$mig.idx=scale(Index_Dat$mig.idx)
Index_Dat$afford.idx=scale(Index_Dat$afford.idx)

Index_Dat$afford.idx=as.numeric(Index_Dat$afford.idx)
Index_Dat$mig.idx=as.numeric(Index_Dat$mig.idx)
Index_Dat$labor_index=as.numeric(Index_Dat$labor_index)
Index_save=Index_Dat
##################

Index_Dat=Index_save
plot(density(Index_Dat$mig.idx, na.rm = T))

capOutlier <- function(x){
   qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
   caps <- quantile(x, probs=c(.05, .95), na.rm = T)
   H <- 1.5 * IQR(x, na.rm = T)
   x[x < (qnt[1] - H)] <- caps[1]
   x[x > (qnt[2] + H)] <- caps[2]
   return(x)
}

Index_Dat$labor_idx=(Index_Dat$labor_index)
Index_Dat$pov_idx=(Index_Dat$PovIntensity)
Index_Dat$edu_idx=(Index_Dat$AverageSchooling)
Index_Dat$afford_idx=(Index_Dat$afford.idx)
Index_Dat$mig_idx=(Index_Dat$mig.idx)

Index_Dat=within(Index_Dat,rm(PovIntensity,AverageSchooling,labor_index,mig.idx,afford.idx))
Index_Dat_uncapped=as.data.frame(Index_Dat)
glimpse(Index_Dat)


Index_Dat_uncapped$labor_idx=scales::rescale(Index_Dat_uncapped$labor_idx, to = c(0, 10))
Index_Dat_uncapped$pov_idx=scales::rescale(Index_Dat_uncapped$pov_idx, to = c(0, 10))
Index_Dat_uncapped$edu_idx=scales::rescale(Index_Dat_uncapped$edu_idx, to = c(0, 10))
Index_Dat_uncapped$afford_idx=scales::rescale(Index_Dat_uncapped$afford_idx, to = c(0, 10))
Index_Dat_uncapped$mig_idx=scales::rescale(Index_Dat_uncapped$mig_idx, to = c(0, 10))

Index_Dat_uncapped$afford_idx=as.numeric(reverse.code(-1,Index_Dat_uncapped$afford_idx))


Index_Dat$labor_idx=capOutlier(Index_Dat$labor_idx)
Index_Dat$pov_idx=capOutlier(Index_Dat$pov_idx)
Index_Dat$edu_idx=capOutlier(Index_Dat$edu_idx)
Index_Dat$afford_idx=capOutlier(Index_Dat$afford_idx)
Index_Dat$mig_idx=capOutlier(Index_Dat$mig_idx)
# 
# Index_Dat$labor_idx=(scales::rescale(Index_Dat$labor_index, to = c(-1, 1)))
# Index_Dat$pov_idx=(scales::rescale(Index_Dat$PovIntensity, to = c(-1, 1)))
# Index_Dat$edu_idx=(scales::rescale(Index_Dat$AverageSchooling, to = c(-1, 1)))
# Index_Dat$afford_idx=(scales::rescale(Index_Dat$afford.idx, to = c(-1, 1)))
# Index_Dat$mig_idx=(scales::rescale(Index_Dat$mig.idx, to = c(-1, 1)))
# 
# plot(density(Index_Dat$mig_idx, na.rm = T))
# 
# Index_Dat$labor_idx=atanh(Index_Dat$labor_idx)
# Index_Dat$pov_idx=atanh(Index_Dat$pov_idx)
# Index_Dat$edu_idx=atanh(Index_Dat$edu_idx)
# Index_Dat$afford_idx=atanh(Index_Dat$afford_idx)
# Index_Dat$mig_idx=atanh(Index_Dat$mig_idx)

plot(density(Index_Dat$mig_idx, na.rm = T))

# Index_Dat$labor_idx=(scales::rescale(Index_Dat$labor_index, to = c(0.001, 10)))
# Index_Dat$pov_idx=(scales::rescale(Index_Dat$PovIntensity, to = c(0.001, 10)))
# Index_Dat$edu_idx=(scales::rescale(Index_Dat$AverageSchooling, to = c(0.001, 10)))
# Index_Dat$afford_idx=(scales::rescale(Index_Dat$afford.idx, to = c(0.001, 10)))
# Index_Dat$mig_idx=(scales::rescale(Index_Dat$mig.idx, to = c(0.001, 10)))

Index_Dat$labor_idx=scales::rescale(Index_Dat$labor_idx, to = c(0, 10))
Index_Dat$pov_idx=scales::rescale(Index_Dat$pov_idx, to = c(0, 10))
Index_Dat$edu_idx=scales::rescale(Index_Dat$edu_idx, to = c(0, 10))
Index_Dat$afford_idx=scales::rescale(Index_Dat$afford_idx, to = c(0, 10))
Index_Dat$mig_idx=scales::rescale(Index_Dat$mig_idx, to = c(0, 10))


# Index_Dat$pov_idx=as.numeric(reverse.code(-1,Index_Dat$pov_idx))
Index_Dat$afford_idx=as.numeric(reverse.code(-1,Index_Dat$afford_idx))

Index_Dat$combined_index = (Index_Dat$labor_idx + Index_Dat$pov_idx + Index_Dat$edu_idx + Index_Dat$afford_idx + Index_Dat$mig_idx) / 5

glimpse(Index_Dat)


#Model with 1 common factor 
OZ_Idx_factor <- ' #start of model

OZ_Idx =~ pov_idx + labor_idx + afford_idx + mig_idx + edu_idx
	OZ_Idx ~~ 1 * OZ_Idx

# latent variable covariances

# latent variable means
	OZ_Idx ~ 0

' #end of model


fit <- cfa(OZ_Idx_factor, data=Index_Dat_uncapped, 
           std.lv=TRUE,  
           missing="fiml")
summary(fit, standardized=TRUE, fit.measures=TRUE)
semPaths(fit, what="std",  sizeLat = 10, sizeMan = 8, edge.label.cex = 1)
inspect(fit,what="std")$lambda


openxlsx::write.xlsx(Index_Dat, "~/Desktop/Welfare_Policy/Struggling_Regions/Index/final_index.xlsx")
openxlsx::write.xlsx(Index_Dat_uncapped, "~/Desktop/Welfare_Policy/Struggling_Regions/Index/final_index_uncapped.xlsx")
