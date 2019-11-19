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

options(scipen = 999)

educ=readstata13::read.dta13("~/Desktop/Welfare_Policy/Struggling_Regions/Index/Raw Data/EducationRawData.dta")
names(educ)
educ.labels_1 = attr(educ,"var.labels")
glimpse(educ.labels_1)

edu_vars=c("% Population 25 Years and Over: No Schooling Completed"                          
,"% Population 25 Years and Over: Nursery School"                                  
,"% Population 25 Years and Over: Kindergarten"                                    
,"% Population 25 Years and Over: 1st Grade"                                       
,"% Population 25 Years and Over: 2nd Grade"                                       
,"% Population 25 Years and Over: 3rd Grade"                                       
,"% Population 25 Years and Over: 4th Grade"                                       
,"% Population 25 Years and Over: 5th Grade"                                       
,"% Population 25 Years and Over: 6th Grade"                                       
,"% Population 25 Years and Over: 7th Grade"                                       
,"% Population 25 Years and Over: 8th Grade"                                       
,"% Population 25 Years and Over: 9th Grade"                                       
,"% Population 25 Years and Over: 10th Grade"                                      
,"% Population 25 Years and Over: 11th Grade"                                      
,"% Population 25 Years and Over: 12th Grade, No Diploma"                          
,"% Population 25 Years and Over: Regular High School Diploma"                     
,"% Population 25 Years and Over: GED or Alternative Credential"                   
,"% Population 25 Years and Over: Some College, Less than 1 Year"                  
,"% Population 25 Years and Over: Some College, 1 or More Years, No Degree"        
,"% Population 25 Years and Over: Associate's Degree"                              
,"% Population 25 Years and Over: Bachelor's Degree"                               
,"% Population 25 Years and Over: Master's Degree"                                 
,"% Population 25 Years and Over: Professional School Degree"                      
,"% Population 25 Years and Over: Doctorate Degree"   )                             

length(edu_vars)

rm(list=ls());gc()
OZ_dat=readstata13::read.dta13("~/Desktop/Welfare_Policy/Struggling_Regions/Cluster_Analyses/cluster_inputs/languagesupplement.dta")[c(1,5)]
HUD.IDX=readxl::read_excel("~/Desktop/Welfare_Policy/Struggling_Regions/Index/Raw Data/IDX.HUD.xlsx")

unique(HUD.IDX$Year)
# HUD.IDX=subset(HUD.IDX)
# IDX.dat = readstata13::read.dta13("~/Desktop/Welfare_Policy/Struggling_Regions/Index/Raw Data/IndexData_Full.dta") #

#2008-2012
IDX.p1 = readstata13::read.dta13("~/Desktop/Welfare_Policy/Struggling_Regions/Index/Raw Data/2008_2012_ACS.dta") #
# IDX.12.p2 = readstata13::read.dta13("~/Desktop/Welfare_Policy/Struggling_Regions/Index/Raw Data/2008_2012_supplement.dta") #
# IDX.12.p3 = readstata13::read.dta13("~/Desktop/Welfare_Policy/Struggling_Regions/Index/Raw Data/2008_2012_Supplement2.dta") #
# IDX.12=list(IDX.12.p1,IDX.12.p2,IDX.12.p3) %>% reduce(full_join, by = "FIPS")

#2013-2017
IDX.p2 = readstata13::read.dta13("~/Desktop/Welfare_Policy/Struggling_Regions/Index/Raw Data/2013_2017_ACS.dta")
# IDX.17.p2 = readstata13::read.dta13("~/Desktop/Welfare_Policy/Struggling_Regions/Index/Raw Data/2013_2017_supplement.dta")
# IDX.17.p3 = readstata13::read.dta13("~/Desktop/Welfare_Policy/Struggling_Regions/Index/Raw Data/2013_2017_Supplement2.dta")
# IDX.17=list(IDX.17.p1,IDX.17.p2,IDX.17.p3) %>% reduce(full_join, by = "FIPS")
IDX.sup1 = readstata13::read.dta13('/Users/rorr/Desktop/Welfare_Policy/Struggling_Regions/Index/Raw Data/IncomeSupplement.dta')
IDX.sup2 = readstata13::read.dta13('~/Desktop/Welfare_Policy/Struggling_Regions/Index/Raw Data/enrollmentdata.dta')

IDX.sup_prof.1 = readstata13::read.dta13("~/Desktop/Welfare_Policy/Struggling_Regions/Index/Raw Data/professions_p1.dta")
IDX.sup_prof.2 = readstata13::read.dta13("~/Desktop/Welfare_Policy/Struggling_Regions/Index/Raw Data/professions_p2.dta")


HUD.IDX$FIPS= ifelse(
	      HUD.IDX$FIPS == "51515050100", "51019050100",
	ifelse(HUD.IDX$FIPS ==  "04019002701", "04019002704",
	ifelse(HUD.IDX$FIPS ==  "04019002903", "04019002906",
	ifelse(HUD.IDX$FIPS ==  "04019410501", "04019004118",
	ifelse(HUD.IDX$FIPS ==  "04019410502", "04019004121",
	ifelse(HUD.IDX$FIPS ==  "04019410503", "04019004125",
	ifelse(HUD.IDX$FIPS ==  "04019470400", "04019005200",
	ifelse(HUD.IDX$FIPS ==  "04019470500", "04019005300",
	ifelse(HUD.IDX$FIPS ==  "06037930401", "06037137000",
	ifelse(HUD.IDX$FIPS ==  "02270000100", "02158000100",
	ifelse(HUD.IDX$FIPS ==  "46113940500", "46102940500",
	ifelse(HUD.IDX$FIPS ==  "46113940800", "46102940800",
	ifelse(HUD.IDX$FIPS ==  "46113940900", "46102940900",
	ifelse(HUD.IDX$FIPS ==  "36053940101", "36053030101",
	ifelse(HUD.IDX$FIPS ==  "36053940102", "36053030102",
	ifelse(HUD.IDX$FIPS ==  "36053940103", "36053030103",
	ifelse(HUD.IDX$FIPS ==  "36053940200", "36053030200",
	ifelse(HUD.IDX$FIPS ==  "36053940300", "36053030300",
	ifelse(HUD.IDX$FIPS ==  "36053940401", "36053030401",
	ifelse(HUD.IDX$FIPS ==  "36053940403", "36053030403",
	ifelse(HUD.IDX$FIPS ==  "36053940600", "36053030600",
	ifelse(HUD.IDX$FIPS ==  "36053940700", "36053030402",
	ifelse(HUD.IDX$FIPS ==  "36065940000", "36065024800",
	ifelse(HUD.IDX$FIPS ==  "36065940100", "36065024700",
	ifelse(HUD.IDX$FIPS ==  "36065940200", "36065024900", HUD.IDX$FIPS)))))))))))))))))))))))))

# HUD.IDX_dups=HUD.IDX[duplicated(HUD.IDX$FIPS)|duplicated(HUD.IDX$FIPS, fromLast=TRUE),]
# glimpse(HUD.IDX_dups)
HUD.IDX_dups=subset(HUD.IDX, Year == 2017)
HUD.IDX_dups=HUD.IDX_dups[duplicated(HUD.IDX_dups$FIPS)|duplicated(HUD.IDX_dups$FIPS, fromLast=TRUE),]


OZ_dat$FIPS= ifelse(
	        OZ_dat$FIPS == "51515050100", "51019050100",
	ifelse(OZ_dat$FIPS ==  "04019002701", "04019002704",
	ifelse(OZ_dat$FIPS ==  "04019002903", "04019002906",
	ifelse(OZ_dat$FIPS ==  "04019410501", "04019004118",
	ifelse(OZ_dat$FIPS ==  "04019410502", "04019004121",
	ifelse(OZ_dat$FIPS ==  "04019410503", "04019004125",
	ifelse(OZ_dat$FIPS ==  "04019470400", "04019005200",
	ifelse(OZ_dat$FIPS ==  "04019470500", "04019005300",
	ifelse(OZ_dat$FIPS ==  "06037930401", "06037137000",
	ifelse(OZ_dat$FIPS ==  "02270000100", "02158000100",
	ifelse(OZ_dat$FIPS ==  "46113940500", "46102940500",
	ifelse(OZ_dat$FIPS ==  "46113940800", "46102940800",
	ifelse(OZ_dat$FIPS ==  "46113940900", "46102940900",
	ifelse(OZ_dat$FIPS ==  "36053940101", "36053030101",
	ifelse(OZ_dat$FIPS ==  "36053940102", "36053030102",
	ifelse(OZ_dat$FIPS ==  "36053940103", "36053030103",
	ifelse(OZ_dat$FIPS ==  "36053940200", "36053030200",
	ifelse(OZ_dat$FIPS ==  "36053940300", "36053030300",
	ifelse(OZ_dat$FIPS ==  "36053940401", "36053030401",
	ifelse(OZ_dat$FIPS ==  "36053940403", "36053030403",
	ifelse(OZ_dat$FIPS ==  "36053940600", "36053030600",
	ifelse(OZ_dat$FIPS ==  "36053940700", "36053030402",
	ifelse(OZ_dat$FIPS ==  "36065940000", "36065024800",
	ifelse(OZ_dat$FIPS ==  "36065940100", "36065024700",
	ifelse(OZ_dat$FIPS ==  "36065940200", "36065024900", OZ_dat$FIPS)))))))))))))))))))))))))



IDX.sup1$FIPS= ifelse(
	        IDX.sup1$FIPS == "51515050100", "51019050100",
	ifelse(IDX.sup1$FIPS ==  "04019002701", "04019002704",
	ifelse(IDX.sup1$FIPS ==  "04019002903", "04019002906",
	ifelse(IDX.sup1$FIPS ==  "04019410501", "04019004118",
	ifelse(IDX.sup1$FIPS ==  "04019410502", "04019004121",
	ifelse(IDX.sup1$FIPS ==  "04019410503", "04019004125",
	ifelse(IDX.sup1$FIPS ==  "04019470400", "04019005200",
	ifelse(IDX.sup1$FIPS ==  "04019470500", "04019005300",
	ifelse(IDX.sup1$FIPS ==  "06037930401", "06037137000",
	ifelse(IDX.sup1$FIPS ==  "02270000100", "02158000100",
	ifelse(IDX.sup1$FIPS ==  "46113940500", "46102940500",
	ifelse(IDX.sup1$FIPS ==  "46113940800", "46102940800",
	ifelse(IDX.sup1$FIPS ==  "46113940900", "46102940900",
	ifelse(IDX.sup1$FIPS ==  "36053940101", "36053030101",
	ifelse(IDX.sup1$FIPS ==  "36053940102", "36053030102",
	ifelse(IDX.sup1$FIPS ==  "36053940103", "36053030103",
	ifelse(IDX.sup1$FIPS ==  "36053940200", "36053030200",
	ifelse(IDX.sup1$FIPS ==  "36053940300", "36053030300",
	ifelse(IDX.sup1$FIPS ==  "36053940401", "36053030401",
	ifelse(IDX.sup1$FIPS ==  "36053940403", "36053030403",
	ifelse(IDX.sup1$FIPS ==  "36053940600", "36053030600",
	ifelse(IDX.sup1$FIPS ==  "36053940700", "36053030402",
	ifelse(IDX.sup1$FIPS ==  "36065940000", "36065024800",
	ifelse(IDX.sup1$FIPS ==  "36065940100", "36065024700",
	ifelse(IDX.sup1$FIPS ==  "36065940200", "36065024900", IDX.sup1$FIPS)))))))))))))))))))))))))

IDX.sup2$FIPS= ifelse(
	        IDX.sup2$FIPS == "51515050100", "51019050100",
	ifelse(IDX.sup2$FIPS ==  "04019002701", "04019002704",
	ifelse(IDX.sup2$FIPS ==  "04019002903", "04019002906",
	ifelse(IDX.sup2$FIPS ==  "04019410501", "04019004118",
	ifelse(IDX.sup2$FIPS ==  "04019410502", "04019004121",
	ifelse(IDX.sup2$FIPS ==  "04019410503", "04019004125",
	ifelse(IDX.sup2$FIPS ==  "04019470400", "04019005200",
	ifelse(IDX.sup2$FIPS ==  "04019470500", "04019005300",
	ifelse(IDX.sup2$FIPS ==  "06037930401", "06037137000",
	ifelse(IDX.sup2$FIPS ==  "02270000100", "02158000100",
	ifelse(IDX.sup2$FIPS ==  "46113940500", "46102940500",
	ifelse(IDX.sup2$FIPS ==  "46113940800", "46102940800",
	ifelse(IDX.sup2$FIPS ==  "46113940900", "46102940900",
	ifelse(IDX.sup2$FIPS ==  "36053940101", "36053030101",
	ifelse(IDX.sup2$FIPS ==  "36053940102", "36053030102",
	ifelse(IDX.sup2$FIPS ==  "36053940103", "36053030103",
	ifelse(IDX.sup2$FIPS ==  "36053940200", "36053030200",
	ifelse(IDX.sup2$FIPS ==  "36053940300", "36053030300",
	ifelse(IDX.sup2$FIPS ==  "36053940401", "36053030401",
	ifelse(IDX.sup2$FIPS ==  "36053940403", "36053030403",
	ifelse(IDX.sup2$FIPS ==  "36053940600", "36053030600",
	ifelse(IDX.sup2$FIPS ==  "36053940700", "36053030402",
	ifelse(IDX.sup2$FIPS ==  "36065940000", "36065024800",
	ifelse(IDX.sup2$FIPS ==  "36065940100", "36065024700",
	ifelse(IDX.sup2$FIPS ==  "36065940200", "36065024900", IDX.sup2$FIPS)))))))))))))))))))))))))

IDX.sup3$FIPS= ifelse(
	        IDX.sup3$FIPS == "51515050100", "51019050100",
	ifelse(IDX.sup3$FIPS ==  "04019002701", "04019002704",
	ifelse(IDX.sup3$FIPS ==  "04019002903", "04019002906",
	ifelse(IDX.sup3$FIPS ==  "04019410501", "04019004118",
	ifelse(IDX.sup3$FIPS ==  "04019410502", "04019004121",
	ifelse(IDX.sup3$FIPS ==  "04019410503", "04019004125",
	ifelse(IDX.sup3$FIPS ==  "04019470400", "04019005200",
	ifelse(IDX.sup3$FIPS ==  "04019470500", "04019005300",
	ifelse(IDX.sup3$FIPS ==  "06037930401", "06037137000",
	ifelse(IDX.sup3$FIPS ==  "02270000100", "02158000100",
	ifelse(IDX.sup3$FIPS ==  "46113940500", "46102940500",
	ifelse(IDX.sup3$FIPS ==  "46113940800", "46102940800",
	ifelse(IDX.sup3$FIPS ==  "46113940900", "46102940900",
	ifelse(IDX.sup3$FIPS ==  "36053940101", "36053030101",
	ifelse(IDX.sup3$FIPS ==  "36053940102", "36053030102",
	ifelse(IDX.sup3$FIPS ==  "36053940103", "36053030103",
	ifelse(IDX.sup3$FIPS ==  "36053940200", "36053030200",
	ifelse(IDX.sup3$FIPS ==  "36053940300", "36053030300",
	ifelse(IDX.sup3$FIPS ==  "36053940401", "36053030401",
	ifelse(IDX.sup3$FIPS ==  "36053940403", "36053030403",
	ifelse(IDX.sup3$FIPS ==  "36053940600", "36053030600",
	ifelse(IDX.sup3$FIPS ==  "36053940700", "36053030402",
	ifelse(IDX.sup3$FIPS ==  "36065940000", "36065024800",
	ifelse(IDX.sup3$FIPS ==  "36065940100", "36065024700",
	ifelse(IDX.sup3$FIPS ==  "36065940200", "36065024900", IDX.sup3$FIPS)))))))))))))))))))))))))

IDX.labels_1 = attr(IDX.p1,"var.labels")
IDX.labels_2 = attr(IDX.p2,"var.labels")

IDX.sup1.labels = attr(IDX.sup1,"var.labels")
IDX.sup2.labels = attr(IDX.sup2,"var.labels")

IDX.prof.1.labels = attr(IDX.sup_prof.1,"var.labels")
IDX.prof.2.labels = attr(IDX.sup_prof.2,"var.labels")

IDX.p1$Year = 2012
IDX.p2$Year = 2017

# IDX.labels_1=append(IDX.labels_1, "Year")
# IDX.labels_2=append(IDX.labels_2, "Year")

# names(IDX.p1);names(IDX.p2)
# glimpse(IDX.p1);glimpse(IDX.p2)

# setdiff(names(IDX.p1), names(IDX.p2))
# setdiff(names(IDX.p2), names(IDX.p1))
# setdiff((IDX.labels_1), (IDX.labels_2))
# setdiff((IDX.labels_2), (IDX.labels_1))

IDX1_vars=cbind(var.label=IDX.labels_1,var.name=names(IDX.p1))
IDX2_vars=cbind(var.label_p2=IDX.labels_2,var.name=names(IDX.p2))
IDXsup.1_vars=cbind(var.label=IDX.sup1.labels,var.name=names(IDX.sup1))
IDXsup.2_vars=cbind(var.label=IDX.sup2.labels,var.name=names(IDX.sup2))

IDX.labels=merge(IDX1_vars,IDX2_vars, by = 'var.name', all = T, sort = F)
# openxlsx::write.xlsx(IDX.labels,'~/Downloads/IDX.labels.xlsx')
IDX.labels$var.label_p2=NULL
IDX.sup.labels=merge(IDX1_vars,IDX2_vars, by = 'var.name', all = T, sort = F)

IDX.dat=rbind(IDX.p1,IDX.p2)
IDX.dat$FIPS= ifelse(
	        IDX.dat$FIPS == "51515050100", "51019050100",
	ifelse(IDX.dat$FIPS ==  "04019002701", "04019002704",
	ifelse(IDX.dat$FIPS ==  "04019002903", "04019002906",
	ifelse(IDX.dat$FIPS ==  "04019410501", "04019004118",
	ifelse(IDX.dat$FIPS ==  "04019410502", "04019004121",
	ifelse(IDX.dat$FIPS ==  "04019410503", "04019004125",
	ifelse(IDX.dat$FIPS ==  "04019470400", "04019005200",
	ifelse(IDX.dat$FIPS ==  "04019470500", "04019005300",
	ifelse(IDX.dat$FIPS ==  "06037930401", "06037137000",
	ifelse(IDX.dat$FIPS ==  "02270000100", "02158000100",
	ifelse(IDX.dat$FIPS ==  "46113940500", "46102940500",
	ifelse(IDX.dat$FIPS ==  "46113940800", "46102940800",
	ifelse(IDX.dat$FIPS ==  "46113940900", "46102940900",
	ifelse(IDX.dat$FIPS ==  "36053940101", "36053030101",
	ifelse(IDX.dat$FIPS ==  "36053940102", "36053030102",
	ifelse(IDX.dat$FIPS ==  "36053940103", "36053030103",
	ifelse(IDX.dat$FIPS ==  "36053940200", "36053030200",
	ifelse(IDX.dat$FIPS ==  "36053940300", "36053030300",
	ifelse(IDX.dat$FIPS ==  "36053940401", "36053030401",
	ifelse(IDX.dat$FIPS ==  "36053940403", "36053030403",
	ifelse(IDX.dat$FIPS ==  "36053940600", "36053030600",
	ifelse(IDX.dat$FIPS ==  "36053940700", "36053030402",
	ifelse(IDX.dat$FIPS ==  "36065940000", "36065024800",
	ifelse(IDX.dat$FIPS ==  "36065940100", "36065024700",
	ifelse(IDX.dat$FIPS ==  "36065940200", "36065024900", IDX.dat$FIPS)))))))))))))))))))))))))
IDX.dat$St_Code=stringi::stri_sub(IDX.dat$FIPS, from=1, to=2)
IDX.dat$admin_type=ifelse(as.numeric(IDX.dat$St_Code) <= 56, 1, 0) 
IDX.dat=subset(IDX.dat, IDX.dat$admin_type != 0, select = -c(admin_type,St_Code,`_merge`,merge1))

IDX.dat=merge(IDX.dat,IDX.sup1, by = c('FIPS', "Year"), all = T, sort = F)
IDX.dat=merge(OZ_dat,IDX.dat, by = 'FIPS', all = T, sort = F)

IDX.dat$median_housing_age = (IDX.dat$Year - IDX.dat$B25035001)
pct_vars <- grepl('PCT_', colnames(IDX.dat))
IDX.dat[pct_vars] =  IDX.dat[pct_vars] * 100

     # ,PCT_C17002002	#                      Population for Whom Poverty Status Is Determined: Under .50
     # ,PCT_C17002003	#                     Population for Whom Poverty Status Is Determined: .50 to .99
     # ,PCT_C17002004	#                   Population for Whom Poverty Status Is Determined: 1.00 to 1.24
     # ,PCT_C17002005	#                   Population for Whom Poverty Status Is Determined: 1.25 to 1.49
     # ,PCT_C17002006	#                   Population for Whom Poverty Status Is Determined: 1.50 to 1.84
     # ,PCT_C17002007	#                   Population for Whom Poverty Status Is Determined: 1.85 to 1.99
     # ,PCT_C17002008	#                  Population for Whom Poverty Status Is Determined: 2.00 and Over

IDX.dat$pov_100 = (IDX.dat$PCT_C17002002+IDX.dat$PCT_C17002003+IDX.dat$PCT_C17002004)
IDX.dat$pov_200 = (IDX.dat$PCT_C17002002+IDX.dat$PCT_C17002003+IDX.dat$PCT_C17002004+IDX.dat$PCT_C17002005+IDX.dat$PCT_C17002006+IDX.dat$PCT_C17002007)

IDX.dat=merge(IDX.dat,HUD.IDX, by = c('FIPS','Year'), all = T, sort = F)

IDX.dat$EPOP=IDX.dat$EPOP *100
IDX.dat$UnemploymentRate=IDX.dat$UnemploymentRate *100	#  Civillian unemployment rate
IDX.dat$VacancyRate=IDX.dat$VacancyRate *100	#     Share of housing units that are vacant
IDX.dat$CashAssistanceRate=IDX.dat$CashAssistanceRate *100  #Share of households receiving food stamps / cash assistance
IDX.dat$UnemploymentRate2554 = IDX.dat$UnemploymentRate2554 *100
IDX.dat$LFPR2554 = IDX.dat$LFPR2554 *100
IDX.dat$EPOP2554=IDX.dat$Employed2554/IDX.dat$Pop2554 * 100

IDX.names=as.data.frame(colnames(IDX.dat));names(IDX.names)[names(IDX.names) == 'colnames(IDX.dat)'] = 'var.name'
IDX.names$var.name=as.character(IDX.names$var.name)

IDX.labels=merge(IDX.labels,IDX.names, by = 'var.name', all.y = T, sort = F)
IDX.labels$var.label=ifelse(is.na(IDX.labels$var.label),as.character(IDX.labels$var.name),as.character(IDX.labels$var.label))
IDX.labels

IDX.dat=subset(IDX.dat,
	Year==2017 &
	B01003001 >= 150)
IDX.dat_names = IDX.dat[,1]
# names(as.vector(IDX.dat))

IDX.dat=IDX.dat[!duplicated(IDX.dat), ]

IDX.dat$PovIntensity = ((
  (IDX.dat$PCT_C17002002 * 1.749 )  #     Population for Whom Poverty Status Is Determined: Under .50
+ (IDX.dat$PCT_C17002003 * 1.24  )  #     Population for Whom Poverty Status Is Determined: .50 to .99
+ (IDX.dat$PCT_C17002004 * 0.87  )  #     Population for Whom Poverty Status Is Determined: 1.00 to 1.24
+ (IDX.dat$PCT_C17002005 * 0.62  )  #     Population for Whom Poverty Status Is Determined: 1.25 to 1.49
+ (IDX.dat$PCT_C17002006 * 0.32  )  #     Population for Whom Poverty Status Is Determined: 1.50 to 1.84
+ (IDX.dat$PCT_C17002007 * 0.07  )) #     Population for Whom Poverty Status Is Determined: 1.85 to 1.99
	/ 4.869) 

theshs=cbind(x = c(.25, .75, 1.12, 1.37, 1.67, 1.92)) 
1.99 - theshs
sum(1.99 - theshs)

as.data.frame(IDX.dat[c('FIPS',
	'PCT_C17002002','PCT_C17002003','PCT_C17002004','PCT_C17002005','PCT_C17002006','PCT_C17002007',
	'PovIntensity')]) %>% summarise_all(funs(sum(is.na(.)))) %>% gather();print(missings_pov)

IDX.dat$St_Code=stringi::stri_sub(IDX.dat$FIPS, from=1, to=2)
IDX.dat$admin_type=ifelse(as.numeric(IDX.dat$St_Code) <= 56, "1", "0") 
IDX.dat=subset(IDX.dat,admin_type=="1") # Remove Puerto Rico
IDX.dat$admin_type=NULL

missing_pov=subset(IDX.dat,is.na(PovIntensity))
missing_pov$FIPS

dups=IDX.dat[duplicated(IDX.dat)|duplicated(IDX.dat, fromLast=TRUE),]
dups_fips=IDX.dat[duplicated(IDX.dat$FIPS)|duplicated(IDX.dat$FIPS, fromLast=TRUE),]

dup_fips=subset(IDX.dat,duplicated(FIPS))

IDX.dat[duplicated(IDX.dat$FIPS)]


any(duplicated(IDX.dat$FIPS))
any(is.na(IDX.dat$FIPS))

IDX.dat_sub = subset(IDX.dat, select = -c( # Remove GEO Vars
          # var.name	                                                                         var.label
             # FIPS #                                                                             FIPS
            B01003001 #                                                                 Total Population
            ,B07001001 #                                  Population 1 Year and Over in the United States
            ,B07001017 #           Population 1 Year and Over in the United States: Same House 1 Year Ago
            ,B07001033 #        Population 1 Year and Over in the United States: Moved Within Same County
            ,B07001049 # Population 1 Year and Over in the United States: Moved From Different County Wit
            ,B07001065 #      Population 1 Year and Over in the United States: Moved From Different State
            ,B07001081 #               Population 1 Year and Over in the United States: Moved From Abroad
            ,B14007001 #                                                      Population 3 Years and Over
            ,B14007002 #                                  Population 3 Years and Over: Enrolled in School
            ,B14007003 # Population 3 Years and Over: Enrolled in School: Enrolled in Nursery School, Pre
            ,B14007004 #        Population 3 Years and Over: Enrolled in School: Enrolled in Kindergarten
            ,B14007005 #             Population 3 Years and Over: Enrolled in School: Enrolled in Grade 1
            ,B14007006 #             Population 3 Years and Over: Enrolled in School: Enrolled in Grade 2
            ,B14007007 #             Population 3 Years and Over: Enrolled in School: Enrolled in Grade 3
            ,B14007008 #             Population 3 Years and Over: Enrolled in School: Enrolled in Grade 4
            ,B14007009 #             Population 3 Years and Over: Enrolled in School: Enrolled in Grade 5
            ,B14007010 #             Population 3 Years and Over: Enrolled in School: Enrolled in Grade 6
            ,B14007011 #             Population 3 Years and Over: Enrolled in School: Enrolled in Grade 7
            ,B14007012 #             Population 3 Years and Over: Enrolled in School: Enrolled in Grade 8
            ,B14007013 #             Population 3 Years and Over: Enrolled in School: Enrolled in Grade 9
            ,B14007014 #            Population 3 Years and Over: Enrolled in School: Enrolled in Grade 10
            ,B14007015 #            Population 3 Years and Over: Enrolled in School: Enrolled in Grade 11
            ,B14007016 #            Population 3 Years and Over: Enrolled in School: Enrolled in Grade 12
            ,B14007017 # Population 3 Years and Over: Enrolled in School: Enrolled in College, Undergradu
            ,B14007018 # Population 3 Years and Over: Enrolled in School: Graduate or Professional School
            ,B14007019 #                              Population 3 Years and Over: Not Enrolled in School
            ,B15003001 #                                                     Population 25 Years and Over
            ,B15003002 #                             Population 25 Years and Over: No Schooling Completed
            ,B15003003 #                                     Population 25 Years and Over: Nursery School
            ,B15003004 #                                       Population 25 Years and Over: Kindergarten
            ,B15003005 #                                          Population 25 Years and Over: 1st Grade
            ,B15003006 #                                          Population 25 Years and Over: 2nd Grade
            ,B15003007 #                                          Population 25 Years and Over: 3rd Grade
            ,B15003008 #                                          Population 25 Years and Over: 4th Grade
            ,B15003009 #                                          Population 25 Years and Over: 5th Grade
            ,B15003010 #                                          Population 25 Years and Over: 6th Grade
            ,B15003011 #                                          Population 25 Years and Over: 7th Grade
            ,B15003012 #                                          Population 25 Years and Over: 8th Grade
            ,B15003013 #                                          Population 25 Years and Over: 9th Grade
            ,B15003014 #                                         Population 25 Years and Over: 10th Grade
            ,B15003015 #                                         Population 25 Years and Over: 11th Grade
            ,B15003016 #                             Population 25 Years and Over: 12th Grade, No Diploma
            ,B15003017 #                        Population 25 Years and Over: Regular High School Diploma
            ,B15003018 #                      Population 25 Years and Over: GED or Alternative Credential
            ,B15003019 #                     Population 25 Years and Over: Some College, Less than 1 Year
            ,B15003020 #           Population 25 Years and Over: Some College, 1 or More Years, No Degree
            ,B15003021 #                                 Population 25 Years and Over: Associate's Degree
            ,B15003022 #                                  Population 25 Years and Over: Bachelor's Degree
            ,B15003023 #                                    Population 25 Years and Over: Master's Degree
            ,B15003024 #                         Population 25 Years and Over: Professional School Degree
            ,B15003025 #                                   Population 25 Years and Over: Doctorate Degree
            ,B19001001 #                                                                       Households
            ,B19001002 #                                                    Households: Less than $10,000
            ,B19001003 #                                                   Households: $10,000 to $14,999
            ,B19001004 #                                                   Households: $15,000 to $19,999
            ,B19001005 #                                                   Households: $20,000 to $24,999
            ,B19001006 #                                                   Households: $25,000 to $29,999
            ,B19001007 #                                                   Households: $30,000 to $34,999
            ,B19001008 #                                                   Households: $35,000 to $39,999
            ,B19001009 #                                                   Households: $40,000 to $44,999
            ,B19001010 #                                                   Households: $45,000 to $49,999
            ,B19001011 #                                                   Households: $50,000 to $59,999
            ,B19001012 #                                                   Households: $60,000 to $74,999
            ,B19001013 #                                                   Households: $75,000 to $99,999
            ,B19001014 #                                                 Households: $100,000 to $124,999
            ,B19001015 #                                                 Households: $125,000 to $149,999
            ,B19001016 #                                                 Households: $150,000 to $199,999
            ,B19001017 #                                                     Households: $200,000 or More
            # ,B19013001 # Households: Median Household Income in the Past 12 Months (In 2017 Inflation-Adj
            ,B19058001 #                                                                       Households
            ,B19058002 #                      Households: with Cash Public Assistance or Food Stamps/Snap
            ,B19058003 #                        Households: No Cash Public Assistance or Food Stamps/Snap
            ,B19083001 #                                                           Households: Gini Index
            ,B23025001 #                                                     Population 16 Years and Over
            ,B23025002 #                                     Population 16 Years and Over: in Labor Force
            ,B23025003 #               Population 16 Years and Over: in Labor Force: Civilian Labor Force
            ,B23025004 #     Population 16 Years and Over: in Labor Force: Civilian Labor Force: Employed
            ,B23025005 #   Population 16 Years and Over: in Labor Force: Civilian Labor Force: Unemployed
            ,B23025006 #                       Population 16 Years and Over: in Labor Force: Armed Forces
            ,B23025007 #                                 Population 16 Years and Over: Not in Labor Force
            ,B25004001 #                                                             Vacant Housing Units
            ,B25004002 #                                                   Vacant Housing Units: for Rent
            ,B25004003 #                                       Vacant Housing Units: Rented, Not Occupied
            ,B25004004 #                                              Vacant Housing Units: for Sale Only
            ,B25004005 #                                         Vacant Housing Units: Sold, Not Occupied
            ,B25004006 #              Vacant Housing Units: for Seasonal, Recreational, or Occasional Use
            ,B25004007 #                                        Vacant Housing Units: for Migrant Workers
            ,B25004008 #                                               Vacant Housing Units: Other Vacant
            # ,B25064001 #                Renter-Occupied Housing Units Paying Cash Rent: Median Gross Rent
            ,B25071001 # Renter-Occupied Housing Units Paying Cash Rent: Median Gross Rent as a Percentag
            ,B25075001 #                                                     Owner-Occupied Housing Units
            ,B25075002 #                                  Owner-Occupied Housing Units: Less than $10,000
            ,B25075003 #                                 Owner-Occupied Housing Units: $10,000 to $14,999
            ,B25075004 #                                 Owner-Occupied Housing Units: $15,000 to $19,999
            ,B25075005 #                                 Owner-Occupied Housing Units: $20,000 to $24,999
            ,B25075006 #                                 Owner-Occupied Housing Units: $25,000 to $29,999
            ,B25075007 #                                 Owner-Occupied Housing Units: $30,000 to $34,999
            ,B25075008 #                                 Owner-Occupied Housing Units: $35,000 to $39,999
            ,B25075009 #                                 Owner-Occupied Housing Units: $40,000 to $49,999
            ,B25075010 #                                 Owner-Occupied Housing Units: $50,000 to $59,999
            ,B25075011 #                                 Owner-Occupied Housing Units: $60,000 to $69,999
            ,B25075012 #                                 Owner-Occupied Housing Units: $70,000 to $79,999
            ,B25075013 #                                 Owner-Occupied Housing Units: $80,000 to $89,999
            ,B25075014 #                                 Owner-Occupied Housing Units: $90,000 to $99,999
            ,B25075015 #                               Owner-Occupied Housing Units: $100,000 to $124,999
            ,B25075016 #                               Owner-Occupied Housing Units: $125,000 to $149,999
            ,B25075017 #                               Owner-Occupied Housing Units: $150,000 to $174,999
            ,B25075018 #                               Owner-Occupied Housing Units: $175,000 to $199,999
            ,B25075019 #                               Owner-Occupied Housing Units: $200,000 to $249,999
            ,B25075020 #                               Owner-Occupied Housing Units: $250,000 to $299,999
            ,B25075021 #                               Owner-Occupied Housing Units: $300,000 to $399,999
            ,B25075022 #                               Owner-Occupied Housing Units: $400,000 to $499,999
            ,B25075023 #                               Owner-Occupied Housing Units: $500,000 to $749,999
            ,B25075024 #                               Owner-Occupied Housing Units: $750,000 to $999,999
            ,B25075025 #                                 Owner-Occupied Housing Units: $1,000,000 or More
            # ,B25076001 #                     Owner-Occupied Housing Units: Lower Value Quartile (Dollars)
            # ,B25077001 #                             Owner-Occupied Housing Units: Median Value (Dollars)
            # ,B25078001 #                     Owner-Occupied Housing Units: Upper Value Quartile (Dollars)
            ,C17002001 #                                 Population for Whom Poverty Status Is Determined
            ,C17002002 #                      Population for Whom Poverty Status Is Determined: Under .50
            ,C17002003 #                     Population for Whom Poverty Status Is Determined: .50 to .99
            ,C17002004 #                   Population for Whom Poverty Status Is Determined: 1.00 to 1.24
            ,C17002005 #                   Population for Whom Poverty Status Is Determined: 1.25 to 1.49
            ,C17002006 #                   Population for Whom Poverty Status Is Determined: 1.50 to 1.84
            ,C17002007 #                   Population for Whom Poverty Status Is Determined: 1.85 to 1.99
            ,C17002008 #                  Population for Whom Poverty Status Is Determined: 2.00 and Over
            ,B25003001 #                                                           Occupied Housing Units
            ,B25003002 #                                           Occupied Housing Units: Owner Occupied
            ,B25003003 #                                          Occupied Housing Units: Renter Occupied
            ,B25034001 #                                                                    Housing Units
            ,B25034002 #                                               Housing Units: Built 2010 or Later
            ,B25034003 #                                                Housing Units: Built 2000 to 2009
            ,B25034004 #                                                Housing Units: Built 1990 to 1999
            ,B25034005 #                                                Housing Units: Built 1980 to 1989
            ,B25034006 #                                                Housing Units: Built 1970 to 1979
            ,B25034007 #                                                Housing Units: Built 1960 to 1969
            ,B25034008 #                                                Housing Units: Built 1950 to 1959
            ,B25034009 #                                                Housing Units: Built 1940 to 1949
            ,B25034010 #                                             Housing Units: Built 1939 or Earlier
            ,B25035001 #                                       Housing Units: Median Year Structure Built
        ,PCT_B07001017 #           Population 1 Year and Over in the United States: Same House 1 Year Ago
        ,PCT_B07001033 #        Population 1 Year and Over in the United States: Moved Within Same County
        ,PCT_B07001049 # Population 1 Year and Over in the United States: Moved From Different County Wit
        ,PCT_B07001065 #      Population 1 Year and Over in the United States: Moved From Different State
        ,PCT_B07001081 #               Population 1 Year and Over in the United States: Moved From Abroad
        ,PCT_B15003002 #                             Population 25 Years and Over: No Schooling Completed
        ,PCT_B15003003 #                                     Population 25 Years and Over: Nursery School
        ,PCT_B15003004 #                                       Population 25 Years and Over: Kindergarten
        ,PCT_B15003005 #                                                Population 25 and over: 1st grade
        ,PCT_B15003006 #                                               Population 25 and older: 2nd grade
        ,PCT_B15003007 #                                                      Pop 25 and older: 3rd grade
        ,PCT_B15003008 #                                                      Pop 25 and older: 4th grade
        ,PCT_B15003009 #                                                      Pop 25 and older: 5th grade
        ,PCT_B15003010 #                                                       Pop 25 and older 6th grade
        ,PCT_B15003011 #                                                      Pop 25 and older: 7th grade
        ,PCT_B15003012 #                                                      Pop 25 and older: 8th grade
        ,PCT_B15003013 #                                                      Pop 25 and older: 9th grade
        ,PCT_B15003014 #                                                     Pop 25 and older: 10th grade
        ,PCT_B15003015 #                                                     Pop 25 and older: 11th grade
        ,PCT_B15003016 #                                         Pop 25 and older: 12th grade, no diploma
        ,PCT_B15003017 #                                    Pop 25 and older: regular high school diploma
        ,PCT_B15003018 #                                       Pop 25 and older: GED or alt HS credential
        ,PCT_B15003019 #                               Pop 25 and older, some college, less than one year
        ,PCT_B15003020 #                       Pop 25 and older: Some college, 1 or more years, no degree
        ,PCT_B15003021 #                                             Pop 25 and older: Associate's degree
        ,PCT_B15003022 #                                                      Pop 25 and older: BA degree
        ,PCT_B15003023 #                                                Pop 25 and older: master's degree
        ,PCT_B15003024 #                                            Pop 25 and older: professional degree
        ,PCT_B15003025 #                                                Pop 25 and older: doctoral degree
        ,PCT_B19001002 #                                                    Households: Less than $10,000
        ,PCT_B19001003 #                                                   Households: $10,000 to $14,999
        ,PCT_B19001004 #                                                   Households: $15,000 to $19,999
        ,PCT_B19001005 #                                                   Households: $20,000 to $24,999
        ,PCT_B19001006 #                                                   Households: $25,000 to $29,999
        ,PCT_B19001007 #                                                   Households: $30,000 to $34,999
        ,PCT_B19001008 #                                                   Households: $35,000 to $39,999
        ,PCT_B19001009 #                                                   Households: $40,000 to $44,999
        ,PCT_B19001010 #                                                   Households: $45,000 to $49,999
        ,PCT_B19001011 #                                                   Households: $50,000 to $59,999
        ,PCT_B19001012 #                                                   Households: $60,000 to $74,999
        ,PCT_B19001013 #                                                   Households: $75,000 to $99,999
        ,PCT_B19001014 #                                                 Households: $100,000 to $124,999
        ,PCT_B19001015 #                                                 Households: $125,000 to $149,999
        ,PCT_B19001016 #                                                 Households: $150,000 to $199,999
        ,PCT_B19001017 #                                                     Households: $200,000 or more
     # ,UnemploymentRate #                                                      Civillian unemployment rate
                 ,EPOP #                                     Share of population over 16 that is employed
          # ,VacancyRate #                                           Share of housing units that are vacant
   ,CashAssistanceRate #               Share of households receiving food stamps or other cash assistance
        ,PCT_B25075002 #                                  Owner-Occupied Housing Units: Less than $10,000
        ,PCT_B25075003 #                                 Owner-Occupied Housing Units: $10,000 to $14,999
        ,PCT_B25075004 #                                 Owner-Occupied Housing Units: $15,000 to $19,999
        ,PCT_B25075005 #                                 Owner-Occupied Housing Units: $20,000 to $24,999
        ,PCT_B25075006 #                                 Owner-Occupied Housing Units: $25,000 to $29,999
        ,PCT_B25075007 #                                 Owner-Occupied Housing Units: $30,000 to $34,999
        ,PCT_B25075008 #                                 Owner-Occupied Housing Units: $35,000 to $39,999
        ,PCT_B25075009 #                                 Owner-Occupied Housing Units: $40,000 to $49,999
        ,PCT_B25075010 #                                 Owner-Occupied Housing Units: $50,000 to $59,999
        ,PCT_B25075011 #                                 Owner-Occupied Housing Units: $60,000 to $69,999
        ,PCT_B25075012 #                                 Owner-Occupied Housing Units: $70,000 to $79,999
        ,PCT_B25075013 #                                 Owner-Occupied Housing Units: $80,000 to $89,999
        ,PCT_B25075014 #                                 Owner-Occupied Housing Units: $90,000 to $99,999
        ,PCT_B25075015 #                               Owner-Occupied Housing Units: $100,000 to $124,999
        ,PCT_B25075016 #                               Owner-Occupied Housing Units: $125,000 to $149,999
        ,PCT_B25075017 #                               Owner-Occupied Housing Units: $150,000 to $174,999
        ,PCT_B25075018 #                               Owner-Occupied Housing Units: $175,000 to $199,999
        ,PCT_B25075019 #                               Owner-Occupied Housing Units: $200,000 to $249,999
        ,PCT_B25075020 #                               Owner-Occupied Housing Units: $250,000 to $299,999
        ,PCT_B25075021 #                               Owner-Occupied Housing Units: $300,000 to $399,999
        ,PCT_B25075022 #                               Owner-Occupied Housing Units: $400,000 to $499,999
        ,PCT_B25075023 #                               Owner-Occupied Housing Units: $500,000 to $749,999
        ,PCT_B25075024 #                               Owner-Occupied Housing Units: $750,000 to $999,999
        ,PCT_B25075025 #                                 Owner-Occupied Housing Units: $1,000,000 or More
        # ,PCT_C17002002 #                      Population for Whom Poverty Status Is Determined: Under .50
        # ,PCT_C17002003 #                     Population for Whom Poverty Status Is Determined: .50 to .99
        # ,PCT_C17002004 #                   Population for Whom Poverty Status Is Determined: 1.00 to 1.24
        # ,PCT_C17002005 #                   Population for Whom Poverty Status Is Determined: 1.25 to 1.49
        # ,PCT_C17002006 #                   Population for Whom Poverty Status Is Determined: 1.50 to 1.84
        # ,PCT_C17002007 #                   Population for Whom Poverty Status Is Determined: 1.85 to 1.99
        ,PCT_C17002008 #                  Population for Whom Poverty Status Is Determined: 2.00 and Over
        ,PCT_B25034002 #                                               Housing Units: Built 2010 or Later
        ,PCT_B25034003 #                                                Housing Units: Built 2000 to 2009
        ,PCT_B25034004 #                                                Housing Units: Built 1990 to 1999
        ,PCT_B25034005 #                                                Housing Units: Built 1980 to 1989
        ,PCT_B25034006 #                                                Housing Units: Built 1970 to 1979
        ,PCT_B25034007 #                                                Housing Units: Built 1960 to 1969
        ,PCT_B25034008 #                                                Housing Units: Built 1950 to 1959
        ,PCT_B25034009 #                                                Housing Units: Built 1940 to 1949
        ,PCT_B25034010 #                                             Housing Units: Built 1939 or Earlier
     # ,AverageSchooling #                    Average years of schooling completed, population 25 and older
           ,A09003_001 #                                                 Average Commute to Work (in Min)
            ,B07204001 #                                  Population 1 Year and Over in the United States
            ,B07204002 #           Population 1 Year and Over in the United States: Same House 1 Year Ago
            ,B07204004 # Population 1 Year and Over in the United States: Different House in United State
            ,B07204007 # Population 1 Year and Over in the United States: Different House in United State
            ,B07204016 #               Population 1 Year and Over in the United States: Abroad 1 Year Ago
            ,B19081001 #                                      Households: Quintile Means: Lowest Quintile
            ,B19081002 #                                      Households: Quintile Means: Second Quintile
            ,B19081003 #                                       Households: Quintile Means: Third Quintile
            ,B19081004 #                                      Households: Quintile Means: Fourth Quintile
            ,B19081005 #                                     Households: Quintile Means: Highest Quintile
            ,B19081006 #                                        Households: Quintile Means: Top 5 Percent
        ,PCT_B07204002 #           Population 1 Year and Over in the United States: Same House 1 Year Ago
        ,PCT_B07204004 # Population 1 Year and Over in the United States: Different House in United State
        # ,PCT_B07204007 # Population 1 Year and Over in the United States: Different House in United State
        ,PCT_B07204016 #               Population 1 Year and Over in the United States: Abroad 1 Year Ago
       ,LaborForce2554 #                                                               Labor force, 25-54
              ,Pop2554 #                                                                Population, 25-54
            ,CivLF2554 #                                                      Civilian labor force, 25-54
         ,Employed2554 #                                                                  Employed, 25-54
       ,Unemployed2554 #                                                                Unemployed, 25-54
		# ,EPOP2554 #
 ,UnemploymentRate2554 #                                                Civilian unemployment rate, 25-54
             ,LFPR2554 #                                            Labor force participation rate, 25-54
              # ,pov_100 #                                                                             <NA>
              # ,pov_200 #                                                                             <NA>
                   # ,OZ #                                                                             <NA>
   	# ,median_housing_age #                                                                           <NA>
		# ,bus_prop
		# ,PerWorkerWageIncome
		# ,Density
		# ,PerCapitaIncome
		# ,PovIntensity
		,Year
		# ,res_vac_prop
		# ,bus_vac_prop
))
missings_IDX=as.data.frame(IDX.dat_sub) %>% summarise_all(funs(sum(is.na(.)))) %>% gather()
# print(missings_IDX, n =50)
glimpse(IDX.dat_sub)

IDX.dat_sub$VacancyRate=as.numeric(reverse.code(-1,IDX.dat_sub$VacancyRate))
IDX.dat_sub$pov_200=as.numeric(reverse.code(-1,IDX.dat_sub$pov_200))
IDX.dat_sub$pov_100=as.numeric(reverse.code(-1,IDX.dat_sub$pov_100))
IDX.dat_sub$UnemploymentRate=as.numeric(reverse.code(-1,IDX.dat_sub$UnemploymentRate))
IDX.dat_sub$median_housing_age=as.numeric(reverse.code(-1,IDX.dat_sub$median_housing_age))
IDX.dat_sub$PovIntensity=as.numeric(reverse.code(-1,IDX.dat_sub$PovIntensity))
IDX.dat_sub$bus_vac_prop=as.numeric(reverse.code(-1,IDX.dat_sub$bus_vac_prop))
IDX.dat_sub$bus_prop=as.numeric(reverse.code(-1,IDX.dat_sub$bus_prop))
IDX.dat_sub$res_vac_prop=as.numeric(reverse.code(-1,IDX.dat_sub$res_vac_prop))

IDX.dat_sub$PCT_C17002002=as.numeric(reverse.code(-1,IDX.dat_sub$PCT_C17002002))
IDX.dat_sub$PCT_C17002003=as.numeric(reverse.code(-1,IDX.dat_sub$PCT_C17002003))
IDX.dat_sub$PCT_C17002004=as.numeric(reverse.code(-1,IDX.dat_sub$PCT_C17002004))
IDX.dat_sub$PCT_C17002005=as.numeric(reverse.code(-1,IDX.dat_sub$PCT_C17002005))
IDX.dat_sub$PCT_C17002006=as.numeric(reverse.code(-1,IDX.dat_sub$PCT_C17002006))
IDX.dat_sub$PCT_C17002007=as.numeric(reverse.code(-1,IDX.dat_sub$PCT_C17002007))
# IDX.dat_sub$PCT_B07204007=reverse.code(-1,IDX.dat_sub$PCT_B07204007)
# IDX.dat_sub$B19013001 = (IDX.dat_sub$B19013001 /1000)
# IDX.dat_sub$PerWorkerWageIncome = (IDX.dat_sub$PerWorkerWageIncome /1000)
# IDX.dat_sub=na.omit(IDX.dat_sub)
glimpse(IDX.dat_sub)
IDX.dat_sub=(as.data.frame(IDX.dat_sub))
insp_names = IDX.dat_sub[,1:2]
IDX.matrix = (IDX.dat_sub[,-c(1:2)])

# IDX.matrix=subset(IDX.dat_sub, select = -c(FIPS,OZ))
# IDX.dat_sub=IDX.dat_sub[!is.na(IDX.dat_sub$EPOP2554),]
# IDX.dat_sub=IDX.dat_sub[!is.na(IDX.dat_sub$UnemploymentRate),]
# IDX.dat_sub=IDX.dat_sub[!is.na(IDX.dat_sub$bus_vac_prop),]
# IDX.dat_sub=IDX.dat_sub[!is.na(IDX.dat_sub$res_vac_prop),]
# IDX.dat_sub=IDX.dat_sub[!is.na(IDX.dat_sub$VacancyRate),]

# IDX.matrix_alt=as.matrix(IDX.matrix[c('bus_vac_prop','EPOP2554','UnemploymentRate')])

# IDX.matrix=scales::rescale(IDX.matrix,to=c(0,1))
IDX.matrix=scale(as.matrix(IDX.matrix[c('bus_prop','res_vac_prop','bus_vac_prop','EPOP2554','UnemploymentRate')]))
# IDX.matrix=scales::rescale(IDX.matrix, to = c(0, 10))

# Correlation Matrix
round(cor(IDX.matrix),2)
corrplot(cor(IDX.matrix), order = "original", tl.col='black', tl.cex=.75) 
colnames(IDX.matrix)

#### Confirmatory Factor Analysis #### 
# https://quantdev.ssri.psu.edu/tutorials/intro-basic-confirmatory-factor-analysis
# http://sachaepskamp.com/files/SEM12017/SEM1Week1.pdf
# http://sachaepskamp.com/files/SEM12017/SEM1Week2.pdf

LBM_1factor_2 <- ' #start of model

# latent variable definitions (common factors)
	LaborMarket =~ EPOP2554 + UnemploymentRate
			+ bus_vac_prop
			# + bus_prop
			+ res_vac_prop
			# + VacancyRate
			# + PerCapitaIncome
			# + B19013001

# latent variable variances
	LaborMarket ~~ 1 * LaborMarket

# latent variable covariances

# latent variable means
	LaborMarket ~ 0

# manifest variable variances (uniquenesses)
	# PerCapitaIncome ~~ 1 * PerCapitaIncome
	bus_vac_prop ~~ 1 * bus_vac_prop
	res_vac_prop ~~ 1 * res_vac_prop
	UnemploymentRate ~~ 1 * UnemploymentRate
	EPOP2554 ~~ 1 *EPOP2554
	# B19013001 ~~ 1 * B19013001
	# VacancyRate ~~ 1 * VacancyRate
	bus_prop ~~ 1* bus_prop

# manifest variable covariances (uniquenesses)
  
	# UnemploymentRate ~~ EPOP2554

	# VacancyRate  ~~  UnemploymentRate
	# VacancyRate ~~ EPOP2554

	# PerCapitaIncome  ~~  UnemploymentRate
	# PerCapitaIncome ~~ EPOP2554
	# PerCapitaIncome ~~ bus_vac_prop
	# PerCapitaIncome  ~~  B19013001
	bus_prop ~~ UnemploymentRate
	bus_prop ~~ EPOP2554
	# bus_prop ~~ bus_vac_prop

	# bus_vac_prop ~~ PerCapitaIncome
	bus_vac_prop  ~~  UnemploymentRate
	bus_vac_prop ~~ EPOP2554

	res_vac_prop ~~ EPOP2554
	res_vac_prop ~~ UnemploymentRate
	res_vac_prop ~~ bus_vac_prop

#manifest variable means 
	# PerCapitaIncome ~ 0 
	# VacancyRate ~ 0
	bus_prop ~ 0
	bus_vac_prop ~ 0
	EPOP2554 ~ 0
	UnemploymentRate ~ 0
	res_vac_prop ~ 0


' #end of model

fit_2v <- cfa(LBM_1factor_2, data=IDX.matrix, 
           std.lv=TRUE,  
           missing="fiml")
summary(fit_2v, standardized=TRUE, fit.measures=TRUE)
semPaths(fit_2v, what="std",  sizeLat = 10, sizeMan = 8, edge.label.cex = 1)
inspect(fit_2v,what="std")$lambda



#Model with 1 common factor 
LBM_1factor <- ' #start of model

# latent variable definitions (common factors)
	LaborMarket =~ EPOP2554 + UnemploymentRate
			+ bus_vac_prop
			# + bus_prop
			# + res_vac_prop
			# + VacancyRate
			# + PerCapitaIncome
			# + B19013001

# latent variable variances
	LaborMarket ~~ 1 * LaborMarket

# latent variable covariances

# latent variable means
	LaborMarket ~ 0

# manifest variable variances (uniquenesses)
	# PerCapitaIncome ~~ 1 * PerCapitaIncome
	bus_vac_prop ~~ 1 * bus_vac_prop
	# res_vac_prop ~~ 1 * res_vac_prop
	UnemploymentRate ~~ 1 * UnemploymentRate
	EPOP2554 ~~ 1 *EPOP2554
	# B19013001 ~~ 1 * B19013001
	# VacancyRate ~~ 1 * VacancyRate
	bus_prop ~~ 1* bus_prop

# manifest variable covariances (uniquenesses)
  
	# UnemploymentRate ~~ EPOP2554

	# VacancyRate  ~~  UnemploymentRate
	# VacancyRate ~~ EPOP2554

	# B19013001 ~~ EPOP2554
	# B19013001 ~~ UnemploymentRate
	# B19013001 ~~ bus_vac_prop
	
	# B19013001 ~~ res_vac_prop

	# PerCapitaIncome  ~~  UnemploymentRate
	# PerCapitaIncome ~~ EPOP2554
	# PerCapitaIncome ~~ bus_vac_prop
	# PerCapitaIncome  ~~  B19013001
	bus_prop ~~ UnemploymentRate
	bus_prop ~~ EPOP2554
	# bus_prop ~~ bus_vac_prop

	# bus_vac_prop ~~ PerCapitaIncome
	bus_vac_prop  ~~  UnemploymentRate
	bus_vac_prop ~~ EPOP2554

	# res_vac_prop ~~ res_vac_prop

#manifest variable means 
	# PerCapitaIncome ~ 0 
	# VacancyRate ~ 0
	bus_prop ~ 0
	bus_vac_prop ~ 0
	EPOP2554 ~ 0
	UnemploymentRate ~ 0

' #end of model


fit <- cfa(LBM_1factor, data=IDX.matrix, 
           std.lv=TRUE,  
           missing="fiml")
summary(fit, standardized=TRUE, fit.measures=TRUE)
semPaths(fit, what="std",  sizeLat = 10, sizeMan = 8, edge.label.cex = 1)
inspect(fit,what="std")$lambda


LBM_alt <- ' #start of model
# latent variable definitions (common factors)
	1*LaborMarket =~ 1*EPOP2554 + 1*UnemploymentRate + 1*bus_vac_prop
' #end of model

lbm_alt_fit <- cfa(LBM_alt, data=IDX.matrix, 
	mimic = "mplus",
	missing="fiml",
	std.lv=TRUE)
summary(lbm_alt_fit, standardized=TRUE, fit.measures=TRUE)
semPaths(lbm_alt_fit, what="std",  sizeLat = 10, sizeMan = 8, edge.label.cex = 1)
inspect(lbm_alt_fit,what="std")$lambda

# Assign Factor Loadings to Index Weights

# var_insp=as.data.frame(IDX.dat_sub[c('FIPS','OZ','bus_vac_prop','EPOP2554','UnemploymentRate','PovIntensity','AverageSchooling')])
# 
# var_insp$bus_vac_prop =scales::rescale(var_insp$bus_vac_prop, to = c(0, 10))
# var_insp$UnemploymentRate =scales::rescale(var_insp$UnemploymentRate, to = c(0, 10))
# var_insp$EPOP2554 =scales::rescale(var_insp$EPOP2554, to = c(0, 10))
# var_insp$EPOP2554 =scales::rescale(var_insp$EPOP2554, to = c(0, 10))

var_insp=as.data.frame(IDX.matrix)
var_insp$labor_index=(
			    (var_insp$EPOP2554         *    0.435)+
			    (var_insp$UnemploymentRate *    0.440)+
			    (var_insp$bus_vac_prop     * 0.122   ) )

#                  LbrMrk
# EPOP2554          0.434
# UnemploymentRate  0.440
# bus_vac_prop      0.132
# bus_prop          0.000

summary(var_insp$labor_index)

var_insp=as.matrix(var_insp)
var_insp=as.data.frame(var_insp)
var_test=cbind(insp_names,var_insp)
glimpse(var_test)

var_test=subset(var_test, select = c(FIPS,labor_index))
test_df=subset(IDX.dat_sub, select = c(FIPS, PovIntensity, AverageSchooling))
any(duplicated(test_df$FIPS))
test_df=merge(var_test,test_df, by = 'FIPS', all = T, sort=F)
openxlsx::write.xlsx(test_df, "~/Desktop/Welfare_Policy/Struggling_Regions/Index/test_index_df.xlsx")

#### Exploratory Factor Analysis #### #

# https://www.statmethods.net/advstats/factor.html
# https://www.r-bloggers.com/exploratory-factor-analysis-in-r/
parallel <- fa.parallel(IDX.matrix, fm = 'minres', fa = 'fa')
# parallel <- fa.parallel(IDX_samp, fm = 'minres', fa = 'fa')

onefactor_oblimin <- fa(IDX.matrix,nfactors = 1,rotate = "oblimin",fm="minres")
print(onefactor_oblimin)

onefactor_varimax <- fa(IDX.matrix,nfactors = 1,rotate = "varimax",fm="minres")
print(onefactor_varimax)

twofactor <- fa(IDX.matrix,nfactors = 2,rotate = "oblimin",fm="minres")
print(twofactor)

# threefactor <- fa(IDX.matrix,nfactors = 3,rotate = "varimax",fm="minres")
# print(threefactor)
# 
fourfactor <- fa(IDX.matrix,nfactors = 4,rotate = "oblimin",fm="minres")
print(fourfactor$loadings,cutoff = 0.3)

glimpse(IDX.matrix)

fit <- factanal(IDX.matrix, 2, rotation="varimax")
print(fit, digits=2, cutoff=.3, sort=TRUE)
# plot factor 1 by factor 2 
load <- fit$loadings[,1:2] 
plot(load,type="n") # set up plot 
