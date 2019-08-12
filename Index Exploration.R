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

rm(list=ls());gc()
OZ_dat=readstata13::read.dta13("~/Desktop/Welfare_Policy/Struggling Regions/Cluster Analyses/cluster_inputs/languagesupplement.dta")[c(1,5)]
HUD.IDX=readxl::read_excel("~/Desktop/Welfare_Policy/Struggling Regions/Index/Raw Data/IDX.HUD.xlsx")

# IDX.dat = readstata13::read.dta13("~/Desktop/Welfare_Policy/Struggling Regions/Index/Raw Data/IndexData_Full.dta") #

#2008-2012
IDX.p1 = readstata13::read.dta13("~/Desktop/Welfare_Policy/Struggling Regions/Index/Raw Data/2008_2012_ACS.dta") #
# IDX.12.p2 = readstata13::read.dta13("~/Desktop/Welfare_Policy/Struggling Regions/Index/Raw Data/2008_2012_supplement.dta") #
# IDX.12.p3 = readstata13::read.dta13("~/Desktop/Welfare_Policy/Struggling Regions/Index/Raw Data/2008_2012_Supplement2.dta") #
# IDX.12=list(IDX.12.p1,IDX.12.p2,IDX.12.p3) %>% reduce(full_join, by = "FIPS")

#2013-2017
IDX.p2 = readstata13::read.dta13("~/Desktop/Welfare_Policy/Struggling Regions/Index/Raw Data/2013_2017_ACS.dta")
# IDX.17.p2 = readstata13::read.dta13("~/Desktop/Welfare_Policy/Struggling Regions/Index/Raw Data/2013_2017_supplement.dta")
# IDX.17.p3 = readstata13::read.dta13("~/Desktop/Welfare_Policy/Struggling Regions/Index/Raw Data/2013_2017_Supplement2.dta")
# IDX.17=list(IDX.17.p1,IDX.17.p2,IDX.17.p3) %>% reduce(full_join, by = "FIPS")
IDX.sup1 = readstata13::read.dta13('/Users/rorr/Desktop/Welfare_Policy/Struggling Regions/Index/Raw Data/IncomeSupplement.dta')
IDX.sup1=subset(IDX.sup1, select = -c(merge1,`_merge`))
IDX.sup2 = readstata13::read.dta13('~/Desktop/Welfare_Policy/Struggling Regions/Index/Raw Data/enrollmentdata.dta')
IDX.sup3 = readstata13::read.dta13("~/Desktop/Welfare_Policy/Struggling Regions/Index/Raw Data/professions.dta")

IDX.labels_1 = attr(IDX.p1,"var.labels")
# IDX.17.labels_2 = attr(IDX.12.p2,"var.labels")
# IDX.17.labels_3 = attr(IDX.12.p3,"var.labels")
IDX.labels_2 = attr(IDX.p2,"var.labels")
# IDX.17.labels_2 = attr(IDX.17.p2,"var.labels")
# IDX.17.labels_3 = attr(IDX.17.p3,"var.labels")
IDX.sup2.labels = attr(IDX.sup2,"var.labels")
IDX.sup3.labels = attr(IDX.sup3,"var.labels")

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
IDXsup_vars=cbind(var.label=IDX.sup2,var.name=names(IDX.sup2))

IDX.labels=merge(IDX1_vars,IDX2_vars, by = 'var.name', all = T, sort = F)
# openxlsx::write.xlsx(IDX.labels,'~/Downloads/IDX.labels.xlsx')
IDX.labels$var.label_p2=NULL

IDX.dat=rbind(IDX.p1,IDX.p2)
IDX.dat$St_Code=stringi::stri_sub(IDX.dat$FIPS, from=1, to=2)
IDX.dat$admin_type=ifelse(as.numeric(IDX.dat$St_Code) <= 56, 1, 0) 
IDX.dat=subset(IDX.dat, IDX.dat$admin_type != 0, select = -c(admin_type,St_Code,`_merge`,merge1))
IDX.dat=merge(IDX.dat,IDX.sup1, by = c('FIPS', "Year"), all.x = T, sort = F)

IDX.dat$FIPS= ifelse(IDX.dat$FIPS == "51515050100", "51019050100",
       ifelse(IDX.dat$FIPS == "02270000100", "02158000100",
       ifelse(IDX.dat$FIPS == "46113940500", "46102940500",
       ifelse(IDX.dat$FIPS == "46113940800", "46102940800",
       ifelse(IDX.dat$FIPS == "46113940900", "46102940900",IDX.dat$FIPS)))))

OZ_dat$FIPS= ifelse(OZ_dat$FIPS == "51515050100", "51019050100",
       ifelse(OZ_dat$FIPS == "02270000100", "02158000100",
       ifelse(OZ_dat$FIPS == "46113940500", "46102940500",
       ifelse(OZ_dat$FIPS == "46113940800", "46102940800",
       ifelse(OZ_dat$FIPS == "46113940900", "46102940900",OZ_dat$FIPS)))))

HUD.IDX$FIPS= ifelse(HUD.IDX$FIPS == "51515050100", "51019050100",
       ifelse(HUD.IDX$FIPS == "02270000100", "02158000100",
       ifelse(HUD.IDX$FIPS == "46113940500", "46102940500",
       ifelse(HUD.IDX$FIPS == "46113940800", "46102940800",
       ifelse(HUD.IDX$FIPS == "46113940900", "46102940900",HUD.IDX$FIPS)))))


IDX.dat=merge(OZ_dat,IDX.dat, by = 'FIPS', all.y = T, sort = F)

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


IDX.dat=merge(IDX.dat,HUD.IDX, by = c('FIPS','Year'), all.x = T, sort = F)


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
	Year==2012 &
	B01003001 > 100)
IDX.dat_names = IDX.dat[,1]
# names(as.vector(IDX.dat))

IDX.dat$PovIntensity = ((
  IDX.dat$PCT_C17002002 * 6  #      Population for Whom Poverty Status Is Determined: Under .50
+ IDX.dat$PCT_C17002003 * 5  #     Population for Whom Poverty Status Is Determined: .50 to .99
+ IDX.dat$PCT_C17002004 * 4  #   Population for Whom Poverty Status Is Determined: 1.00 to 1.24
+ IDX.dat$PCT_C17002005 * 3 # Population for Whom Poverty Status Is Determined: 1.25 to 1.49
+ IDX.dat$PCT_C17002006 * 2 # Population for Whom Poverty Status Is Determined: 1.50 to 1.84
+ IDX.dat$PCT_C17002007 * 1)  #   Population for Whom Poverty Status Is Determined: 1.85 to 1.99
	/ 21) 

missing_oz=subset(IDX.dat,is.na(OZ))
missing_oz$FIPS

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
		,bus_prop
		# ,PerWorkerWageIncome
		# ,Density
		# ,PerCapitaIncome
		# ,PovIntensity
		,Year
))
# missings_IDX=as.data.frame(IDX.dat_sub) %>% summarise_all(funs(sum(is.na(.)))) %>% gather()
# print(missings_IDX, n =50)
glimpse(IDX.dat_sub)

IDX.dat_sub$VacancyRate=as.numeric(reverse.code(-1,IDX.dat_sub$VacancyRate))
IDX.dat_sub$pov_200=as.numeric(reverse.code(-1,IDX.dat_sub$pov_200))
IDX.dat_sub$pov_100=as.numeric(reverse.code(-1,IDX.dat_sub$pov_100))
IDX.dat_sub$UnemploymentRate=as.numeric(reverse.code(-1,IDX.dat_sub$UnemploymentRate))
IDX.dat_sub$median_housing_age=as.numeric(reverse.code(-1,IDX.dat_sub$median_housing_age))
IDX.dat_sub$PovIntensity=as.numeric(reverse.code(-1,IDX.dat_sub$PovIntensity))

IDX.dat_sub$PCT_C17002002=as.numeric(reverse.code(-1,IDX.dat_sub$PCT_C17002002))
IDX.dat_sub$PCT_C17002003=as.numeric(reverse.code(-1,IDX.dat_sub$PCT_C17002003))
IDX.dat_sub$PCT_C17002004=as.numeric(reverse.code(-1,IDX.dat_sub$PCT_C17002004))
IDX.dat_sub$PCT_C17002005=as.numeric(reverse.code(-1,IDX.dat_sub$PCT_C17002005))
IDX.dat_sub$PCT_C17002006=as.numeric(reverse.code(-1,IDX.dat_sub$PCT_C17002006))
IDX.dat_sub$PCT_C17002007=as.numeric(reverse.code(-1,IDX.dat_sub$PCT_C17002007))
# IDX.dat_sub$PCT_B07204007=reverse.code(-1,IDX.dat_sub$PCT_B07204007)
# IDX.dat_sub$B19013001 = (IDX.dat_sub$B19013001 /1000)
# IDX.dat_sub$PerWorkerWageIncome = (IDX.dat_sub$PerWorkerWageIncome /1000)
IDX.dat_sub=na.omit(IDX.dat_sub)
glimpse(IDX.dat_sub)
IDX.dat_sub=(as.data.frame(IDX.dat_sub))
insp_names = IDX.dat_sub[,1:2]
IDX.matrix = as.matrix(IDX.dat_sub[,-c(1:2)])

# IDX.matrix=subset(IDX.dat_sub, select = -c(FIPS,OZ))
IDX.matrix=as.matrix(IDX.matrix)
# IDX.matrix=scales::rescale(IDX.matrix,to=c(0,1))
IDX.matrix=scale(IDX.matrix)

# Correlation Matrix
round(cor(IDX.matrix),2)
corrplot(cor(IDX.matrix), order = "original", tl.col='black', tl.cex=.75) 
colnames(IDX.matrix)

#### Exploratory Factor Analysis #### 

# https://www.statmethods.net/advstats/factor.html
# https://www.r-bloggers.com/exploratory-factor-analysis-in-r/
parallel <- fa.parallel(IDX.matrix, fm = 'minres', fa = 'fa')
parallel <- fa.parallel(IDX_samp, fm = 'minres', fa = 'fa')

onefactor_oblimin <- fa(IDX.matrix,nfactors = 1,rotate = "oblimin",fm="minres")
print(onefactor_oblimin)

onefactor_varimax <- fa(IDX.matrix,nfactors = 1,rotate = "varimax",fm="minres")
print(onefactor_varimax)

twofactor <- fa(IDX.matrix,nfactors = 2,rotate = "oblimin",fm="minres")
print(twofactor)

# threefactor <- fa(IDX.matrix,nfactors = 3,rotate = "varimax",fm="minres")
# print(threefactor)
# 
# fourfactor <- fa(IDX.matrix,nfactors = 4,rotate = "oblimin",fm="minres")
# print(fourfactor$loadings,cutoff = 0.3)

glimpse(IDX.matrix)

fit <- factanal(IDX.matrix, 2, rotation="varimax")
print(fit, digits=2, cutoff=.3, sort=TRUE)
# plot factor 1 by factor 2 
load <- fit$loadings[,1:2] 
plot(load,type="n") # set up plot 


#### Confirmatory Factor Analysis #### 
# https://quantdev.ssri.psu.edu/tutorials/intro-basic-confirmatory-factor-analysis
# http://sachaepskamp.com/files/SEM12017/SEM1Week1.pdf
# http://sachaepskamp.com/files/SEM12017/SEM1Week2.pdf


#Model with 0 common factors 
OPI_0factor <- ' #start of model

# latent variable definitions (common factors)
# latent variable variances
# latent variable covariances
# latent variable means

# manifest variable variances (uniquenesses)
  UnemploymentRate ~~ UnemploymentRate
  PovIntensity ~~ PovIntensity
  EPOP2554 ~~ EPOP2554

# manifest variable covariances (uniquenesses)
  EPOP2554 ~~ PovIntensity

#manifest variable means 

' #end of model

fit0 <- lavaan(OPI_0factor, data=IDX.matrix, mimic = "mplus", model.type = "cfa", auto.fix.first = TRUE, auto.var = TRUE)
summary(fit0, standardized=TRUE, fit.measures=TRUE)
varTable(fit0)
semPaths(fit0, what="std",  sizeLat = 7, sizeMan = 7, edge.label.cex = .75)

#Model with 1 common factor 

LBM_1factor <- ' #start of model

# latent variable definitions (common factors)
  LaborMarket =~ EPOP2554 + UnemploymentRate +	PerWorkerWageIncome

# latent variable variances
  LaborMarket ~~ 1*LaborMarket

# latent variable covariances

# latent variable means

# manifest variable variances (uniquenesses)
  PerWorkerWageIncome ~~ PerWorkerWageIncome
  UnemploymentRate ~~ UnemploymentRate
  EPOP2554 ~~ EPOP2554

# manifest variable covariances (uniquenesses)
  PerWorkerWageIncome  ~~  UnemploymentRate
  PerWorkerWageIncome ~~ EPOP2554

#manifest variable means 


' #end of model

lbm_fit1 <- lavaan(LBM_1factor, data=IDX.matrix, mimic = "mplus")
summary(lbm_fit1, standardized=TRUE, fit.measures=TRUE)
semPaths(lbm_fit1, what="std",  sizeLat = 7, sizeMan = 7, edge.label.cex = .75)
inspect(lbm_fit1,what="std")$lambda

#Model with 1 common factor 

HC_1factor <- ' #start of model

# latent variable definitions (common factors)
	HumanCapital =~ AverageSchooling +
		B19013001

# latent variable variances
  HumanCapital ~~ 1*HumanCapital

# latent variable covariances
# latent variable means

# manifest variable variances (uniquenesses)
AverageSchooling ~~ AverageSchooling
B19013001  ~~  B19013001

# manifest variable covariances (uniquenesses)
AverageSchooling ~~ B19013001

#manifest variable means 
' #end of model

fit1 <- lavaan(HC_1factor, data=IDX.matrix, mimic = "mplus", model.type = "cfa", auto.fix.first = TRUE, auto.var = TRUE
)
summary(fit1, standardized=TRUE, fit.measures=TRUE)
semPaths(fit1, what="std",  sizeLat = 7, sizeMan = 7, edge.label.cex = .75)
inspect(fit1,what="std")$lambda

#### 2-factor CFA Model ####
OPP_2factor <- ' #start of model

# latent variable definitions (common factors)
score =~ LaborMarket + HumanCapital
	LaborMarket =~ EPOP2554 + UnemploymentRate + PerWorkerWageIncome
	Mobility =~ PCT_B07204007 + VacancyRate + median_housing_age
	
# latent variable variances
  LaborMarket ~~ LaborMarket
  Mobility ~~ Mobility

# latent variable covariances
  LaborMarket ~~ Mobility

# latent variable means

# manifest variable variances (uniquenesses)
  PerWorkerWageIncome ~~ PerWorkerWageIncome
  AverageSchooling ~~ AverageSchooling
  UnemploymentRate ~~ UnemploymentRate
  EPOP2554 ~~ EPOP2554
  PCT_B07204007 ~~ PCT_B07204007 
  VacancyRate ~~ VacancyRate 
  median_housing_age ~~ median_housing_age

# manifest variable covariances (uniquenesses)
  UnemploymentRate ~~ PerWorkerWageIncome
  EPOP2554 ~~ PerWorkerWageIncome
    
  UnemploymentRate ~~ PerWorkerWageIncome
  PCT_B07204007 ~~ median_housing_age
  PCT_B07204007 ~~ VacancyRate
  median_housing_age ~~ VacancyRate

#manifest variable means 
  PerWorkerWageIncome ~~ PerWorkerWageIncome 
  AverageSchooling ~~ AverageSchooling 
  UnemploymentRate ~~ UnemploymentRate 
  EPOP2554 ~~ EPOP2554 
  PCT_B07204007 ~~ PCT_B07204007 
  VacancyRate ~~ VacancyRate 
  median_housing_age ~~ median_housing_age 
' #end of model

fit2 <- lavaan(OPP_2factor, data=IDX.matrix, mimic = "mplus")
summary(fit2, standardized=TRUE, fit.measures=TRUE)
semPaths(fit2, what="std",  sizeLat = 7, sizeMan = 7, edge.label.cex = .75)
inspect(fit2,what="std")$lambda

#### 3-factor CFA Model ####
OPP_3factor <- ' #start of model

# latent variable definitions (common factors)
score =~ LaborMarket + HumanCapital
	LaborMarket =~ EPOP2554 + UnemploymentRate + PerWorkerWageIncome
	HumanCapital =~ AverageSchooling + PerWorkerWageIncome
	Mobility =~ PCT_B07204007 + VacancyRate + median_housing_age

# latent variable variances
  LaborMarket ~~ LaborMarket
  HumanCapital ~~ HumanCapital
  Mobility ~~ Mobility

# latent variable covariances
  LaborMarket ~~ HumanCapital
  LaborMarket ~~ Mobility

# latent variable means

# manifest variable variances (uniquenesses)
  PerWorkerWageIncome ~~ PerWorkerWageIncome
  AverageSchooling ~~ AverageSchooling
  UnemploymentRate ~~ UnemploymentRate
  EPOP2554 ~~ EPOP2554

# manifest variable covariances (uniquenesses)
  AverageSchooling ~~ PerWorkerWageIncome
  EPOP2554 ~~ PerWorkerWageIncome
  PCT_B07204007 
  VacancyRate
  median_housing_age


#manifest variable means 
  PerWorkerWageIncome ~ 1
  AverageSchooling ~ 1
  UnemploymentRate ~ 1
  EPOP2554 ~ 1

' #end of model

fit3 <- lavaan(OPP_3factor, data=IDX.matrix, mimic = "mplus")
summary(fit3, standardized=TRUE, fit.measures=TRUE)
semPaths(fit3, what="std",  sizeLat = 7, sizeMan = 7, edge.label.cex = .75)
inspect(fit3,what="std")$lambda

OPI_CFA <- ' #start of model
# Score =~LaborMarket+HumanCapital
	LaborMarket =~ EPOP2554 +
		UnemploymentRate 
	HumanCapital =~ AverageSchooling +
		PerWorkerWageIncome
' #end of model

OPI_CFA <- ' #start of model
	LaborMarket =~ EPOP2554 +
		UnemploymentRate +
		PerWorkerWageIncome
' #end of model

OPI_CFA <- ' #start of model
		LaborMarket =~ EPOP2554 +
		UnemploymentRate +
		PerWorkerWageIncome +
		B19013001 
' #end of model

colnames(IDX.matrix)
OPI_CFA <- ' #start of model
		OPI =~ 
		# B19013001 +         
		# B25077001 +           # Owner-Occupied Housing Units: Median Value (Dollars)
		UnemploymentRate +   
		VacancyRate +        
		# PCT_C17002002 +      
		# PCT_C17002003 +      
		# PCT_C17002004 +      
		# PCT_C17002005 +      
		# PCT_C17002006 +      
		# PCT_C17002007 +      
		# AverageSchooling +   
		# PCT_B07204007 +      
		# Density +            
		# PerCapitaIncome +    
		# PerWorkerWageIncome +
		# median_housing_age + 
		pov_100 +            
		# pov_200 +            
		# PovIntensity +                
		EPOP2554          
 #end of model'

fit <- cfa(OPI_CFA, IDX.matrix)
summary(fit, standardized=TRUE, fit.measures=TRUE)
semPaths(fit, what="std",  sizeLat = 7, sizeMan = 7, edge.label.cex = .75)
lambda_scores=inspect(fit,what="std")$lambda
lambda_scores^2

modindices(fit)

var_insp=as.data.frame(IDX.matrix)
var_insp$labor_index=(((var_insp$EPOP2554       *      0.671)+
			    (var_insp$UnemploymentRate *    0.713)+
			    (var_insp$PerWorkerWageIncome * 0.553)))

var_insp$labor_index=(
			    (var_insp$EPOP2554       *      0.639)+
			    (var_insp$UnemploymentRate *    0.724)+
			    (var_insp$pov_100 * 0.482) +
			    (var_insp$VacancyRate * 0.368) * 100)

var_insp=as.matrix(var_insp)
# var_insp['labor_index']=scales::rescale(var_insp['labor_index'],to=c(0,1))
var_insp=as.data.frame(var_insp)
var_test=cbind(insp_names,var_insp)
glimpse(var_test)

test_df=subset(var_test, select = c(FIPS,OZ,labor_index))
openxlsx::write.xlsx(test_df, "~/Desktop/Welfare_Policy/Struggling Regions/Index/test_df.xlsx")

