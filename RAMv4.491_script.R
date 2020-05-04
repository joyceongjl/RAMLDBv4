###################################################################################
###################################################################################
###################################################################################
#
# This will load database data from the file DBdata.RData. Put the data file in the
# working directory, then run the line at the bottom of the file.
#
###################################################################################
###################################################################################
###################################################################################
#
# The following objects are tables from RAM (data frames):
#
# --- metadata
#	Summarized metadata
# --- stock
#	General stock metadata
# --- assessment
#	General assessment metadata
# --- taxonomy
#	Taxonomic metadata
# --- management
#	Management authority metadata
# --- assessor
#	Stock assessor metadata
# --- assessmethod
#	Assessment method metadata
# --- area
#	Area metadata
# --- biometrics
#	Parameter data types with descriptions
# --- tsmetrics
#	Time series data types with descriptions
# --- timeseries
#	Full time series data listing
# --- bioparams
#	Full parameter data listing
# --- timeseries_values_views
#	Values by stock and year of common time series types
# --- timeseries_units_views
#	Units corresponding to values in timeseries_values_views
# --- timeseries_ids_views
#	Time series IDs corresponding to values in timeseries_values_views
# --- timeseries_assessments_views
#	Assessment IDs corresponding to values in timeseries_values_views
# --- timeseries_notes_views
#	Notes corresponding to values in timeseries_values_views
# --- timeseries_sources_views
#	Sources corresponding to values in timeseries_values_views
# --- timeseries_years_views
#	Year range corresponding to values in timeseries_values_views
# --- bioparams_values_views
#	Values by stock of common parameter types
# --- bioparams_units_views
#	Units corresponding to values in bioparams_values_views
# --- bioparams_ids_views
#	Parameter IDs corresponding to values in bioparams_values_views
# --- bioparams_assessments_views
#	Assessment IDs corresponding to values in bioparams_values_views
# --- bioparams_sources_views
#	Sources corresponding to values in bioparams_values_views
# --- bioparams_notes_views
#	Notes corresponding to values in bioparams_values_views
#
# ---------------------------------------------------------------------------------------------------
#
# There are also dataframes for the individual most-used time series:
#
# --- tb.data --- Total biomass data
# --- ssb.data --- Spawning stock biomass data
# --- tn.data --- Total abundance data
# --- r.data --- Recruits data
# --- tc.data --- Total catch data
# --- tl.data --- Total landings data
# --- recc.data --- Recreational catch data
# --- f.data --- Fishing mortality data (usually an instantaneous rate)
# --- er.data --- Exploitation rate data (usually an annual fraction harvested)
# --- divtb.data --- TB/TBmsy data
# --- divssb.data --- SSB/SSBmsy data
# --- divf.data --- F/Fmsy data
# --- diver.data --- ER/ERmsy data
# --- divbpref.data --- B/Bmsy pref data (B/Bmsy if available, otherwise B/Bmgt)
# --- divupref.data --- U/Umsy pref data (U/Umsy if available, otherwise U/Umgt)
# --- tbbest.data --- TBbest data (all in MT)
# --- tcbest.data --- TCbest data (all in MT)
# --- erbest.data --- ERbest data (usually an annual fraction harvested)
# --- divtb.mgt.data --- TB/TBmgt data
# --- divssb.mgt.data --- SSB/SSBmgt data
# --- divf.mgt.data --- F/Fmgt data
# --- diver.mgt.data --- ER/ERmgt data
# --- divbpref.mgt.data --- B/Bmgt pref data (B/Bmgt if available, otherwise B/Bmsy)
# --- divupref.mgt.data --- U/Umgt pref data (U/Umgt if available, otherwise U/Umsy)
# --- cpair.data --- Catch data that pairs with tac.data and/or cadv.data
# --- tac.data --- TAC data
# --- cadv.data --- Scientific advice for catch limit data
# --- survb.data --- Fishery-independent survey abundance data
# --- cpue.data --- CPUE data (fishery-dependent)
# --- effort.data --- Fishing effort data (fishery-dependent)
# --- divtn.data --- TN/TNmsy data
# --- divtn.mgt.data --- TN/TNmgt data
# --- cdivmeanc.data --- Catch/(mean catch) data
# --- cdivmsy.data --- Catch/MSY data
#
###################################################################################
###################################################################################
###################################################################################
#
# Once the DBdata.RData file is in the working directory, simply run the following command to
# load up the database data into matrix/dataframe files for the assessment only version of the database.
library(dplyr)

load("DBdata[asmt][v4.491].RData")
####Finding timeseries with TC, TB and F for global MS revision
#load(file="D:/Rutgers_postdoc/data/RAM legacy/RAM_v4.491/RAMLDB v4.491/DB Files With Assessment Data/R Data/DBdata[asmt][v4.491].RData")
str(tbbest.data)#219 obs (1800-2018) of 471 variables (stocks)
tbbest.data[1:5,1:7]
rownames(tbbest.data)#for 1955-2014, I want row 156-215. Or should this be 1950-2018 [151:219,]
tbbest1950.2018<-tbbest.data[151:219,]
str(tbbest1950.2018)#filter so that # NAs per column is less than or equal to 20? 69 obs/yrs of 471 vars/stocks
#there are 69 possible yrs of data, FAO has 60 years of data, so longest ts is 20 years.
#remove columns that are entirely NAs
df[colSums(!is.na(df)) > 0]
df[, colSums(is.na(df)) < nrow(df) * 0.5]#to keep columns with at least 50% non blanks
length(which(colSums(!is.na(tbbest1950.2018)) > 0))#none of the columns are entirely NAs
length(which(colSums(is.na(tbbest1950.2018)) < 31))#only 156 stocks have <21 NAs, 179 <26 NAs,  228 stocks < 31 NAs
#Perhaps keep it to 228 stocks first, then subset to the 3 hotspot.
tb1950.2018sub<-tbbest1950.2018[,colSums(is.na(tbbest1950.2018)) < 31]
#69 obs/yrs of 228 vars/stocks. 

#from metadata, I want stockid,  primary_FAOarea and scientificname
colnames(metadata)
stk_fao_sp_key<-metadata[,c(2,13,5)]
str(stk_fao_sp_key)

tb.69ts<-as.data.frame(t(tb1950.2018sub))
tb.69ts$stockid<-rownames(tb.69ts)
#left join? with key to get fao regions and sp names
tb.69tsfaosp<-tb.69ts %>% left_join(stk_fao_sp_key, by="stockid")
length(which(is.na(tb.69tsfaosp$primary_FAOarea)))
str(tb.69tsfaosp)
tb.69tsfaosp$fao_sp<-paste(tb.69tsfaosp$primary_FAOarea,tb.69tsfaosp$scientificname,sep="_")
write.csv(tb.69tsfaosp, "biomass_fao_sp_1950-2018.csv")

## Subsetting biomass data for IOE (57)
ioe.tb.69ts<-tb.69tsfaosp %>% filter(primary_FAOarea=="57")#only 8 stocks with biomass time series
#if I choose the years 1968-2011, minimal NAs, can put in the median value for those NAs, 44yr ts, up to 14yr ts
#try 1965-2014 instead, 50 year time series. 
#perhaps tb.data will have more timeseries? Ans: No.  

#####trial of using tb.data instead of tbbest.data
str(tb.data)#219 obs (1800-2018) of 471 variables (stocks)
rownames(tb.data)#for 1955-2014, I want row 156-215. Or should this be 1950-2018 [151:219,]
tb1950.2018<-tb.data[151:219,]
str(tb1950.2018)#69 obs/yrs of 495 vars/stocks
length(which(colSums(is.na(tb1950.2018)) < 31))#231 stocks. Not much difference. 
#####tb.data is not much better at all. 

#for ioe, choose years from 1965-2014[,16:65] and fao_sp[,73]
str(ioe.tb.69ts)
colnames(ioe.tb.69ts)
ioe.tb.50ts<-ioe.tb.69ts[,c(16:65,70)]
rownames(ioe.tb.50ts)<-ioe.tb.50ts[,51]
str(ioe.tb.50ts)#df of 8 stocks over 50 years
ioe.tb.50yr.mat<-as.matrix(ioe.tb.50ts[,1:50])

#replace NAs in matrix with rowMedians.
#indx <- which(is.na(ioe.tb.50yr.mat), arr.ind = TRUE)#create index of NAs first
#use apply over rows, rowMed below is to check
rowMed<-matrixStats::rowMedians(ioe.tb.50yr.mat, na.rm = TRUE)
#ioe.tb.50yr.mat[,51]<-rowMed#did not work
ioe.tb.50yr.mat2<-apply(ioe.tb.50yr.mat, 1, function (x) {
  ifelse(is.na(x), median(x, na.rm=T), x)
})
ioe.tb.50yr.mat2<-t(ioe.tb.50yr.mat2)#worked


##subsetting biomass data for ANE (27)
ane.tb.69ts<-tb.69tsfaosp %>% filter(primary_FAOarea=="27")#37 stocks with biomass time series
#can go up to 2017
#should the same species stocks be summed? No, keep separate.
unique(ane.tb.69ts$scientificname)#16 unique species
#1965-2017 is a 53 yr timeseries. 
colnames(ane.tb.69ts)
ane.tb.53yr<-ane.tb.69ts[,c(16:68,70)]
rownames(ane.tb.53yr)<-ane.tb.53yr[,54]
ane.tb.53yr.mat<-as.matrix(ane.tb.53yr[,1:53])

#have row Medians somewhere to check
anerowMed<-matrixStats::rowMedians(ane.tb.53yr.mat, na.rm = TRUE)
ane.tb.53yr.mat2<-apply(ane.tb.53yr.mat, 1, function (x) {
  ifelse(is.na(x), median(x, na.rm=T), x)
})
ane.tb.53yr.mat2<-t(ane.tb.53yr.mat2)#worked


##subsetting biomass data for PWC (71)
pwc.tb.69ts<-tb.69tsfaosp %>% filter(primary_FAOarea=="71")#only 4 stocks (3 tuna, 1 marlin species) with biomass time series


#other timeseries I want
str(tcbest.data)#perhaps only to show that the catches within these regions are also coherent?
str(f.data)
str(er.data)
str(effort.data)