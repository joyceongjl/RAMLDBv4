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
ioe.tb.50yr.mat2<-t(ioe.tb.50yr.mat2)#worked, 8 stocks from 1965-2014
length(which(is.na(ioe.tb.50yr.mat)))#replaced 36 NAs with median of each biomass timeseries
#total of 400 values, 36 were NAs = 9%
write.csv(ioe.tb.50yr.mat2, "ioe_tb_1965-2014.csv")

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
ane.tb.53yr.mat2<-t(ane.tb.53yr.mat2)#worked, 37 stocks from 1965-2017
length(which(is.na(ane.tb.53yr.mat)))#replaced 188 NAs with median of each biomass timeseries
#total of 1961 values, 188 were NAs = 9.6%
write.csv(ane.tb.53yr.mat2, "ane_tb_1965-2017.csv")

##subsetting biomass data for PWC (71)
pwc.tb.69ts<-tb.69tsfaosp %>% filter(primary_FAOarea=="71")#only 4 stocks (3 tuna, 1 marlin species) with biomass time series
#could do 1966-2015, 50yr timeseries.
colnames(pwc.tb.69ts)
pwc.tb.50yr<-pwc.tb.69ts[,c(17:66,70)]
rownames(pwc.tb.50yr)<-pwc.tb.50yr[,51]
pwc.tb.50yr.mat<-as.matrix(pwc.tb.50yr[,1:50])

#have row Medians somewhere to check
pwcrowMed<-matrixStats::rowMedians(pwc.tb.50yr.mat, na.rm = TRUE)
pwc.tb.50yr.mat2<-apply(pwc.tb.50yr.mat, 1, function (x) {
  ifelse(is.na(x), median(x, na.rm=T), x)
})
pwc.tb.50yr.mat2<-t(pwc.tb.50yr.mat2)#worked, 4 stocks from 1966-2015
length(which(is.na(pwc.tb.50yr.mat)))#replaced 11 NAs with median of each biomass timeseries
#total of 200 values, 11 were NAs = 5.5%
write.csv(pwc.tb.50yr.mat, "pwc_tb_1966-2015.csv")

#other timeseries I want
str(tcbest.data)#perhaps only to show that the catches within these regions are also coherent?
str(f.data)#218 (1800-2017) obs of 478 vars/stocks
str(er.data)#147 obs (1872-2018) of 745 vars/stocks
str(effort.data)# 70 obs (1948-2017) of 29 stocks
#limit all fishing effort timeseries to 1950 onwards, then combine? perhaps combine measure to stockid

#####f.data timeseries
rownames(f.data)# 1948-2017 [149:218,]
f1948.2017<-f.data[149:218,]
str(f1948.2017)#70 obs/years of 478 stocks
#remove columns that are entirely NAs
length(which(colSums(is.na(f1948.2017)) < 36))#only 120 stocks have <21 NAs, 146 <26 NAs,  184 stocks < 31 NAs, 255 stocks < 36 NAs
#Perhaps keep it to 255 stocks first that have <50% NAs.
f1948.2017sub<-f1948.2017[,colSums(is.na(f1948.2017)) < 36]#70 obs/yrs of 255 vars/stocks. 

str(stk_fao_sp_key)

f.70yr<-as.data.frame(t(f1948.2017sub))
f.70yr$stockid<-rownames(f.70yr)
#left join with key to get fao regions and sp names
f.70yrfaosp<-f.70yr %>% left_join(stk_fao_sp_key, by="stockid")
length(which(is.na(f.70yrfaosp$primary_FAOarea)))#no NAs
str(f.70yrfaosp)
f.70yrfaosp$fao_sp<-paste(f.70yrfaosp$primary_FAOarea,f.70yrfaosp$scientificname,"fdat",sep="_")
f.70yrfaosp$stock_dat<-paste(f.70yrfaosp$stockid,"fdat",sep="_")
write.csv(f.70yrfaosp, "fdat_fao_sp_1948-2017.csv")

#####er.data timeseries
rownames(er.data)# 1948-2017 [77:146,]
er1948.2017<-er.data[77:146,]
str(er1948.2017)#70 obs/years of 745 stocks
#remove columns that are entirely NAs
length(which(colSums(is.na(er1948.2017)) < 21))#185 stocks have <21 NAs, 231 <26 NAs,  276 stocks < 31 NAs, 381 stocks < 36 NAs
#Perhaps keep it to 381 stocks first that have <50% NAs.
er1948.2017sub<-er1948.2017[,colSums(is.na(er1948.2017)) < 36]#70 obs/yrs of 381 vars/stocks. 

str(stk_fao_sp_key)

er.70yr<-as.data.frame(t(er1948.2017sub))
er.70yr$stockid<-rownames(er.70yr)
#left join with key to get fao regions and sp names
er.70yrfaosp<-er.70yr %>% left_join(stk_fao_sp_key, by="stockid")
length(which(is.na(er.70yrfaosp$primary_FAOarea)))#no NAs
str(er.70yrfaosp)
er.70yrfaosp$fao_sp<-paste(er.70yrfaosp$primary_FAOarea,er.70yrfaosp$scientificname,"erdat",sep="_")
er.70yrfaosp$stock_dat<-paste(er.70yrfaosp$stockid,"erdat",sep="_")
write.csv(er.70yrfaosp, "erdat_fao_sp_1948-2017.csv")

#####effort.data timeseries
rownames(effort.data)# 1948-2017 [77:146,]
effort1948.2017<-effort.data #70 obs/years of 29 stocks
length(which(colSums(is.na(effort1948.2017)) < 36))#1 stocks have <21 NAs, 1 <26 NAs,  1 < 31 NAs, 8 stocks < 36 NAs
#Perhaps keep it to 8 stocks first that have <50% NAs.
effort1948.2017sub<-effort1948.2017[,colSums(is.na(effort1948.2017)) < 36]#70 obs/yrs of 8 vars/stocks. 

effort.70yr<-as.data.frame(t(effort1948.2017sub))
effort.70yr$stockid<-rownames(effort.70yr)
#left join with key to get fao regions and sp names
effort.70yrfaosp<-effort.70yr %>% left_join(stk_fao_sp_key, by="stockid")
effort.70yrfaosp$fao_sp<-paste(effort.70yrfaosp$primary_FAOarea,effort.70yrfaosp$scientificname,"effort",sep="_")
effort.70yrfaosp$stock_dat<-paste(effort.70yrfaosp$stockid,"effort",sep="_")
write.csv(effort.70yrfaosp, "effort_fao_sp_1948-2017.csv")

#combine all df of f, er and effort together, should be 8+381+255=644 stocks/rows of 75 variables.
str(effort.70yrfaosp)#dataframe
allf.70yrfaosp<-bind_rows(f.70yrfaosp, er.70yrfaosp, effort.70yrfaosp)
write.csv(allf.70yrfaosp, "allfishingeffort_fao_sp_1948-2017.csv")

##subsetting all fishing effort data for IOE (57)
ioe.allf.70yr<-allf.70yrfaosp %>% filter(primary_FAOarea=="57")#12 stocks with fishing effort time series
#can go up to 2016, consider deleting NZ pink cusk eel, Genypterus blacodes, or just leave it.
#note that 4 species are repeats, such that there is 1 f.dat ts and 1 er.dat ts.
unique(ioe.allf.70yr$scientificname)#8 unique species out of the 12 stocks
#1960-2017 is a 57 yr timeseries. 
colnames(ioe.allf.70yr)
ioe.allf.57yr<-ioe.allf.70yr[,c(13:69,75)]
rownames(ioe.allf.57yr)<-ioe.allf.57yr[,58]
ioe.allf.57yr.mat<-as.matrix(ioe.allf.57yr[,1:57])
length(which(is.na(ioe.allf.57yr.mat)))#will replace 68 NAs with median of each fishing effort timeseries
#total of 684 values, 68 were NAs = 9.9%

#have row Medians somewhere to check
ioefrowMed<-matrixStats::rowMedians(ioe.allf.57yr.mat, na.rm = TRUE)
ioe.allf.57yr.mat2<-apply(ioe.allf.57yr.mat, 1, function (x) {
  ifelse(is.na(x), median(x, na.rm=T), x)
})
ioe.allf.57yr.mat2<-t(ioe.allf.57yr.mat2)#worked, 12 stocks from 1960-2016
write.csv(ioe.allf.57yr.mat2, "ioe_allf_1960-2016.csv")

##subsetting all fishing effort data for ANE (27)
ane.allf.70yr<-allf.70yrfaosp %>% filter(primary_FAOarea=="27")#105 stocks with fishing effort time series
#can go up to 2017, try from 1968 to make a 50yr ts
unique(ane.allf.70yr$scientificname)#26 unique species out of the 105 stocks
colnames(ane.allf.70yr)
ane.allf.50yr<-ane.allf.70yr[,c(21:70,75)]
rownames(ane.allf.50yr)<-ane.allf.50yr[,51]
ane.allf.50yr.mat<-as.matrix(ane.allf.50yr[,1:50])
length(which(is.na(ane.allf.50yr.mat)))#will replace 708 NAs with median of each fishing effort timeseries
#total of 5250 values, 708 were NAs = 13.5%, too high.
length(which(is.na(ane.allf.50yr.mat[,5:50])))#need to do from 1972-2017, 46yr timeseries
ane.allf.46yr.mat<-ane.allf.50yr.mat[,5:50]
length(which(is.na(ane.allf.46yr.mat)))#will replace 465 NAs with median of each fishing effort timeseries
#total of 4830 values, 465 were NAs = 9.6%

#have row Medians somewhere to check
anefrowMed<-matrixStats::rowMedians(ane.allf.46yr.mat, na.rm = TRUE)
ane.allf.46yr.mat2<-apply(ane.allf.46yr.mat, 1, function (x) {
  ifelse(is.na(x), median(x, na.rm=T), x)
})
ane.allf.46yr.mat2<-t(ane.allf.46yr.mat2)#worked, 105 stocks from 1972-2017, 46yr ts
write.csv(ane.allf.46yr.mat2, "ane_allf_1972-2017.csv")

##subsetting all fishing effort data for PWC (71)
pwc.allf.70yr<-allf.70yrfaosp %>% filter(primary_FAOarea=="71")#10 stocks with fishing effort time series
#can go up to 2015, try from 1966 to make a 50yr ts
unique(pwc.allf.70yr$scientificname)#7 unique species out of the 10 stocks
colnames(pwc.allf.70yr)
pwc.allf.50yr<-pwc.allf.70yr[,c(19:68,75)]
rownames(pwc.allf.50yr)<-pwc.allf.50yr[,51]
pwc.allf.50yr.mat<-as.matrix(pwc.allf.50yr[,1:50])
length(which(is.na(pwc.allf.50yr.mat)))#will replace 52 NAs with median of each fishing effort timeseries
#total of 500 values, 52 were NAs = 10.4%, too high.
length(which(is.na(pwc.allf.50yr.mat[,2:50])))#need to do from 1967-2015, 49yr timeseries
pwc.allf.49yr.mat<-pwc.allf.50yr.mat[,2:50]
length(which(is.na(pwc.allf.49yr.mat)))#will replace 47 NAs with median of each fishing effort timeseries
#total of 490 values, 47 were NAs = 9.6%

#have row Medians somewhere to check
pwxfrowMed<-matrixStats::rowMedians(pwc.allf.49yr.mat, na.rm = TRUE)
pwc.allf.49yr.mat2<-apply(pwc.allf.49yr.mat, 1, function (x) {
  ifelse(is.na(x), median(x, na.rm=T), x)
})
pwc.allf.49yr.mat2<-t(pwc.allf.49yr.mat2)#worked, 10 stocks from 1967-2015, 49yr ts
write.csv(pwc.allf.49yr.mat2, "pwc_allf_1967-2015.csv")

##############################################
###wavelet coherences analyses of biomass and fishing effort timeseries for 3 hotspots
str(ioe.tb.50yr.mat2)#8 stocks from 1965-2014, 50yr ts
str(ane.tb.53yr.mat2)#37 stocks from 1965-2017, 53yr ts
str(pwc.tb.50yr.mat2)#4 stocks from 1966-2015, 50yr ts

str(ioe.allf.57yr.mat2)#12 stocks from 1960-2016, 57yr ts
#only 1 stock, BGRDRNSWWA has both f and er ts. manually deleted f.dat
ioe.allf.57yr.mat3<-ioe.allf.57yr.mat2[-1,]
#11 stocks from 1960-2016, 57yr ts
str(ane.allf.46yr.mat2)#105 stocks from 1972-2017, 46yr ts
str(pwc.allf.49yr.mat2)#10 stocks from 1967-2015, 49yr ts
#BIGEYECWPAC, SKJCWPAC, YFINCWPAC has f and er, delete f.dat for 3 stocks
pwc.allf.49yr.mat3<-pwc.allf.49yr.mat2[-c(1,5,6),]
#7 stocks from 1967-2015, 49yr ts

####From Olaf, ranking is ER, F, Effort. 
#figure out, for stocks with multiple fishing ts, first use ER, if not than F, and lastly, effort
#order matrix by rownames
ane.allf.46yr.mat2.ord<-ane.allf.46yr.mat2[order(rownames(ane.allf.46yr.mat2)),]
head(ane.allf.46yr.mat2)
head(ane.allf.46yr.mat2.ord)
#separate rownames by "_"
ane.allf.46yr.ord.df<-as.data.frame(ane.allf.46yr.mat2.ord)
ane.allf.46yr.ord.df$stkid <- sub("_.*", "", rownames(ane.allf.46yr.ord.df))
ane.allf.46yr.ord.df$dat <- sub(".*_", "", rownames(ane.allf.46yr.ord.df))
unique(ane.allf.46yr.ord.df$dat)#only er and f dats
#find the replicate stocks, then delete the f.dat for replicates.
ane.allf.46yr.ord.df2<-distinct(ane.allf.46yr.ord.df, stkid, .keep_all = T)
length(which(ane.allf.46yr.ord.df$dat=="fdat"))#48 erdat, 57 fdat
length(which(ane.allf.46yr.ord.df2$dat=="fdat"))#48 erdat, 11 fdat
#seems to have worked and taken out the duplicate stocks that are fdat.
ane.allf.46yr.ord.df2$stk_dat<-paste(ane.allf.46yr.ord.df2$stkid,ane.allf.46yr.ord.df2$dat,sep="_")
rownames(ane.allf.46yr.ord.df2)<-ane.allf.46yr.ord.df2$stk_dat
colnames(ane.allf.46yr.ord.df2)
ane.allf.46yr.mat3<-as.matrix(ane.allf.46yr.ord.df2[,1:46])#should be 59 obs of 46 variables
str(ane.allf.46yr.mat3)#59 stocks from 1972-2017, 46yr ts

#############################
#now can start wavelet coh
###wavelet coherences analyses of biomass and fishing effort timeseries for 3 hotspots
str(ioe.tb.50yr.mat2)#8 stocks from 1965-2014, 50yr ts
str(ane.tb.53yr.mat2)#37 stocks from 1965-2017, 53yr ts
str(pwc.tb.50yr.mat2)#4 stocks from 1966-2015, 50yr ts

str(ioe.allf.57yr.mat3)#11 stocks from 1960-2016, 57yr ts
str(ane.allf.46yr.mat3)#59 stocks from 1972-2017, 46yr ts
str(pwc.allf.49yr.mat3)#7 stocks from 1967-2015, 49yr ts