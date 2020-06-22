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

str(ioe.allf.57yr.mat3)#11 stocks from 1960-2016, 57yr ts, after taking out dups, its 9 stocks
str(ane.allf.46yr.mat3)#59 stocks from 1972-2017, 46yr ts
str(pwc.allf.49yr.mat3)#7 stocks from 1967-2015, 49yr ts

##perhaps do synmat of coh and coh.sig first, then see the number of sig rships betw biomass and F?
##IOE biomass and fishing mortality coherences
ioe.tb.50yr.cd<-cleandat(ioe.tb.50yr.mat2, times=1965:2014, clev=5)$cdat
str(ioe.tb.50yr.cd)
ioe.tb.coh<-synmat(ioe.tb.50yr.cd, times=1965:2014, method="coh", scale.min=2, scale.max=16)
rownames(ioe.tb.coh)<-dimnames(ioe.tb.50yr.cd)[[1]]
colnames(ioe.tb.coh)<-dimnames(ioe.tb.50yr.cd)[[1]]
ioe.tb.cohsig<-synmat(ioe.tb.50yr.cd, times=1965:2014, method="coh.sig.fast", scale.min=2, scale.max=16, nsurrogs=1000)
ioe.tb.cohpv<-1-ioe.tb.cohsig
length(which(ioe.tb.cohpv<0.005))#10 are p<0.05, 7 are p<0.01, 7 are p<0.005
ioe.tb.cohqv<-ioe.tb.cohpv
ioe.tb.cohqv[lower.tri(ioe.tb.cohqv)]<-p.adjust(ioe.tb.cohqv[lower.tri(ioe.tb.cohqv)], method="fdr")
ioe.tb.cohqv[1:5,1:5]#check
ioe.tb.cohqv.utri<-ioe.tb.cohqv
ioe.tb.cohqv.utri[upper.tri(ioe.tb.cohqv.utri)]<-NA
ioe.tb.cohqv.utri<-t(ioe.tb.cohqv.utri)
ioe.tb.cohqv[upper.tri(ioe.tb.cohqv)]<-ioe.tb.cohqv.utri[upper.tri(ioe.tb.cohqv.utri)]
ioe.tb.cohqv[1:5,1:5]#check
length(which(ioe.tb.cohqv<0.10))#16 are less than 20% fdr, 14 < 15% fdr, 6 < 10% fdr
length(which(!is.na(ioe.tb.cohqv)))#56 not NAs, ie. 28 possible combinations
#with fdr<20%, 16 out of the 28 (57.14%) pairwise rships were significantly coherent across all timescales.

ioe.allf.57yr.cd<-cleandat(ioe.allf.57yr.mat3, times=1960:2016, clev=5)$cdat
str(ioe.allf.57yr.cd)
ioe.allf.coh<-synmat(ioe.allf.57yr.cd,times=1960:2016, method="coh", 
                     scale.min=2, scale.max=19)
rownames(ioe.allf.coh)<-dimnames(ioe.allf.57yr.cd)[[1]]
colnames(ioe.allf.coh)<-dimnames(ioe.allf.57yr.cd)[[1]]
ioe.allf.cohsig<-synmat(ioe.allf.57yr.cd, times=1960:2016, method="coh.sig.fast", 
                      scale.min=2, scale.max=19, nsurrogs=1000)
ioe.allf.cohpv<-1-ioe.allf.cohsig
length(which(ioe.allf.cohpv<0.05))#12 are p<0.05, 3 are p<0.01, 2 are p<0.005
ioe.allf.cohqv<-ioe.allf.cohpv
ioe.allf.cohqv[lower.tri(ioe.allf.cohqv)]<-p.adjust(ioe.allf.cohqv[lower.tri(ioe.allf.cohqv)], method="fdr")
ioe.allf.cohqv[1:5,1:5]#check
ioe.allf.cohqv.utri<-ioe.allf.cohqv
ioe.allf.cohqv.utri[upper.tri(ioe.allf.cohqv.utri)]<-NA
ioe.allf.cohqv.utri<-t(ioe.allf.cohqv.utri)
ioe.allf.cohqv[upper.tri(ioe.allf.cohqv)]<-ioe.allf.cohqv.utri[upper.tri(ioe.allf.cohqv.utri)]
ioe.allf.cohqv[1:5,1:5]#check
length(which(ioe.allf.cohqv<0.20))#10 are less than 20% fdr, 3 < 15% fdr, 2 < 10% fdr
length(which(!is.na(ioe.allf.cohqv)))#110 not NAs, ie. 55 possible combinations
#with fdr<20%, 10 out of the 55 (18.18%) pairwise rships were significantly coherent across all timescales.
#similar stocks for both biomass and erdat, tunas, marlins, rock lobsters.
#so it seems like the coherences in IOE are more likely due to coherences in biomass rather than fishing mortality,
#due to the higher percentages of significantly coherent rships in biomass (57%) compared to F (18%).
#how to show this as a figure? Perhaps have a figure showing catch of some species that are coherent, and same species of biomass that are coh?
#could probably check if BLKMARLINIO, SBT (southern bluefin tuna, thunnus maccoyi), SKJCIO (skipjack, katsuwonus pelamis), rock lobster occur in both sets

#delete SBT_fdat from all the results. can I just delete the first row and first column?
ioe.allf.coh2<-ioe.allf.coh[-1,-1]
ioe.allf.cohqv2<-ioe.allf.cohqv[-1,-1]
length(which(!is.na(ioe.allf.cohqv2)))#90 obs, ie. 45 possible pairwise obs.
length(which(ioe.allf.cohqv2<0.10))#8 are fdr<20%, 2 are fdr<15% and fdr<10%
#with fdr<20%, 8 out of 45 (17.78%) pairwise rships were significantly coherent.

#delete SWhitse_fdat
head(ioe.allf.coh2)
ioe.allf.coh3<-ioe.allf.coh2[-1,-1]
ioe.allf.cohqv3<-ioe.allf.cohqv2[-1,-1]
rownames(ioe.allf.coh3)
head(ioe.allf.cohqv3)
length(which(!is.na(ioe.allf.cohqv3)))#72 obs, ie. 36 possible pairwise obs.
length(which(ioe.allf.cohqv3<0.20))#5 are fdr<20%, 1 are fdr<15% and fdr<10%
#with fdr<20%, 5 out of 36 (13.89%) pairwise rships were significantly coherent.


#quick plot of biomass timeseries?
str(ioe.tb.50yr.cd)#8 stocks from 1965-2014
ioe.tb.df<-as.data.frame(t(ioe.tb.50yr.cd))
ioe.tb.df$year<-seq(1965,2014,1)
ioe.tb.df2<-melt(ioe.tb.df, id.vars="year", value.name = "cdat", variable.name = "stkid")
str(ioe.tb.df2)
str(stk_sp_key)
stk_sp_key<-stk_fao_sp_key[,-2]
ioe.tb.df3<-ioe.tb.df2 %>% left_join(stk_sp_key, by=c("stkid"="stockid"))
str(ioe.tb.df3)
ioe.tb.df3$sp<-as.factor(ioe.tb.df3$scientificname)
unique(ioe.tb.df3$sp)#2 stocks of rock lobsters

ioe.tb.p1<-ggplot(ioe.tb.df3, aes(x=year, y=cdat, color=stkid))
ioe.tb.p1 + geom_line() + theme_bw() + labs(x="year", y="Transformed index") + 
  theme(legend.box = "horizontal", legend.position = "bottom") +
  scale_y_continuous(limits = c(-3.5,3.5)) +  guides(col=guide_legend(ncol=3)) 
#  scale_color_discrete(name="Species", labels=c("sp1loc1", "sp2loc1"))
##not so easy to see the coherent timeseries.
#perhaps focus on SKJCIO, rocklobsterSZ, blue grenadier, SWHITSE?

##ANE biomass and fishing mortality coherences
str(ane.tb.53yr.mat2)#37 stocks from 1965-2017, 53yr ts
str(ane.allf.46yr.mat3)#59 stocks from 1972-2017, 46yr ts

ane.tb.53yr.cd<-cleandat(ane.tb.53yr.mat2, times=1965:2017, clev=5)$cdat
str(ane.tb.53yr.cd)
ane.tb.coh<-synmat(ane.tb.53yr.cd, times=1965:2017, method="coh", scale.min=2, 
                   scale.max=17)
rownames(ane.tb.coh)<-dimnames(ane.tb.53yr.cd)[[1]]
colnames(ane.tb.coh)<-dimnames(ane.tb.53yr.cd)[[1]]
str(ane.tb.coh)
ane.tb.cohsig<-synmat(ane.tb.53yr.cd, times=1965:2017, method="coh.sig.fast", 
                      scale.min=2, scale.max=17, nsurrogs=1000)
ane.tb.cohpv<-1-ane.tb.cohsig
length(which(ane.tb.cohpv<0.05))#102 are p<0.05, 51 are p<0.01, 38 are p<0.005
ane.tb.cohqv<-ane.tb.cohpv
ane.tb.cohqv[lower.tri(ane.tb.cohqv)]<-p.adjust(ane.tb.cohqv[lower.tri(ane.tb.cohqv)], method="fdr")
ane.tb.cohqv[1:5,1:5]#check
ane.tb.cohqv.utri<-ane.tb.cohqv
ane.tb.cohqv.utri[upper.tri(ane.tb.cohqv.utri)]<-NA
ane.tb.cohqv.utri<-t(ane.tb.cohqv.utri)
ane.tb.cohqv[upper.tri(ane.tb.cohqv)]<-ane.tb.cohqv.utri[upper.tri(ane.tb.cohqv.utri)]
ane.tb.cohqv[1:5,1:5]#check
length(which(ane.tb.cohqv<0.20))#64 are less than 20% fdr, 53 < 15% fdr, 41 < 10% fdr
length(which(!is.na(ane.tb.cohqv)))#1332 not NAs, ie. 666 possible combinations
#with fdr<20%, 64 out of the 666 (9.61%) pairwise rships were significantly coherent across all timescales.

ane.allf.46yr.cd<-cleandat(ane.allf.46yr.mat3, times=1972:2017, clev=5)$cdat
str(ane.allf.46yr.cd)
ane.allf.coh<-synmat(ane.allf.46yr.cd,times=1972:2017, method="coh", 
                     scale.min=2, scale.max=15)
rownames(ane.allf.coh)<-dimnames(ane.allf.46yr.cd)[[1]]
colnames(ane.allf.coh)<-dimnames(ane.allf.46yr.cd)[[1]]
ane.allf.cohsig<-synmat(ane.allf.46yr.cd, times=1972:2017, method="coh.sig.fast", 
                        scale.min=2, scale.max=15, nsurrogs=1000)
ane.allf.cohpv<-1-ane.allf.cohsig
length(which(ane.allf.cohpv<0.005))#130 are p<0.05, 34 are p<0.01, 22 are p<0.005
ane.allf.cohqv<-ane.allf.cohpv
ane.allf.cohqv[lower.tri(ane.allf.cohqv)]<-p.adjust(ane.allf.cohqv[lower.tri(ane.allf.cohqv)], method="fdr")
ane.allf.cohqv[1:5,1:5]#check
ane.allf.cohqv.utri<-ane.allf.cohqv
ane.allf.cohqv.utri[upper.tri(ane.allf.cohqv.utri)]<-NA
ane.allf.cohqv.utri<-t(ane.allf.cohqv.utri)
ane.allf.cohqv[upper.tri(ane.allf.cohqv)]<-ane.allf.cohqv.utri[upper.tri(ane.allf.cohqv.utri)]
ane.allf.cohqv[1:5,1:5]#check
length(which(ane.allf.cohqv<0.30))#12 are less than 30% fdr, 0 are less than 20% fdr, 0 < 15% fdr, 0 < 10% fdr
length(which(!is.na(ane.allf.cohqv)))#3422 not NAs, ie. 1711 possible combinations
#with fdr<20%, 0 out of the 1711 (0%) pairwise combinations were sig.

##PWC biomass and fishing mortality coherences
str(pwc.tb.50yr.mat2)#4 stocks from 1966-2015, 50yr ts
str(pwc.allf.49yr.mat3)#7 stocks from 1967-2015, 49yr ts

pwc.tb.50yr.cd<-cleandat(pwc.tb.50yr.mat2, times=1966:2015, clev=5)$cdat
str(pwc.tb.50yr.cd)
pwc.tb.coh<-synmat(pwc.tb.50yr.cd, times=1966:2015, method="coh", scale.min=2, 
                   scale.max=16)
rownames(pwc.tb.coh)<-dimnames(pwc.tb.50yr.cd)[[1]]
colnames(pwc.tb.coh)<-dimnames(pwc.tb.50yr.cd)[[1]]
str(pwc.tb.coh)
pwc.tb.cohsig<-synmat(pwc.tb.50yr.cd, times=1966:2015, method="coh.sig.fast", 
                      scale.min=2, scale.max=16, nsurrogs=1000)
pwc.tb.cohpv<-1-pwc.tb.cohsig
length(which(pwc.tb.cohpv<0.005))#1 are p<0.05, 0 are p<0.01, 0 are p<0.005
pwc.tb.cohqv<-pwc.tb.cohpv
pwc.tb.cohqv[lower.tri(pwc.tb.cohqv)]<-p.adjust(pwc.tb.cohqv[lower.tri(pwc.tb.cohqv)], method="fdr")
pwc.tb.cohqv[1:4,1:4]#check
pwc.tb.cohqv.utri<-pwc.tb.cohqv
pwc.tb.cohqv.utri[upper.tri(pwc.tb.cohqv.utri)]<-NA
pwc.tb.cohqv.utri<-t(pwc.tb.cohqv.utri)
pwc.tb.cohqv[upper.tri(pwc.tb.cohqv)]<-pwc.tb.cohqv.utri[upper.tri(pwc.tb.cohqv.utri)]
pwc.tb.cohqv[1:4,1:4]#check
length(which(pwc.tb.cohqv<0.20))#2 are less than 20% fdr, 1 < 15% fdr, 0 < 10% fdr
length(which(!is.na(pwc.tb.cohqv)))#12 not NAs, ie. 6 possible combinations
#with fdr<20%, 2 out of the 6 (33.33%) pairwise rships were significantly coherent across all timescales.

pwc.allf.49yr.cd<-cleandat(pwc.allf.49yr.mat3, times=1967:2015, clev=5)$cdat
str(pwc.allf.49yr.cd)
pwc.allf.coh<-synmat(pwc.allf.49yr.cd,times=1967:2015, method="coh", 
                     scale.min=2, scale.max=16)
rownames(pwc.allf.coh)<-dimnames(pwc.allf.49yr.cd)[[1]]
colnames(pwc.allf.coh)<-dimnames(pwc.allf.49yr.cd)[[1]]
pwc.allf.cohsig<-synmat(pwc.allf.49yr.cd, times=1967:2015, method="coh.sig.fast", 
                        scale.min=2, scale.max=16, nsurrogs=1000)
pwc.allf.cohpv<-1-pwc.allf.cohsig
length(which(pwc.allf.cohpv<0.005))#6 are p<0.05, 5 are p<0.01, 4 are p<0.005
pwc.allf.cohqv<-pwc.allf.cohpv
pwc.allf.cohqv[lower.tri(pwc.allf.cohqv)]<-p.adjust(pwc.allf.cohqv[lower.tri(pwc.allf.cohqv)], method="fdr")
pwc.allf.cohqv[1:5,1:5]#check
pwc.allf.cohqv.utri<-pwc.allf.cohqv
pwc.allf.cohqv.utri[upper.tri(pwc.allf.cohqv.utri)]<-NA
pwc.allf.cohqv.utri<-t(pwc.allf.cohqv.utri)
pwc.allf.cohqv[upper.tri(pwc.allf.cohqv)]<-pwc.allf.cohqv.utri[upper.tri(pwc.allf.cohqv.utri)]
pwc.allf.cohqv[1:5,1:5]#check
length(which(pwc.allf.cohqv<0.10))#7 are less than 20% fdr, 6 < 15% fdr, 6 < 10% fdr
length(which(!is.na(pwc.allf.cohqv)))#42 not NAs, ie. 21 possible combinations
#with fdr<20%, 7 out of the 21 (33.33%) pairwise rships were significantly coherent across all timescales.

#####plot of barchart
pcentsig<-read.csv("D:/Rutgers_postdoc/data/RAM legacy/RAM_v4.491_hotspots_percentsig_20200601.csv")
str(pcentsig)
psigp1<-ggplot(pcentsig, aes(x=fao, y=percentsig, fill=dat))
psigp1 + geom_bar(stat="identity", position="dodge") + theme_bw(base_size = 14) +
  scale_fill_discrete(name="Data type") + scale_y_continuous(expand = c(0, 0), limits=c(0,65)) +
  labs(x="FAO region", y="Percentage of significant coherences (FDR<20%)") + 
  theme(legend.position = c(.85, .85)) + geom_text(aes(label=totstocks), position=position_dodge(0.9), vjust=-0.2) 
ggsave(filename="D:/Rutgers_postdoc/Global MS/ecol_applications_journal/Reject_resubmit_reviews/new_fig/bar_RAM_hotspots_fdr20_20200605.eps", device="eps", scale=1, width=7, height=4, units="in", dpi=300)


#####plots of synchrony matrices
colbwr<-colorRampPalette(c("blue", "white", "red"))#to specify colour palette
png(filename="D:/Rutgers_postdoc/Global MS/ecol_applications_journal/Reject_resubmit_reviews/new_fig/corrplot_RAM_ioe_f_fdr20_20200605.png", 
    width=1400, height=1300, units="px", res=120)
corrplot(ioe.allf.coh3, method="number", type="lower", tl.pos="ld", tl.srt=40, tl.offset=0.5, 
         col=colbwr(10), is.corr=TRUE, diag=F, tl.col="black", p.mat=ioe.allf.cohqv3, 
         sig.level=0.20, insig="blank", cl.ratio=0.1, tl.cex=1)
mtext("IOE Fishing Coherence (fdr<20%)", side=3, line=2)
dev.off()

#change names from pwc.allf.coh and ioe.allf.coh2 to get rid of _fdat and _erdat
rownames(ioe.allf.coh2)<-gsub("_fdat", "", rownames(ioe.allf.coh2))
rownames(ioe.allf.coh2)<-gsub("_erdat", "", rownames(ioe.allf.coh2))
colnames(ioe.allf.coh2)<-rownames(ioe.allf.coh2)

ane.allf.names<-gsub("_fdat", "", dimnames(ane.allf.46yr.cd)[[1]])
ane.allf.names<-gsub("_erdat", "", ane.allf.names)
duplicated(ane.allf.names)#no duplicates
sort(ane.allf.names)
ane.allf.namesdf<-as.data.frame(sort(ane.allf.names))

#making flexible color bar
color.bar <- function(lut, min, max=-min, nticks=11, ticks=seq(min, max, len=nticks), title='') {
  scale = (length(lut)-1)/(max-min)
  
  dev.new(width=1.75, height=5)
  plot(c(0,10), c(min,max), type='n', bty='n', xaxt='n', xlab='', yaxt='n', ylab='', main=title)
  axis(2, ticks, las=1)
  for (i in 1:(length(lut)-1)) {
    y = (i-1)/scale + min
    rect(0,y,10,y+1/scale, col=lut[i], border=NA)
  }
}
color.bar(colorRampPalette(c("blue", "white", "red"))(100), -1)
#save as colorbar.eps in folder for global MS.

#table of stockid, FAO regions and species names
str(er.70yrfaosp)#381 obs of 75 var
str(tb.69tsfaosp)#228 obs of 73 vars
er.fao.sp<-er.70yrfaosp[,71:74]
str(er.fao.sp)#381 obs of 4 vars
hotspot<-c("21", "27", "57")
er.fao3.sp<-er.fao.sp %>% filter(primary_FAOarea %in% hotspot) %>% as.data.frame()
str(er.fao3.sp)#82 stocks but should only be 75
er.fao3.sp$stockid#seems wrong

#or perhaps, just do left_join using stockid after compiling all the stockids from each place
str(pwc.allf.49yr.mat3)#7 stocks
dimnames(pwc.allf.49yr.mat3)[[1]]
str(ane.allf.namesdf)#59 stocks
ane.allf.names2<-as.data.frame(ane.allf.namesdf)
colnames(ane.allf.names2)<-"stockid"
str(ane.allf.names2)
pwc.allf.names<-gsub("_fdat", "", dimnames(pwc.allf.49yr.mat3)[[1]])
pwc.allf.names<-gsub("_erdat", "", pwc.allf.names)
duplicated(pwc.allf.names)#no duplicates
str(pwc.allf.names)#character
ioe.allf.names<-rownames(ioe.allf.coh3)#9 stocks
str(ioe.allf.names)#character
allf.75stks<-c(pwc.allf.names, ioe.allf.names)
allf.75stks<-as.data.frame(allf.75stks)
str(allf.75stks)
colnames(allf.75stks)<-"stockid"
allf.75stks2<-rbind(ane.allf.names2, allf.75stks)

#now do left_join
str(stk_fao_sp_key)
allf.75stks3<-allf.75stks2 %>% left_join(stk_fao_sp_key, by="stockid") %>% as.data.frame()
str(allf.75stks3)
allf.75stks4<-allf.75stks3 %>% 
  mutate(primary_FAOarea=recode(primary_FAOarea, "27" = "ANE", "57" = "IOE", "71" = "PWC")) %>%
  as.data.frame()
write.csv(allf.75stks4, "allf.75stks4.csv")

#table for biomass stocks, combine with fishing effort
dimnames(ane.tb.53yr.cd)[[1]]
dimnames(pwc.tb.50yr.cd)[[1]]
dimnames(ioe.tb.50yr.cd)[[1]]
#49 stocks in total for biomass
tb49.stks<-c(dimnames(ane.tb.53yr.cd)[[1]], dimnames(pwc.tb.50yr.cd)[[1]], dimnames(ioe.tb.50yr.cd)[[1]])
tb49.stks<-as.data.frame(tb49.stks)
colnames(tb49.stks)<-"stockid"
str(tb49.stks)
str(allf.75stks2)
tb.f.names<-rbind(tb49.stks, allf.75stks2)
str(tb.f.names)
unique(tb.f.names$stockid)#75 unique values
tb.f.names2<-tb.f.names %>% distinct() %>% as.data.frame()
str(tb.f.names2)
tb.f.names3<-tb.f.names2 %>% left_join(stk_fao_sp_key, by="stockid") %>% as.data.frame()
str(tb.f.names3)
tb.f.names4<-tb.f.names3 %>% 
  mutate(primary_FAOarea=recode(primary_FAOarea, "27" = "ANE", "57" = "IOE", "71" = "PWC")) %>%
  as.data.frame()
str(tb.f.names4)
write.csv(tb.f.names4, "tb.f.75stocks.csv")
