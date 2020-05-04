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
# The data is stored in the following objects:
# --- timeseries
#	The time series data is a matrix object with the following headers/columns:
#	(1) assessid (2) stockid (3) stocklong (4) tsid (5) tsyear (6) tsvalue
# --- bioparams
#	The time series data is a matrix object with the following headers/columns:
#	(1) assessid (2) stockid (3) stocklong (4) bioid (5) biovalue (6) bioyear (7) bionotes
# --- timeseries.views.data
#	This stores the timeseries values with timeseries type along the columns (TB, SSB, TN, R,
#	TC, TL, F, ER, TB/TBmsy, SSB/SSBmsy, F/Fmsy, ER/ERmsy, Btouse, Ctouse, Utouse, B/Bmsytouse, U/Umsytouse,
#	TB/TBmgt, SSB/SSBmgt, F/Fmgt, ER/ERmgt, B/Bmgttouse, U/Umgttouse) and stocks along the rows	
# --- timeseries.views.units
#	This stores the timeseries units (or time series source for touse time series), with timeseries type 
#	along the columns (TB, SSB, TN, R, TC, TL, F, ER) and stocks along the rows
# --- timeseries.views.ids
#	This stores the timeseries ids with timeseries id along the columns (TB, SSB, TN, R,
#	TC, TL, F, ER, TB/TBmsy, SSB/SSBmsy, F/Fmsy, ER/ERmsy, Btouse, Ctouse, Utouse, B/Bmsytouse, U/Umsytouse,
#	TB/TBmgt, SSB/SSBmgt, F/Fmgt, ER/ERmgt, B/Bmgttouse, U/Umgttouse) and stocks along the rows
# --- bioparams.views.data
#	This stores the bioparams values, with bioparam type along the columns
#	(TBmsy, SSBmsy, Nmsy, MSY, Fmsy, ERmsy, TB0, SSB0, M, Bmsytouse, Umsytouse, TBmgt, SSBmgt, Fmgt, ERmgt, 
#	Bmgttouse, Umgttouse) and stocks along the rows
# --- bioparams.views.units
#	This stores the bioparams units (or parameter source for touse parameters), with bioparam type 
#	along the columns (TBmsy, SSBmsy, Nmsy, MSY, Fmsy, ERmsy, TB0, SSB0, M, TBmgt, SSBmgt, Fmgt, ERmgt) and 
#	stocks along the rows 
# --- bioparams.views.ids
#	This stores the bioparams ids, with bioparam id along the columns
#	(TBmsy, SSBmsy, Nmsy, MSY, Fmsy, ERmsy, TB0, SSB0, M, Bmsytouse, Umsytouse, TBmgt, SSBmgt, Fmgt, ERmgt, 
#	Bmgttouse, Umgttouse) and stocks along the rows
# --- meta.data
#	This stores assorted metadata associated with the stock, with datatypes along the columns 
#	(assessid, stockid, stocklong, scientificname, FisheryType, region, areaid, areaname, 
#	assessorid, mgmt, management authority) and stock by row
#
###################################################################################
###################################################################################
###################################################################################
#
# Once the DBdata.RData file is in the working directory, simply run the following command to
# load up the database data into R objects
library(dplyr)

load(file="D:/Rutgers_postdoc/data/RAM legacy/From Chris/ramldb_v3.8/DB Files With Assessment Data/DBdata.RData")
str(timeseries)
head(timeseries)
unique(timeseries[,4])
metiwt<-c("TB-MT", "TC-MT", "R-E00", "SSB-MT", "TN-E00", "TB-index", "TN-index", "R-MT",
          "TC-E00", "SSB-E00larvae", "STB-MT", "SSB-E00eggs", "SSB-ratio", "SSB-E00",
          "TL-E00", "CPUE-index", "survB-MT", "survB-E00", "AQ-MT", "SSBf-MT", "SSBm-MT",
          "survB-2-E00", "survB-3-E00", "R-index", "TN-relative", "ER-index", "SSB-1-index", 
          "SSB-2-index", "TB-1-index", "TB-2-index", "CPUE-MT", "survB-1-E00", "survB-4-E00",
          "survB-5-E00",  "survB-6-MT", "survB-7-MT", "survB-10-MT", "survB-11-MT", "survB-5-MT",
          "survB-6-E00", "survB-7-E00", "survB-8-E00", "survB-9-MT")
#ts_sub<-subset(timeseries, timeseries[,4] == metiwt[1:43])
tssub<-subset(timeseries, timeseries[,4] %in% metiwt)
str(tssub)
unique(tssub[,4])
dimnames(tssub)
write.csv(tssub, file="D:/Rutgers_postdoc/data/RAM legacy/From Chris/ramldb_v3.8/DB Files With Assessment Data/ts_sub_assessid.csv")
ts1<-as.matrix(tssub[,2:6])
write.csv(ts1, file="D:/Rutgers_postdoc/data/RAM legacy/From Chris/ramldb_v3.8/DB Files With Assessment Data/ts_sub.csv")
#use the correct nyrs in the SAUP dataset.then take out the short series, 
#then combine with Chris's FAO key, note that region is in his all_stocks_in_ramldb.csv

ts1<-read.csv(file="D:/Rutgers_postdoc/data/RAM legacy/From Chris/ramldb_v3.8/DB Files With Assessment Data/ts_sub.csv")
ram<-ts1[,-1]
rm(ts1)
ram[1:10,]
str(ram)
min(ram$tsyear)
which(ram1$tsyear==1800)
ram1[4849:4999,]#Atlantic halibut has data from 1800, but I should only use data from 1900 onwards.
athal.del<-c(4644:4743)
ram1<-ram[-(athal.del),]
#need to also get rid of total catch for atlantic halibut. 
ram2[4750:4950,]
del2<-c(4752:4851)
ram2<-ram1[-del2,]
#NB, data still has NA in tsvalue.
md<-which(is.na(ram2$tsvalue))#there are 9943 NAs
ram3<-ram2[-c(unique(md)),]
which(is.na(ram3$tsvalue))

#install dplyr
de1<- ram3 %>% group_by(stockid, tsid) %>%
  summarise(maxyr=max(tsyear), minyr=min(tsyear), nyrs=n())
#There are 1013 distinct species groups, but 2781 groups with tsindex separated.
max(de1$nyrs)#min=1, max=140

de1sub<-subset(de1, subset=(nyrs>=60))
#344 species groups in total satisfy >=60 year time series

#get full dataset of species groups with time series>=60 years
#addition of nyrs into full dataset def
names(de1)
names(ram3)
ram4<- ram3 %>% left_join(de1, by=c("stockid", "tsid"))
names(ram4)# 103928 obs of 8 vars
ram.nyr60<-subset(ram4, subset=(nyrs>=60))#28804 rows of 8 vars
max(ram.nyr60$nyrs)#min=60, max=140
which(is.na(ram.nyr60$tsvalue))#no NA values
unistkid<-unique(ram.nyr60$stockid)#128 unique stocks (stockid) with >=60yr ts
write.csv(unistkid, file="D:/Rutgers_postdoc/data/RAM legacy/From Chris/ramldb_v3.8/DB Files With Assessment Data/list_stocks_nyr60.csv")
write.csv(ram.nyr60, file="D:/Rutgers_postdoc/data/RAM legacy/ram_nyr60.csv")

#plot graph that has a timeline
ram5<-read.csv(file="D:/Rutgers_postdoc/data/RAM legacy/ram_nyr60.csv")
ram5<-ram5[,-1]
names(ram.nyr60)
min(ram.nyr60$tsyear)#min=1872, max=2015

par(mar=c(3,5.5,2,5)+0.1, oma=c(0.5,0.5,0.5,0.5))
plot(ram5$tsyear, ram5$stockid, type="p", pch=20, cex=0.5, axes=F, xlab="", ylab="", xlim=c(1867, 2017))
axis(2, 1:128, levels(ram5$stockid), las=1, cex.axis=0.3, tck=0)
mtext(side=2, text="Stock ID", line=4)
axis(1, xlim=c(186, 201), labels=TRUE, at=seq(1867,2017,by=30))
mtext(side=1, text="Year", line=2.3)
#combine with whatever FAO area Chris has.

#######combine ram_nyr60 with assessid in order to assign fao areas
str(tssub)
asid<-as.matrix(tssub[,1:2])
dedup.asid<-unique(asid[,])
write.csv(dedup.asid, file="D:/Rutgers_postdoc/data/RAM legacy/ts_sub_assesid_stockid.csv", row.names = FALSE)
asid.mat<-read.csv(file="D:/Rutgers_postdoc/data/RAM legacy/ts_sub_assesid_stockid.csv")
#x=ram5, y=asid, resulting should only have 28804 rows
ram6<- ram5 %>% left_join(asid.mat, by="stockid")
fao.key<-read.csv(file="D:/Rutgers_postdoc/data/RAM legacy/ramldb_v3.8_stock_boundary_centroids_areas.csv")
str(fao.key)
fao.area<-c(1,6)
fao.key1<-fao.key[,fao.area]
ram7<-ram6 %>% left_join(fao.key1, by="assessid")
unique(ram7$fao_area)#16 different areas, one with NA.
rows.NA<-which(is.na(ram7$fao_area))#1733 rows has NAs
ram.NAs<-ram7[rows.NA,]
ctk<-c(1,2,9)
stk.NAs<-unique(ram.NAs[,ctk])
xtrafao<-c(34,41,81,27,81,41,34,67,67,81)
stk.NAs$fao_area<-xtrafao
head(fao.key1)
fao.key2<-bind_rows(fao.key1, stk.NAs[,3:4])
str(fao.key2)
write.csv(fao.key2, file="D:/Rutgers_postdoc/data/RAM legacy/fao.key.csv", row.names = FALSE)
###########################################
######just use the updated fao_area_key_subset, NB that fao_area_key is the full list
fao.key2<-read.csv(file="D:/Rutgers_postdoc/data/RAM legacy/fao_area_key_subset.csv")
str(fao.key2)
ram8<-ram6 %>% left_join(fao.key2, by="assessid")
str(ram8)
unique(ram8$fao_area)#15 unique fao areas
max(ram8$tsyear)#min =1872, max=2015.

#now ram8 contains all the ts (128 stocks) with >=60 years of data, including fao areas. 
de2<- ram8 %>% group_by(fao_area) %>%
  summarise(n=n_distinct(stockid))
#shows number of stocks in each of the 15 fao areas.
write.csv(ram8, file="D:/Rutgers_postdoc/data/RAM legacy/ram_nyr60_fao.csv", row.names = FALSE)

ram8<-read.csv(file="D:/Rutgers_postdoc/data/RAM legacy/ram_nyr60_fao.csv", header=TRUE)
str(ram8)
####subset data to be from 1950 onwards
ram1950<-subset(ram8, subset=(tsyear>=1950))
names(ram1950)
ram1950$fao_id<-paste(ram1950$fao_area, ram1950$stockid, sep="")
de3<-ram1950 %>% group_by(fao_area, tsid) %>%
  summarise(n=n_distinct(stockid))
de3a<-ram1950 %>% group_by(tsid, fao_area) %>%
  summarise(n=n_distinct(stockid))
de3b<-ram1950 %>% group_by(tsid) %>%
  summarise(n=n_distinct(stockid))
#hm. perhaps I should only be using catch data, ie. TC.
ram1950tc<-subset(ram1950, tsid=="TC-MT" | tsid=="TC-E00")
head(ram1950tc)
unique(ram1950tc$tsid)#only TC-MT and TC-E00
unique(ram1950tc$fao_id)#103 stocks
de4<-ram1950tc %>% group_by(fao_area) %>%
  summarise(n=n_distinct(stockid))

#organise datasets by fao area, then stockid
ram1950ord<-ram1950[order(ram1950[,10], ram1950[,1]),]
ram1950tc.ord<-ram1950tc[order(ram1950tc[,10], ram1950tc[,1]),]

#converting data into time series format for Reuman package
library(Reumannplatz)
ram50<-subset(ram1950ord, select=c(tsyear, fao_id, tsvalue))
head(ram50)
m.ram50<-table2matrix(ram50)
tm.ram50<-t(m.ram50)#128 stocks originally, however there are NAs.
md.tm<-which(is.na(tm.ram50), arr.ind=TRUE)
head(md.tm)#519 missing data, 125 unique row values
#too much missing values, try to limit data to 2012 instead. 
tm.ram50[1:3,]
tmr63<-tm.ram50[,1:63]
md.tmr63<-which(is.na(tmr63), arr.ind=TRUE)#203 missing data, 73 missing row values
unique(md.tmr63[,1])
#1955-2012 is a 58 year time series, which means i can get wavelet timescale up to 19 years.
tmr58<-tm.ram50[,6:63]
md.tmr58<-which(is.na(tmr58), arr.ind=TRUE)#203 missing data, 60 rows
unique(md.tmr58[,1])
#maybe take data till 2010 instead. 
tmr56<-tm.ram50[,6:61]#1955-2010, 56 yr ts, wavelet timescale up to 18 years.
md.tmr56<-which(is.na(tmr56), arr.ind=TRUE)#103 missing data, 34 rows
unique(md.tmr56[,1])
tmr55<-tm.ram50[,6:60]#1955-2009, 55 yr ts, wavelet timescale up to 18 years.
md.tmr55<-which(is.na(tmr55), arr.ind=TRUE)#69 missing data, 23 rows
unique(md.tmr55[,1])
ts.ram55<-tmr55[-c(unique(md.tmr55[,1])),]#105 stocks from 1955-2009. 
which(is.na(ts.ram55))#no more NAs

rtc<-subset(ram1950tc.ord, select=c(tsyear, fao_id, tsvalue))
m.rtc<-table2matrix(rtc)
tm.rtc<-t(m.rtc)#103 stocks originally, however there are NAs.
tm.rtc[1:3,]
rtc55<-tm.rtc[,6:60]#1955-2009, 55 yr ts, wavelet timescale up to 18 years.
md.rtc55<-which(is.na(rtc55), arr.ind=TRUE)#63 missing data, 20 rows
unique(md.rtc55[,1])
ts.rtc55<-rtc55[-c(unique(md.rtc55[,1])),]#83 stocks from 1955-2009. 
which(is.na(ts.rtc55))#no more NAs

#wavelet analysis
#apply CleanData function to every row to normalize data using box-cox transformations
#data has to be a matrix of time series with rows as separate species (loc_sp) and columns as years

ram.t1<-apply(ts.ram55, MARGIN=1, CleanData)
ram.t2<-NULL
for (i in 1:length(ram.t1)){
  ram.t2<-rbind(ram.t2, ram.t1[[i]]$cleandat)
}
#put dimnames into t2
str(ts.ram55)
dimnames(ts.ram55)[1]
dimnames(ram.t2)[1]<-dimnames(ts.ram55)[1]
str(ram.t2)
write.csv(ram.t2, file="D:/Rutgers_postdoc/data/RAM legacy/ram55_cln.csv")

#global wpmf for all 105 stocks
ram.t2<-read.csv(file="D:/Rutgers_postdoc/data/RAM legacy/ram55_cln.csv", header = TRUE)
str(ram.t2)
rownames(ram.t2)<-ram.t2[,1]
ram.t2<-ram.t2[,-1]
colnames(ram.t2)<-seq(1955,2009, by=1)
ram.t2<-as.matrix(ram.t2)#this finally worked.
wsurfplot(ram.t2, times=1955:2009, type="wmf", colorbar=T,neat=T, title="Global RAM wmf")
wsurfplot(ram.t2, times=1955:2009, type="wpmf", colorbar=T,neat=T, title="Global RAM wpmf")

wmf.ram<-wmf(ram.t2, times=1955:2009, scale.min=2, scale.max.input=18, sigma=1.05, f0=1)
timeavg.wmf<-colMeans(Mod(wmf.ram$wmf)^2,  na.rm=T)
plot(wmf.ram$timescales, timeavg.wmf, type="l", xlab="Timescale", ylab="Power", main="Global RAM wmf power")

wpmf.ram<-wpmf(ram.t2, times=1955:2009, scale.min=2, scale.max.input = 18, sigma=1.05, f0=1, surrog=FALSE)
timeavg.wpmf<-colMeans(Mod(wpmf.ram$wpmf)^2, na.rm=T)
plot(wpmf.ram$timescales, timeavg.wpmf, type="l", xlab="Timescale", ylab="Power", main="Global RAM wpmf power")

png(file="D:/Rutgers_postdoc/data/RAM legacy/RAM wmf wpmf plots/GlobRAM_wpmf.png", width=480, height=720, units="px")
par(mfrow=c(2,1))
wsurfplot(ram.t2, times=1955:2009, type="wpmf", colorbar=T,neat=T, title="Global RAM wpmf")
plot(wpmf.ram$timescales, timeavg.wpmf, type="l", xlab="Timescale", ylab="Power", main="Global RAM wpmf power")
dev.off()

#wavelet analysis
#apply CleanData function to every row to normalize data using box-cox transformations
#data has to be a matrix of time series with rows as separate species (loc_sp) and columns as years

rtc.t1<-apply(ts.rtc55, MARGIN=1, CleanData)
rtc.t2<-NULL
for (i in 1:length(rtc.t1)){
  rtc.t2<-rbind(rtc.t2, rtc.t1[[i]]$cleandat)
}
#put dimnames into t2
str(ts.rtc55)
dimnames(ts.rtc55)[1]
dimnames(rtc.t2)[1]<-dimnames(ts.rtc55)[1]
str(rtc.t2)
write.csv(rtc.t2, file="D:/Rutgers_postdoc/data/RAM legacy/rtc55_cln.csv")

#to load rtc.t2 again, follow lines 248-253 to load correctly.
#try to order rtc.t2 by fao area, then family then genus.
#to do that, need to split fao_id into fao, then id. 
stk<-read.csv(file="D:/Rutgers_postdoc/data/RAM legacy/From Chris/ramldb_v3.8/all_stocks_in_ramldb.csv", header=TRUE)
names(stksub)
vts<-c(2,15,17)
stksub<-stk[,vts]

rtc.t3<-read.csv(file="D:/Rutgers_postdoc/data/RAM legacy/rtc55_cln.csv", header=TRUE)
head(rtc.t3)
rownames(rtc.t3)<-rtc.t3[,1]
rtc.t3<-rtc.t3[,-1]
colnames(rtc.t3)<-seq(1955,2009, by=1)
rtc.t3$fao<-as.factor(substr(rownames(rtc.t3), 1,2))
names(rtc.t3)
rtc.t3$stockid<-substring(rownames(rtc.t3), 3)
rtc.t4<- rtc.t3 %>% left_join(stksub, by="stockid")
names(rtc.t4)
which(is.na(rtc.t4$stockid))
rtc.ord<-rtc.t4[order(rtc.t4[,56], rtc.t4[,58], rtc.t4[,59]),]
names(rtc.ord)
rtc.ord$fao_id<-paste(rtc.ord$fao, rtc.ord$stockid, sep="")
#now rtc.ord is ordered by fao area, then family, then genus.
write.csv(rtc.ord, file="D:/Rutgers_postdoc/data/RAM legacy/rtc_cln_ord_stkid_fam_sp.csv")

rtc.t5<-read.csv(file="D:/Rutgers_postdoc/data/RAM legacy/rtc_cln_ord_stkid_fam_sp.csv", header=TRUE)
rtc.t5<-rtc.ord[1:55]
rownames(rtc.t5)<-rtc.ord[,60]
head(rtc.t5)
rtc.t5<-as.matrix(rtc.t5)

####putting rtc data into separate regions
ram.region<-c(21,27,31,34,37,41,47,51,57,61,67,71,77,81,87)
ram.dat<-list()
for (i in 1:15){
  ram.dat[[i]]<-subset(rtc.t5, substr(dimnames(rtc.t5)[[1]], 1,2) == as.character(ram.region[i]))
}

ram.dat[[13]]#18 species.
ram.titles<-c("Atlantic NW", "Atlantic NE", "Atlantic WC", "Atlantic EC", "Med & Black Sea", 
          "Atlantic SW", "Atlantic SE", "Indian West", "Indian East", "Pacific NW",
          "Pacific NE", "Pacific WC", "Pacific EC", "Pacific SW", "Pacific SE")
ram.titles[13]
#NB, 31 (Atl WC) and 57 (Indian East) only have one time series each.

wsurfplot(ram.dat[[13]], times=1955:2009, type="wpmf", colorbar=T,neat=T, title=as.character(ram.titles[13]))
####again putting rtc data into separate regions, but with 31 and 57 at the back
ram.region2<-c(21,27,34,37,41,47,51,61,67,71,77,81,87,31,57)
ram.dat2<-list()
for (i in 1:15){
  ram.dat2[[i]]<-subset(rtc.t5, substr(dimnames(rtc.t5)[[1]], 1,2) == as.character(ram.region2[i]))
}

ram.titles2<-c("Atlantic NW", "Atlantic NE", "Atlantic EC", "Med & Black Sea", "Atlantic SW",
              "Atlantic SE", "Indian West", "Pacific NW", "Pacific NE", "Pacific WC",
              "Pacific EC", "Pacific SW", "Pacific SE", "Atlantic WC", "Indian East")
ram.titles2[13]

wpmflist<-list()
timeavg<-list()
for (j in 1:13){
  wpmflist[[j]]<-wpmf(ram.dat2[[j]], times=1955:2009, scale.min=2, scale.max.input = 18, sigma=1.05, f0=1, surrog=FALSE)
  timeavg[[j]]<-colMeans(Mod(wpmflist[[j]]$wpmf)^2, na.rm=T)
  mypath=file.path("D:", "Rutgers_postdoc", "data", "RAM legacy", "RAM wmf wpmf plots", paste("wpmf_", ram.titles2[j], ".png", sep=""))
  png(file=mypath, width=480, height=720, units="px")
  par(mfrow=c(2,1))
  wsurfplot(ram.dat2[[j]], times=1955:2009, type="wpmf", colorbar=T,neat=T, title=as.character(ram.titles2[j]))
  plot(wpmflist[[j]]$timescales, timeavg[[j]], type="l", xlab="Timescale", ylab="Power", main=as.character(ram.titles2[j]))
  dev.off() 
}

wtlist<-list()
wt.timeavg<-list()
for (k in 14:15){
  wtlist[[k]]<-wt(ram.dat2[[k]], times=1955:2009, scale.min=2, scale.max.input = 18, sigma=1.05, f0=1)
  wt.timeavg[[k]]<-colMeans(Mod(wtlist[[k]]$wave)^2, na.rm=T)
  mypath=file.path("D:", "Rutgers_postdoc", "data", "RAM legacy", "RAM wmf wpmf plots", paste("wt_", ram.titles2[k], ".png", sep=""))
  png(file=mypath, width=480, height=720, units="px")
  par(mfrow=c(2,1))
  wsurfplot(ram.dat2[[k]], times=1955:2009, type="power", colorbar=T,neat=T, title=as.character(ram.titles2[k]))
  plot(wtlist[[k]]$timescales, wt.timeavg[[k]], type="l", xlab="Timescale", ylab="Power", main=as.character(ram.titles2[k]))
  dev.off() 
}

#test of code above for separate regions, eg. Atl NW, first one. 
wsurfplot(ram.dat2[[13]], times=1955:2009, type="wpmf", colorbar=T,neat=T, title="Pac SE wpmf")
plot(wpmflist[[1]]$timescales, timeavg[[1]], type="l", xlab="Timescale", ylab="Power", main=as.character(ram.titles2[1]))
#note that Atl NE (2-albacore tuna and swordfish), Atl NW (1 - both are yellowtail flounder), 
#Pac NW (8 - jap anchovy and swordfish), Pac SE (13 - yellowfin tuna and swordfish) and 
#Pac WC (10 - yellowfin and skipjack tuna) is unusual 
#check to see if they consist of the same species.
ram.dat2[[10]]

#see if i can get surplus production, calculate from biomass.t2-biomass.t1+catch.
#ram8 contains all the ts (128 stocks) with >=60 years of data, including fao areas. 
ram8<-read.csv(file="D:/Rutgers_postdoc/data/RAM legacy/ram_nyr60_fao.csv", header=TRUE)
str(ram8)

de2<- ram8 %>% group_by(fao_area) %>%
  summarise(n=n_distinct(stockid))
#shows number of stocks in each of the 15 fao areas.
de3<- ram8 %>% group_by(stockid, tsid) %>%
  summarise(meanyr=mean(nyrs))
unique(ram8$tsid)
de4<-subset(de3, de3$tsid=="TC-MT" | de3$tsid=="TB-MT")
de5<- de4 %>% group_by(stockid)
  filter(de5, de5$tsid=="TC-MT" & de5$tsid=="TB-MT")
df4<-as.data.frame(de4)
unique(df4$tsid)
dup.row<-which(duplicated(df4[,1]))
abc<-df4[dup.row,]
stk.sp<-as.character(abc[,1])
str(ram8)
de6<-ram8[ram8$stockid %in% stk.sp,]
str(de6)
unique(de6$stockid)#great, only has the 58 stocks
unique(de6$tsid)
de7<-subset(de6, de6$tsid=="TC-MT" | de6$tsid=="TB-MT")
str(de7)
unique(de7$stockid)#great, only has the 58 stocks
unique(de7$tsid)#only has TB and TC. 
tbonly<-subset(de7, de7$tsid=="TB-MT")
unique(tbonly$tsid)
str(tbonly)
tconly<-subset(de7, de7$tsid=="TC-MT")
unique(tconly$tsid)
tbonly$tb<-tbonly$tsvalue
var2sel<-c(1,2,4,6:11)
tbonly2<-tbonly[,var2sel]
str(tbonly2)
str(tconly)
tconly$tc<-tconly$tsvalue
v2s2<-c(1,4,11)
tconly2<-tconly[,v2s2]
str(tconly2)
unique(tconly2$stockid)
splusprod<-left_join(tbonly2, tconly2, by=c("stockid", "tsyear"))
str(splusprod)
splusprod2<- splusprod %>% group_by(stockid) %>% 
  mutate(sprod = (tb - lag(tb)) + tc)
str(splusprod2)
write.csv(splusprod2, file="D:/Rutgers_postdoc/data/RAM legacy/surplusprod_58stocks.csv")
#need to get rid of NAs and the years that aren't good. 
sppd58<-read.csv(file="D:/Rutgers_postdoc/data/RAM legacy/surplusprod_58stocks.csv")
sppd58<-sppd58[,2:12]
#need to add in sp name and family
#I went through sppd58 and got rid of the NAs and only retained reliable years.
sppd58<-read.csv(file="D:/Rutgers_postdoc/data/RAM legacy/surplusprod_58stocks_reliable_yrs.csv")
str(sppd58)
spkey<-read.csv(file="D:/Rutgers_postdoc/data/RAM legacy/stockid_fam_sp_fao_key.csv")
str(spkey)
sprod58<- sppd58 %>% left_join(spkey, by="stockid")
str(sprod58)
which(is.na(sprod58))
sprod58[which(is.na(sprod58)),]
sprod58[4533:4560,]
sprod58<-sprod58[,-10]
sprod58only<-sprod58[1:4532,]
which(is.na(sprod58only))
write.csv(sprod58, file="D:/Rutgers_postdoc/data/RAM legacy/surplusprod_58_all.csv")
sprod2<-read.csv(file="D:/Rutgers_postdoc/data/RAM legacy/surplusprod_58_all_2.csv")
which(is.na(sprod2))#why is there still na? Because there was one row of NAs
sprod3<-read.csv(file="D:/Rutgers_postdoc/data/RAM legacy/surplusprod_58_all_3.csv")
str(sprod3)
which(is.na(sprod3))
xyz<-sprod3 %>% group_by(fao_area) 
abc<- sprod3 %>% group_by(stockid) %>%
  summarise(minyr=min(tsyear), maxyr=max(tsyear), fao=mean(fao_area), mean=mean(sprod))
#plot data with ggplot2
p1<-ggplot(data=sprod3, aes(x=tsyear, y=sprod, colour=stockid)) + geom_line(show.legend=FALSE)
p1 + facet_wrap(~as.factor(fao_area), nrow=3, scales="free_y") +theme_bw()

p1a<-ggplot(data=sprod3, aes(x=tsyear, y=sprod, colour=as.factor(fao_area))) + geom_line()
p1a + facet_wrap(~family, nrow=3, scales="free_y") +theme_bw()

################this to line 496 is my stupidity
max(sprod3$sprod)#-196000, 1044000
#maybe need to separate data for herring and tuna from the rest since the numbers are huge.
#or can I just put the negative values into CleanData?
#ok skipjack tuna from Indian ocean has huge numbers, try changing only those.
sprod4<-sprod3 %>% mutate(sprod1=ifelse(stockid=="SKJCIO", sprod/100, sprod))
str(sprod4)#worked.
p2<-ggplot(data=sprod4, aes(x=tsyear, y=sprod1, colour=stockid)) + geom_line(show.legend = FALSE)
p2 + facet_wrap(~as.factor(fao_area), nrow=2) +theme_bw()
#still did not work, use conditional if sprod>10000 or less than -10000
sprod5<-sprod3 %>% mutate(sprod2=ifelse(sprod>=10000 | sprod<=(-10000), sprod/1000, sprod))
#can't do this because some stocks have values below and above 10000.
#do specifically using stocks.
sprod6<-sprod3 %>% mutate(sprod3=if_else(stockid=="SKJCIO" | stockid=="ALBANATL" | stockid=="BIGEYEATL" 
                                        | stockid=="CHAKESA" | stockid=="CTRACSA" | stockid=="DEEPCHAKESA" 
                                        |  stockid=="HERRSOG" | stockid=="PACBTUNA" | stockid=="SBT" 
                                        |  stockid=="SKJCIO" | stockid=="SKJWATL" | stockid=="SWORDNATL" 
                                        |  stockid=="SWORDNPAC" | stockid=="YFINATL" | stockid=="HERRCC"
                                        | stockid=="HERRPRD" | stockid=="HERRQCI" | stockid=="HERRWCVANI" , sprod/100, sprod))
p3<-ggplot(data=sprod6, aes(x=tsyear, y=sprod3, colour=stockid)) + geom_line(show.legend = FALSE)
p3 + facet_wrap(~as.factor(fao_area), nrow=2) +theme_bw()
which(sprod6$sprod3>10000)
#fine, maybe don't use ggplot for multiple graphs. plot according to different scales.
unique(sprod3$family)
p4<-ggplot(data=sprod3, aes(x=tsyear, y=sprod, colour=stockid)) + geom_line(show.legend = FALSE)
p4 + facet_wrap(~family, nrow=2) +theme_bw()
sprod7<- filter(sprod3, !family=="Scombridae")
unique(sprod7$family)
p5<-ggplot(data=sprod7, aes(x=tsyear, y=sprod, colour=stockid)) + geom_line(show.legend = FALSE)
p5 + facet_wrap(~family, nrow=2) +theme_bw()
less50k<-filter(sprod3, family=="Anoplopomatidae" | family=="Haliotidae" | family=="Istiophoridae"
                | family=="Palinuridae" | family=="Pleuronectidae" | family=="Rajidae" | family=="Sebastidae"
                | family=="Sparidae" | family=="Squalidae" | family=="Xiphiidae")
p6<-ggplot(data=less50k, aes(x=tsyear, y=sprod, colour=stockid)) + geom_line(show.legend = FALSE)
p6 + facet_wrap(~family, nrow=2) +theme_bw()
########################################################
#test to see if CleanData function can handle negative values
testmat<-matrix(NA, nrow=2, ncol=55)
testmat[,]<-runif(110, min=-50000, max=150000)
str(testmat)
rownames(testmat)<-c("sp1","sp2")
colnames(testmat)<-seq(1955, 2009, by=1)
testcln<-CleanData(testmat, normalize=TRUE, each.ts=TRUE, detrend=FALSE)
testcln$cleandat
max(testmat[1,])#min=-45437.72, max=148918.3
max(testcln$cleandat[1,])#min=-2.168, max=1.436449
par(mar=c(5,6,2,3), mfrow=c(2,1))
plot(x=colnames(testmat), y=testmat[1,], type="l")
plot(x=colnames(testcln$cleandat), y=testcln$cleandat[1,], type="l")
#conclusion, looks like CleanDat can handle large negative values.
#ok so now need to get the data into a format for CleanData, each row is one stock and each column is one year.
#but maybe I need to order it by fao, then family then species.
head(sprod3)
sprod3arr<-arrange(sprod3, fao_area, family, species)
#subset sprod3 by tsyear, fao_id and sprod
str(sprod3arr)
ciw<-c(11,3,8)
sp3arsub<-sprod3arr[,ciw]
str(sp3arsub)
sp3ord<-table2matrix(sp3arsub)
head(sp3ord)
#full time series dataset of 58 stocks from 1893 with heaps of NAs.
write.csv(sp3ord, file="D:/Rutgers_postdoc/data/RAM legacy/surplusprod_58stocks_ordered_1893-2015.csv")
seq(1893, 2015, by=1)
#limit to data from 1953-2010 (should be 41 stocks), 58yr time series
sp19532010<-sp3ord[,61:118]
which(is.na(sp19532010))
sp1953.2010<-sp19532010[complete.cases(sp19532010),]
which(is.na(sp1953.2010))

##finding thorny head, SSTHORNHPCOAST
thornyhead<-subset(timeseries, timeseries[,2] %in% "SSTHORNHPCOAST")
str(thornyheaddf)
thornyheaddf<-as.data.frame(thornyhead)
tail(thornyheaddf)
unique(thornyheaddf[,4])
thrct<-subset(thornyheaddf, thornyheaddf[,4] %in% "R-E00")
str(thrct)

#find sablefish SABLEFPCOAST and dover sole DSOLEPCOAST and petrale sole PSOLEPCOAST
fish<-c("SABLEFPCOAST", "DSOLEPCOAST", "PSOLEPCOAST")
fishdat<-subset(timeseries, timeseries[,2] %in% fish)
head(fishdat)
unique(fishdat[,4])
fishdatrct<-subset(fishdat, fishdat[,4] %in% "R-E00")

fish2<-c("LSTHORNHPCOAST", "PHAKEPCOAST", "ALBANPAC")
fish2dat<-subset(timeseries, timeseries[,2] %in% fish2)
head(fish2dat)
unique(fish2dat[,4])
fish2datrct<-subset(fish2dat, fish2dat[,4] %in% "R-E00")
head(fish2datrct)

####################################################
