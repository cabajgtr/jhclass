
cpb <- function(x) {
     write.table(x, "clipboard", sep="\t", row.names=FALSE)
}
 
require(RODBC)

conn <- odbcDriverConnect('driver={SQL Server};server=NAEMSQL02\\SPREPORTING;database=SP_REPORTING;trusted_connection=true')
sql <- "SELECT SEGMENT2,SEGMENT4,GIN, TT_CUSTOMER, POS_YEAR, POS_MTH_NBR, POS_WK_NBR, SUM(POS_UNITS) as units
     FROM R_POS_VIEW_MONTHLY
WHERE SEGMENT2 NOT IN  ('SPECIALS','LEGACY')
AND SEGMENT2 NOT LIKE ('OTHER%')
AND POS_YEAR > 2009
AND POS_WK_NBR < 53
AND POS_UNITS not between -5 and 5
GROUP BY SEGMENT2,SEGMENT4,GIN, POS_YEAR, POS_MTH_NBR, POS_WK_NBR, TT_CUSTOMER
HAVING SUM(POS_UNITS) <> 0
ORDER BY SEGMENT4, TT_CUSTOMER, POS_YEAR, POS_MTH_NBR, POS_WK_NBR"
sql <- strwrap(sql, width=10000, simplify=TRUE)
TOYS <- data.table(sqlQuery(conn, sql))
odbcClose(conn)

#TOYS.SOME <- split(TOYS,TOYS$SEGMENT4)


##TOYS -- SPLIT by Season (months 1-7 vs 8-12)
TOYS.SEASON <- TOYS[,.(units=sum(units)),keyby=.(POS_YEAR,TT_CUSTOMER,SEGMENT4,GIN,POS_MTH_NBR,SPRING=POS_MTH_NBR<8)]
TOYS.SEASON <- TOYS.SEASON[,.(units=sum(units),MoCt=.N),keyby=.(SEGMENT4,TT_CUSTOMER,GIN,POS_YEAR,SPRING)]
##GROUP and count months for complete seasons:
     #ASSUME FALSE, and then reset for TRUE
     TOYS.SEASON[,full_season:=FALSE]
     #SPRING & Complete (7 months)
     TOYS.SEASON[SPRING==TRUE & MoCt==7,full_season:=TRUE]
     #SPRING & incomplete (7 months)
     TOYS.SEASON[SPRING==FALSE & MoCt==5,full_season:=TRUE]
     #FALL & incomplete (5 months)
     TOYS.SEASON[,season_index:=0]
     TOYS.SEASON[SPRING==FALSE,season_index:=1]

#Consolidate Seasonal "full_season" to get "full_calyear"  Will join with TOYS.SEASONAL.MULTIPLE BELOW:
TOYS.CY_COMPLETE <- TOYS.SEASON[,.(full_cy=as.logical(min(full_season)),ssn_ct=.N),keyby=.(SEGMENT4,TT_CUSTOMER,GIN,POS_YEAR)]
     #fix false positives when only 1 TRUE season existed in Year
     TOYS.CY_COMPLETE[ssn_ct==1,full_cy:=FALSE]

#DCAST SEASONS onto one row, to get ratio and full_cy
TOYS.SEASON.MULTIPLE <- dcast.data.table(
     TOYS.SEASON[full_season==TRUE]
     ,POS_YEAR + TT_CUSTOMER + SEGMENT4 + GIN ~ SPRING
     ,value.var = "units",fun.aggregate = sum)
     #cleanup
     setnames(TOYS.SEASON.MULTIPLE,c("FALSE","TRUE"),c("FALL","SPRING"))
     #Calc Ratios and Cal Year unit Total
     TOYS.SEASON.MULTIPLE[,`:=`(SSN_MULTIPLE = round(FALL / SPRING,3),CAL_UNITS = (FALL+SPRING))]

##JOIN all data
     setkey(TOYS.CY_COMPLETE,TT_CUSTOMER,SEGMENT4,GIN,POS_YEAR)
     setkey(TOYS.SEASON.MULTIPLE,TT_CUSTOMER,SEGMENT4,GIN,POS_YEAR)
     ##Calcs Number of completed seasons and joins to TOYS.SEASON table
     TOYS.CY_COMPLETE <- TOYS.SEASON[full_season==TRUE,.(seasons_completed=.N),keyby=.(TT_CUSTOMER,SEGMENT4,GIN)][TOYS.CY_COMPLETE]
     setkey(TOYS.CY_COMPLETE,TT_CUSTOMER,SEGMENT4,GIN,POS_YEAR)
     #Join CY_COMPLETE to SEASON.MULTIPLE
     TOYS.SEASON.MULTIPLE <- TOYS.CY_COMPLETE[TOYS.SEASON.MULTIPLE]
     #SET "full_cy" to FALSE in case where SSN_MULTITUE is less than 1 (this means item was in clearance, not a good multiple)
     TOYS.SEASON.MULTIPLE[SSN_MULTIPLE<1,full_cy:=FALSE]
     #Save to file
     write.table(TOYS.SEASON.MULTIPLE,file="seasonality.tab",sep="\t")

###########
###########     
###########
###########
###########
##########BY SEGMENT4 ONLY (no GIN)
     ##TOYS -- SPLIT by Season (months 1-7 vs 8-12)
     TOYS.SEASON <- TOYS[,.(units=sum(units)),keyby=.(POS_YEAR,TT_CUSTOMER,SEGMENT4,POS_MTH_NBR,SPRING=POS_MTH_NBR<8)]
     TOYS.SEASON <- TOYS.SEASON[,.(units=sum(units),MoCt=.N),keyby=.(SEGMENT4,TT_CUSTOMER,POS_YEAR,SPRING)]
     ##GROUP and count months for complete seasons:
     #ASSUME FALSE, and then reset for TRUE
     TOYS.SEASON[,full_season:=FALSE]
     #SPRING & Complete (7 months)
     TOYS.SEASON[SPRING==TRUE & MoCt==7,full_season:=TRUE]
     #SPRING & incomplete (7 months)
     TOYS.SEASON[SPRING==FALSE & MoCt==5,full_season:=TRUE]
     #FALL & incomplete (5 months)
     TOYS.SEASON[,season_index:=0]
     TOYS.SEASON[SPRING==FALSE,season_index:=1]
     
     #Consolidate Seasonal "full_season" to get "full_calyear"  Will join with TOYS.SEASONAL.MULTIPLE BELOW:
     TOYS.CY_COMPLETE <- TOYS.SEASON[,.(full_cy=as.logical(min(full_season)),ssn_ct=.N),keyby=.(SEGMENT4,TT_CUSTOMER,POS_YEAR)]
     #fix false positives when only 1 TRUE season existed in Year
     TOYS.CY_COMPLETE[ssn_ct==1,full_cy:=FALSE]
     
     #DCAST SEASONS onto one row, to get ratio and full_cy
     TOYS.SEASON.MULTIPLE <- dcast.data.table(
          TOYS.SEASON[full_season==TRUE]
          ,POS_YEAR + TT_CUSTOMER + SEGMENT4 ~ SPRING
          ,value.var = "units",fun.aggregate = sum)
     #cleanup
     setnames(TOYS.SEASON.MULTIPLE,c("FALSE","TRUE"),c("FALL","SPRING"))
     #Calc Ratios and Cal Year unit Total
     TOYS.SEASON.MULTIPLE[,`:=`(SSN_MULTIPLE = round(FALL / SPRING,3),CAL_UNITS = (FALL+SPRING))]
     
     ##JOIN all data
     setkey(TOYS.CY_COMPLETE,TT_CUSTOMER,SEGMENT4,POS_YEAR)
     setkey(TOYS.SEASON.MULTIPLE,TT_CUSTOMER,SEGMENT4,POS_YEAR)
     ##Calcs Number of completed seasons and joins to TOYS.SEASON table
     TOYS.CY_COMPLETE <- TOYS.SEASON[full_season==TRUE,.(seasons_completed=.N),keyby=.(TT_CUSTOMER,SEGMENT4)][TOYS.CY_COMPLETE]
     setkey(TOYS.CY_COMPLETE,TT_CUSTOMER,SEGMENT4,POS_YEAR)
     #Join CY_COMPLETE to SEASON.MULTIPLE
     TOYS.SEASON.MULTIPLE <- TOYS.CY_COMPLETE[TOYS.SEASON.MULTIPLE]
     #SET "full_cy" to FALSE in case where SSN_MULTITUE is less than 1 (this means item was in clearance, not a good multiple)
     TOYS.SEASON.MULTIPLE[SSN_MULTIPLE<1,full_cy:=FALSE]
     #Save to file
     write.table(TOYS.SEASON.MULTIPLE,file="seasonality_PL.tab",sep="\t")     
     
     
###########
###########     
     # function for mean labels
     mean.n <- function(x){
          return(c(y = median(x)*1.1, label = round(mean(x),1))) 
          # experiment with the multiplier to find the perfect position
     }
     # function for number of observations 
     give.n <- function(x){
          return(data.frame(y = median(x)*0, label = paste0("n = ",length(x))))
          # experiment with the multiplier to find the perfect position
     }
##BOXPLOT SEASONALITY DISTRIBUTION BY ACCOUNT
g = ggplot(TOYS.SEASON.MULTIPLE[full_cy==TRUE & TT_CUSTOMER %in% c("WMT","TGT","TRU","AMZ")],aes(y=SSN_MULTIPLE,x=TT_CUSTOMER))
g = g + geom_boxplot(binaxis = "x", size = 1, stackdir = "center")
g = g + scale_y_continuous(breaks=seq(0,20,2),limits=c(0,15))
g = g + geom_text(data = means, aes(label = weight, y = weight + 0.08))
g = g + stat_summary(fun.data = mean.n, geom = "text", fun.y = median, color = "red")
g = g + stat_summary(fun.data = give.n, geom = "text")
g +coord_flip() #flip horizontal for easier reading
##BOXPLOT SEASONALITY DISTRIBUTION BY ACCOUNT
g = ggplot(TOYS.SEASON.MULTIPLE[full_cy==TRUE & TT_CUSTOMER %in% c("WMT","TGT","TRU","AMZ")],aes(y=SSN_MULTIPLE,x=SEGMENT4))
g = g + geom_boxplot(binaxis = "x", size = 1, stackdir = "center")
g = g + scale_y_continuous(breaks=seq(0,8,2),limits=c(0,7))
g = g + theme(axis.text.x = element_text(angle = 90, hjust = 1))
g = g+ facet_grid(.~TT_CUSTOMER)
g = g + stat_summary(fun.data = mean.n, geom = "text", fun.y = mean, color = "red")
g = g + stat_summary(fun.data = give.n, geom = "text")
g +coord_flip() #flip horizontal for easier reading

###########
###########
###########
     
##DCAST BY YEAR, TO HAVE SUMMARY BY ITEM for CLUSTERING     
     TOYS.SSN.YRCLUSTER <- dcast.data.table(
          TOYS.SEASON.MULTIPLE[full_cy==TRUE]
          ,TT_CUSTOMER + SEGMENT4 ~ POS_YEAR
          ,value.var = "SSN_MULTIPLE")     



####Weekly Seaonality Prep
##Eliminating GIN from the get-go
TOYS.WEEKLY <- TOYS[,.(units=sum(units)),keyby=.(POS_YEAR,TT_CUSTOMER,SEGMENT4,POS_MTH_NBR,POS_WK_NBR,SPRING=POS_MTH_NBR<8)]
#create seasonality index for KEY
TOYS.WEEKLY[,season_index:=0]
TOYS.WEEKLY[SPRING==FALSE,season_index:=1]

#JOIN
setkey(TOYS.WEEKLY,POS_YEAR,TT_CUSTOMER,SEGMENT4,season_index)
setkey(TOYS.SEASON,POS_YEAR,TT_CUSTOMER,SEGMENT4,season_index)
TOYS.SEASON[TOYS.WEEKLY]




#> # April is missing
#     > zym <- zoo(1:5, as.yearmon("2000-01-01") + c(0, 1, 2, 4, 5)/12)
#> g <- seq(start(zym), end(zym), by = "week")
#> na.locf(zym, xout = g)
