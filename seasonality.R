
cpb <- function(x) {
     write.table(x, "clipboard", sep="\t", row.names=FALSE)
}

require(RODBC)

conn <- odbcDriverConnect('driver={SQL Server};server=NAEMSQL02\\SPREPORTING;database=SP_REPORTING;trusted_connection=true')
sql <- "SELECT SEGMENT4,GIN, TT_CUSTOMER, POS_YEAR, POS_MTH_NBR, POS_WK_NBR, SUM(POS_UNITS) as units
     FROM R_POS_VIEW_MONTHLY
WHERE SEGMENT1 = 'TOYS'
AND POS_YEAR > 2009
AND POS_WK_NBR < 53
AND POS_UNITS not between -5 and 5
GROUP BY SEGMENT4,GIN, POS_YEAR, POS_MTH_NBR, POS_WK_NBR, TT_CUSTOMER
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
     write.table(TOYS.CY_COMPLETE[TOYS.SEASON.MULTIPLE],file="seasonality.tab",sep="\t")


     
     
     

p <- ts(TOYS[SEGMENT4 == 'PICNIC BASKET' & TT_CUSTOMER == 'WMT'
             ,.(units = sum(units))
             ,keyby=.(POS_YEAR,POS_MTH_NBR)]$units)
        ,start = c(2010,11),frequency = 12)
plot(stl(p,"per"))


#> # April is missing
#     > zym <- zoo(1:5, as.yearmon("2000-01-01") + c(0, 1, 2, 4, 5)/12)
#> g <- seq(start(zym), end(zym), by = "week")
#> na.locf(zym, xout = g)
