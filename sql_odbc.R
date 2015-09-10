
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



d <- dcast.data.table(
          TOYS[,.(units:=sum(units)),keyby=.(POS_YEAR,TT_CUSTOMER,SEGMENT4,GIN,SPRING=POS_MTH_NBR<8)]
          ,POS_YEAR + TT_CUSTOMER + SEGMENT4 + GIN ~ SPRING
          ,value.var = "units")
setnames(d,c("FALSE","TRUE"),c("FALL","SPRING"))
d[,`:=`(SSN_MULTIPLE = round(FALL / SPRING,3),CAL_UNITS = (FALL+SPRING))]



p <- ts(TOYS[SEGMENT4 == 'PICNIC BASKET' & TT_CUSTOMER == 'WMT'
             ,.(units = sum(units))
             ,keyby=.(POS_YEAR,POS_MTH_NBR)]$units)
        ,start = c(2010,11),frequency = 12)
plot(stl(p,"per"))


#> # April is missing
#     > zym <- zoo(1:5, as.yearmon("2000-01-01") + c(0, 1, 2, 4, 5)/12)
#> g <- seq(start(zym), end(zym), by = "week")
#> na.locf(zym, xout = g)
