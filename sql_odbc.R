
require(RODBC)
odbcConnect()
conn <- odbcDriverConnect('driver={SQL Server};server=NAEMSQL02\\SPREPORTING;database=SP_REPORTING;trusted_connection=true')
sql <- "SELECT SEGMENT3,POS_YEAR,POS_MTH_NBR,SUM(POS_UNITS) as units
  FROM R_POS_VIEW_MONTHLY
  WHERE SEGMENT1 = 'LEARNING TOYS'
  GROUP BY POS_YEAR, POS_MTH_NBR,SEGMENT1,SEGMENT2,SEGMENT3
  ORDER BY POS_YEAR, POS_MTH_NBR"
sql <- strwrap(sql, width=10000, simplify=TRUE)
TOYS <- sqlQuery(conn, sql)
TOYS.ALL <- split(TOYS,TOYS$SEGMENT3)
lapply(TOYS.ALL,HoltWinters)