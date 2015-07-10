
require(RODBC)
--odbcConnect()
conn <- odbcDriverConnect('driver={SQL Server};server=NAEMSQL02\\SPREPORTING;database=SP_REPORTING;trusted_connection=true')
sql <- "SELECT POS_YEAR,POS_MTH_NBR,SUM(POS_UNITS) as units
  FROM R_POS_VIEW_MONTHLY
  WHERE SEGMENT1 = 'MULTIMEDIA LEARNING PLATFORMS' AND SEGMENT2 = 'PLATFORM'
  GROUP BY POS_YEAR, POS_MTH_NBR,SEGMENT1,SEGMENT2
  ORDER BY POS_YEAR, POS_MTH_NBR"
sql <- strwrap(sql, width=10000, simplify=TRUE)
MMLSUM <- sqlQuery(conn, sql)
odbcClose(conn)

MML.HW <- split(TOYS,TOYS$SEGMENT3)
