##INPUT EXPECTATIONS: 
#item[,forecast_table(.SD),by=.(SEGMENT4,TT_CUSTOMER)]
#where data.table "item" contains: SEGMENT4,TT_CUSTOMER,POS_YEAR,POS_MTH_NBR,units

#Sample to Run By Customer and Segment4
item <<- TOYS[#SEGMENT4=="MY PAL SCOUT" # & TT_CUSTOMER == "WMT"
     ,.(units = sum(units))
     ,keyby=.(SEGMENT4,TT_CUSTOMER,POS_YEAR,POS_MTH_NBR)][order(SEGMENT4,TT_CUSTOMER,POS_YEAR,POS_MTH_NBR)]
item[,forecast_table(.SD),by=.(SEGMENT4,TT_CUSTOMER)]

setkey(item,SEGMENT4,POS_YEAR,POS_MTH_NBR)
setkey(item.test,SEGMENT4,POS_YEAR,POS_MTH_NBR)


##
##
##
item <- TOYS[POS_YEAR > 2011#SEGMENT4=="MY PAL SCOUT" # & TT_CUSTOMER == "WMT" ##May want to reduce items to forecast
             ,.(units = sum(units))
             ,keyby=.(SEGMENT4,POS_YEAR,POS_MTH_NBR)][order(SEGMENT4,POS_YEAR,POS_MTH_NBR)]
item.test <- item[,forecast_table(.SD),by=.(SEGMENT4)]

#JOIN and MERGE ORIGINAL ACTUALS FOR COMPARISON
setkey(item,SEGMENT4,POS_YEAR,POS_MTH_NBR)
setkey(item.test,SEGMENT4,POS_YEAR,POS_MTH_NBR)
item.test <- item[item.test]

#CREATE SECONDARY TABLE FOR ACCURACY RESULTS              
item.acc <- (item.test[UNITS_FC!=0 & POS_YEAR == 2014])
item.acc.summary <- 
     item.acc[,acc(.SD),by=.(SEGMENT4),.SDcols = c("UNITS_FC","units")]

#setnames(item.fc2,1:7,c("POS_MTH_NBR","SSN_PROFILE","POS_YEAR","UNITS_FC","AF_FLAG","FC_CONF_80","MODEL"))

setkey(item,SEGMENT4,TT_CUSTOMER,POS_YEAR,POS_MTH_NBR)
setkey(item.test,SEGMENT4,TT_CUSTOMER,POS_YEAR,POS_MTH_NBR)
##
##
##
