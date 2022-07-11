#Connecting R using a SQL server. 


library(odbc)
library(DBI)
library(RODBC)
trial <- "DDS_Central"
con <- DBI::dbConnect(odbc::odbc(), Driver="SQL Server", Server="WBGMSSQLDCSP001",
                      UID      = "DEC_ICPUser",
                      PWD      = rstudioapi::askForPassword("DEC_2011!ICRdnY"),
                      Database="ICP_Working",Port = 1433)








#Host:WBGMSSQLDCSP001
#Port: 1433
#Username: DEC_ICPUser
#Password: DEC_2011!ICRdnY


