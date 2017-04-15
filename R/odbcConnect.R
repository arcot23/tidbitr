library(RODBC)

#create a DSN under Control Panel -> Administrative tools
# -> ODBC with the intended connection

odbc.run_query <- function (query, dsn = "mydsn") {
  #open the connection
  myconn <- RODBC::odbcConnect(dsn)
  #dataset <- sqlFetch(myconn, "seg_accounts_attr")

  #fetch the data
  dataset <- RODBC::sqlQuery(myconn, query)

  #close the connection
  RODBC::close(myconn)

  #return the dataset
  dataset
}
