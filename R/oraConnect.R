require(rJava)
require(RJDBC)

ora.run_query <- function (query, host_name, port ="1521", sid = "xe", user_name, pwd) {
  # requires libary RJDBC
  jdbcDriver <- RJDBC::JDBC(driverClass="oracle.jdbc.OracleDriver", classPath="C:\\oraclexe\\app\\oracle\\product\\11.2.0\\server\\jdbc\\lib\\ojdbc5.jar")

  # open connection
  jdbcConnection <- RJDBC::dbConnect(jdbcDriver, paste0("jdbc:oracle:thin:@", host_name, ":", port, ":", sid), user_name,
                                     pwd)

  # execute query
  resultset <- RJDBC::dbGetQuery(jdbcConnection, query)

  # close connection
  RJDBC::dbDisconnect(jdbcConnection)
  resultset
}


ora.from_date <- function(x, century = F)
{
  as.Date(x, ifelse(century,"%d-%b-%Y", "%d-%b-%y" )  )
}
