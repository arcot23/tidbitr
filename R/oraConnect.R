require(rJava)
require(RJDBC)


#' @title Runs a SQL query in Oracle
#' @description Runs a SQL query in Oracle and returns a resultset.
#' @param query SQL Query to execute.
#' @param host_name Host name.
#' @param port Port name. Defaults to port 1521.
#' @param sid Service Id. Defaults to xe.
#' @param user_name User name for the connection in plain text.
#' @param pwd Password for the connection in plain text.
#' @return SQL resultset in a dataframe
#' @examples
#' ora.run_query("SELECT * FROM ALL_TABLES", "act", 1580, xe, "scott", "tiger")
#'
ora.run_query <-
  function (query,
            host_name,
            port = "1521",
            sid = "xe",
            user_name,
            pwd) {
    # requires libary RJDBC
    jdbcDriver <-
      RJDBC::JDBC(driverClass = "oracle.jdbc.OracleDriver", classPath = "C:\\oraclexe\\app\\oracle\\product\\11.2.0\\server\\jdbc\\lib\\ojdbc5.jar")

    # open connection
    jdbcConnection <-
      RJDBC::dbConnect(
        jdbcDriver,
        paste0("jdbc:oracle:thin:@", host_name, ":", port, ":", sid),
        user_name,
        pwd
      )

    # execute query
    resultset <- RJDBC::dbGetQuery(jdbcConnection, query)

    # close connection
    RJDBC::dbDisconnect(jdbcConnection)
    resultset
  }

#' @title Converts an Oracle date string to a date
#' @description Converts a string in Oracle date format(dd-MMM-yy) into a R Date.
#'
#' @param x Date string to be converted.
#' @param century Informs if the string has the century included (dd-MM-yyyy). Default is F.
#' @return Returns the date string as a date
#' @examples
#' ora.from_date("01-JAN-16")
#' ora.from_date("01-JAN-2017", T)

ora.from_date <- function(x, century = F)
{
  as.Date(x, ifelse(century, "%d-%b-%Y", "%d-%b-%y"))
}
