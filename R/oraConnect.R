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
#' @return SQL resultset in a tibble
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
    resultset <- tibble::as_data_frame(RJDBC::dbGetQuery(jdbcConnection, query))

    # close connection
    RJDBC::dbDisconnect(jdbcConnection)
    resultset
  }


#' @title Runs a SQL query in preferred environment.
#' @description Runs a SQL query in Oracle and returns a resultset.
#' @param query SQL Query to execute.
#' @param env Environment name as string. The function will parse the environment name to its equivalent connection string stored under ora.connstr.[env]. This connection must be defined in advance that must contain five variables host_name, port, sid, user_name, pwd. Defaults to environment dev.
#' E.g., ora.connstr.dev <- list(host_name = "localhost", port = "1521", sid = "xe", user_name = "scott", pwd = "tiger")
#'
#' @return SQL resultset as a tibble
#' @examples
#' ora.run("SELECT * FROM ALL_TABLES", "stg")
#'
ora.run <- function(query, env = "dev")
{
  eval(parse(text = paste0("conn_str = ora.connstr.", env)))
  ora.run_query(query, conn_str["host_name"], conn_str["port"], conn_str["sid"], conn_str["user_name"], conn_str["pwd"])
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
