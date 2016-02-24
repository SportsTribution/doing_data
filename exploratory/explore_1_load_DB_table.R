##This code won't work, but is an example of how to directly integrate a database in R
jdbcDriver <- JDBC(driverClass="oracle.jdbc.OracleDriver",
                   classPath="C:/ORACLE/Client64/jdbc/lib/ojdbc6.jar")
jdbcConnection <- RJDBC::dbConnect(jdbcDriver, ...)

rs = dbSendQuery(jdbcConnection, "SELECT * FROM TABLE_NAME")
DF = fetch(rs, n = 1)
DF.names <- names(DF)
dateCol <- DF.names[grep("DATE",DF.names)[1]]

ptm<-proc.time()
rs = dbSendQuery(jdbcConnection, 
                 "SELECT * FROM (SELECT * FROM TABLE_NAME ORDER BY dbms_random.value) WHERE rownum <= 100000")
print(proc.time() - ptm)
DF = fetch(rs, n = 20000)
dbClearResult(rs)
