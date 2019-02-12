# Load RODBC as the RODBC package and the ODBC driver for Db2 Warehouse are pre-installed
library(RODBC)

#Create a database connection
dsn_driver <- "{IBM DB2 ODBC Driver}"
dsn_database <- "BLUDB"            # e.g. "BLUDB"
dsn_hostname <- "dashdb-entry-yp-lon02-01.services.eu-gb.bluemix.net" # e.g.: "awh-yp-small03.services.dal.bluemix.net"
dsn_port <- "50000"                # e.g. "50000" 
dsn_protocol <- "TCPIP"            # i.e. "TCPIP"
dsn_uid <- "dash13782"         # e.g. "dash104434"
dsn_pwd <-  "_PH_F1tmxWy4"         # e.g. "7dBZ39xN6$o0JiX!m"

#Create a connection string and connect to the database
conn_path <- paste("DRIVER=",dsn_driver,
                   ";DATABASE=",dsn_database,
                   ";HOSTNAME=",dsn_hostname,
                   ";PORT=",dsn_port,
                   ";PROTOCOL=",dsn_protocol,
                   ";UID=",dsn_uid,
                   ";PWD=",dsn_pwd,sep="")
conn <- odbcDriverConnect(conn_path)
conn

#View database and driver information
sql.info <- sqlTypeInfo(conn)
conn.info <- odbcGetInfo(conn)
conn.info["DBMS_Name"]
conn.info["DBMS_Ver"]
conn.info["Driver_ODBC_Ver"]

# Create the tables
myschema <- "dash13782"
tables <- c("BOARD", "SCHOOL")

for (table in tables){  
  # Drop School table if it already exists
  out <- sqlTables(conn, tableType = "TABLE", schema = myschema, tableName =table)
  if (nrow(out)>0) {
    err <- sqlDrop (conn, paste(myschema,".",table,sep=""), errors=FALSE)  
    if (err==-1){
      cat("An error has occurred.\n")
      err.msg <- odbcGetErrMsg(conn)
      for (error in err.msg) {
        cat(error,"\n")
      }
    } else {
      cat ("Table: ",  myschema,".",table," was dropped\n")
    }
  } else {
    cat ("Table: ",  myschema,".",table," does not exist\n")
  }
}

df1 <- sqlQuery(conn, "CREATE TABLE BOARD (
                            B_ID CHAR(6) NOT NULL, 
                            B_NAME VARCHAR(75) NOT NULL, 
                            TYPE VARCHAR(50) NOT NULL, 
                            LANGUAGE VARCHAR(50), 
                            PRIMARY KEY (B_ID))", 
                errors=FALSE)
#to check if succesful
if (df1 == -1){
  cat ("An error has occurred.\n")
  msg <- odbcGetErrMsg(conn)
  print (msg)
} else {
  cat ("Table was created successfully.\n")
}

df2 <- sqlQuery(conn, "CREATE TABLE SCHOOL (
                  B_ID CHAR(6) NOT NULL, 
                S_ID CHAR(6) NOT NULL, 
                S_NAME VARCHAR(50), 
                LEVEL VARCHAR(70), 
                ENROLLMENT INTEGER WITH DEFAULT 10,
                PRIMARY KEY (B_ID, S_ID))", errors=FALSE)
if (df2 == -1){
  cat ("An error has occurred.\n")
  msg <- odbcGetErrMsg(conn)
  print (msg)
} else {
  cat ("Table was created successfully.\n")
}

#Create the dataframe from the database data.
tab.frame <- sqlTables(conn, schema=myschema)
nrow(tab.frame)
tab.frame$TABLE_NAME

#Print columns 4, 6, 7, and 18.
for (table in tables){  
  cat ("\nColumn info for table", table, ":\n")
  col.detail <- sqlColumns(conn, table)
  print(col.detail[c(4,6,7,18)], row.names=FALSE)
}

#Load the data 
boarddf <- read.csv("/resources/data/samples/osb/board.csv", header = FALSE)
schooldf <- read.csv("/resources/data/samples/osb/school.csv",header=FALSE)

colnames(boarddf) <- c('B_ID','B_NAME','TYPE','LANGUAGE')
colnames(schooldf) <- c('B_ID','S_ID','S_NAME','LEVEL','ENROLLMENT')

sqlSave(conn, boarddf, "BOARD", append=TRUE, fast=FALSE, rownames=FALSE, colnames=FALSE)
boarddb <- sqlFetch(conn, "BOARD")
tail(boarddb)

schooldf$S_NAME <- iconv(schooldf$S_NAME, "latin1", "ASCII//TRANSLIT")

sqlSave(conn, schooldf, "SCHOOL", append=TRUE, fast=FALSE, rownames=FALSE, colnames=FALSE)
schooldb <- sqlFetch(conn, "SCHOOL")
tail(schooldb)

#Plot the data
library(ggplot2)

elequery <- query <- paste("select s.enrollment as ENROLLMENT 
from school s, board b 
where b.b_name = 'Toronto DSB' and b.b_id=s.b_id 
and s.level = 'Elementary' 
order by enrollment desc")

eledf <- sqlQuery(conn, elequery)
dim(eledf)

qplot(ENROLLMENT, data=eledf, geom="density",  main="TDSB School Size - Elementary")


secquery <- paste("select s.enrollment as ENROLLMENT 
from school s, board b 
where b.b_name = 'Toronto DSB' and b.b_id=s.b_id 
and s.level = 'Secondary' 
order by enrollment desc")
secdf <- sqlQuery(conn, secquery)
dim(secdf)

qplot(ENROLLMENT, data=secdf, geom="density",  main="TDSB School Size - Secondary")


denquery <- paste("select b.b_name, s.s_name, level as LEVEL, enrollment 
 from board b, school s where b.b_id = s.b_id and b.b_name = 'Toronto DSB'")
dendf <- sqlQuery(conn, denquery)
dendf$LEVEL <- as.factor(dendf$LEVEL)
boxplot(ENROLLMENT ~ LEVEL, dendf, names =c("Secondary","Elementary"), main="Toronto DSB")

#Disconnect
close(conn)
