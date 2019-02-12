#Load the RODBC library
library(RODBC)

#Enter the values for your database connection
#Ensure you update the values for hostname, uid, and pwd
dsn_driver <- "{IBM DB2 ODBC Driver}"
dsn_database <- "BLUDB"            # e.g. "BLUDB"
dsn_hostname <- "dashdb-entry-yp-lon02-01.services.eu-gb.bluemix.net" # e.g.: "awh-yp-small03.services.dal.bluemix.net"
dsn_port <- "50000"                # e.g. "50000" 
dsn_protocol <- "TCPIP"            # i.e. "TCPIP"
dsn_uid <- "dash13782"         # e.g. "dash104434"
dsn_pwd <- "_PH_F1tmxWy4"         # e.g. "7dBZ39xN6$o0JiX!m"

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

#Drop the table in case you have already created the table and are re-running this cell
dropttab <- sqlDrop(conn, "TESTR")

#Create a test table
createtab <- sqlQuery(conn, "CREATE TABLE TESTR (ID INTEGER PRIMARY KEY NOT NULL, NAME CHAR(6))", errors=FALSE)

#Perform error checking to see if the table was created successfully
if(createtab==-1){
  cat("An error has occurred:\n")
  err.msg <- odbcGetErrMsg(conn)
  for (error in err.msg) {
    cat(error,"\n")
  } 
} else {
  cat ("Table created successfully.\n")
}

#Get column information for the newly created test table
testcol.detail <- sqlColumns(conn, "TESTR")
print(testcol.detail[c(4,6,7,18)], row.names=FALSE)

#Create a dataframe with sample values
id <- c(1, 2, 3)
name <- c("BAT   ", "CAT   ", "DOG   ")
testrdf <- data.frame(id, name)
#view the contents of the dataframe
testrdf

#save the contents of the test dataframe into the test table
sqlSave(conn, testrdf, "TESTR", append=TRUE, fast=FALSE, rownames=FALSE, colnames=FALSE)

#check the contents were saved in the database by fetching the contents of the table
testrdb <- sqlFetch(conn, "TESTR", stringsAsFactors=FALSE)
testrdb


#find the ID of last row in the table and increment it
newID <- nrow(testrdb) + 1
newID

#create a data frame with the new row with incremented ID
newRow <- data.frame("ID"=newID,"NAME"="FOG   ")
newRow

#Now let's insert the new row in the table
#Note below append=TRUE
sqlSave(conn, newRow, "TESTR", append=TRUE, fast=FALSE, rownames=FALSE, colnames=FALSE)

#check the new row was inserted in the table by fetching the contents of the table
testrdbnew <- sqlFetch(conn, "TESTR", stringsAsFactors=FALSE)
testrdbnew

#Add a few more rows to the database
#First create a dataframe with couple more rows
ID <- c(5,6,7)
NAME <- c('JOG   ','LOG   ','FOG   ')
newRows <- data.frame(ID,NAME)
newRows

#Add these new rows to the dataframe we retrieved from the database
testrdbnewrows <- rbind(testrdbnew,newRows)
testrdbnewrows

# Insert the dataframe into a NEW table
dropttab <- sqlDrop(conn, "TESTR2")  #Ensure the table does not exist
#Because TESTR2 does not exist it will be created
sqlSave(conn, testrdbnewrows, "TESTR2", fast=FALSE, rownames=FALSE, colnames=FALSE)

#let's fetch the contents of the new table
testrdb2 <- sqlFetch(conn, "TESTR2", stringsAsFactors=FALSE)
#See if the contents o fthe new table match those of testdbnewrows we inserted
testrdb2

#Now lets update some data
#Set a new value for the second column in sixth row
testrdb2[6,2] <- "MEG   "


sqlUpdate(conn, testrdb2,"TESTR2", index="ID")
#fetch the contents of the updated table
testrdb3 <- sqlFetch(conn, "TESTR2", stringsAsFactors=FALSE)
testrdb3
#Note: Rather than use SQLUpdate, sometimes it may be more efficient to modify the data using the SQL UPDATE statement via sqlQuery()
sqlQuery(conn,"UPDATE TESTR2 SET NAME='GOT   ' WHERE ID = 5", errors=FALSE)
testrdb4 <- sqlFetch(conn, "TESTR2", stringsAsFactors=FALSE)
testrdb4

#close connection
close(conn)