# SteroidDoseStudy

This is the repository for steroid study - complication vs dose association
The index steroids are prednisolone and dexamethasone with systemic usage, and steroid dose is calculated to prednisolone equivalent dose with this code. 
We try to find the cutoff level of steroid dose causing musculoskeletal complications (osteoporosis, bone fracture, osteonecrosis), and with this package, the cutoff level of maximum AUC is determined in your own database.


## Features
1. Cumulative dose analysis
2. Cumulative days of use analysis
3. Average dose analysis

## Usage
Add below details of your CDM database in "CodeToRun.R"
```
cdmDatabaseSchema <- "" ## server_database.server_scheme 

connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = "", #"postgresql"
                                                                server = "",
                                                                user = "",
                                                                password = "",
                                                                port = NULL)
```

When error caused by missing JDBC drivers, download required JDBC drivers using:
```
Sys.setenv("DATABASECONNECTOR_JAR_FOLDER" = "C:/JDBC") 
downloadJdbcDrivers() #postgresql, redshift .. etc
``` 
