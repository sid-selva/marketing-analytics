# install.packages("RPostgres")

library(DBI)
library(odbc)


# Connect with postgres
con <- DBI::dbConnect(RPostgres::Postgres(),dbname = 'Employees',
                 host = 'localhost',
                 port = 5432,
                 user = 'postgres',
                 password = 'root')

dbListTables(con)

dbDisconnect(con)

con <- dbConnect(RPostgres::Postgres(),
                 host = 'localhost',
                 port = 5432,
                 user = 'postgres',
                 password = 'root')

dbListTables(con)

# Connect with ODBC
con <- DBI::dbConnect(odbc::odbc(),
                      Driver   = "PostgreSQL Driver",
                      Server   = "localhost",
                      Database = "Employees",
                      UID      = rstudioapi::askForPassword("postgres"),
                      PWD      = rstudioapi::askForPassword("root"),
                      Port     = 5432)

sort(unique(odbcListDrivers()[[1]]))

?odbc

# odbcListObjects(con)

dbGetQuery(con, "SELECT first_name FROM employees")
dbGetQuery(con, "SELECT count(first_name) FROM employees")
