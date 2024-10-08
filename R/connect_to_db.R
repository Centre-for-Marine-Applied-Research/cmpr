library(keyring)
library(DBI)

# Get configuration file with database connection details
db_config <- config::get(config = "default")

# Initialize database connection
con <- dbConnect(RPostgres::Postgres(),
                 user = db_config$user,
                 password = db_config$password,
                 host = db_config$host,
                 port = db_config$port,
                 dbname = db_config$dbname)

# Close database connection
#dbDisconnect(con)
