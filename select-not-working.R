library(DBI)
library(dbplyr)
suppressPackageStartupMessages(library(dplyr))

cn <- dbConnect(odbc::odbc(), dsn = "access-odbc")

pe2000 <- tbl(cn,"PE2000")

pe2000

select(pe2000, Timestep, Trial)

# inside collect
debugonce(as.data.frame)

# collect.tbl_sql -> db_sql_render -> db_sql_render.DBIConnection -> sql_build -> sql_build.tbl_lazy -> sql_build.op_head ->

names_to_as <- function(x, names = names2(x), con = NULL) {
  as <- ifelse(names == "" | sql_escape_ident(con, names) == x, "", paste0(" AS ", sql_escape_ident(con, names)))

  paste0(x, as)
}

names_to_as <- function(x, names = names2(x), con = NULL) {
  as <- ifelse(names == "", "", paste0(" AS ", sql_escape_ident(con, names)))

  paste0(x, as)
}




#### Follow up for Edgar

# These don't work
dbGetQuery(cn, "SELECT `Trial` AS `Trial` FROM `PE2000`")

dbGetQuery(cn, 'SELECT "Trial" AS "Trial" FROM `PE2000`')

tr <- dbGetQuery(cn, "SELECT 'Trial' AS 'Trial' FROM `PE2000`")
tibble::as.tibble(tr)

# This works
trial <- dbGetQuery(cn, "SELECT `Trial` AS `Trial2` FROM `PE2000`")
tibble::as.tibble(trial)

# This works
trial <- dbGetQuery(cn, "SELECT `Trial` FROM `PE2000`")
tibble::as.tibble(trial)
