library(DBI)
library(dbplyr)
suppressPackageStartupMessages(library(dplyr))

options(tibble.width = Inf)

cn <- dbConnect(odbc::odbc(), dsn = "access-odbc")

#dbListTables(cn)

#### Make a tbl connection --------------------------------------------


pe2000 <- tbl(cn, "PE2000")

#### Testing tbl connection -------------------------------------------

# No TOP needed
show_query(pe2000)

collect_pe2000 <- collect(pe2000)
collect_pe2000

# TOP used
show_query(head(pe2000, 10))

pe2000

head(pe2000, 10)

#### Testing DISTINCT -----------------------------------------------

pe2000_dis <- pe2000 %>% distinct()
show_query(pe2000_dis)
pe2000_dis

#### (FIX) Testing SELECT --------------------------------------------

pe_trial <- pe2000 %>% select(Trial)
show_query(pe_trial)

pe2000 %>% select(Trial, `"ESGAssetsEquityAssetsPrivateEquityTotalReturn"`)
show_query(pe_trial)

# Circular reference caused by alias 'Trial' in query definition's SELECT list.
# Cant do `Trial` as `Trial` in Access
pe_trial

# Fixed with:
# names_to_as <- function(x, names = names2(x), con = NULL) {
#   as <- ifelse(names == "" | sql_escape_ident(con, names) == x, "", paste0(" AS ", sql_escape_ident(con, names)))
#
#   paste0(x, as)
# }

#### Testing WHERE ----------------------------------------------------

pe_where <- pe2000 %>%
  filter(Trial == 1)

show_query(pe_where)

pe_where

#### Testing GROUP BY ----------------------------------------------------

# It correctly doesn't show up until a real grouped computation is done
pe_gb <- pe2000 %>% group_by(Trial)
show_query(pe_gb)

pe_gb

# Now it shows up
pe_gb_sum <- pe_gb %>% summarise(sum = sum(Trial))
show_query(pe_gb_sum)

pe_gb_sum

#### (Fix?) Testing ORDER BY ----------------------------------------------------

# Why does order by display 2000 rows??

pe_order <- pe2000 %>%
  arrange(Timestep)
show_query(pe_order)

# This displays 2000 rows?
# pe_order

head(pe_order)

#### (NEED TEST) Testing HAVING ------------------------------------------

# ???

#### (ANTI, FULL) Testing Joins --------------------------------------------------------

bar <- tbl(cn, "BAR Wide")

# Left
pe_bar_lj <- left_join(pe2000, bar, by = c("Trial", "Timestep"))
show_query(pe_bar_lj)
pe_bar_lj

# Right
pe_bar_rj <- right_join(pe2000, bar, by = c("Trial", "Timestep"))
show_query(pe_bar_rj)
pe_bar_rj

# Inner
pe_bar_ij <- inner_join(pe2000, bar, by = c("Trial", "Timestep"))
show_query(pe_bar_ij)
pe_bar_ij

# Semi
pe_bar_sj <- semi_join(pe2000, bar, by = c("Trial", "Timestep"))
show_query(pe_bar_sj)
pe_bar_sj

# (THIS ONE RAN FOREVER?) Anti
# pe_bar_aj <- anti_join(pe2000, bar, by = c("Trial", "Timestep"))
# show_query(pe_bar_aj)
# pe_bar_aj

# Full
# Coalesce not supported. NZ() is equivalent with 2 params?
pe_bar_fj <- full_join(pe2000, bar, by = c("Trial", "Timestep"))
show_query(pe_bar_fj)

pe_bar_fj

#### (Logical = 0/-1) Conversion ------------------------------------------------------

pe_conv <- pe2000 %>%
  mutate(Timestep_num = as.numeric(Timestep),
         Timestep_dbl = as.double(Timestep),
         Timestep_int = as.integer(Timestep),
         Timestep_log = as.logical(Timestep),
         Timestep_chr = as.character(Timestep),
         Timestep_date = as.Date("2017-01-01")
         )

show_query(pe_conv)

pe_conv

#### Math ------------------------------------------------------------

pe_math <- pe2000 %>%
  mutate(ts_exp     = exp(Timestep),
         ts_sqrt    = sqrt(Timestep),
         ts_atan    = atan(Timestep),
         ts_power   = Timestep ^ 3,
         ts_floor   = floor(Timestep+.01),
         ts_ceiling = ceiling(Timestep+.01),
         ts_ceiling_whole_num = ceiling(Timestep)
         )

show_query(pe_math)

pe_math

#### Strings ---------------------------------------------------------

pe_strings <- pe2000 %>%
  mutate(
    test_str        = " hello world ",
    test_nchar      = nchar(test_str),
    test_substr     = substr(test_str, 2, 5), # remember empty space at beginning
    test_trimws     = trimws(test_str),
    test_nchar_trim = trimws(test_str) %>% nchar()
    )

show_query(pe_strings)

pe_strings

#### Logic ---------------------------------------------------------

pe_logic <- pe2000 %>%
  mutate(
    test_null   = is.null(Trial),
    test_na     = is.na(Trial),
    test_ifelse = ifelse(Trial == 1, "its a 1", "its not a 1")
  )

show_query(pe_logic)

pe_logic

#### Dates ---------------------------------------------------------

pe_dates <- pe2000 %>%
  mutate(
    test_date = Sys.Date()
  )

show_query(pe_dates)

pe_dates

#### Agg functions -------------------------------------------------

pe_agg <- pe2000 %>%
  group_by(Timestep) %>%
  summarise(
    test_mean = mean(Trial),
    test_sd   = sd(Trial),
    test_var  = var(Trial)
  )

show_query(pe_agg)

pe_agg


#### (FIX) What happens when convering with a NA in the col? ---------------

# Notice the NA in Trial
head(pe2000)

# Invalid use of Null
pe2000 %>%
  mutate(trial_num = as.double(Trial))


#### (FIX) Can't copy to Access / Write tables --------------------------------------------

# I don't think temporary tables work, even though they say they do online
# https://msdn.microsoft.com/en-us/library/office/bb177893(v=office.12).aspx
# https://stackoverflow.com/questions/29698198/ms-access-database-2010-how-to-create-temporary-table-procedure-view-from-quer

# Syntax error in CREATE TABLE statement.
#<SQL> CREATE TEMPORARY TABLE `mtcars` (
# `row_names` VARCHAR(255),
# `mpg` DOUBLE,
# `cyl` DOUBLE,
# `disp` DOUBLE,
# `hp` DOUBLE,
# `drat` DOUBLE,
# `wt` DOUBLE,
# `qsec` DOUBLE,
# `vs` DOUBLE,
# `am` DOUBLE,
# `gear` DOUBLE,
# `carb` DOUBLE
# )
copy_to(cn, mtcars)

# There is actually a deeper problem, when you turn temporary off,
# it actually can't insert_results_dataframe
# This has been posted on odbc's github issues
# It does create the table though!!
# Invalid precision value
copy_to(cn, mtcars, temporary = FALSE)

# The table is there, just empty
"mtcars" %in% dbListTables(cn)

# You can't drop them with overwrite = TRUE though, the DROP TABLE syntax is wrong
# Syntax error in DROP TABLE or DROP INDEX.
# <SQL> DROP TABLE IF EXISTS mtcars
# Access doesn't support IF EXISTS
# https://stackoverflow.com/questions/5847770/how-to-drop-table-in-access-if-exists
copy_to(cn, mtcars, temporary = FALSE, overwrite = TRUE)

# But you can drop them with db_drop_table, as it doesnt check if it exists
db_drop_table(cn, "mtcars")
