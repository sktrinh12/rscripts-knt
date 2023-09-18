library(ROracle)
library(dplyr)

library(rJava)
library(RJDBC)

##===============================READ SECURITY FILE=============================
sec_file <- file("/Users/spencer.trinhkinnate.com/Documents/security_files/oracle", open ="r")
ls_vars <- list()

lines = readLines(sec_file)
for (i in 1:length(lines)) {
  split.str = strsplit(x = lines[i], split = ",")[[1]]
  var_name = split.str[1]
  var_value = split.str[2]
  ls_vars[[var_name]] = var_value
}
close(sec_file)

ls_vars

connect.string <- paste(
  "(DESCRIPTION=",
  "(ADDRESS=(PROTOCOL=tcp)(HOST=", ls_vars$host, ")(PORT=", ls_vars$port, "))",
  "(CONNECT_DATA=(SID=", ls_vars$sid, ")))", sep = "")
connect.string

#===============================RJDBC===========================================

# Set JAVA_HOME, set max. memory, and load rJava library
Sys.setenv(JAVA_HOME='/Users/spencer.trinhkinnate.com//.sdkman/candidates/java/current/bin/java')
options(java.parameters="-Xmx2g")
library(rJava)

# Output Java version
.jinit()
print(.jcall("java/lang/System", "S", "getProperty", "java.version"))

# Load RJDBC library
library(RJDBC)

# Create connection driver and open connection
jdbcDriver <- JDBC(driverClass="oracle.jdbc.OracleDriver", 
                   classPath="/Users/spencer.trinhkinnate.com/lib/ojdbc8.jar")
connect.string <- paste0("jdbc:oracle:thin:@",
                         ls_vars$host,
                         ":", 
                         ls_vars$port, 
                         ":", 
                         ls_vars$sid
                         )
connect.string
jdbcConnection <- dbConnect(jdbcDriver, 
                            connect.string, 
                            ls_vars$username_userdata, 
                            ls_vars$password_userdata)
# Close connection
dbDisconnect(jdbcConnection)


#================================RORACLE=======================================

drv <- dbDriver("Oracle")

con_userdata <- dbConnect(drv, 
                 username = ls_vars$username_userdata,
                 password = ls_vars$password_userdata, 
                 dbname = connect.string)

con_pinpoint <- dbConnect(drv, 
                          username = ls_vars$username_pinpoint,
                          password = ls_vars$password_pinpoint, 
                          dbname = connect.string
)

#================================QUERY=======================================

# JDBC 
dbGetQuery(jdbcConnection,"SELECT COUNT(DISTINCT FORMATTED_ID) FROM C$PINPOINT.REG_DATA WHERE FORMATTED_ID LIKE 'FT%'")
df <- fetch_cmpd_data("FT007426", jdbcConnection)

HGNC <- data.table(read_excel("/Users/spencer.trinhkinnate.com/Documents/gitrepos//kinome-tree/data-input/HGNC-protein-coding-genes.xlsx"))
ht <- HGNC %>% select(SYMBOL, HGNC_ID, SYMBOL) 

dtmp <- df %>% 
  filter(grepl(KINASE, pattern = "\\("), !is.na(KINASE)) %>% 
  separate(KINASE, into = c("KINASE_SP1", "MUTANT_SP2", "XTRA_SP3"), remove = F, extra = 'drop') %>% 
  rename(Result = PCT_INHIBITION_AVG) %>%
  mutate(KINASE_NAME = case_when(
    nchar(KINASE_SP1) < 2 ~ stringr::str_to_upper(MUTANT_SP2),
    TRUE ~ KINASE_SP1
  ),
  KINASE_NAME = case_when(
    grepl(x = KINASE_NAME, pattern = "aurora", ignore.case = T) ~ paste0(KINASE_NAME, ".{1,}", MUTANT_SP2),
    grepl(x = KINASE_NAME, pattern = "p38a", ignore.case = T) ~ "MAPK14",
    grepl(x = KINASE_NAME, pattern = "CK1epsilon", ignore.case = T) ~ "CSNK1E",
    TRUE ~ KINASE_NAME)
  ) %>%
  merge(ht, by.x = 'KINASE_NAME', by.y = 'SYMBOL', all.x = TRUE, all.y = FALSE) %>%
  filter(Result >= 20) %>%
  arrange(KINASE)

dtmp 

dt <- dtmp %>% mutate(KINASE_CHECK = case_when(is.na(HGNC_ID) ~KINASE_NAME, TRUE ~ "NA")) %>% select(KINASE_CHECK)
dt

kinase_check <- lapply(dt$KINASE_CHECK, regex_alias)
dtmp <- dtmp %>% mutate(KINASE_CHECK = kinase_check)

t <- t(data.frame(dtmp$KINASE_CHECK))
names(t) <- c("test1", "test2")

re1 <- "1"
re2 <- "2"

dtmp$KINASE
  

cbind(dtmp,t(data.frame(dtmp$KINASE_CHECK)) ) %>%
rename(SYMBOL_CHECK = !!as.name(re1), HGNC_ID_CHECK = !!as.name(re2))

dtmp %>%
rename(SYMBOL_CHECK = !!as.name(rename_col1), HGNC_ID_CHECK = !!as.name(rename_col2)) %>% 
  

regex_alias <- function(the_string) {
  rtn_c <- c("NA", "NA") 
  if (the_string == "NA") {return(rtn_c)}
  pattern <- paste0("(?<![A-Za-z])", the_string, "(?![A-Za-z])") 
  colnames <- c("SYMBOL", "ALIAS_SYMBOL", "PREV_SYMBOL", "NAME", "PREV_NAME", "ALIAS_NAME") 
  nbr_rows <- -1
  cnt <- 1
  nbr_of_fields <- length(colnames)
  
  while (nbr_rows != 1) {
    hgnc_df <- HGNC %>%
      filter(grepl(x = !!as.name(colnames[cnt]), 
                   pattern = pattern,
                   perl = T,
                   ignore.case = T)
      )
    
    nbr_rows <- nrow(hgnc_df)
    if (cnt == nbr_of_fields) {break}
    cnt <- cnt + 1
  }
  
  
  if (nrow(hgnc_df) != 0) {
    rtn_c <- c(hgnc_df$SYMBOL, hgnc_df$HGNC_ID)
  }
  return(rtn_c)
}





df %>% 
  filter(grepl(KINASE, pattern = "\\("), !is.na(KINASE)) %>% 
  separate(KINASE, into = c("KINASE_SP1", "MUTANT_SP2", "XTRA_SP3"), remove = F, extra = 'drop') %>% 
  rename(Result = PCT_INHIBITION_AVG) %>%
  mutate(KINASE_NAME = case_when(
    nchar(KINASE_SP1) < 2 ~ stringr::str_to_upper(MUTANT_SP2),
    TRUE ~ KINASE_SP1
  ),
  KINASE_NAME = case_when(
    grepl(x = KINASE_NAME, pattern = "aurora", ignore.case = T) ~ paste0(KINASE_NAME, ".{1,}", MUTANT_SP2),
    grepl(x = KINASE_NAME, pattern = "p38a", ignore.case = T) ~ "MAPK14",
    grepl(x = KINASE_NAME, pattern = "CK1epsilon", ignore.case = T) ~ "CSNK1E",
    TRUE ~ KINASE_NAME)
  ) %>% merge(ht, by.x = 'KINASE_NAME', by.y = 'SYMBOL', all.x = TRUE, all.y = FALSE) %>%
  filter(Result >= 10)

#dbGetQuery(con,"SELECT DISTINCT RESULT_ALPHA FROM DS3_USERDATA.TM_CONCLUSIONS WHERE RESULT_ALPHA IS NOT NULL")

dbGetQuery(con_pinpoint,"SELECT COUNT(DISTINCT FORMATTED_ID) FROM C$PINPOINT.REG_DATA WHERE FORMATTED_ID LIKE 'FT%'")

fetch_cmpd_data <- function(compd_id, con ) {
  query_str <- paste0("SELECT * FROM (
    SELECT  REGEXP_SUBSTR (BATCH_ID, '[^-]+', 1, 1)    AS COMPD_ID,
    REGEXP_SUBSTR (BATCH_ID, '[^-]+', 1, 2)    AS BATCH_NUM,
    TECHNOLOGY,
    PCT_INHIBITION_AVG,
    EXPERIMENT_ID,
    BATCH_ID,
    KINASE,
    ORDER_NUMBER,
    CONC
    FROM    FT_KINASE_PANEL
  ) WHERE COMPD_ID = '", compd_id, "'")
  query_str <- strwrap(query_str, width= 10000, simplify = TRUE)
  print(query_str)
  d <- dbGetQuery(con, query_str)
  return(d)
}

fetch_exp_mdata <- function(exp_id) {
  query_str <- paste0("SELECT * FROM tm_prot_exp_fields_values 
         WHERE experiment_id = '", exp_id, "' 
         AND PROPERTY_NAME = 'CRO'")
  query_str <- strwrap(query_str, width = 10000, simplify = TRUE)
  d <- dbGetQuery(con_userdata, query_str)
return(d)
}

dt <- fetch_cmpd_data("FT007426", jdbcConnection)
exp_ids <- dt %>% select(EXPERIMENT_ID) %>% distinct(EXPERIMENT_ID) %>% pull() 
exp_ids
r <- sapply(exp_ids, function(x) fetch_exp_mdata(x))
length(r)
paste0(r[,1]$EXPERIMENT_ID, " (", r[,1]$PROPERTY_VALUE, ") [", r[,1]$CONC, ' nM]')


dtt <- fetch_exp_mdata("144027")
dtt

dt <- fetch_cmpd_data("FT002787")
exp_ids <- dt %>% select(EXPERIMENT_ID) %>% distinct(EXPERIMENT_ID) %>% pull() 
dtt <- fetch_exp_mdata(exp_ids[3])
dtt

dbDisconnect(con)