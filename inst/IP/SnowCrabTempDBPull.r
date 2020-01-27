#get temp database from snowcrab
require(RODBC)
oracle.snowcrab.server='ptran'

con8 = odbcConnect(oracle.snowcrab.server,oracle.snowcrab.user,oracle.snowcrab.password)

res <- sqlQuery(con8, "ALTER SESSION SET NLS_DATE_FORMAT = 'YYYY-MM-DD'")
res <- sqlQuery(con8, "ALTER SESSION SET NLS_TIMESTAMP_FORMAT = 'YYYY-MM-DD HH24:MI:SSXFF'")
res <- sqlQuery(con8, "ALTER SESSION SET  NLS_TIMESTAMP_TZ_FORMAT = 'YYYY-MM-DD HH24:MI:SSXFF TZR'")
res <- sqlQuery(con8, "select * from SC_TEMP_MERGE")

saveRDS(res,file.path(project.datadirectory('bio.lobster'),'Temperature Data','CompiledSnowCrabTJan2020.rsd'))