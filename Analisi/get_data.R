library(dutils)
library(lubridate)
library(reticulate)
library(ggplot2)
library(plotly)

reticulate::use_python('/usr/local/bin/python3', required = T)
boto3 <- reticulate::import('boto3')

dynamodb <- get_dynamodb_py(
  aws_access_key_id = 'AKIAYYQ2Q4FXGEDPQC6C',
  aws_secret_access_key = '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',
  region_name = 'eu-west-1'
)

dynamo_table <- get_dynamo_table_py(dynamodb, 'sensor_potencia')

tab <- query_timeseries_data_table_py(
  dynamo_table = dynamo_table,
  partition_key_name = 'id',
  partition_key_values =c('3cd6','ffffe4f0','ffffb450','ffffb848','343c','7064','ffffe833','8f5','182e'),
  sort_key_name = 'timestamp',
  start_date = dmy('09042022'),
  end_date = dmy('31052022'),
  query_interval_days = 2,
)

tab2 = tab %>% 
  mutate(purrr::map_dfr(data, ~ .x))

write_csv(tab2, 'taula2.csv')

