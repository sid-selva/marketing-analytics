# IBM Watson Marketting  Data 
# GLM Regression 

# Libaries ----
library(tidyverse)
library(janitor)

# Read Data ----

df <- read_csv(file = "00_data/ibm_watson_data/watson_marketing-Customer-Value-Analysis.csv",col_names = TRUE) %>% 
  janitor::clean_names()

# Checking Parsing Errors ---- 
problems(df)
spec(df)

df %>% glimpse()

df <- df %>% 
  mutate(y = ifelse(response == "Yes",1,0)) %>% 
  dplyr::relocate(y, .after = "response")


?table

# get summary with Table and Split and totals 
state_summary_tbl <- table(df$state,df$response,dnn = c("state","response")) %>% as_tibble() %>% 
  mutate(response = as_factor(response)) %>% 
  split(.[,"response"],drop = TRUE)%>%
  purrr::map_df(., janitor::adorn_totals)

sapply(df,class)
