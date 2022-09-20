library(tidyverse)
library(lubridate)

tbl <- read_csv(file = "data2.csv")


tbl2 <- readxl::read_excel(path = "data2.xlsx")

tbl2 %>%  group_by(date) %>% 
  summarise(total_clicks = sum(ads_clicked),
            total_time_spent = sum(total_user_time_spent_in_mins)) %>%
  mutate(ad_clicked_per_day = total_clicks/total_time_spent) %>% 
  arrange(desc(ad_clicked_per_day))

# time arnages 

tbl2 %>% 
  mutate(time_spent_a = total_user_time_spent_in_mins - user_time_spent_versionB_in_mins) %>% 
  group_by(time_slot) %>% 
  summarise(timeA = sum(time_spent_a),
            timeB = sum(user_time_spent_versionB_in_mins)) %>% 
  mutate(check = ifelse(timeA<timeB,1,0))

tbl2 %>% 
  mutate(day = wday(date)) %>% 
  mutate(add_a = ads_clicked - ads_clicked_versionB) %>% 
  select(add_a,ads_clicked_versionB,day,ads_clicked) %>% 
  group_by(day) %>% 
  summarise(sum_a = sum(add_a),
            sum_b = sum(ads_clicked_versionB),
            sum_t = sum(ads_clicked))

tbl2 %>% group_by(time_slot) %>% 
  summarise(total_ads = sum(total_ads_watched_in_mins)) %>% 
  mutate(pct = total_ads/sum(total_ads))


tbl3 <- read_csv(file = "data/ads_statistics.csv")

tbl3 %>% distinct(platform_id)   

tbl4 <- read_csv(file = "data/platforms.csv")

tbl4 %>% left_join(tbl3, by= c("platform_id")) %>% 
  distinct(platform_id)


tbl4 %>% 
 group_by(contact_mail) %>% 
  summarise(list = list(website)) %>% arrange(list) %>% view()

tbl4 %>% 
  group_by(website) %>% 
  summarise(list = list(contact_mail)) %>% arrange(list) %>% view()

tbl3 %>% 
  group_by(platform_id) %>% 
  summarise(list = list(total_time_watched)) %>% 
  arrange((list)) %>% view()

  
