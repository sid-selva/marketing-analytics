# Marketing Data Exploration 
# UCI Bank Data 

# Load Libraries ----

library(tidyverse)

# Read Data 
conversions_tbl_base <- read.csv2("00_data/bank/bank-additional-full.csv",header = TRUE)

conversions_tbl <- read_csv2(file = "00_data/bank/bank-additional-full.csv")

conversions_tbl %>% glimpse()


# Mutate Conversion ----
conversions_tbl <- conversions_tbl %>% 
  mutate(conversion = ifelse(y == "yes",1,0)) %>% select(-y)


# Conversion Rate 

sprintf("The Total Conversions: %i out if %i",sum(conversions_tbl$conversion),nrow(conversions_tbl) )

sum_conversions <- sum(conversions_tbl$conversion)

sprintf("The Conversions percent is : %0.2f%%",sum(conversions_tbl$conversion)/nrow(conversions_tbl)*100)

# Conversion Rate by Age ----

conversions_by_age_tbl <- conversions_tbl %>% 
  group_by(age) %>% 
  summarise(count = n(),
            conversions = sum(conversion)) %>% 
  mutate(conversion_pct = (conversions/count)*100) %>% 
  ungroup()

conversions_by_age_tbl %>% 
  ggplot(aes(age, conversion_pct)) + 
  geom_line() + 
  labs(
    title = "Conversion Rate by Age",
    x = "Age",
    y = "Conversion Rate"
  ) + 
  theme(plot.title = element_text(hjust = 0.5))

## Conversion by Binning using Cut ---- 

min(conversions_tbl$age)
max(conversions_tbl$age)

cut(conversions_tbl$age,breaks = seq(20,70,by = 10))

conversions_Byage_group <- conversions_tbl %>% 
  group_by(age_group = cut(age,breaks = seq(10,70,by = 10))
           )%>% 
  summarise(count = n(),
            conversions = sum(conversion)) %>% 
  mutate(conversion_pct = (conversions/count)*100) %>% 
  ungroup()

conversions_Byage_group <-  conversions_Byage_group %>% 
  mutate(age_group = as.character(age_group))

conversions_Byage_group$age_group[7] <- "70+"

conversions_Byage_group %>% 
  ggplot(aes(age_group,conversion_pct)) + 
  geom_bar(width = 0.5,stat = "identity") +
  ggtitle('Conversion Rates by Age Groups') +
  xlab("Age") +
  ylab("Conversion Rate (%)") +
  theme(plot.title = element_text(hjust = 0.5)) 

# Other Factors ---- 

conversions_by_martial <- conversions_tbl %>% 
  group_by(marital,conversion) %>% 
  summarise(count = n()) %>% 
  ungroup()


conversions_by_martial %>% 
  ggplot(aes(x="",y =count, fill = marital)) + 
  geom_col(width = 1,position = position_fill()) + 
  coord_polar("y")  + 
  facet_wrap(~ conversion)+  
  ggtitle('Marital Status (0: Non Conversions, 1: Conversions)') +  
  theme(    
    axis.title.x=element_blank(),    
    axis.title.y=element_blank(),    
    plot.title=element_text(hjust=0.5),    
    legend.position='bottom'  
    )

# By Age and Marital Status ---- 
conversions_byage_marital <- conversions_tbl %>% 
  group_by(age_group = cut(age,breaks = seq(10,70,by = 10)),
           marital_status = marital)%>% 
  summarise(count = n(),
            conversions = sum(conversion)) %>% 
  mutate(total_count = sum(count)) %>% 
  mutate(conversion_pct = (conversions/total_count)*100) %>% 
  ungroup() %>% view()

conversions_byage_marital %>% 
  ggplot(aes(x = age_group,y = conversion_pct, fill = marital_status)) + 
  geom_col(width = 0.5,position = "dodge") +
  ylab("Conversion Rate (%)") +
  xlab("Age") +
  ggtitle("Conversion Rates by Age and Marital Status") +
  theme(plot.title=element_text(hjust=0.5))

conversions_byage_marital %>% 
  ggplot(aes(x = age_group,y = conversion_pct, fill = marital_status)) + 
  geom_bar(width = 0.5,stat = "identity",position = "dodge")


# Stacked bar Chart 

conversions_byage_marital %>% 
  ggplot(aes(x = age_group,y = conversion_pct, fill = marital_status)) + 
  geom_bar(width = 0.5,stat = "identity",position = "stack") +
  ylab("Conversion Rate (%)") +
  xlab("Age") +
  ggtitle("Conversion Rates by Age and Marital Status") +
  theme(plot.title=element_text(hjust=0.5))


# Education ----

conversions_edu <- conversions_tbl %>% 
  group_by(education,conversion) %>% 
  summarise(Count=n())
  

conversions_edu %>% 
  ggplot(aes(x ="", y = Count, fill = education))+ 
  geom_bar(width=1, stat = "identity", position=position_fill()) +
  geom_text(aes(x=1.25, label=Count), position=position_fill(vjust = 0.5)) +
  coord_polar("y") +
  facet_wrap(~conversion) +
  ggtitle('Education (0: Non Conversions, 1: Conversions)') +
  theme(
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    plot.title=element_text(hjust=0.5),
    legend.position='bottom'
  )

# Last Contact Duration ----

conversion_contact <- conversions_tbl %>% 
  mutate(duration = duration/(60*60))

conversion_contact %>% 
  ggplot(aes(x="", y = duration)) + 
  geom_boxplot() +
  facet_wrap(~conversion) +
  ylab("duration (in hours)") +
  xlab("0: Non-Conversion, 1: Conversion") +
  ggtitle("Conversion vs. Non-Conversions: Last Contact Duration") +
  theme(plot.title=element_text(hjust=0.5))
