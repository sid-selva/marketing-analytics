# IBM Watson Marketting  Data 
# GLM Regression 

# Libaries ----
library(tidyverse)
library(janitor)
library(tidyquant)

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


engagementRate <- df %>% 
  group_by(y) %>% 
  summarise(Count=n()) %>%
  mutate(Percentage=Count/nrow(df)*100.0)

engagementRate

# Transpose
transposed <- t(engagementRate)

colnames(transposed) <- engagementRate$y
transposed <- transposed[-1,]
transposed

# Sales Channels ---- 

sales_channel <- df %>% 
  group_by(y,channel = sales_channel) %>% 
  summarise(count =n())

# Claim Amount ---- 

ggplot(df, aes(x="", y=total_claim_amount)) + 
  geom_boxplot() +
  facet_wrap(~y) +
  ylab("Total Claim Amount") +
  xlab("0: Not Engaged, 1: Engaged") +
  ggtitle("Engaed vs. Not Engaged: Total Claim Amount") +
  theme(plot.title=element_text(hjust=0.5))

# without outliers
ggplot(df, aes(x="", y = total_claim_amount)) + 
  geom_boxplot(outlier.shape = NA) +
  scale_y_continuous(limits = quantile(df$total_claim_amount, c(0.1, 0.9))) +
  facet_wrap(~ y) +
  ylab("Total Claim Amount") +
  xlab("0: Not Engaged, 1: Engaged") +
  ggtitle("Engaed vs. Not Engaged: Total Claim Amount") +
  theme(plot.title=element_text(hjust=0.5))


# Income ----

# boxplot
ggplot(df, aes(x="", y = income)) + 
  geom_boxplot() +
  facet_wrap(~ y) +
  ylab("Income") +
  xlab("0: Not Engaged, 1: Engaged") +
  ggtitle("Engaged vs. Not Engaged: Income") +
  theme(plot.title=element_text(hjust=0.5))

# summary statistics
incomeDescription <- df %>% 
  group_by(y) %>% 
  summarise(
    Min=min(income), Q1=quantile(income, 0.25), 
    Median=median(income), Q3=quantile(income, 0.75),
    Max=max(income)
  )

incomeDescription


# Logit Regression ---- 

sapply(df, class)

continuous_tbl <- df %>% 
  select_if(is.numeric)

model_fit_logit_continuous <- glm(y ~ .-customer_lifetime_value, data = continuous_tbl,family = binomial)
summary(model_fit_logit_continuous)

# Categorical Variables ----

model_fit_logit_education <- glm(y ~ factor(education), data = df, family = binomial)
summary(model_fit_logit_education)

model_fit_logit_edu2 <- glm(y ~ factor(education) + 
                                factor(gender),
                            data = df,
                            family = binomial
                              )

summary(model_fit_logit_edu2)

# Correlation Plot ----

cor_y <- continuous_tbl %>% 
  cor(use = "pairwise.complete.obs") %>% 
  as_tibble() %>% 
  mutate(feature = names(.)) %>% 
  select(feature, y) %>% 
  filter(!(feature == "y")) %>% 
  mutate_if(is.character,as_factor)

cor_plot <- cor_y %>% 
  mutate(feature_name = round(y,2)) %>% 
  mutate(cor = case_when(
    y >= 0 ~ "positive",
    TRUE ~ "negative") %>% as_factor()) %>% 
  rename(cor_response = y)

cor_plot %>%
  mutate(feature = feature %>% fct_reorder(cor_response)) %>% 
  ggplot(aes(x = cor_response, y = feature, group = feature)) +
  geom_point(aes(color = cor), size = 4) +
  geom_segment(aes(xend = 0, yend = feature, color = cor), size = 2) +
  geom_vline(xintercept = 0, color = palette_light()[[1]], size = 1) +
  theme_tq() +
  scale_color_manual(values = c(palette_light()[[2]], palette_light()[[1]])) 
