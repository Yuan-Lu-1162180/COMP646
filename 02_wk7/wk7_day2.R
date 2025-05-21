library(dplyr)
x<-c(2,5,11,11,19,35)
x_lag<-lag(x)
x_lead<-lead(x)
lead_lag<-tibble(
  x, x_lag, x_lead
)
lead_lag

quantile(mtcars$mpg, 0.7)
median(mtcars$mpg)

library(nycflights13)
flights %>% 
  group_by(year, month, day) %>% 
  summarise(
    max=max(dep_delay, na.rm = TRUE),
    q95=quantile(dep_delay, 0.95, na.rm=TRUE),
    .groups="drop"
  )

x1<-c("Dec", "Apr","Jan","Mar")
typeof(x1)
class(x1)

f<-factor(c("low","medium","high"))
typeof(f)
class(f)


sort(x1)

month_levels<-c(
  "Jan","Feb","Mar","Apr","May","Jun",
  "Jul","Aug","Sep","Oct","Nov","Dec"
)

y1<-factor(x1, levels=month_levels )
y1

sort(y1)

x2<-c("Dec","Apr","Jam","Mar")
x2
y2<-factor(x2, levels=month_levels)

y2
library(forcats) 
y2<-fct(x2, levels=month_levels)
x1
factor(x1)
fct(x1)

csv<-"
month, value
Jan, 12
Feb, 56
Mar, 12

"

library(tidyverse)
df <- read_csv(csv, col_types = cols(month = readr::col_factor(levels = month_levels)))

df$month

gss_cat

gss_cat %>% 
  count(race)


relig_summary<-gss_cat %>% 
  group_by(relig) %>% 
  summarise(
    tvhours=mean(tvhours, na.rm=TRUE),
    n=n()
    
  )


relig_summary %>% 
  arrange(desc(tvhours))


library(ggplot2)
library(forcats)
ggplot(relig_summary, aes(x=tvhours, y=relig))+
  geom_point()


ggplot(relig_summary, aes(x = tvhours, y = fct_reorder(relig, tvhours)))+
  geom_point()


rincome_summary<-gss_cat %>%
  group_by(rincome) %>% 
  summarise(
    age=mean(age, na.rm=TRUE
             )
  )

ggplot(rincome_summary, aes(x=age, y=fct_reorder(rincome, age)))+
  geom_point()











