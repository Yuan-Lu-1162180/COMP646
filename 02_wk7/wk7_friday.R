library(tidyverse)
x1<-c("Dec","Apr","Jan","Mar")
typeof(x1)
class(x1)
sort(x1)
month_levels<-c(
  "Jan","Feb","Mar","Apr","May","Jun",
  "Jul","Aug","Sep","Oct","Nov","Dec"
)

y1<-factor(x1, levels=month_levels)
y1
sort(y1)


gss_cat



gss_cat %>% 
  count(race)


relig_summary<-gss_cat %>% 
  group_by(relig) %>% 
  summarise(
    tvhours=mean(tvhours, na.rm=TRUE)
  )

relig_summary %>% 
  arrange(desc(tvhours))

ggplot(relig_summary, aes(x=tvhours, y=relig))+
         geom_point()

ggplot(relig_summary, aes(x=tvhours, y=fct_reorder(relig, tvhours)))+
  geom_point()

rincome_summary<-gss_cat %>% 
  group_by(rincome) %>% 
  summarise(
    age=mean(age, na.rm=TRUE)
  )

ggplot(rincome_summary, aes(x=age, y=fct_reorder(rincome, age)))+
  geom_point()

ggplot(rincome_summary, aes(x=age, y=fct_relevel(rincome, "Not applicable")))+
  geom_point()

gss_cat %>% 
  mutate(marital =marital %>% fct_infreq()) %>% 
  ggplot(aes(x=marital))+
  geom_bar()
  

gss_cat %>% 
  count(partyid)

gss_cat |>
  mutate(
    partyid = fct_recode(partyid,
                         "Republican, strong"    = "Strong republican",
                         "Republican, weak"      = "Not str republican",
                         "Independent, near rep" = "Ind,near rep",
                         "Independent, near dem" = "Ind,near dem",
                         "Democrat, weak"        = "Not str democrat",
                         "Democrat, strong"      = "Strong democrat",
                         "Other"="No answer",
                         "Other"="Don't know",
                         "Other"="Other party"
    )
  ) |>
  count(partyid)


gss_cat |>
  mutate(
    partyid = fct_collapse(partyid,
                           "other" = c("No answer", "Don't know", "Other party"),
                           "rep" = c("Strong republican", "Not str republican"),
                           "ind" = c("Ind,near rep", "Independent", "Ind,near dem"),
                           "dem" = c("Not str democrat", "Strong democrat")
    )
  ) |>
  count(partyid)
gss_cat |> 
  count(relig, sort = TRUE)


gss_cat %>% 
  mutate(relig=fct_lump_n(relig, n=4)) %>% 
  count(relig, sort=TRUE)
install.packages("babynames")
library(babynames)



string1 <- "This is a string"


double_quote <- "\""  # double quote
double_quote
single_quote <- '\''   # single quote
single_quote
backslash <- "\\"       # backslash
backslash


x <- c(single_quote, double_quote, backslash) # these objects are already there
x
str_view(x)  # renders the actual characters in output

df <- tibble(name = c("Flora", "David", "Terra", NA))
df

# Use str_c() to combine text and data
mutated_df <- df |> 
  mutate(greeting = str_c("Hi ", name, "!")) # refer to the name column in the dataframe df
mutated_df

ben_example<-tibble(Season=c("2022-23","2023-24"))
ben_example %>% 
  mutate(year=str_sub(Season, 1, 4))
















