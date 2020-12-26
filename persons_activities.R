pacman::p_load(tidyverse, ggplot2, here, magrittr, lubridate, lavaan, ggx, hms)

source("little_helpers.R")


# preprocessing -----------------------------------------------------------


data <- read_csv(here("data", "ESMData.csv"))


# reformat date/time variables --------------------------------------------

data %<>% mutate(date = dmy(date))

data %<>% rename(evn_infl = evn_inflmood)

data %<>% 
  unite(datetime, c(date, resptime_e), sep = "-") %>% 
  mutate(datetime = parse_date_time(datetime, "ymdHMS")) %>% 
  select(X1, datetime) %>% 
  right_join(data, by = "X1")

data %<>% 
  mutate(dayno = ifelse(dayno > 200, dayno-225, dayno+141))

# recode stuff ------------------------------------------------------------

act_codes <- c(
  "0" = "nothing",
  "1" = "resting",
  "10" = "work/studies",
  "20" = "housekeeping/shopping",
  "21" = "caring for others",
  "26" = "medical care",
  "27" = "taking care of oneself",
  "41" = "sports",
  "43" = "active relaxation",
  "45" = "passive relaxation",
  "47" = "chatting/texting/facebook etc",
  "49" = "chilling",
  "51" = "talking",
  "60" = "eating/drinking",
  "88" = "traveling",
  "89" = "other"
)

soc_codes <- c(
  "0" = "nobody",
  "10" = "partner",
  "17" = "family resident",
  "19" = "roommates",
  "27" = "family non-resident",
  "29" = "family living at other places",
  "30" = "friends",
  "40" = "colleagues",
  "49" = "acquaintances",
  "50" = "strangers/others"
)


data %<>% mutate(act_what1 = recode(act_what1, !!!act_codes),
                 act_what2 = recode(act_what2, !!!act_codes),
                 soc_who1 = recode(soc_who1, !!!soc_codes),
                 soc_who2 = recode(soc_who2, !!!soc_codes),
                 soc_who3 = recode(soc_who3, !!!soc_codes))

# activities --------------------------------------------------------------

data %>% ggplot() + geom_bar(aes(x = phy_hungry))

data %>% ggplot() + geom_bar(aes(x = soc_who1))
data %>% ggplot() + geom_bar(aes(x = soc_who2))

data %>% ggplot() + geom_bar(aes(x = act_what1)) + 
  gg_("rotate x-axis labels by 90 degrees")
data %>% ggplot() + geom_bar(aes(x = soc_who1)) + 
  gg_("rotate x-axis labels by 90 degrees")

data %>% ggplot() + geom_point(aes(x = datetime, y = act_what1))
data %>% ggplot() + geom_point(aes(x = datetime, y = act_what2))
data %>% ggplot() + geom_point(aes(x = datetime, y = soc_who1))

data %>% 
  filter(
    soc_who1 %in% c("strangers/others", "partner", "nobody", "friends")) %>% 
  ggplot() + 
  geom_histogram(aes(x = mood_enthus, y = ..density..)) +
  facet_wrap(~soc_who1)

data %>% 
  filter(
    soc_who1 %in% c("strangers/others", "partner", "nobody", "friends")) %>% 
  ggplot() + 
  geom_violin(aes(x = soc_who1, y = mood_enthus))

data %>% filter(!is.na(soc_who1)) %>% 
  mutate(partner = (soc_who1 == "partner")) %>% 
  pivot_longer(contains("mood"), values_to = "value",
               names_to = "variable") %>% 
  ggplot() + 
  geom_violin(aes(x = partner, y = value)) +
  facet_wrap(~variable)


data %>% 
  mutate(partner = (soc_who1 == "friends")) %>% 
  ggplot() +
  geom_line(aes(x = datetime, y = mood_enthus, color = partner))

ggsave("plot2.svg")
