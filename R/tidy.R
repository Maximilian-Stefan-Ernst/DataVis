pacman::p_load(tidyverse, ggplot2, here, magrittr, lubridate, lavaan, ggx, hms)

source(here("little_helpers.R"))


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


# missings ----------------------------------------------------------------

data_exp <- complete(data, dayno, beepno)

data_exp %<>% mutate(duration = as.duration(resptime_e - resptime_s))


# scale data --------------------------------------------------------------

data_exp %<>% mutate(across(c(contains("mood"), contains("evn")), scale))

# daily data --------------------------------------------------------------

data_daily <- data_exp %>%
  group_by(dayno) %>%
  summarise(
    across(
      c(starts_with("evn"), starts_with("mor")),
      unique_naomit
    ),
    date = unique(na.omit(date)),
    across(starts_with("mood"), 
           list(mean = mean, sd = sd, max = max, min = min),  
           na.rm = TRUE)
  )

data_daily %>% filter(dayno < 100) %>% 
  ggplot() +
  geom_path(aes(x = date, y = scale(evn_niceday))) +
  geom_path(aes(x = date, y = scale(mood_enthus_mean)), color = "red")

sumstat_cors <- data_daily %>% 
  select(starts_with("mood")) %>% 
  map(~cor(., data_daily$evn_niceday, use = "pairwise.complete.obs")) %>% 
  tibble(names = names(.), values = unlist(.)) %>% 
  select(names, values)

sumstat_cors %<>% mutate(statistic = map_chr(sumstat_cors$names, statfromname))

# sumstat_cors %>% 
#   ggplot() + 
#   geom_histogram(aes(x = values)) +
#   facet_wrap(~statistic)

sumstat_cors %>% arrange(desc(values))

data_exp$evn_niceday <- 
  map2_dbl(
    data_exp$dayno, 
    data_exp$evn_niceday,
    get_value_evn_nd,
    keys = data_daily)

# make datetime variable

data_daily <- data.frame(dayno = rep(c(1:124, 126:239), each = 2)) %>% 
  left_join(data_daily, by = "dayno")

data_daily %<>% mutate(time = rep(c("00:00:00", "23:59:59"), 238)) %>% 
  unite(datetime, c(date, time), sep = "-") %>% 
  mutate(datetime = parse_date_time(datetime, "ymdHMS"))


### time + day plot plot 

ggplot() +
  geom_line(
    data = filter(data_daily, dayno < 8), 
    aes(x = datetime, y = scale(evn_niceday)),
    size = 1) +
  geom_point(
    data = filter(data_exp, dayno < 8), 
    aes(x = datetime, y = scale(mood_enthus)),
    color = "red",
    size = 2
  ) +
  geom_line(
    data = filter(data_exp, dayno < 8), 
    aes(x = datetime, y = scale(mood_enthus), group = date),
    color = "red",
    size = 1
  )


# weekly data -------------------------------------------------------------

data_exp %<>% mutate(weekday = wday(datetime, label = T),
                     week = isoweek(datetime),
                     studyweek = (dayno - 1) %/% 7)

data_daily %<>% mutate(weekday = wday(datetime, label = T),
                       week = week(datetime),
                       year = year(datetime),
                       wy = str_c(week, year),
                       studyweek = (dayno - 1) %/% 7)

first_monday <- as_date("2012-08-13")

data_weekly <- data_exp %>% 
  mutate(evn_niceday = scale(evn_niceday)) %>% 
  group_by(studyweek) %>% 
  summarise(across(contains("SCL"), unique_naomit),
            n_obs = length(na.omit(evn_niceday)),
            across(c(evn_niceday), 
                   list(mean = mean, sd = sd, max = max, min = min), 
                   na.rm = T))

data_weekly %<>% rename(evn_niceday = evn_niceday_mean)

data_weekly %<>% mutate(
  evn_niceday_max = ifelse(is.infinite(evn_niceday_max), NA, evn_niceday_max),
  evn_niceday_min = ifelse(is.infinite(evn_niceday_min), NA, evn_niceday_min))

data_weekly$SCL_mean = rowMeans(select(data_weekly, 
                                       contains("SCL")), na.rm = T)

# data_weekly <- data_exp %>% filter(!is.na(SCL.90.R.14)) %>% 
#   group_by(date) %>% 
#   summarise(studyweek = unique(studyweek)) %>% 
#   right_join(data_weekly, by = "studyweek")

data_weekly %<>% mutate(date = first_monday + weeks(studyweek),
                        datetime = as_datetime(str_c(date, "-00:00:00")))


### mean day/day/week plot

data_weekly_safe <- data_weekly

data_weekly <- data.frame(studyweek = rep(c(0:34), each = 2)) %>% 
  left_join(data_weekly, by = "studyweek")

data_weekly %<>% 
  mutate(
    datetime_help = 
      date + rep(c(days(0), days(6)), 35),
    datetime_help = 
      as_datetime(
        str_c(
          datetime_help, 
          rep(c("-00:00:00", "-23:59:59"), 35)
        )
      )
  )

data_daily_sum <- data_exp %>% 
  group_by(dayno) %>% 
  summarise(
    date = unique(na.omit(date)),
    evn_niceday = unique_naomit(evn_niceday),
    n_obs = length(na.omit(mood_enthus)),
    across(contains("mood"), 
           list(mean = mean, sd = sd, max = max, min = min),
           na.rm = T),
    studyweek = unique(studyweek)
  )

# data_daily_mean %<>% 
#   mutate(datetime = as_datetime(str_c(date, "-12:00:00")))
