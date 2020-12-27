pacman::p_load(tidyverse, ggplot2, 
               here, magrittr, lubridate, lavaan, ggx, hms,
               viridis, ggthemes)

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
            evn_niceday = mean(evn_niceday, na.rm = T)) 

data_weekly$SCL_mean = rowMeans(select(data_weekly, 
                                       contains("SCL")), na.rm = T)

data_weekly %>% 
  pivot_longer(contains("SCL"), 
               values_to = "value", 
               names_to = "variable") %>% 
  ggplot() + 
  geom_line(aes(x = studyweek, y = value)) +
  #geom_smooth(aes(x = week, y = value)) +
  facet_wrap(~variable)

# data_weekly <- data_exp %>% filter(!is.na(SCL.90.R.14)) %>% 
#   group_by(date) %>% 
#   summarise(studyweek = unique(studyweek)) %>% 
#   right_join(data_weekly, by = "studyweek")

data_weekly %<>% mutate(date = first_monday + weeks(studyweek),
                        datetime = as_datetime(str_c(date, "-00:00:00")))

data_weekly %>% ggplot() +
  geom_line(aes(x = date, y = SCL_mean), size = 2)


### mean day/day/week plot

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

# ggplot(data_daily) +
#   geom_line(aes(x = date, y = (-1)*scale(evn_niceday))) +
#   geom_line(aes(x = date, y = (-1)*scale(mood_enthus_mean)), color = "red") +
#   geom_line(
#     data = data_weekly,
#     aes(x = date, y = scale(SCL_mean)), color = "blue")

source("R/tidy.R")

x_labels <- 
  data.frame(
    x = as_datetime(str_c(data_daily$datetime[seq(1, 14, 2)], "12:00:00")),
    y = rep(-3.0, 7),
    label = c("Mo", "Tue", "We", "Thu", "Fr", "Sa", "So"))



lower_week = -1
upper_week = 1


ggplot() +
  # geom_line(
  #   data = filter(data_weekly,
  #                 studyweek < upper_week, studyweek > lower_week),
  #   aes(x = (datetime_help-weeks(studyweek)), y = evn_niceday),
  #   alpha = .5
  # ) +
  geom_line(
    data = filter(
      mutate(data_weekly, SCL_mean = (-1)*scale(SCL_mean),
             studyweek = studyweek - 1),
      studyweek < upper_week, studyweek > lower_week),
    aes(x = (datetime_help-weeks(studyweek + 1)), y = SCL_mean),
    color = "#56B4E9", alpha = .5, size = 1
  ) +
  geom_line(
    data = filter(data_daily, studyweek < upper_week, studyweek > lower_week), 
    aes(x = (datetime-weeks(studyweek)), y = evn_niceday, group = dayno),
    size = 1.2, color = "#E69F00") + 
  # geom_line(
  #   data = filter(data_daily_mean,
  #                 studyweek < upper_week, studyweek > lower_week), 
  #   aes(x = (datetime-weeks(studyweek)), y = mood_enthus_mean),
  #   size = 1, color = "orange") + 
  geom_point(
    data = filter(
      mutate(data_exp, mood_enthus = scale(mood_enthus)),
      studyweek < upper_week, studyweek > lower_week),
    aes(x = (datetime-weeks(studyweek)), y = mood_enthus),
    color = "#000000",
    size = 1,
    shape = 1
  ) +
  geom_line(
    data = filter(
      mutate(data_exp, mood_enthus = scale(mood_enthus)),
      studyweek < upper_week, studyweek > lower_week), 
    aes(x = (datetime-weeks(studyweek)), y = mood_enthus, group = date),
    color = "#000000",
    size = 0.5
  ) + 
  facet_grid(rows = vars(studyweek)) +
  theme_minimal() +
  scale_y_continuous(
    name = "Standardized Value",
    breaks = c(-2, -1, 0, 1, 2),
    minor_breaks = NULL,
    labels = NULL) +
  scale_x_datetime(name = "First Studyweek", 
                   breaks = data_daily$datetime[1:14],
                   minor_breaks = NULL,
                   labels = NULL) +
  coord_cartesian(ylim = c(-2.5, 2.5), clip="off") +
  geom_text(data = x_labels, aes(x = x, y = y, label = label)) +
  theme(axis.title.x = element_text(margin = margin(20, 0, 0, 0)),
        panel.grid.major.y = element_blank(),
        legend.position = "none",
        axis.ticks.y = element_line())

  # theme(legend.position = "none",
  #       plot.title = element_text(margin=margin(10,0,12,0)),
  #       plot.title.position = "plot",
  #       axis.title.y = element_text(margin = c(50, 0, 0, 0))) +
  # labs(title = "Live expectancy over time in 142 countries") +

###


# weekly plot -------------------------------------------------------------


cor(data_weekly$SCL_mean[2:35], data_weekly$evn_niceday[1:34], use = "pairwise.complete.obs")
cor(data_weekly$SCL_mean[2:35], data_weekly$evn_niceday_min[1:34], use = "pairwise.complete.obs")
cor(data_weekly$SCL_mean[2:35], data_weekly$evn_niceday_max[1:34], use = "pairwise.complete.obs")
cor(data_weekly$SCL_mean[2:35], data_weekly$evn_niceday_sd[1:34], use = "pairwise.complete.obs")

latest_start <- min(data$date) + weeks(10)

latest_null <- min(data$date) + weeks(18)

data_weekly_safe %<>%
  mutate(
    SCL_mean_ribbon = c(scale(SCL_mean)[2:35], NA),
    ribbon_max_1 = pmax((-1) * SCL_mean_ribbon, evn_niceday),
    ribbon_min_1 = pmin((-1) * SCL_mean_ribbon, evn_niceday),
    ribbon_max_2 = pmax((-1) * SCL_mean_ribbon, evn_niceday),
    ribbon_min_2 = pmin((-1) * SCL_mean_ribbon, evn_niceday),
    ribbon_max_1 = ifelse(evn_niceday > SCL_mean_ribbon, NA, ribbon_max_1),
    ribbon_min_1 = ifelse(evn_niceday > SCL_mean_ribbon, NA, ribbon_min_1),
    ribbon_max_2 = ifelse(evn_niceday < SCL_mean_ribbon, NA, ribbon_max_2),
    ribbon_min_2 = ifelse(evn_niceday < SCL_mean_ribbon, NA, ribbon_min_2),
    ribbon_color = ifelse(evn_niceday > SCL_mean_ribbon, 1, 0)
  )

data_weekly_safe %>%
  # ggplot() +
  # geom_ribbon(
  #   aes(x = date, ymin = evn_niceday, ymax = (-1) *SCL_mean_ribbon),
  #   alpha = .2
  # ) +
  # geom_ribbon(
  #   aes(x = date, ymin = ribbon_min_1,
  #       ymax = ribbon_max_1),
  #   fill = "black",
  #   alpha = .5
  # ) +
  # geom_ribbon(
  #   aes(x = date, ymin = ribbon_min_2,
  #       ymax = ribbon_max_2),
  #   fill = "red",
  #   alpha = .5
  # ) +
  geom_line(aes(x = date, y = evn_niceday), 
            color = "#E69F00", size = 1) +
  geom_line(aes(x = date - weeks(1),
                y = (-1) * scale(SCL_mean)),
            color = "#56B4E9", size = 1) +
  theme_tufte()  +
  scale_y_continuous(
    name = "Standardized Value",
    breaks = c(-2, -1, 0, 1, 2),
    minor_breaks = NULL,
    labels = NULL) +
  scale_x_date(name = NULL, 
               breaks = as_date(
                 c("2012-08-01", "2012-12-01", "2013-04-01")),
               minor_breaks = NULL, 
               date_labels ="%b"
               ) +
  coord_cartesian(ylim = c(-2.5, 2.5), clip="off") +
  # geom_smooth(aes(x = date, y = evn_niceday), 
  #           color = "#E69F00", size = 1, se = F) +
  # geom_smooth(aes(x = date - weeks(1),
  #               y = (-1) * scale(SCL_mean)),
  #           color = "#56B4E9", size = 1, se = F) +
  geom_vline(xintercept = latest_start) +
  geom_vline(xintercept = latest_null)
  

scales::show_col(colorblind_pal()(8))

# daily plot --------------------------------------------------------------

data_daily_sum %>%
  ggplot() +
  # geom_ribbon(
  #   aes(x = date, ymin = evn_niceday, ymax = mood_enthus_mean),
  #   alpha = .2
  # ) +
  geom_line(aes(x = date, y = evn_niceday), 
            color = "#E69F00", alpha = 1) +
  geom_line(aes(x = date, y = mood_enthus_mean), 
            color = "#000000", alpha = .6) +
  theme_tufte() +
  scale_y_continuous(
    name = "Standardized Value",
    breaks = c(-2,-1, 0, 1, 2),
    minor_breaks = NULL,
    labels = NULL
  ) +
  scale_x_date(
    name = NULL,
    breaks = as_date(c("2012-08-01", "2012-12-01", "2013-04-01")),
    minor_breaks = NULL,
    date_labels = "%b",
    limits = as_date(c("2012-07-30", "2013-04-09"))
  ) +
  coord_cartesian(ylim = c(-2.5, 2.5), clip = "off")


ggsave("plot7.svg")#, 
# width = 20,
# height = 50,
# units = "cm")