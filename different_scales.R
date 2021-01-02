pacman::p_load(tidyverse, ggplot2, 
               here, magrittr, lubridate, lavaan, ggx, hms,
               viridis, ggthemes)

source("little_helpers.R")

source("R/tidy.R")

x_labels <- 
  data.frame(
    x = as_datetime(str_c(data_daily$datetime[seq(1, 14, 2)], "12:00:00")),
    y = rep(-3.0, 7),
    label = c("Mo", "Tue", "We", "Thu", "Fr", "Sa", "So"))



lower_week = -1
upper_week = 1

line_labels <- 
  data.frame(
    labels = c(
      "Momentary enthusiuasm",
      "Was this a nice day?",
      "Weekly measured \n depression"
    ),
    x = as_datetime(c(
      "2012-08-20 04:00:00 UTC",
      "2012-08-20 04:00:00 UTC",
      "2012-08-20 04:00:00 UTC"
    )),
    y = c(0.3, -1, 0.8),
    color = c("1","2","3")) 

ggplot() +
  # geom_line(
  #   data = filter(data_weekly,
  #                 studyweek < upper_week, studyweek > lower_week),
  #   aes(x = (datetime_help-weeks(studyweek)), y = evn_niceday),
  #   alpha = .5, color = "#E69F00"
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
        axis.ticks.y = element_line()) +
  geom_text(
    data = line_labels, 
    aes(x = x, y = y, label = labels, color = color),
    size = 2.5, hjust = 0
    ) +
  scale_color_manual(values=c("#000000", "#E69F00", "#56B4E9"))

#Verbesserte Version: 
"#000000"
"#ffd047"
"#0084b7"

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
  ggplot() +
  geom_ribbon(
    aes(x = date, ymin = evn_niceday, ymax = (-1) *SCL_mean_ribbon),
    alpha = .2
  ) +
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
    labels = NULL,
    position = "right") +
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
  

scales::show_col(colorblind_pal()(4))

# daily plot --------------------------------------------------------------

# data_daily_sum %>%
#   ggplot() +
#   # geom_ribbon(
#   #   aes(x = date, ymin = evn_niceday, ymax = mood_enthus_mean),
#   #   alpha = .2
#   # ) +
#   geom_line(aes(x = date, y = evn_niceday), 
#             color = "#E69F00", alpha = 1) +
#   geom_line(aes(x = date, y = mood_enthus_mean), 
#             color = "#000000", alpha = .6) +
#   theme_tufte() +
#   scale_y_continuous(
#     name = "Standardized Value",
#     breaks = c(-2,-1, 0, 1, 2),
#     minor_breaks = NULL,
#     labels = NULL
#   ) +
#   scale_x_date(
#     name = NULL,
#     breaks = as_date(c("2012-08-01", "2012-12-01", "2013-04-01")),
#     minor_breaks = NULL,
#     date_labels = "%b",
#     limits = as_date(c("2012-07-30", "2013-04-09"))
#   ) +
#   coord_cartesian(ylim = c(-2.5, 2.5), clip = "off")


# medicine plot -----------------------------------------------------------

data %>% 
  ggplot() +
  geom_line(aes(x = date, y = concentrat), color = "#009E73") +
  geom_ribbon(aes(x = date, ymax = concentrat, ymin = 0),
              fill = "#009E73", alpha = .1) +
  theme_tufte() +
  scale_y_continuous(
    name = "Antidepressant \n dosis (mg)",
    position = "right") +
  scale_x_date(name = NULL, 
               breaks = as_date(
                 c("2012-08-01", "2012-12-01", "2013-04-01")),
               minor_breaks = NULL, 
               date_labels ="%b",
               limits = as_date(c("2012-07-30", "2013-04-09"))
  )


ggsave("data.svg")#, 
# width = 20,
# height = 50,
# units = "cm")