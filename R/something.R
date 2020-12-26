pacman::p_load(tidyverse, ggplot2, here, magrittr, 
               lubridate, lavaan, ggx, hms)

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



# check correlatedness of momentary items ---------------------------------

cortab <- data %>% select(contains("mood")) %>% rename_with(function(x)str_remove(x, "mood_"), everything()) %>% 
  cor(use = "pairwise.complete.obs")

cortab <- round(cortab, digits = 1)

cortab[cortab < 0.5] <- ""

cortab[upper.tri(cortab)] <- ""

cortab <- as.data.frame(cortab)

cortab

cfa_data <- data %>% select(contains("mood")) %>% rename_with(function(x)str_remove(x, "mood_"), everything())

cfa_model <- "negative =~ down + irritat + lonely + anxious + suspic + guilty + doubt
              positive =~ relaxed + satisfi + enthus + cheerf + strong"

cfa_fit <- cfa(cfa_model, cfa_data)

cfa_fit %>% summary(standardized = TRUE, fit.measures = TRUE)

# activities --------------------------------------------------------------

data %>% ggplot() + geom_line(aes(x = datetime, y = phy_hungry))
  
mean(data$phy_hungry, na.rm = TRUE)

data %>% ggplot() + geom_bar(aes(x = phy_hungry))

data %>% ggplot() + geom_bar(aes(x = soc_who1))
data %>% ggplot() + geom_bar(aes(x = soc_who1))

data %>% ggplot() + geom_bar(aes(x = act_what1)) + 
  gg_("rotate x-axis labels by 90 degrees")
data %>% ggplot() + geom_bar(aes(x = soc_who1)) + 
  gg_("rotate x-axis labels by 90 degrees")
  
data %>% ggplot() + geom_point(aes(x = datetime, y = act_what1))
data %>% ggplot() + geom_point(aes(x = datetime, y = act_what2))
data %>% ggplot() + geom_point(aes(x = datetime, y = soc_who1))


# missings ----------------------------------------------------------------

data %>% ggplot() + geom_bar(aes(x = factor(beepno)))

data %>% complete(dayno, beepno) %>% 
  ggplot() + geom_bar(aes(x = factor(beepno)))

data %>% pull(resp_abort) %>% is.na %>% sum

data %>% pull(resp_abort) %>% table

data %>% filter(!is.na(SCL.90.R.14)) %>% 
  select(dayno, beepno, SCL.90.R.14) %>% 
  View()

data_exp <- complete(data, dayno, beepno)

# average daily -----------------------------------------------------------

data %$% aov(phy_hungry ~ dayno*beepno) %>% summary

data_exp %>% filter(dayno < 10) %>% 
  ggplot(aes(x = beepno, y = mood_satisfi)) +
  geom_line() + facet_wrap(~dayno)

data_exp %<>% mutate(duration = as.duration(resptime_e - resptime_s))

data_exp %>% ggplot() + geom_point(aes(x = datetime, y = duration))

data_exp %>% ggplot(aes(x = factor(beepno), y = factor(mood_satisfi))) +
  geom_bin2d()

data_exp %>% ggplot(aes(x = beepno, y = mood_satisfi)) +
  geom_smooth() + geom_jitter(alpha = .5)

data_exp %>% 
  filter(dayno < 200, dayno > 100) %>% 
  ggplot() + 
  geom_point(aes(x = datetime, y = evn_niceday))

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

sumstat_cors <- data_daily %>% 
  select(starts_with("mood")) %>% 
  map(~cor(., data_daily$evn_niceday, use = "pairwise.complete.obs")) %>% 
  tibble(names = names(.), values = unlist(.)) %>% 
  select(names, values)

data_daily <- data.frame(dayno = rep(c(1:124, 126:239), each = 2)) %>% 
  left_join(data_daily, by = "dayno")

data_daily %>% filter(dayno < 140, dayno > 70) %>% 
  ggplot() + 
  geom_line(aes(x = dayno, y = scale(mood_enthus_mean)), color = "red") +
  geom_line(aes(x = dayno, y = scale(evn_niceday))) 

data_daily %>% filter(dayno < 140, dayno > 70) %>% 
  ggplot() + 
  geom_line(aes(x = dayno, y = scale(mood_down_mean)), color = "red") +
  geom_line(aes(x = dayno, y = scale(evn_niceday))) 

data_daily %>%# filter(dayno < 140, dayno > 70) %>% 
  mutate(diff_enthus = scale(mood_enthus_mean) - scale(evn_niceday),
         diff_relaxed = scale(mood_relaxed_mean) - scale(evn_niceday)) %>% 
  ggplot() + 
  geom_line(aes(x = dayno, y = diff_enthus), color = "red") +
  geom_line(aes(x = dayno, y = diff_relaxed))

diffvars <-
  map_dfc(select(select(data_daily, contains("mood_")), contains("_mean")), 
          ~scale(.x) - scale(data_daily$evn_niceday))

data_daily %>% select(contains("sd"), dayno) %>% 
  pivot_longer(contains("sd"), names_to = "var", values_to = "sd") %>% 
  ggplot() + geom_line(aes(x = dayno, y = sd)) + 
  geom_smooth(aes(x = dayno, y = sd)) + facet_wrap(~var)

data_daily %>% select(contains("mean"), dayno) %>% 
  pivot_longer(contains("mean"), names_to = "var", values_to = "mean") %>% 
  ggplot() + geom_line(aes(x = dayno, y = mean)) + 
  geom_smooth(aes(x = dayno, y = mean)) + facet_wrap(~var)

data_daily %>% select(contains("max"), dayno) %>% 
  pivot_longer(contains("max"), names_to = "var", values_to = "max") %>% 
  ggplot() + geom_line(aes(x = dayno, y = max)) + 
  geom_smooth(aes(x = dayno, y = max)) + facet_wrap(~var)

data_daily %>% select(contains("min"), dayno) %>% 
  pivot_longer(contains("min"), names_to = "var", values_to = "min") %>% 
  ggplot() + geom_line(aes(x = dayno, y = min)) + 
  geom_smooth(aes(x = dayno, y = min)) + facet_wrap(~var)

data_daily %>% 
  ggplot() + geom_line(aes(x = dayno, y = evn_niceday)) + 
  geom_smooth(aes(x = dayno, y = evn_niceday))

cortab <- cor(diffvars, use = "pairwise.complete.obs")

cortab <- round(cortab, digits = 1)

cortab[abs(cortab) < 0.5] <- ""

cortab[upper.tri(cortab)] <- ""

cortab <- as.data.frame(cortab)

cortab

lm(evn_niceday ~ mood_down_mean + mood_enthus_mean + mood_satisfi_mean + 
     mood_strong_mean + mood_cheerf_mean + mood_relaxed_mean + 
     mood_anxious_mean +
     mood_irritat_mean + mood_suspic_mean + mood_lonely_mean +
     mood_guilty_mean + mood_doubt_mean, data = data_daily) %>% 
  summary

lm(evn_niceday ~ mood_enthus_mean, data = data_daily) %>% 
  summary


sumstat_cors %<>% mutate(statistic = map_chr(sumstat_cors$names, statfromname))

sumstat_cors %>% 
  ggplot() + 
  geom_histogram(aes(x = values)) +
  facet_wrap(~statistic)

data_daily %<>% mutate(time = rep(c("00:00:00", "23:59:59"), 238)) %>% 
  unite(datetime, c(date, time), sep = "-") %>% 
  mutate(datetime = parse_date_time(datetime, "ymdHMS"))

data_exp %>% ggplot() +
  geom_density(aes(x = time))


#safe <- data_exp

data_exp$evn_niceday <- 
  map2_dbl(
    data_exp$dayno, 
    data_exp$evn_niceday,
    get_value_evn_nd,
    keys = data_daily)

ggplot() +
  geom_line(
    data = filter(data_daily, dayno < 10), 
    aes(x = datetime, y = evn_niceday),
    size = 1) +
  geom_point(
    data = filter(data_exp, dayno < 10), 
    aes(x = datetime, y = mood_satisfi),
    color = "red",
    size = 2
  ) +
  facet_wrap(~dayno)

ggplot() +
  geom_line(
    data = filter(data_daily, dayno < 10), 
    aes(x = as_hms(datetime), y = evn_niceday),
    size = 1) +
  geom_point(
    data = filter(data_exp, dayno < 10), 
    aes(x = as_hms(datetime), y = mood_satisfi),
    color = "red",
    size = 2
  ) +
  facet_wrap(~dayno)

ggplot() +
  geom_line(
    data = filter(data_daily, dayno < 10000), 
    aes(x = datetime, y = evn_niceday),
    size = 1) +
  geom_smooth(
    data = filter(data_daily, dayno < 10000), 
    aes(x = datetime, y = evn_niceday)
  )

data_exp %>% 
  filter(dayno < 6) %>% 
  ggplot() +
  geom_line(aes(x = beeptime, y = mood_satisfi, color = factor(dayno)),
            size = 1, alpha = .5) 

data_exp$mood_satisfi %>% sd(na.rm = T)

data_daily$evn_niceday %>% sd(na.rm = T)

nmis <- data_exp %>% 
  group_by(dayno) %>% 
  summarise(n = sum(!is.na(mood_satisfi))) %>% 
  pull(n)

var <- data_daily %>% 
  group_by(dayno) %>% 
  summarise(evn_niceday = unique(evn_niceday)) %>% 
  pull(evn_niceday)

cor(nmis, is.na(var), 
    use = "pairwise.complete.obs", 
    method = "spearman")


data_exp %<>% mutate(weekday = wday(datetime),
                     week = isoweek(datetime))

data_daily %<>% mutate(weekday = wday(datetime),
                     week = week(datetime),
                     year = year(datetime),
                     wy = str_c(week, year))

data_daily$filt <- rep(c(TRUE, FALSE), 238)

data_daily %>% filter(filt, year == 2012, week > 45) %>% 
  ggplot() +
  geom_line(aes(x = weekday, y = evn_niceday, color = wy), size = 2) +
  facet_wrap(~wy)


# depression items --------------------------------------------------------

data_exp %<>% mutate(weekday = wday(datetime, label = T),
                     week = isoweek(datetime))

data_daily %<>% mutate(weekday = wday(datetime),
                       week = week(datetime),
                       year = year(datetime),
                       wy = str_c(week, year))

data_exp %>% select(contains("SCL")) %>% View()

data_exp %>% pull(date) %>% min(na.rm = T) %>% 
  wday()

first_sunday <- as_date("2012-08-13")

data_exp %<>% 
  mutate(
    #studyweek = interval(first_sunday, date) %/% weeks(1),
    studyweek = (dayno - 1) %/% 7)

data_exp %>% select(studyweek, dayno, studyweek_2,
                    weekday) %>% View()

data_weekly <- data_exp %>% 
  group_by(studyweek) %>% 
  summarise(across(contains("SCL"), unique_naomit)) 

data_weekly$SCL_mean = rowMeans(select(data_weekly, contains("SCL")), na.rm = T)

data_weekly %>% 
  pivot_longer(contains("SCL"), 
               values_to = "value", 
               names_to = "variable") %>% 
  ggplot() + 
  geom_line(aes(x = studyweek, y = value)) +
  #geom_smooth(aes(x = week, y = value)) +
  facet_wrap(~variable)

data_exp %>% arrange(datetime) %>% View()

# more stuff --------------------------------------------------------------

    

data %>% ggplot() + geom_line(aes(x = datetime, y = mood_satisfi)) +
  geom_smooth(aes_string(x = "datetime", y = "mood_satisfi"))

mypl <- function(str){
  str <- sym(str)
  ggplot(data, aes(x = datetime2, y = !!str)) + geom_point()
}

map(moodnames, 
    mypl
)

# mood_, pat_, se_, soc_, phy_, act_ : momentary
# mor_, eve_ : daily
# SCL, dep: weekly


data %>% psych::describe()

data %>% ggplot() + geom_point(aes(x = date, y = dep))

moodnames <- select(data, contains("mood")) %>% names

moodgroups = list(g1 = moodnames[1:4],
              g2 = moodnames[5:8],
              g3 = moodnames[9:13])

get_mood_group <- function(char){
  for(i in 1:3){
    if(char %in% moodgroups[[i]]){return(i)}
  }
  return(NA)
}

data %>% 
  pivot_longer(
    starts_with("mood"), 
    names_to = "item", 
    values_to = "response") %>% 
  mutate(moodgroup = map_dbl(item, get_mood_group)) %>% 
  ggplot() + geom_line(aes_string(x = "date", y = "response", color = "item"),
                       alpha = .5) #+ 
  facet_grid(cols = vars(moodgroup))

data %>% ggplot() + geom_line(aes(x = date, y = ))

data %>% filter(dayno < 20) %>% 
  ggplot(aes(x = datetime, y = mood_down)) + geom_line() +
  geom_point(aes(color = factor(beepno > 5)), alpha = .5)#+ geom_smooth()

data %>% filter(dayno < 15) %>% 
  ggplot(aes(x = resptime_s, y = mood_down)) + geom_line() +
  geom_point(alpha = .5) + facet_wrap(~dayno)

data %>% filter(dayno < 29) %>% 
  ggplot(aes(x = datetime, y = mood_lonely)) + geom_point() + geom_smooth()

data %>% 
  ggplot(aes(x = datetime, y = mood_anxious)) + geom_point() + geom_smooth()

data %>% mutate(meanmood = pmax(mood_down + mood_lonely + mood_anxious)) %>% 
  ggplot(aes(x = datetime, y = meanmood)) + geom_point() + geom_smooth() +
  facet_wrap(~beepno)

data %>% filter(dayno < 29) %>% group_by(date) %>% 
  summarise(mean_down = mean(mood_down, na.rm = TRUE),
            mean_lonely = mean(mood_lonely, na.rm = TRUE),
            mean_anxious = mean(mood_anxious, na.rm = TRUE)) %>% 
  pivot_longer(contains("mean"), names_to = "item", values_to = "response") %>% 
  ggplot() + 
  geom_line(aes(x = date, y = response, color = factor(item)), size = 1,
                                                  alpha = .5) +
  scale_x_date() +
  theme_minimal()
