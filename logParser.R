library(tidyverse)
library(lubridate)
library(scales)
library(rmarkdown)

# Input log ---------------------------------------------------------------

logdata <- read_lines("xena.nginx.log")
#logdata <- logdata[startsWith(logdata, "access")]
# all(startsWith(logdata, "access"))
# head(logdata[!startsWith(logdata, "access")], 5)


# Parse log ---------------------------------------------------------------

#$remote_addr $remote_user $remote_user [$time_local] "$request" $status $bytes_sent "$http_referer" "$http_user_agent"
logformat <- "^([^ ]+) - ([^ ]+) \\[([^\\]]+)\\] \"([^\"]+)\" ([^ ]+) ([^ ]+) \"([^\"]+)\" \"([^\"]+)\""
logdf <- str_match(logdata, pattern = logformat)[, -1] %>%
  na.omit() %>%
  as.data.frame() %>%
  set_names(c("remote_addr", "remote_user", "time_local",
              "request", "status", "bytes_sent", "http_referer",
              "http_user_agent")) %>%
  mutate(
    remote_addr = ifelse(
      grepl("^[^0-9]", remote_addr),
      sub("^([^0-9]+)", "", remote_addr),
      remote_addr
    )
  ) %>%
  as_tibble() %>%
  filter(status == "200") %>%
  mutate(
    time_local = as_datetime(time_local,
                             format = "%d/%b/%Y:%T"),
    status = as.integer(status),
    bytes_sent = as.integer(bytes_sent),
    day = as.Date(time_local)
  )

# Summarize log -----------------------------------------------------------

# Here,
# how to define a visit or a download?
# Ref: https://www.jianshu.com/p/537a0bddda94

# floor_date(logdf$time_local[1:100], unit = "10minutes")

UV = logdf %>%
  distinct(day, remote_addr) %>%
  count(day)

PV = logdf %>%
  mutate(interval = floor_date(time_local, unit = "10minutes")) %>% # 10 分钟内的相同页面(http_referer)多次访问看作一次 PV
  distinct(day, interval, http_referer) %>%
  count(day)

Downloads = logdf %>%
  group_by(day) %>%
  summarise(
    downloads = sum(grepl("GET", request) & grepl("/download", request)),
    .groups = "drop"
  )

save(UV, PV, Downloads, file = "XenaLog.RData")

# Visualize ---------------------------------------------------------------

render("xena.Rmd", output_file = "index.html")
