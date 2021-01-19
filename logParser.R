library(tidyverse)
logdata <- read_lines("xena.nginx.log")

all(startsWith(logdata, "access"))
head(logdata[!startsWith(logdata, "access")], 5)

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
  )
