# Script for analyzing clay caterpillar predation

library(dplyr)
library(gsheet)
library(lubridate)

# new comment
url = "https://docs.google.com/spreadsheets/d/1hi7iyi7xunriU2fvFpgNVDjO5ERML4IUgzTkQjLVYCo/edit?gid=0#gid=0"

df = gsheet2tbl(url)