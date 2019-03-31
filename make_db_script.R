# データベースを作るスクリプト
library(tidyverse)

source("make_address_db.R", encoding = "UTF-8")
source("make_kouji_db.R", encoding = "UTF-8")

# 市区町村データベース
city_p     <- "data/cities.csv"
big_city_p <- "data/big_cities.csv"
history_p  <- "data/history.csv"

cities_db <- make_city_code_db(city_p, big_city_p, history_p)

# 地価公示データベース
kouji_db_org <- read_kouji_csvfile("data/L01-31P-2K.csv")

# 住所の結合
kouji_db <- kouji_db_org %>% left_join(cities_db, by = c("所在地コード" = "団体コード"))

#オブジェクトをファイルとして保存
save(kouji_db, file = "kouji_db.RData")
