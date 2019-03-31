###############################################################
# 地価公示データベース作成ルーチン
# syunsuke.fukuda@gmail.com
# 2019/03/27
#
# 国土数値情報 地価公示データ
# http://nlftp.mlit.go.jp/ksj/index.html
# データ形式は「テキスト」で、csvファイルを扱う 
###############################################################
library(tidyverse)

###############################################################
# データの読み込み
###############################################################
read_kouji_csvfile <- function(path){
  # 所在地コードを5桁文字列で取り込む処理
  tmp_df <- read_csv(path,
                     locale = locale(encoding = "cp932"),
                     col_types = cols(`所在地コード` = col_double())) %>% 
    mutate(`所在地コード` = sprintf("%05d",`所在地コード`),
           std_number = make_stdnum_string(`用途`,`連番`,`市区町村名`))
  
  return(tmp_df)
}

###############################################################
# 標準地番号で探す
###############################################################
search_by_stdnumber <- function(df, str){
  # str処理
  str <- str %>% my_prepro_for_string()
  
  target <- str_match(str, pattern = "([^\\d]*)(\\d*+)-(.*)")
  city <- target[,2]
  kind <- ifelse(target[,3] == "", "0", target[,3]) %>% as.numeric()
  numb <- target[,4] %>% as.numeric()
  
  ans <- df %>% filter(`市区町村名` == city,
                       `用途` ==  sprintf("%03d",kind),
                       `連番` ==  sprintf("%03d",numb))

}

###############################################################
# 価格列を行へ変換して、各年間の変動率を求める
# 入力のデータは１列とは限らないので各年でグループ化して
# その平均をとる
###############################################################
calc_change_rate <- function(df, base_year = 2019, year_filter = 2010, desc = FALSE){
  
  base_year_index <- 2019 - base_year
  
  ans <- df %>% 
    gather(`Ｓ５８価格`:`Ｈ３１価格`,key = "year_str", value = "unit_price") %>% 
    mutate(year = mod_year(year_str)) %>% 
    mutate(unit_price = ifelse(unit_price == 0, NA, unit_price)) %>% 
    group_by(year) %>% 
    summarise(count = n(), 
              notna_count = sum(!is.na(unit_price)), 
              mean_price = mean(unit_price, na.rm = T)) %>% 
    mutate(change_rate = mean_price / lag(mean_price) * 100 - 100,
           rate = mean_price / mean_price[length(mean_price) - base_year_index]) %>% 
    filter(year >= year_filter)
  
  if(desc == TRUE){
    ans <- arrange(ans, desc(`year`))
  }
  
  return(ans)
}


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# サブルーチン
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

###############################################################
# 標準宅地番号を作る
###############################################################
make_stdnum_string <- function(youto,renban,name){
  youto <- as.numeric(youto)
  renban <- as.numeric(renban)
  ifelse(youto == 0,
         sprintf("%s-%s",name,renban),
         sprintf("%s%s-%s",name,youto,renban))
  
}

###############################################################
# 年数変換
###############################################################

mod_year <- function(str){
  
  target <- str %>% my_prepro_for_string() %>% 
    stringr::str_match(pattern = "(S|H)(\\d+)")
  ans <- ifelse(target[,2] == "S", as.numeric(target[,3]) + 1925, 
                ifelse(target[,2] == "H", as.numeric(target[,3]) + 1988, NA))
  
}

###############################################################
# 大文字小文字等の前処理
###############################################################
my_prepro_for_string <- function(str){
  # origin
  org <- c("Ｓ","Ｈ"," ","　","－","ー","０","１","２","３","４","５","６","７","８","９")
  # dist
  dist <- c("S","H","","","-","-","0","1","2","3","4","5","6","7","8","9")

  str %>% stringr::str_replace_all(setNames(dist,org))
}

