###############################################################
# 市町村コードデータベース作成ルーチン
# syunsuke.fukuda@gmail.com
# 2019/03/23
#
#
# 総務省管轄のWebページ
# http://www.soumu.go.jp/denshijiti/code.html
# 全国地方公共団体コード
# 都道府県コード及び市区町村コードがExcelファイルで
# 公開されている。
# また改正一覧表もExcelファイルで公開されている。
# このデータをRに取り込み、実際に使える形に成型するルーチン
#
# ファイルについては、コードのExcelファイルを、
# シートごとに手動でCSVファイルとして保存して準備し、
# これをRへ取り込むこととする
###############################################################

library(tidyverse)

##############################
# function
##############################


# 都道府県コード DB
make_prefecture_code <- function(){
    
  tribble( 
                   ~code, ~name, ~kana,
                    "01000","北海道","ほっかいどう",
                    "02000","青森県","あおもりけん",
                    "03000","岩手県","いわてけん",
                    "04000","宮城県","みやぎけん",
                    "05000","秋田県","あきたけん",
                    "06000","山形県","やまがたけん",
                    "07000","福島県","ふくしまけん",
                    "08000","茨城県","いばらきけん",
                    "09000","栃木県","とちぎけん",
                    "10000","群馬県","ぐんまけん",
                    "11000","埼玉県","さいたまけん",
                    "12000","千葉県","ちばけん",
                    "13000","東京都","とうきょうと",
                    "14000","神奈川県","かながわけん",
                    "15000","新潟県","にいがたけん",
                    "16000","富山県","とやまけん",
                    "17000","石川県","いしかわけん",
                    "18000","福井県","ふくいけん",
                    "19000","山梨県","やまなしけん",
                    "20000","長野県","ながのけん",
                    "21000","岐阜県","ぎふけん",
                    "22000","静岡県","しずおかけん",
                    "23000","愛知県","あいちけん",
                    "24000","三重県","みえけん",
                    "25000","滋賀県","しがけん",
                    "26000","京都府","きょうとふ",
                    "27000","大阪府","おおさかふ",
                    "28000","兵庫県","ひょうごけん",
                    "29000","奈良県","ならけん",
                    "30000","和歌山県","わかやまけん",
                    "31000","鳥取県","とっとりけん",
                    "32000","島根県","しまねけん",
                    "33000","岡山県","おかやまけん",
                    "34000","広島県","ひろしまけん",
                    "35000","山口県","やまぐちけん",
                    "36000","徳島県","とくしまけん",
                    "37000","香川県","かがわけん",
                    "38000","愛媛県","えひめけん",
                    "39000","高知県","こうちけん",
                    "40000","福岡県","ふくおかけん",
                    "41000","佐賀県","さがけん",
                    "42000","長崎県","ながさきけん",
                    "43000","熊本県","くまもとけん",
                    "44000","大分県","おおいたけん",
                    "45000","宮崎県","みやざきけん",
                    "46000","鹿児島県","かごしまけん",
                    "47000","沖縄県","おきなわけん") %>% 
  mutate(kana = hira2kata_fixed(kana))

}


# メインの市町村データ読み込み
read_cities <- function(csv_file_path){
  res <- read_csv(csv_file_path, locale = locale(encoding = "UTF-8"), 
                  col_names = c("団体コード",
                                "都道府県名（漢字）",
                                "市区町村名（漢字）",
                                "都道府県名（カナ）",
                                "市区町村名（カナ）"),
                  col_types = cols(`団体コード` = col_double()),
                  skip = 1) 
  
  # 団体コード部分の書式の書式を整形
  # オリジナルexcelファイルをcsvで保存したときに、
  # 文字列から数値に変換される場合があることに留意
  res <- res %>% 
    mutate(`団体コード` = sprintf("%06d",`団体コード` )) %>% 
    mutate(`団体コード` = str_sub(`団体コード`,1,5))  %>%
    select(`団体コード`,`市区町村名（漢字）`,`市区町村名（カナ）`) %>%
    filter(! is.na(`市区町村名（漢字）`)) %>% 
    mutate(`市区町村名（カナ）` = hankana2zenkana(`市区町村名（カナ）`))

  return(res)  
}

# 大都市データ読み込み
read_big_cities <- function(path){
  res <- read_csv(path, locale = locale(encoding = "UTF-8"), 
                  col_names = c("団体コード",
                                "市区町村名（漢字）",
                                "市区町村名（カナ）"),
                  col_types = cols(`団体コード` = col_double())) 
  res <- res %>% 
    mutate(`団体コード` = sprintf("%06d",`団体コード` )) %>% 
    mutate(`団体コード` = str_sub(`団体コード`,1,5)) %>% 
    mutate(`市区町村名（カナ）` = hira2kata_fixed(`市区町村名（カナ）`))
  
  return(res)  
}

# 廃止された市区町村コードデータ読み込み
read_delete_cities <- function(path){
    
    target <- read_csv(path, skip = 3) %>% select(X6:X9) %>% 
      mutate(X9 = myseq(X9,"〃")) %>% 
      filter(X9 == "欠番") %>% select(-X9)

    colnames(target) <- c("団体コード", "市区町村名（漢字）","市区町村名（カナ）")
    
    target <- target %>% mutate(`団体コード` = str_sub(`団体コード`,1,5))
    return(target)
}

# 市区町村コードのデータベースを作る
make_city_code_db <- function(city_path, big_city_path, history_path){

    cities <- read_cities(city_path)
    big_cities <- read_big_cities(big_city_path)
    hist_sities <- read_delete_cities(history_path)
    
    ans_db <- bind_rows(cities, big_cities, hist_sities) 

    prefecture_db <- make_prefecture_code()
    colnames(prefecture_db) <- c("団体コード", "都道府県名（漢字）","都道府県名（カナ）")
    
    # 都道府県名列等を付け加える
    ans_db <- ans_db %>% 
        mutate(pre_tmp_col = str_c(str_sub(`団体コード`,1,2),"000")) %>%
        left_join(prefecture_db, by = c( "pre_tmp_col" = "団体コード")) %>%
        select(-pre_tmp_col)

    return(ans_db)
}


##################################################
# 外部プログラム  
##################################################


# 参考資料
# https://gist.github.com/dichika/5273e7bd540ba3d5d551
# http://ja.wikipedia.org/wiki/%E5%8D%8A%E8%A7%92%E3%82%AB%E3%83%8A#.E5.8D.8A.E8.A7.92.E3.82.AB.E3.83.8A.E4.B8.80.E8.A6.A7

hankana2zenkana <- function(x){
  # character変換
  if (!is.character(x)){ x <- as.character(x) }
  
  # 濁点、半濁点文字の置換
  dh <- c("ｶﾞ","ｷﾞ","ｸﾞ","ｹﾞ","ｺﾞ","ｻﾞ","ｼﾞ","ｽﾞ","ｾﾞ","ｿﾞ","ﾀﾞ","ﾁﾞ","ﾂﾞ","ﾃﾞ","ﾄﾞ","ﾊﾞ","ﾋﾞ","ﾌﾞ","ﾍﾞ","ﾎﾞ","ﾊﾟ","ﾋﾟ","ﾌﾟ","ﾍﾟ","ﾎﾟ")
  dz <- c("ガ","ギ","グ","ゲ","ゴ","ザ","ジ","ズ","ゼ","ゾ","ダ","ヂ","ヅ","デ","ド","バ","ビ","ブ","ベ","ボ","パ","ピ","プ","ペ","ポ")
  for( i in 1:length(dz) ){ x <- gsub(dh[i],dz[i],x) }
  
  # 1bite文字の置換
  x <- chartr("ｱｲｳｴｵｶｷｸｹｺｻｼｽｾｿﾀﾁﾂﾃﾄﾅﾆﾇﾈﾉﾊﾋﾌﾍﾎﾏﾐﾑﾒﾓﾔﾕﾖﾗﾘﾙﾚﾛﾜｦﾝ｡｢｣､･ｦｧｨｩｪｫｬｭｮｯｰ"
              , "アイウエオカキクケコサシスセソタチツテトナニヌネノハヒフヘホマミムメモヤユヨラリルレロワヲン。「」、・ヲァィゥェォャュョッー"
              , x)
  
  print(x)
}

##################################################
# a Tokyo.R member teached it me
##################################################
kata <- c(Nippon:::jpn.syllabary$Katakana, Nippon:::jpn.syllabary.add$Katakana)
# Encodingを設定
Encoding(kata) <- "UTF-8"

hira <- c(Nippon:::jpn.syllabary$Hiragana, Nippon:::jpn.syllabary.add$Hiragana)
# Encodingを設定
Encoding(hira) <- "UTF-8"

hira2kata_fixed <- function (x) {
  stringr::str_replace_all(x, setNames(kata, hira))
}

kata2hira_fixed <- function (x) {
  stringr::str_replace_all(x, setNames(hira, kata))
}


# 前列と同じ値を入れる
myseq <- function(v, char){
  dummy <- v
  for (i in 1:length(dummy)){
    if (! is.na(dummy[i])){
      if (dummy[i] == char) {
        dummy[i] <- dummy[i-1]
      }
    }
  }
  return(dummy)
}


