


library("rvest")
library(vctrs)
library(tidyverse)
library(Rmisc)

sessionInfo()

######データの抽出方法を確認
if(0){
data <- read_html("https://imascg-slstage.boom-app.wiki/entry/idol-68")
media <- html_nodes(data,".basic") %>%
  html_text()
strsplit(media,"\r\n")

#ナマエ
STR[[1]][2]
#フリガナ
STR[[1]][4]
#タイプ
STR[[1]][6]
#身長
STR[[1]][8]
#年齢
STR[[1]][12]
#体重
STR[[1]][16]
#誕生日
STR[[1]][19]
#血液
STR[[1]][21]
#利き手
STR[[1]][23]
#seiza
STR[[1]][27]
#出身
STR[[1]][29]
#趣味
STR[[1]][31]

#3size
BWH=strsplit(STR[[1]][25],"/")
bust=BWH[[1]][1]
waist=BWH[[1]][2]
hip=BWH[[1]][3]
}


###本番のデータ抽出

idol_data<-NULL

for(i in 1:190){
  data <- read_html(paste0("https://imascg-slstage.boom-app.wiki/entry/idol-",i))
  
  media <- html_nodes(data,".basic") %>%
    html_text()
  
  STR <- strsplit(media[2],"\r\n")
  
  #3size
  BWH=strsplit(STR[[1]][25],"/")
  bust=BWH[[1]][1]
  waist=BWH[[1]][2]
  hip=BWH[[1]][3]
  
  df <- data.frame(
    name=STR[[1]][2],
    hurigana=STR[[1]][4],
    type=STR[[1]][6],
    hight=STR[[1]][8],
    old=STR[[1]][12],
    weight=STR[[1]][16],
    birthday=STR[[1]][19],
    blood=STR[[1]][21],
    dominant=STR[[1]][23],
    constellation=STR[[1]][27],
    born=STR[[1]][29],
    hobby=STR[[1]][31],
    bust=bust,
    waist=waist,
    hip=hip
  )
  
  idol_data <- rbind(idol_data,df)
  
  Sys.sleep(20)
  
}

#ここまででidol_dataに何が入っているかを確認。
#例外のページレイアウトが2ページほどある様子。
#特別処理で追加する

data <- read_html(paste0("https://imascg-slstage.boom-app.wiki/entry/idol-",184))
media <- html_nodes(data,".basic") %>%
  html_text()
STR <- strsplit(media[3],"\r\n")

#3size
BWH=strsplit(STR[[1]][25],"/")
bust=BWH[[1]][1]
waist=BWH[[1]][2]
hip=BWH[[1]][3]

df <- data.frame(
  name=STR[[1]][2],
  hurigana=STR[[1]][4],
  type=STR[[1]][6],
  hight=STR[[1]][8],
  old=STR[[1]][12],
  weight=STR[[1]][16],
  birthday=STR[[1]][19],
  blood=STR[[1]][21],
  dominant=STR[[1]][23],
  constellation=STR[[1]][27],
  born=STR[[1]][29],
  hobby=STR[[1]][31],
  bust=bust,
  waist=waist,
  hip=hip
)

idol_data <- rbind(idol_data,df)

data <- read_html(paste0("https://imascg-slstage.boom-app.wiki/entry/idol-",185))
media <- html_nodes(data,".basic") %>%
  html_text()
STR <- strsplit(media[3],"\r\n")

#3size
BWH=strsplit(STR[[1]][25],"/")
bust=BWH[[1]][1]
waist=BWH[[1]][2]
hip=BWH[[1]][3]

df <- data.frame(
  name=STR[[1]][2],
  hurigana=STR[[1]][4],
  type=STR[[1]][6],
  hight=STR[[1]][8],
  old=STR[[1]][12],
  weight=STR[[1]][16],
  birthday=STR[[1]][19],
  blood=STR[[1]][21],
  dominant=STR[[1]][23],
  constellation=STR[[1]][27],
  born=STR[[1]][29],
  hobby=STR[[1]][31],
  bust=bust,
  waist=waist,
  hip=hip
)

idol_data <- rbind(idol_data,df)

#例外レイアウトでNAになった行を削除
idol_data <- idol_data[-c(184,185),]

#文字化け対策
#ggplotとか日本語弱いから
idol_data$type<-gsub("キュート","cute",idol_data$type)
idol_data$type<-gsub("クール","cool",idol_data$type)
idol_data$type<-gsub("パッション","passion",idol_data$type)

write.csv(idol_data, "idol_data.csv", fileEncoding = "CP932",row.names=F)

#手打ちで必死に名前にローマ字のフリガナをつけてidol_data_roma.csvとしておく。
