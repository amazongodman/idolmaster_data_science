
library(ggplot2)
library(GGally)
library(vctrs)
library(tidyverse)
library(skimr)
library(glmnet)
library(rpart)
library(rpart.plot)
library(Rmisc)
library(plotly)
library(magrittr)


idol_csv_data <- read.csv("idol_data.csv")
head(idol_csv_data)
idol_csv_data$number <- c(1:nrow(idol_csv_data))
idol_csv_data$hight<-as.numeric(gsub("cm","",idol_csv_data$hight))
idol_csv_data$hight<-as.numeric(as.character(idol_csv_data$hight))
idol_csv_data$weight<-as.numeric(gsub("kg","",idol_csv_data$weight))
idol_csv_data$weight<-as.numeric(as.character(idol_csv_data$weight))
idol_csv_data$old<-as.numeric(gsub("歳","",idol_csv_data$old))
idol_csv_data$old<-as.numeric(as.character(idol_csv_data$old))
idol_csv_data$waist<-as.numeric(as.character(idol_csv_data$waist))
idol_csv_data$bust<-as.numeric(as.character(idol_csv_data$bust))
idol_csv_data$hip<-as.numeric(as.character(idol_csv_data$hip))
idol_csv_data$hi_meter<-(idol_csv_data$hight/100)
idol_csv_data$BMI <- idol_csv_data$we/(idol_csv_data$hi_meter**2)
head(idol_csv_data)

data <- data.frame(idol_csv_data[,c(4,5,6,13,14,15,16,18,3,1)])
head(data)
unknown_weight <- data[is.na(data$weight),]
unknown_old <- data[is.na(data$old),]
unknown_bust <- data[is.na(data$bust),]
unknown_waist <- data[is.na(data$waist),]
unknown_hip <- data[is.na(data$hip),]
remove_index<-c(unknown_hip$number,unknown_old$number)
data <- data[is.na(match(data$number, remove_index)),]
head(data)
nrow(data)
colSums(is.na(data))
colnames(data)
skim(data)

#01
p<-ggpairs(data[,-c(7,10)],aes_string(colour="type", alpha=0.8)) 

for(i in 1:p$nrow) {
  for(j in 1:p$ncol){
    p[i,j] <- p[i,j] + 
      scale_fill_manual(values=c("#191970", "#ff1493", "#ff4500")) + 
      scale_color_manual(values=c("#191970", "#ff1493", "#ff4500"))
    
  }
}

p

#02
p1<-ggplot(data, aes(x = old, y = weight, color = as.factor(type))) +
  geom_point()+
  geom_smooth(method = "lm",se = FALSE)+
  scale_color_manual(values=c("#191970", "#ff1493", "#ff4500"))

p2<-ggplot(data, aes(x = old, y = hight, color = as.factor(type))) +
  geom_point()+
  geom_smooth(method = "lm",se = FALSE)

plot_multi<-list()
plot_multi[[1]]<-p1
plot_multi[[2]]<-p2

multiplot(plotlist = plot_multi, cols = 2)

#03
p3<-ggplot(data, aes(x = old, y = waist, color = as.factor(type))) +
  geom_point()+
  geom_smooth(method = "lm",se = FALSE)

p4<-ggplot(data, aes(x = old, y = bust, color = as.factor(type))) +
  geom_point()+
  geom_smooth(method = "lm",se = FALSE)

p5<-ggplot(data, aes(x = old, y = hip, color = as.factor(type))) +
  geom_point()+
  geom_smooth(method = "lm",se = FALSE)

plot_multi<-list()
plot_multi[[1]]<-p3
plot_multi[[2]]<-p4
plot_multi[[3]]<-p5

multiplot(plotlist = plot_multi, cols = 3)
nrow(data)

#04
p_b <- ggpairs(data[,1:6])
p_b

#05
plot_ly(data = data,
        x = ~weight,
        y = ~hight,
        split = ~type,
        type = "scatter")

idol_csv_data2 <- read.csv("idol_data_roma.csv")
head(idol_csv_data2)

idol_csv_data2$number <- c(1:nrow(idol_csv_data2))
idol_csv_data2$hight<-as.numeric(gsub("cm","",idol_csv_data2$hight))
idol_csv_data2$hight<-as.numeric(as.character(idol_csv_data2$hight))
idol_csv_data2$weight<-as.numeric(gsub("kg","",idol_csv_data2$weight))
idol_csv_data2$weight<-as.numeric(as.character(idol_csv_data2$weight))
idol_csv_data2$old<-as.numeric(gsub("歳","",idol_csv_data2$old))
idol_csv_data2$old<-as.numeric(as.character(idol_csv_data2$old))
idol_csv_data2$waist<-as.numeric(as.character(idol_csv_data2$waist))
idol_csv_data2$bust<-as.numeric(as.character(idol_csv_data2$bust))
idol_csv_data2$hip<-as.numeric(as.character(idol_csv_data2$hip))
idol_csv_data2$hi_meter<-(idol_csv_data2$hight/100)
idol_csv_data2$BMI <- idol_csv_data2$we/(idol_csv_data2$hi_meter**2)
head(idol_csv_data2)
data2 <- data.frame(idol_csv_data2[,c(4,5,6,13,14,15,19,3,17,16,11)])
head(data2)

unknown_weight2 <- data2[is.na(data2$weight),]
unknown_old2 <- data2[is.na(data2$old),]
unknown_bust2 <- data2[is.na(data2$bust),]
unknown_waist2 <- data2[is.na(data2$waist),]
unknown_hip2 <- data2[is.na(data2$hip),]

remove_index2<-c(unknown_hip2$number,unknown_old2$number)
data2 <- data2[is.na(match(data2$number, remove_index2)),]
head(data2)
nrow(data2)
colSums(is.na(data2))
colnames(data2)
skim(data2)

plot_ly(data = data2,
        x = ~weight,
        y = ~hight,
        split = ~roma,
        type = "scatter")

head(data2)

#06
fig <- plot_ly(data=data2, x = ~weight, y = ~waist, text = ~roma, type = 'scatter', mode = 'markers',color = ~bust, colors = 'Reds',
               marker = list(size = ~log(bust), opacity = 0.9))
fig <- fig %>% layout(title = 'weight-waist and bust',
                      xaxis = list(showgrid = FALSE),
                      yaxis = list(showgrid = FALSE))
fig

#07
p1<-ggplot(data, aes(x = old, y = weight)) +
  geom_point()+
  geom_smooth(method = "lm",se = FALSE)

p2<-ggplot(data, aes(x = old, y = hight)) +
  geom_point()+
  geom_smooth(method = "lm",se = FALSE)

plot_multi<-list()
plot_multi[[1]]<-p1
plot_multi[[2]]<-p2

multiplot(plotlist = plot_multi, cols = 2)

#08
p3<-ggplot(data, aes(x = old, y = waist)) +
  geom_point()+
  geom_smooth(method = "lm",se = FALSE)

p4<-ggplot(data, aes(x = old, y = bust)) +
  geom_point()+
  geom_smooth(method = "lm",se = FALSE)

p5<-ggplot(data, aes(x = old, y = hip)) +
  geom_point()+
  geom_smooth(method = "lm",se = FALSE)

plot_multi<-list()
plot_multi[[1]]<-p3
plot_multi[[2]]<-p4
plot_multi[[3]]<-p5

multiplot(plotlist = plot_multi, cols = 3)



write.csv(data, "plot_data.csv", fileEncoding = "CP932",row.names=F)
write.csv(data2, "plot_data2.csv", fileEncoding = "CP932",row.names=F)
