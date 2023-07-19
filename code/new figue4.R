






setwd("C:/Users/a/Desktop/数据分析/皮肤油脂/xlsx")

library(ggplot2)
library(ggpubr)
library(dplyr) #加载dplyr包
library(ggpmisc) #加载ggpmisc包
library(readxl)

dtest <- read_xlsx(  "CLSER仪器数据测一次.xlsx",
                     sheet = "测试一次仪器数据",
                     range = NULL,
                     col_names = TRUE,
                     col_types = NULL,
                     na = ""
                     
)

etest <- read_xlsx(  "CS-2011-66 受试者信息3.xlsx",
                     sheet = "Sheet1",
                     range = NULL,
                     col_names = TRUE,
                     col_types = NULL,
                     na = ""
                     
)

gtest <- read_xls(  "临床评估.xls",
                    sheet = "Sheet1",
                    range = NULL,
                    col_names = TRUE,
                    col_types = NULL,
                    na = ""
                    
)



ftest <- merge(dtest,etest,by = "编号")


htest <- merge(ftest,gtest,by = "编号")
htest<- subset(htest,age<=35)

m<-"male"
f<-"female"

htest$sex <- gsub("男", "male", htest$sex, fixed = TRUE)
htest$sex <- gsub("女", "female", htest$sex, fixed = TRUE)

mtest <- htest[htest$sex=="male",]

m1test <- mtest[mtest$skintype==1,]

m2test <- mtest[mtest$skintype==2,]

wtest <- htest[htest$sex=="female",]

w1test <- wtest[wtest$skintype==1,]

w2test <- wtest[wtest$skintype==2,]

twotest <- htest[htest$skintype==2,]
onetest <- htest[htest$skintype==1,]


htest$agelabel <- htest$age
htest$agelabel[htest$agelabel<25&htest$agelabel>=20 ] <- "20-25"
htest$agelabel[htest$agelabel<30&htest$agelabel>=25 ] <- "25-30"
htest$agelabel[htest$agelabel<=35&htest$agelabel>=30 ] <- "30-35"
  
#ctest <- openxlsx::read.xlsx("测量三次删减成第一次总.xlsx", sheet="Sheet1", rowNames=T,na.strings = ".")


cols <- c("male" = "green", "female" = "red")
sha <- c("male" = "19", "female" = "17")
shapes <- c("male" = 6, "female" = 4)

htest$sex <- factor(htest$sex )

#####new figure age determined


#agetest <- htest[c]

#柱状图
shapiro.test(htest$leftcheekSER)


p <- ggplot(data = htest,aes(x=agelabel,y=leftcheekSER,fill = agelabel),show.legend=F)+
  geom_bar(stat="identity",position=position_dodge(0.75),show.legend =F ,width=0.6)+
 coord_cartesian(ylim=c(0,7))+
  scale_y_continuous(expand = c(0, 0))+#消除x轴与绘图区的间隙
  scale_fill_manual(values =c("#FC4E07","#00AFBB", "#E7B800"))+#颜色的十六进制代码，或直接用red、blue、green等也可
  

 # stat_compare_means(method = "t.test",paired = TRUE, comparisons=list(c("20-25","25-30"),c("25-30","30-35"),c("20-25","30-35")))
theme(text=element_text(size=15,  family="sans", face = "bold")#times new roma#sans/宋体#mono/雅黑  
      ,axis.line = element_line(colour = "black")
      ,panel.grid.major=element_blank()
      ,panel.grid.minor=element_blank()
      ,panel.background = element_blank()
      ,axis.ticks.length.y = unit(.15, "cm"),
      legend.key = element_blank (),
      legend.position = "bottom",
)+
labs(title ="Sebum-level dynamics in the initial stage of aging", x="Age stage (year)", y="real SER (a.u./min)")+
stat_compare_means(method = "t.test",paired = F, comparisons=list(c("20-25","25-30"),c("25-30","30-35"),c("20-25","30-35")))#配对样本t检验 要求样本服从正态分布
p

ggsave("Sebum-level dynamics in the initial stage of aging.tiff",device = "tiff",width = 7,height =7,dpi = 300,path = "C:/Users/a/Desktop/数据分析/皮肤油脂/xlsx/new_figure/figure4")

###########age but CSL

aov.mean<-aggregate(htest$second_leftcheek_CL,by=list(htest$agelabel),FUN=mean)
aov.sd<-aggregate(htest$second_leftcheek_CL,by=list(htest$agelabel),FUN=sd)
aov<-data.frame(aov.mean,sd=aov.sd$x)
aov$mean <-aov$x

shapiro.test(htest$second_leftcheek_CL)

p <- ggplot(data = htest,aes(x=agelabel,y=second_leftcheek_CL,fill = agelabel),show.legend=F)+
  geom_bar(stat="identity",position=position_dodge(0.75),show.legend =F ,width=0.6)+
  coord_cartesian(ylim=c(0,500))+
  scale_y_continuous(expand = c(0, 0))+#消除x轴与绘图区的间隙
  scale_fill_manual(values =c("#FC4E07","#00AFBB", "#E7B800"))+#颜色的十六进制代码，或直接用red、blue、green等也可
  
  #geom_errorbar(data=aov,aes(x=Group.1,ymax=mean+sd,ymin=mean-sd),position=position_dodge(0.9),width=0.15)+
  # stat_compare_means(method = "t.test",paired = TRUE, comparisons=list(c("20-25","25-30"),c("25-30","30-35"),c("20-25","30-35")))
  theme(text=element_text(size=15,  family="sans", face = "bold")#times new roma#sans/宋体#mono/雅黑  
        ,axis.line = element_line(colour = "black")
        ,panel.grid.major=element_blank()
        ,panel.grid.minor=element_blank()
        ,panel.background = element_blank()
        ,axis.ticks.length.y = unit(.15, "cm"),
        legend.key = element_blank (),
        legend.position = "bottom",
  )+
  labs(title ="Sebum-level dynamics in the initial stage of aging", x="Age stage (year)", y="one-time CSL (a.u.)")+
  stat_compare_means(method = "t.test",paired = F, comparisons=list(c("20-25","25-30"),c("25-30","30-35"),c("20-25","30-35")))#t检验 要求样本服从正态分布
p


ggsave("Sebum-level dynamics in the initial stage of aging_onetimecsl.tiff",device = "tiff",width = 7,height =7,dpi = 300,path = "C:/Users/a/Desktop/数据分析/皮肤油脂/xlsx/new_figure/figure4")



########################stage of dry?
htest$skinlabel <- htest$skintype
htest$skinlabel[htest$skintype==1] <- "Non-oliy skin"
htest$skinlabel[htest$skintype==2] <- "Oliy skin"

p <- ggplot(data = htest,aes(x=skinlabel,y=second_leftcheek_CL,fill = skinlabel))+
  geom_bar(stat="identity",position=position_dodge(0.75),show.legend =F, width=0.6)+
  coord_cartesian(ylim=c(0,400))+
  scale_y_continuous(expand = c(0, 0))+#消除x轴与绘图区的间隙
  scale_fill_manual(values =c("#FC4E07","#00AFBB", "#E7B800"))+#颜色的十六进制代码，或直接用red、blue、green等也可
  
  
  # stat_compare_means(method = "t.test",paired = TRUE, comparisons=list(c("20-25","25-30"),c("25-30","30-35"),c("20-25","30-35")))
  theme(text=element_text(size=15,  family="sans", face = "bold")#times new roma#sans/宋体#mono/雅黑  
        ,axis.line = element_line(colour = "black")
        ,panel.grid.major=element_blank()
        ,panel.grid.minor=element_blank()
        ,panel.background = element_blank()
        ,axis.ticks.length.y = unit(.15, "cm"),
        legend.key = element_blank (),
        legend.position = "bottom",
  )+
  labs(title ="Sebum-level dynamics in the initial stage of aging_onetimecsl", x="Skin Type", y="one-time CSL (a.u.)")+
  stat_compare_means(method = "t.test",paired = F, comparisons=list(c(1,2)))#配对样本t检验 要求样本服从正态分布
p

ggsave("Skintype_onetimecsl.tiff",device = "tiff",width = 7,height =7,dpi = 300,path = "C:/Users/a/Desktop/数据分析/皮肤油脂/xlsx/new_figure/figure4")

###################stage of sexsual difference


p <- ggplot(data = htest,aes(x=sex,y=second_leftcheek_CL,fill = sex))+
  geom_bar(stat="identity",position=position_dodge(0.75),show.legend =F, width=0.6)+
  coord_cartesian(ylim=c(0,400))+
  scale_y_continuous(expand = c(0, 0))+#消除x轴与绘图区的间隙
  scale_fill_manual(values =c("#FC4E07","#00AFBB", "#E7B800"))+#颜色的十六进制代码，或直接用red、blue、green等也可
  
  
  # stat_compare_means(method = "t.test",paired = TRUE, comparisons=list(c("20-25","25-30"),c("25-30","30-35"),c("20-25","30-35")))
  theme(text=element_text(size=15,  family="sans", face = "bold")#times new roma#sans/宋体#mono/雅黑  
        ,axis.line = element_line(colour = "black")
        ,panel.grid.major=element_blank()
        ,panel.grid.minor=element_blank()
        ,panel.background = element_blank()
        ,axis.ticks.length.y = unit(.15, "cm"),
        legend.key = element_blank (),
        legend.position = "bottom",
  )+
  labs(title ="Sexual difference in the initial stage of aging_onetimecsl", x="Sexual difference", y="one-time CSL (a.u.)")+
  stat_compare_means(method = "t.test",paired = F, comparisons=list(c(1,2)))#配对样本t检验 要求样本服从正态分布
p

ggsave("Sex_onetimecsl.tiff",device = "tiff",width = 7,height =7,dpi = 300,path = "C:/Users/a/Desktop/数据分析/皮肤油脂/xlsx/new_figure/figure4")








