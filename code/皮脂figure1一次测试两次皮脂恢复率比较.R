


setwd("C:/Users/a/Desktop/数据分析/皮肤油脂/xlsx")

library(ggplot2)
library(ggpubr)
library(dplyr) #加载dplyr包
library(ggpmisc) #加载ggpmisc包
library(readxl)
library(ggsignif)

dtest <- read_xlsx(  "CLSER仪器数据图1.xlsx",
                     sheet = "测试一次仪器数据",
                     range = NULL,
                     col_names = TRUE,
                     col_types = NULL,
                     na = ""
                     
)


ptest <- read_xlsx(  "CLSER仪器数据图1.xlsx",
                     sheet = "测试三次仪器数据",
                     range = NULL,
                     col_names = TRUE,
                     col_types = NULL,
                     na = ""
                     
)




firsttest <- dtest[,c(grep("first-",colnames(dtest)))]

secondtest <- dtest[,c(grep("second-",colnames(dtest)))]


thirdtest <- dtest[,c(grep("third-",colnames(dtest)))]


#chint3 <- thirdtest$'chin the third-time measurement'
#chint3 <- as.data.frame(chint3)
#chint3$label <- "Chin"
#colnames(chint3)[1] = 'CL'
#chint3$Times <- "Third-time"

#foreheadt3 <- thirdtest$'forehead the third-time measurement'
#foreheadt3 <- as.data.frame(foreheadt3)
#foreheadt3$label <- "Forehead"
#colnames(foreheadt3)[1] = 'CL'
#foreheadt3$Times <- "Third-time"

#leftcheekt3 <- thirdtest$'leftcheek the third-time measurement'
#leftcheekt3 <- as.data.frame(leftcheekt3)
#leftcheekt3$label <- "Leftcheek"
#colnames(leftcheekt3)[1] = 'CL'
#leftcheekt3$Times <- "Third-time"

#rightcheekt3 <- thirdtest$'rightcheek the third-time measurement'
#rightcheekt3 <- as.data.frame(rightcheekt3)
#rightcheekt3$label <- "Rightcheek"
#colnames(rightcheekt3)[1] = 'CL'
#rightcheekt3$Times <- "Third-time"

#noset3 <- thirdtest$'nose the third-time measurement'
#noset3 <- as.data.frame(noset3)
#noset3$label <- "Nose"
#colnames(noset3)[1] = 'CL'
#noset3$Times <- "Third-time"






chint1 <- firsttest$'first-chin-CL'
chint1 <- as.data.frame(chint1)
chint1$label <- "Chin"
colnames(chint1)[1] = 'CL'
chint1$Times <- "First-time"




chint2 <- secondtest$'second-chin-CL'
chint2 <- as.data.frame(chint2)
chint2$label <- "Chin"
colnames(chint2)[1] = 'CL'
chint2$Times <- "Second-time"

chint <- rbind(chint1,chint2)



foreheadt1 <- firsttest$'first-foreheacd-CL'
foreheadt1 <- as.data.frame(foreheadt1)
foreheadt1$label <- "Forehead"
colnames(foreheadt1)[1] = 'CL'
foreheadt1$Times <- "First-time"


foreheadt2 <- secondtest$'second-foreheacd-CL'
foreheadt2 <- as.data.frame(foreheadt2)
foreheadt2$label <- "Forehead"
colnames(foreheadt2)[1] = 'CL'
foreheadt2$Times <- "Second-time"

foreheadt <- rbind(foreheadt1,foreheadt2)



leftcheekt1 <- firsttest$'first-leftcheek-CL'
leftcheekt1 <- as.data.frame(leftcheekt1)
leftcheekt1$label <- "Leftcheek"
colnames(leftcheekt1)[1] = 'CL'
leftcheekt1$Times <- "First-time"


leftcheekt2 <- secondtest$'second-leftcheek-CL'
leftcheekt2 <- as.data.frame(leftcheekt2)
leftcheekt2$label <- "Leftcheek"
colnames(leftcheekt2)[1] = 'CL'
leftcheekt2$Times <- "Second-time"

leftcheekt <- rbind(leftcheekt1,leftcheekt2)


rightcheekt1<- firsttest$'first-rightcheek-CL'
rightcheekt1 <- as.data.frame(rightcheekt1)
rightcheekt1$label <- "Rightcheek"
colnames(rightcheekt1)[1] = 'CL'
rightcheekt1$Times <- "First-time"

rightcheekt2 <- secondtest$'second-rightcheek-CL'
rightcheekt2 <- as.data.frame(rightcheekt2)
rightcheekt2$label <- "Rightcheek"
colnames(rightcheekt2)[1] = 'CL'
rightcheekt2$Times <- "Second-time"

rightcheekt <- rbind(rightcheekt1,rightcheekt2)



noset1 <- firsttest$'first-nose-CL'
noset1 <- as.data.frame(noset1)
noset1$label <- "Nose"
colnames(noset1)[1] = 'CL'
noset1$Times <- "First-time"

noset2 <- secondtest$'second-nose-CL'
noset2 <- as.data.frame(noset2)
noset2$label <- "Nose"
colnames(noset2)[1] = 'CL'
noset2$Times <- "Second-time"

noset <- rbind(noset1,noset2)








t1 <- rbind(chint,foreheadt)
t2 <- rbind(t1,leftcheekt)
t3 <- rbind(t2,rightcheekt)
t4 <- rbind(t3,noset)

#t4$label = as.factor(t4$label)
#levels(t4$label)=ordered(c("Chin","Forehead","Leftcheek","Rightcheek","Nose")) 





t5 = t4 %>% dplyr::group_by(Times,label) %>% dplyr::summarise(Mean = mean(CL, na.rm = T), 
                                                                             Sd = sd(CL, na.rm = T), 
                                                                             Se = sd(CL, na.rm = T) / sqrt(length(CL)),
                                                                             T1 = tryCatch(
                                                                               expr = {t.test(CL)$conf.int[1]},
                                                                               error = function(e) mean(Value)),  
                                                                             T2 = tryCatch(
                                                                               expr = {t.test(CL)$conf.int[2]},
                                                                               error = function(e) mean(CL)))

t5$label = factor(t5$label, levels=c("Chin","Forehead","Leftcheek","Rightcheek","Nose"))


#drop_na (t4,CL) 

#ew.plot.data = plot.data %>% dplyr::group_by(Time) %>% dplyr::summarise(Mean = mean(Value, na.rm = T), 
#                                                                       Sd = sd(Value, na.rm = T), 
  #                                                                      Se = sd(Value, na.rm = T) / sqrt(length(Value)),
##                                                                 T1 = tryCatch(
                                                                          #                                                                         expr = {t.test(Value)$conf.int[1]},
                                                                          #                                                                         error = function(e) mean(Value)),  
                                                                          #                                                                      T2 = tryCatch(
                                                                          #expr = {t.test(Value)$conf.int[2]},
#                                                                    error = function(e) mean(Value)))



dodge <- position_dodge2(width=0.5,padding = 0.1,reverse = FALSE)

p3 <- ggplot(data=t5,mapping=aes(x = label, y = Mean,fill=Times),width=0.5)+
  geom_bar(stat="identity",position=dodge,width=0.6)+
  geom_errorbar(mapping = aes(ymin=Mean-Se,ymax=Mean+ Se ), width = 0.6,position = dodge,size =0.3)+
  coord_cartesian(ylim=c(0,200))+
  scale_y_continuous(expand = c(0, 0))+#消除x轴与绘图区的间隙
  scale_fill_manual(values =c("#FC4E07","#00AFBB", "#E7B800","#4682B4" ))#颜色的十六进制代码，或直接用red、blue、green等也可

p3 <- p3+theme(text=element_text(size=10,  family="sans", face = "bold")#times new roma#sans/宋体#mono/雅黑  
               ,axis.line = element_line(linetype="solid",colour = "black")
               ,panel.grid.major=element_blank()
               ,panel.grid.minor=element_blank()
               ,panel.background = element_blank()
               ,axis.ticks.length.y = unit(.15, "cm"),
               legend.key = element_blank (),
               legend.position = "bottom",
)+
  labs(title ="                                  Sebum recovery rate", x="Testing points ", y="One-time CSL (a.u.)")




p3



  
  
  ggsave("一次SRR.tiff",device = "tiff",dpi = 300,path = "C:/Users/a/Desktop/数据分析/皮肤油脂/xlsx/恢复程度")
  
  
  
  
  
  
  
  
