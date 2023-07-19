
setwd("C:/Users/a/Desktop/数据分析/皮肤油脂/xlsx")

library(ggplot2)
library(ggpubr)
library(dplyr) #加载dplyr包
library(ggpmisc) #加载ggpmisc包
library(readxl)
library(ggsignif)

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

m2test <- mtest[mtest$skintype==1,]

wtest <- htest[htest$sex=="female",]

w1test <- wtest[wtest$skintype==1,]

w2test <- wtest[wtest$skintype==2,]

twotest <- htest[htest$skintype==2,]
onetest <- htest[htest$skintype==1,]


#ctest <- openxlsx::read.xlsx("测量三次删减成第一次总.xlsx", sheet="Sheet1", rowNames=T,na.strings = ".")


cols <- c("male" = "green", "female" = "red")
sha <- c("male" = "19", "female" = "17")
shapes <- c("male" = 6, "female" = 4)

htest$sex <- factor(htest$sex )


boxtest <- dtest[,3:7]





chint <- boxtest$chinCL
chint <- as.data.frame(chint)
chint$label <- "Chin"
colnames(chint)[1] = 'CL'


foreheadt <- boxtest$foreheadCL
foreheadt <- as.data.frame(foreheadt)
foreheadt$label <- "Forehead"
colnames(foreheadt)[1] = 'CL'

leftcheekt <- boxtest$leftcheekCL
leftcheekt <- as.data.frame(leftcheekt)
leftcheekt$label <- "Leftcheek"
colnames(leftcheekt)[1] = 'CL'

rightcheekt<- boxtest$rightcheekCL
rightcheekt <- as.data.frame(rightcheekt)
rightcheekt$label <- "Rightcheek"
colnames(rightcheekt)[1] = 'CL'

noset <- boxtest$noseCL
noset <- as.data.frame(noset)
noset$label <- "Nose"
colnames(noset)[1] = 'CL'


t1 <- rbind(chint,foreheadt)
t2 <- rbind(t1,leftcheekt)
t3 <- rbind(t2,rightcheekt)
t4 <- rbind(t3,noset)

#t4$label = as.factor(t4$label)
#levels(t4$label)=ordered(c("chin","forehead","leftcheek","rightcheek","nose")) 


t4$label = factor(t4$label, levels=c("Chin","Forehead","Leftcheek","Rightcheek","Nose"))



p2 <- ggplot(data = t4,aes(x=label,y=CL))+
  geom_boxplot(outlier.colour="brown", outlier.shape=1,notch = TRUE,show.legend=TRUE,coef=2,
               outlier.size=1, linetype=1, width=0.5, position = "dodge2")
p2
#p2 + geom_dotplot(binaxis='y', stackdir='center', dotsize=0.5)
# Box plot with jittered points
# 0.2 : degree of jitter in x direction
#p2 + geom_jitter(shape=16, position=position_jitter(0.2))+

#stat_summary(aes(y=CL),fun = "mean", geom = "point", shape = 23, size=4)

#p2+scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9","#999999","Red","green","yellow"))+
p2 <- p2+theme(text=element_text(size=13,  family="sans", face = "bold")#times new roma#sans/宋体#mono/雅黑  
         ,axis.line = element_line(linetype="solid",colour = "black")
         ,panel.grid.major=element_blank()
         ,panel.grid.minor=element_blank()
         ,panel.background = element_blank()
         ,axis.ticks.length.y = unit(.15, "cm"),
         legend.key = element_blank (),
         legend.position = "bottom",
)+scale_fill_manual(values =c("#FC4E07","#00AFBB", "#E7B800","#4682B4" ))+
  labs(title ="  The sebum level of the five testing points", x="Testing points ", y="One-time CSL (a.u.)")


p2 <- p2 + theme(axis.line = element_line(linetype = "solid"),
           axis.title = element_text(face = "bold"),
           axis.text = element_text(face = "bold"),
           panel.background = element_rect(fill = NA))+
  labs(title ="  The sebum level of the five testing points", x="Testing points ", y="One-time CSL (a.u.)")+

 #geom_signif(comparisons = list(c("Chin","Forehead")), 
            # map_signif_level = TRUE, test = pairwise.wilcox.test, y_position = c(500,500), 
            # tip_length = c(0.05,0.05))+
 
 
 geom_signif(comparisons = list(c("Leftcheek","Rightcheek")), 
             map_signif_level = TRUE, test = pairwise.wilcox.test, y_position = c(500,500), 
             tip_length = c(0.05,0.05))
 
 
 
p2

ggsave("CL5.tiff",device = "tiff",dpi = 300,path = "C:/Users/a/Desktop/数据分析/皮肤油脂/xlsx/五个部分")






aov5 <- aov(formula = CL ~ label, data = t4)
summary(aov5)
aov6 <- kruskal.test(formula=CL ~ label, data= t4)
aov6

with(data=t4,
     pairwise.wilcox.test(x=CL,g=label,p.adjust.method = "BH",paired=TRUE)``
)


with(data=t4,
     pairwise.t.test(x=CL,g=label,p.adjust.method = "none",paired=TRUE)
)


shapiro.test(t4[c(grep("Forehead",t4$label)),]$CL)
shapiro.test(t4[c(grep("Chin",t4$label)),]$CL)

