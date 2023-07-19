

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




mtest <- htest[c(grep("男",htest$sex)),]

m1test <- mtest[c(grep("1",htest$skintype)),]

m2test <- mtest[c(grep("2",htest$skintype)),]

wtest <- htest[c(grep("女",htest$sex)),]

w1test <- wtest[c(grep("1",htest$skintype)),]

w2test <- wtest[c(grep("2",htest$skintype)),]


#ctest <- openxlsx::read.xlsx("测量三次删减成第一次总.xlsx", sheet="Sheet1", rowNames=T,na.strings = ".")


p <- ggplot(data = m1test,aes(x=age,y=foreheadSER))+
  geom_point(pch=16, color="blue",size=1)+
  geom_smooth(method="loess",formula = y ~ x ,color="red",linetype=2,level=.95)+  
  
  labs(title ="m1", x="age", y="SER")

p+theme(text=element_text(size=20,  family="mono", face = "bold")#times new roma#sans/宋体#mono/雅黑  
        ,axis.line = element_line(colour = "black")
        ,panel.grid.major=element_blank()
        ,panel.grid.minor=element_blank()
        ,panel.background = element_blank()
        ,axis.ticks.length.y = unit(.15, "cm"),
        
)













#Figure1  m1test


p <- ggplot(data = m2test,aes(x=age,y=foreheadSER))+
  geom_point(pch=16, color="blue",size=1)+
  geom_smooth(method="loess",formula = y ~ x ,color="red",linetype=2,level=.95)+       
  labs(title ="m2", x="age", y="SER")

p+theme(text=element_text(size=20,  family="mono", face = "bold")#times new roma#sans/宋体#mono/雅黑  
        ,axis.line = element_line(colour = "black")
        ,panel.grid.major=element_blank()
        ,panel.grid.minor=element_blank()
        ,panel.background = element_blank()
        ,axis.ticks.length.y = unit(.15, "cm"),
        
)




#figure



p <- ggplot(data = w1test,aes(x=age,y=foreheadSER))+
  geom_point(pch=16, color="blue",size=1)+
  geom_smooth(method="loess",formula = y ~ x ,color="red",linetype=2,level=.95)+       
  labs(title ="w1", x="age", y="SER")

p+theme(text=element_text(size=20,  family="mono", face = "bold")#times new roma#sans/宋体#mono/雅黑  
        ,axis.line = element_line(colour = "black")
        ,panel.grid.major=element_blank()
        ,panel.grid.minor=element_blank()
        ,panel.background = element_blank()
        ,axis.ticks.length.y = unit(.15, "cm"),
        
)

#figureleftcheek


p <- ggplot(data = w2test,aes(x=age,y=foreheadSER))+
  geom_point(pch=16, color="blue",size=1)+
  geom_smooth(method="loess",formula = y ~ x ,color="red",linetype=2,level=.95)+       
  labs(title ="w2", x="age", y="SER")

p+theme(text=element_text(size=20,  family="mono", face = "bold")#times new roma#sans/宋体#mono/雅黑  
        ,axis.line = element_line(colour = "black")
        ,panel.grid.major=element_blank()
        ,panel.grid.minor=element_blank()
        ,panel.background = element_blank()
        ,axis.ticks.length.y = unit(.15, "cm"),
        
)

#figure nose

p <- ggplot(data = htest,aes(x=age,y=foreheadSER))+
  geom_point(pch=16, color="blue",size=1)+
  geom_smooth(method="loess",formula = y ~ x ,color="red",linetype=2,level=.95)+       
  labs(title ="总", x="age", y="SER")

p+theme(text=element_text(size=20,  family="mono", face = "bold")#times new roma#sans/宋体#mono/雅黑  
        ,axis.line = element_line(colour = "black")
        ,panel.grid.major=element_blank()
        ,panel.grid.minor=element_blank()
        ,panel.background = element_blank()
        ,axis.ticks.length.y = unit(.15, "cm"),
        
)

#figrem


p <- ggplot(data = mtest,aes(x=age,y=foreheadSER))+
  geom_point(pch=16, color="blue",size=1)+
  geom_smooth(method="loess",formula = y ~ x ,color="red",linetype=2,level=.95)+       
  labs(title ="m", x="age", y="SER")

p+theme(text=element_text(size=20,  family="mono", face = "bold")#times new roma#sans/宋体#mono/雅黑  
        ,axis.line = element_line(colour = "black")
        ,panel.grid.major=element_blank()
        ,panel.grid.minor=element_blank()
        ,panel.background = element_blank()
        ,axis.ticks.length.y = unit(.15, "cm"),
        
)



#figrew

p <- ggplot(data = wtest,aes(x=age,y=foreheadSER))+
  geom_point(pch=16, color="blue",size=1)+
  geom_smooth(method="loess",formula = y ~ x ,color="red",linetype=2,level=.95)+       
  labs(title ="w", x="age", y="SER")

p+theme(text=element_text(size=20,  family="mono", face = "bold")#times new roma#sans/宋体#mono/雅黑  
        ,axis.line = element_line(colour = "black")
        ,panel.grid.major=element_blank()
        ,panel.grid.minor=element_blank()
        ,panel.background = element_blank()
        ,axis.ticks.length.y = unit(.15, "cm"),
        
)


