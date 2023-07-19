

setwd("C:/Users/a/Desktop/数据分析/皮肤油脂/xlsx")

library(ggplot2)
library(ggpubr)
library(dplyr) #加载dplyr包
library(ggpmisc) #加载ggpmisc包
library(readxl)

dtest <- read_xlsx(  "测量三次删减成第一次总.xlsx",
                     sheet = "Sheet1",
                     range = NULL,
                     col_names = TRUE,
                     col_types = NULL,
                     na = ""
                     
)

#ctest <- openxlsx::read.xlsx("测量三次删减成第一次总.xlsx", sheet="Sheet1", rowNames=T,na.strings = ".")

#figure1 chin

p <- ggscatter(dtest, x = "chin the third-time measurement", y = "chin the sum of the three measurements",
          combine = FALSE,
          color = "black",
          shape = 19,
          size =2,
          title ="                         Chin",
          xlab = "one-time CSL (a.u.)" ,#"??"
          ylab = "real CSL (a.u.)" ,#"??"
          add = "reg.line", conf.int = TRUE,    
          add.params = list(fill = "lightgray")
          
)+
  stat_cor(method = "spearman", 
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           r.accuracy = 0.001,
           p.accuracy = 0.001,

           label.x = -21, label.y = 600
           ,position = "identity")
p+theme(text=element_text(size=15,  family="sans", face = "bold")#times new roma#sans/宋体#mono/雅黑  
        ,axis.line = element_line(colour = "black")
        ,panel.grid.major=element_blank()
        ,panel.grid.minor=element_blank()
        ,panel.background = element_blank()
        ,axis.ticks.length.y = unit(.15, "cm"),
        
)


ggsave("Chin.tiff",device = "tiff",dpi = 300,path = "C:/Users/a/Desktop/数据分析/皮肤油脂/xlsx/一次测试CL与三次CL")


#figure1 

p <- ggscatter(dtest, x = "forehead the third-time measurement", y = "forehead the sum of the three measurements",
               combine = FALSE,
               color = "black",
               shape = 19,
               size =2,
               title ="                         Forehead",
               xlab = "one-time CSL (a.u.)" ,#"??"
               ylab = "real CSL (a.u.)" ,#"??"
               add = "reg.line", conf.int = TRUE,    
               add.params = list(fill = "lightgray")
               
)+
  stat_cor(method = "spearman", 
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           r.accuracy = 0.001,
           p.accuracy = 0.001,
           
           label.x = -11, label.y = 600
           ,position = "identity")
p+theme(text=element_text(size=15,  family="sans", face = "bold")#times new roma#sans/宋体#mono/雅黑  
        ,axis.line = element_line(colour = "black")
        ,panel.grid.major=element_blank()
        ,panel.grid.minor=element_blank()
        ,panel.background = element_blank()
        ,axis.ticks.length.y = unit(.15, "cm"),
        
)


ggsave("Forehead.tiff",device = "tiff",dpi = 300,path = "C:/Users/a/Desktop/数据分析/皮肤油脂/xlsx/一次测试CL与三次CL")



#figure1 


p <- ggscatter(dtest, x = "leftcheek the third-time measurement", y = "leftcheek the sum of the three measurements",
               combine = FALSE,
               color = "black",
               shape = 19,
               size =2,
               title ="                    Left cheek",
               xlab = "one-time CSL (a.u.)" ,#"??"
               ylab = "real CSL (a.u.)" ,#"??"
               add = "reg.line", conf.int = TRUE,    
               add.params = list(fill = "lightgray")
               
)+
  stat_cor(method = "spearman", 
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           r.accuracy = 0.001,
           p.accuracy = 0.001,
           
           label.x = -15, label.y = 700
           ,position = "identity")
p+theme(text=element_text(size=15,  family="sans", face = "bold")#times new roma#sans/宋体#mono/雅黑  
        ,axis.line = element_line(colour = "black")
        ,panel.grid.major=element_blank()
        ,panel.grid.minor=element_blank()
        ,panel.background = element_blank()
        ,axis.ticks.length.y = unit(.15, "cm"),
        
)


ggsave("Leftcheek.tiff",device = "tiff",dpi = 300,path = "C:/Users/a/Desktop/数据分析/皮肤油脂/xlsx/一次测试CL与三次CL")


#figure1 


p <- ggscatter(dtest, x = "rightcheek the third-time measurement", y = "rightcheek the sum of the three measurements",
               combine = FALSE,
               color = "black",
               shape = 19,
               size =2,
               title ="                    Right cheek",
               xlab = "one-time CSL (a.u.)" ,#"??"
               ylab = "real CSL (a.u.)" ,#"??"
               add = "reg.line", conf.int = TRUE,    
               add.params = list(fill = "lightgray")
               
)+
  stat_cor(method = "spearman", 
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           r.accuracy = 0.001,
           p.accuracy = 0.001,
           
           label.x = -11, label.y = 700
           ,position = "identity")
p+theme(text=element_text(size=15,  family="sans", face = "bold")#times new roma#sans/宋体#mono/雅黑  
        ,axis.line = element_line(colour = "black")
        ,panel.grid.major=element_blank()
        ,panel.grid.minor=element_blank()
        ,panel.background = element_blank()
        ,axis.ticks.length.y = unit(.15, "cm"),
        
)


ggsave("Rightcheek.tiff",device = "tiff",dpi = 300,path = "C:/Users/a/Desktop/数据分析/皮肤油脂/xlsx/一次测试CL与三次CL")



#figure1 nose


p <- ggscatter(dtest, x = "nose the third-time measurement", y = "nose the sum of the three measurements",
               combine = FALSE,
               color = "black",
               shape = 19,
               size =2,
               title ="                        Nose",
               xlab = "one-time CSL (a.u.)" ,#"??"
               ylab = "real CSL (a.u.)" ,#"??"
               add = "reg.line", conf.int = TRUE,    
               add.params = list(fill = "lightgray")
               
)+
  stat_cor(method = "spearman", 
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           r.accuracy = 0.001,
           p.accuracy = 0.001,
           
           label.x = -11, label.y = 900
           ,position = "identity")
p+theme(text=element_text(size=15,  family="sans", face = "bold")#times new roma#sans/宋体#mono/雅黑  
        ,axis.line = element_line(colour = "black")
        ,panel.grid.major=element_blank()
        ,panel.grid.minor=element_blank()
        ,panel.background = element_blank()
        ,axis.ticks.length.y = unit(.15, "cm"),
        
)


ggsave("Nose.tiff",device = "tiff",dpi = 300,path = "C:/Users/a/Desktop/数据分析/皮肤油脂/xlsx/一次测试CL与三次CL")



