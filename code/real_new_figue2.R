



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



p <- ggscatter(dtest, x = "leftcheek_the_first_time_measurement", y = "leftcheek the sum of the three measurements",
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
           
           label.x = -15, label.y = 950
           ,position = "identity")
p+theme(text=element_text(size=15,  family="sans", face = "bold")#times new roma#sans/宋体#mono/雅黑  
        ,axis.line = element_line(colour = "black")
        ,panel.grid.major=element_blank()
        ,panel.grid.minor=element_blank()
        ,panel.background = element_blank()
        ,axis.ticks.length.y = unit(.15, "cm"),
        
)


ggsave("Leftcheek-CL-CL.tiff",device = "tiff",width = 5, height =5,dpi = 300,path = "C:/Users/a/Desktop/数据分析/皮肤油脂/xlsx/new_figure/figure2")






setwd("C:/Users/a/Desktop/数据分析/皮肤油脂/xlsx")

library(ggplot2)
library(ggpubr)
library(dplyr) #加载dplyr包
library(ggpmisc) #加载ggpmisc包
library(readxl)

dtest <- read_xlsx(  "CL与SER.xlsx",
                     sheet = "调整1后",
                     range = NULL,
                     col_names = TRUE,
                     col_types = NULL,
                     na = ""
                     
)

#ctest <- openxlsx::read.xlsx("测量三次删减成第一次总.xlsx", sheet="Sheet1", rowNames=T,na.strings = ".")
#Figure1




#figureleftcheek

p <- ggscatter(dtest, x = "leftcheek first SER", y = "leftcheek real SER",
               combine = FALSE,
               color = "black",
               shape = 19,
               size =2,
               title ="                    Left cheek",
               xlab = "one-time SER (a.u./min)" ,#"??"
               ylab = "real SER (a.u./min)" ,#"??"
               add = "reg.line", conf.int = TRUE,    
               add.params = list(fill = "lightgray"),
               #conf.int = TRUE,# Add confidence interval
               #cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
               #cor.coeff.args = list(method = "pearson", label.x = 3, label.sep = "\n"),
               #cor.coef.size = 5
)+stat_cor(method = "spearman", 
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           r.accuracy = 0.001,
           p.accuracy = 0.001,
           #label.x.npc = 'left',
           #label.y.npc = 'top',
           label.x = -0.5, label.y = 5
           , position = "identity")
p+theme(text=element_text(size=15,  family="sans", face = "bold")#times new roma#sans/宋体#mono/雅黑  
        ,axis.line = element_line(colour = "black")
        ,panel.grid.major=element_blank()
        ,panel.grid.minor=element_blank()
        ,panel.background = element_blank()
        ,axis.ticks.length.y = unit(.15, "cm"),
        
)


ggsave("Leftctheek-ser-ser.tiff",device = "tiff",width = 5, height =5,dpi = 300,path = "C:/Users/a/Desktop/数据分析/皮肤油脂/xlsx/new_figure/figure2")
