



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



p <- ggscatter(dtest, x = "rightcheek_the_first_time_measurement", y = "rightcheek the sum of the three measurements",
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
           
           label.x = -15, label.y = 1100
           ,position = "identity")
p+theme(text=element_text(size=15,  family="sans", face = "bold")#times new roma#sans/宋体#mono/雅黑  
        ,axis.line = element_line(colour = "black")
        ,panel.grid.major=element_blank()
        ,panel.grid.minor=element_blank()
        ,panel.background = element_blank()
        ,axis.ticks.length.y = unit(.15, "cm"),
        
)


ggsave("rightcheek-CL-CL.tiff",device = "tiff",width = 5, height =5,dpi = 300,path = "C:/Users/a/Desktop/数据分析/皮肤油脂/xlsx/new_figure/figure2")






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

p <- ggscatter(dtest, x = "rightcheek first SER", y = "rightcheek real SER",
               combine = FALSE,
               color = "black",
               shape = 19,
               size =2,
               title ="                    Right cheek",
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
           label.x = -0.5, label.y = 6
           , position = "identity")
p+theme(text=element_text(size=15,  family="sans", face = "bold")#times new roma#sans/宋体#mono/雅黑  
        ,axis.line = element_line(colour = "black")
        ,panel.grid.major=element_blank()
        ,panel.grid.minor=element_blank()
        ,panel.background = element_blank()
        ,axis.ticks.length.y = unit(.15, "cm"),
        
)


ggsave("Rightcheek-ser-ser.tiff",device = "tiff",width = 5, height =5,dpi = 300,path = "C:/Users/a/Desktop/数据分析/皮肤油脂/xlsx/new_figure/figure2")


######################Nose



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



p <- ggscatter(dtest, x = "nose_the_first_time_measurement", y = "nose the sum of the three measurements",
               combine = FALSE,
               color = "black",
               shape = 19,
               size =2,
               title ="                       Nose",
               xlab = "one-time CSL (a.u.)" ,#"??"
               ylab = "real CSL (a.u.)" ,#"??"
               add = "reg.line", conf.int = TRUE,    
               add.params = list(fill = "lightgray")
               
)+
  stat_cor(method = "spearman", 
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           r.accuracy = 0.001,
           p.accuracy = 0.001,
           
           label.x = -15, label.y = 1450
           ,position = "identity")
p+theme(text=element_text(size=15,  family="sans", face = "bold")#times new roma#sans/宋体#mono/雅黑  
        ,axis.line = element_line(colour = "black")
        ,panel.grid.major=element_blank()
        ,panel.grid.minor=element_blank()
        ,panel.background = element_blank()
        ,axis.ticks.length.y = unit(.15, "cm"),
        
)


ggsave("nose-CL-CL.tiff",device = "tiff",width = 5, height =5,dpi = 300,path = "C:/Users/a/Desktop/数据分析/皮肤油脂/xlsx/new_figure/figure2")






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

p <- ggscatter(dtest, x = "nose first SER", y = "nose real SER",
               combine = FALSE,
               color = "black",
               shape = 19,
               size =2,
               title ="                      Nose",
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
           label.x = -0.5, label.y = 12
           , position = "identity")
p+theme(text=element_text(size=15,  family="sans", face = "bold")#times new roma#sans/宋体#mono/雅黑  
        ,axis.line = element_line(colour = "black")
        ,panel.grid.major=element_blank()
        ,panel.grid.minor=element_blank()
        ,panel.background = element_blank()
        ,axis.ticks.length.y = unit(.15, "cm"),
        
)


ggsave("Nose-ser-ser.tiff",device = "tiff",width = 5, height =5,dpi = 300,path = "C:/Users/a/Desktop/数据分析/皮肤油脂/xlsx/new_figure/figure2")

##########chin



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



p <- ggscatter(dtest, x = "chin_the_first_time_measurement", y = "chin the sum of the three measurements",
               combine = FALSE,
               color = "black",
               shape = 19,
               size =2,
               title ="                    Chin",
               xlab = "one-time CSL (a.u.)" ,#"??"
               ylab = "real CSL (a.u.)" ,#"??"
               add = "reg.line", conf.int = TRUE,    
               add.params = list(fill = "lightgray")
               
)+
  stat_cor(method = "spearman", 
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           r.accuracy = 0.001,
           p.accuracy = 0.001,
           
           label.x = -15, label.y = 800
           ,position = "identity")
p+theme(text=element_text(size=15,  family="sans", face = "bold")#times new roma#sans/宋体#mono/雅黑  
        ,axis.line = element_line(colour = "black")
        ,panel.grid.major=element_blank()
        ,panel.grid.minor=element_blank()
        ,panel.background = element_blank()
        ,axis.ticks.length.y = unit(.15, "cm"),
        
)


ggsave("chin-CL-CL.tiff",device = "tiff",width = 5, height =5,dpi = 300,path = "C:/Users/a/Desktop/数据分析/皮肤油脂/xlsx/new_figure/figure2")






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

p <- ggscatter(dtest, x = "chin first SER", y = "chin real SER",
               combine = FALSE,
               color = "black",
               shape = 19,
               size =2,
               title ="                    Chin",
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
           label.x = -0.5, label.y = 5.5
           , position = "identity")
p+theme(text=element_text(size=15,  family="sans", face = "bold")#times new roma#sans/宋体#mono/雅黑  
        ,axis.line = element_line(colour = "black")
        ,panel.grid.major=element_blank()
        ,panel.grid.minor=element_blank()
        ,panel.background = element_blank()
        ,axis.ticks.length.y = unit(.15, "cm"),
        
)


ggsave("Chin-ser-ser.tiff",device = "tiff",width = 5, height =5,dpi = 300,path = "C:/Users/a/Desktop/数据分析/皮肤油脂/xlsx/new_figure/figure2")


#########forehead




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



p <- ggscatter(dtest, x = "forehead_the_first_time_measurement", y = "forehead the sum of the three measurements",
               combine = FALSE,
               color = "black",
               shape = 19,
               size =2,
               title ="                    Forehead",
               xlab = "one-time CSL (a.u.)" ,#"??"
               ylab = "real CSL (a.u.)" ,#"??"
               add = "reg.line", conf.int = TRUE,    
               add.params = list(fill = "lightgray")
               
)+
  stat_cor(method = "spearman", 
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           r.accuracy = 0.001,
           p.accuracy = 0.001,
           
           label.x = -15, label.y = 900
           ,position = "identity")
p+theme(text=element_text(size=15,  family="sans", face = "bold")#times new roma#sans/宋体#mono/雅黑  
        ,axis.line = element_line(colour = "black")
        ,panel.grid.major=element_blank()
        ,panel.grid.minor=element_blank()
        ,panel.background = element_blank()
        ,axis.ticks.length.y = unit(.15, "cm"),
        
)


ggsave("forehead-CL-CL.tiff",device = "tiff",width = 5, height =5,dpi = 300,path = "C:/Users/a/Desktop/数据分析/皮肤油脂/xlsx/new_figure/figure2")






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

p <- ggscatter(dtest, x = "forehead first SER", y = "forehead real SER",
               combine = FALSE,
               color = "black",
               shape = 19,
               size =2,
               title ="                    Forehead",
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
           label.x = -0.5, label.y = 8
           , position = "identity")
p+theme(text=element_text(size=15,  family="sans", face = "bold")#times new roma#sans/宋体#mono/雅黑  
        ,axis.line = element_line(colour = "black")
        ,panel.grid.major=element_blank()
        ,panel.grid.minor=element_blank()
        ,panel.background = element_blank()
        ,axis.ticks.length.y = unit(.15, "cm"),
        
)


ggsave("Forehead-ser-ser.tiff",device = "tiff",width = 5, height =5,dpi = 300,path = "C:/Users/a/Desktop/数据分析/皮肤油脂/xlsx/new_figure/figure2")










