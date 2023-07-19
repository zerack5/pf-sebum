

setwd("C:/Users/a/Desktop/数据分析/皮肤油脂/xlsx")

library(ggplot2)
library(ggpubr)
library(dplyr) #加载dplyr包
library(ggpmisc) #加载ggpmisc包
library(readxl)

dtest <- read_xlsx(  "C:/Users/a/Desktop/数据分析/皮肤油脂/xlsx/new_figure/CLSER仪器数据.xlsx",
                     sheet = "测试三次仪器数据",
                     range = NULL,
                     col_names = TRUE,
                     col_types = NULL,
                     na = ""
                     
)

#ctest <- openxlsx::read.xlsx("测量三次删减成第一次总.xlsx", sheet="Sheet1", rowNames=T,na.strings = ".")
#Figure1



#figure除脂前leftcheek

p <- ggscatter(dtest, x = "first_leftcheek_first_time_measurement", y = "leftcheek_real_SER",
               combine = FALSE,
               color = "black",
               shape = 19,
               size =2,
               title =" Left cheek (Before Sebum Removal)",
               xlab = "one-time CSL" ,#"??"
               ylab = "real SER" ,#"??"
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
           label.x = -10, label.y = 8
           , position = "identity")
p+theme(text=element_text(size=15,  family="sans", face = "bold")#times new roma#sans/宋体#mono/雅黑  
        ,axis.line = element_line(colour = "black")
        ,panel.grid.major=element_blank()
        ,panel.grid.minor=element_blank()
        ,panel.background = element_blank()
        ,axis.ticks.length.y = unit(.15, "cm"),
        
)


ggsave("Before sebum removal Leftctheek.tiff",device = "tiff",width = 5,height =5,dpi = 300,path = "C:/Users/a/Desktop/数据分析/皮肤油脂/xlsx/new_figure/figure3")


######除脂后
p <- ggscatter(dtest, x = "second_leftcheek_first_time_measurement", y = "leftcheek_real_SER",
               combine = FALSE,
               color = "black",
               shape = 19,
               size =2,
               title =" Left cheek (After Sebum Removal)",
               xlab = "one-time CSL" ,#"??"
               ylab = "real SER" ,#"??"
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
           label.x = -10, label.y = 6.5
           , position = "identity")
p+theme(text=element_text(size=15,  family="sans", face = "bold")#times new roma#sans/宋体#mono/雅黑  
        ,axis.line = element_line(colour = "black")
        ,panel.grid.major=element_blank()
        ,panel.grid.minor=element_blank()
        ,panel.background = element_blank()
        ,axis.ticks.length.y = unit(.15, "cm"),
        
)


ggsave("After sebum removal Leftctheek.tiff",device = "tiff",width = 5,height =5,dpi = 300,path = "C:/Users/a/Desktop/数据分析/皮肤油脂/xlsx/new_figure/figure3")





