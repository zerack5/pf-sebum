

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


p <- ggscatter(dtest, x = "chin the first-time measurement", y = "chin real SER",
          combine = FALSE,
          color = "black",
          shape = 19,
          size =2,
          title ="                           Chin",
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
           label.x = 0, label.y = 5.5
           , position = "identity")
p+theme(text=element_text(size=15,  family="sans", face = "bold")#times new roma#sans/宋体#mono/雅黑  
        ,axis.line = element_line(colour = "black")
        ,panel.grid.major=element_blank()
        ,panel.grid.minor=element_blank()
        ,panel.background = element_blank()
        ,axis.ticks.length.y = unit(.15, "cm"),
        
)
ggsave("Chin.tiff",device = "tiff",dpi = 300,path = "C:/Users/a/Desktop/数据分析/皮肤油脂/xlsx/一次CL与真实SER")


#figure


p <- ggscatter(dtest, x = "rightcheek the first-time measurement", y = "rightcheek real SER",
          combine = FALSE,
          color = "black",
          shape = 19,
          size =2,
          title ="                 Right cheek",
          xlab = "one-time CSL (a.u.)" ,#"??"
          ylab = "real SER (a.u./min)" ,#"??"
          add = "reg.line", conf.int = TRUE,    
          add.params = list(fill = "lightgray"),
          #conf.int = TRUE,# Add confidence interval
          #cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
          #cor.coeff.args = list(method = "pearson", label.x = 3, label.sep = "\n"),
          #cor.coef.size = 5
)+stat_cor(method = "spearman", 
           aes(label = paste(..rr.label.., ..p.label.., sep = "~` ,`~")),
           r.accuracy = 0.001,
           p.accuracy = 0.001,
           #label.x.npc = 'left',
           #label.y.npc = 'top',
           label.x = -8, label.y = 6
           , position = "identity")
p+theme(text=element_text(size=15,  family="sans", face = "bold")#times new roma#sans/宋体#mono/雅黑  
        ,axis.line = element_line(colour = "black")
        ,panel.grid.major=element_blank()
        ,panel.grid.minor=element_blank()
        ,panel.background = element_blank()
        ,axis.ticks.length.y = unit(.15, "cm"),
        
)

ggsave("Rightcheek.tiff",device = "tiff",dpi = 300,path = "C:/Users/a/Desktop/数据分析/皮肤油脂/xlsx/一次CL与真实SER")





#figureleftcheek

p <- ggscatter(dtest, x = "leftcheek the first-time measurement", y = "leftcheek real SER",
          combine = FALSE,
          color = "black",
          shape = 19,
          size =2,
          title ="                    Left cheek",
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
           label.x = -10, label.y = 6
           , position = "identity")
p+theme(text=element_text(size=15,  family="sans", face = "bold")#times new roma#sans/宋体#mono/雅黑  
        ,axis.line = element_line(colour = "black")
        ,panel.grid.major=element_blank()
        ,panel.grid.minor=element_blank()
        ,panel.background = element_blank()
        ,axis.ticks.length.y = unit(.15, "cm"),
        
)


ggsave("Leftctheek.tiff",device = "tiff",dpi = 300,path = "C:/Users/a/Desktop/数据分析/皮肤油脂/xlsx/一次CL与真实SER")


#figure nose

p <- ggscatter(dtest, x = "nose the first-time measurement", y = "nose real SER",
          combine = FALSE,
          color = "black",
          shape = 19,
          size =2,
          title ="                    Nose",
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
           r.accuracy = 0.0001,
           p.accuracy = 0.001,
           #label.x.npc = 'left',
           #label.y.npc = 'top',
           label.x = 8, label.y = 11
           , position = "identity")
p+theme(text=element_text(size=15,  family="sans", face = "bold")#times new roma#sans/宋体#mono/雅黑  
        ,axis.line = element_line(colour = "black")
        ,panel.grid.major=element_blank()
        ,panel.grid.minor=element_blank()
        ,panel.background = element_blank()
        ,axis.ticks.length.y = unit(.15, "cm"),
        
)


ggsave("Nose.tiff",device = "tiff",dpi = 300,path = "C:/Users/a/Desktop/数据分析/皮肤油脂/xlsx/一次CL与真实SER")



#figure nose

p <- ggscatter(dtest, x = "forehead the first-time measurement", y = "forehead real SER",
          combine = FALSE,
          color = "black",
          shape = 19,
          size =2,
          title ="                     Forehead",
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
           label.x = 5, label.y = 8.5
           , position = "identity")


p+theme(text=element_text(size=15,  family="sans", face = "bold")#times new roma#sans/宋体#mono/雅黑  
        ,axis.line = element_line(colour = "black")
        ,panel.grid.major=element_blank()
        ,panel.grid.minor=element_blank()
        ,panel.background = element_blank()
        ,axis.ticks.length.y = unit(.15, "cm"),
        
)


ggsave("Forehead.tiff",device = "tiff",dpi = 300,path = "C:/Users/a/Desktop/数据分析/皮肤油脂/xlsx/一次CL与真实SER")



