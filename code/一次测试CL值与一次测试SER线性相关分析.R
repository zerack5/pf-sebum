setwd("C:/Users/a/Desktop/数据分析/皮肤油脂/xlsx")

library(ggplot2)
library(ggpubr)
library(dplyr) #加载dplyr包
library(ggpmisc) #加载ggpmisc包
#oilcl <- read.csv('测量三次.csv', sep = ',', header = TRUE , encoding = "UTF-8")
btest <- openxlsx::read.xlsx("测量三次.xlsx", sheet="总", rowNames=T)

atest <- openxlsx::read.xlsx("测量三次.xlsx", sheet="油脂恢复水平-测三次", rowNames=F)

dtest <- read_xlsx( "一次测量CL与SER.xlsx",
                     sheet = "Sheet1",
                     range = NULL,
                     col_names = TRUE,
                     col_types = NULL,
                     na = ""
                     
)


#ctest <- openxlsx::read.xlsx("测量三次删减成第一次总.xlsx", sheet="Sheet1", rowNames=T,na.strings = ".")

ggscatter(dtest, x = "下颌CL", y = "下颌SER",
          combine = FALSE,
          color = "black",
          shape = 19,
          size =2,
          #title ="chin",
          #xlab = FALSE ,#"??"
          #ylab = FALSE ,#"??"
          add = "reg.line", conf.int = TRUE,    
          add.params = list(fill = "lightgray")
          #cor.method = "pearson",
          #cor.coeff.args = list(method = "pearson", label.x.npc = "right", label.y.npc = "top",
          #cor.coef.size = 4
          
)+
  stat_cor(method = "spearman", 
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           r.accuracy = 0.0001,
           p.accuracy = 0.001,
           label.x.npc = 0.5,
           label.y.npc = 0.5,
           label.x = 110, label.y = 10
           , position = "identity")
