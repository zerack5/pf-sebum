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

ggscatter(dtest, x = "chin the third-time measurement", y = "chin the sum of the three measurements",
          combine = FALSE,
          color = "black",
          shape = 19,
          size =2,
          title ="chin",
          #xlab = FALSE ,#"??"
          #ylab = FALSE ,#"??"
          add = "reg.line", conf.int = TRUE,    
          add.params = list(fill = "lightgray")
          
)+
  stat_cor(method = "pearson", 
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           r.accuracy = 0.001,
           p.accuracy = 0.001,
           label.x.npc = 0.5,
           label.y.npc = 0.5,
           label.x = 110, label.y = 110
           , position = "identity")


