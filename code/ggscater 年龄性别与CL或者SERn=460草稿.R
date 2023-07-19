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

ftest <- merge(dtest,etest,by = "编号")


#ctest <- openxlsx::read.xlsx("测量三次删减成第一次总.xlsx", sheet="Sheet1", rowNames=T,na.strings = ".")
#Figure1


ggscatter(ftest, x = "age", y = "leftcheekCL",
          combine = FALSE,
          color = "black",
          shape = 19,
          size =2,
          title ="age",
          #xlab = FALSE ,#"??"
          #ylab = FALSE ,#"??"
          #add = "reg.line", conf.int = TRUE,    
          #add.params = list(fill = "lightgray"),
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
           label.x = 15, label.y = 6
           , position = "identity")

#figure


ggscatter(dtest, x = "rightcheek the sum of the three measurements", y = "rightcheek real SER",
          combine = FALSE,
          color = "black",
          shape = 19,
          size =2,
          title ="rightcheek",
          #xlab = FALSE ,#"??"
          #ylab = FALSE ,#"??"
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
           label.x = 5, label.y = 6
           , position = "identity")

#figureleftcheek

ggscatter(dtest, x = "leftcheek the sum of the three measurements", y = "leftcheek real SER",
          combine = FALSE,
          color = "black",
          shape = 19,
          size =2,
          title ="leftcheek",
          #xlab = FALSE ,#"??"
          #ylab = FALSE ,#"??"
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
           label.x = 8, label.y = 6
           , position = "identity")

#figure nose

ggscatter(dtest, x = "nose the sum of the three measurements", y = "nose real SER",
          combine = FALSE,
          color = "black",
          shape = 19,
          size =2,
          title ="nose",
          #xlab = FALSE ,#"??"
          #ylab = FALSE ,#"??"
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
           label.x = 8, label.y = 15
           , position = "identity")



#figure nose

ggscatter(dtest, x = "forehead the sum of the three measurements", y = "forehead real SER",
          combine = FALSE,
          color = "black",
          shape = 19,
          size =2,
          title ="forehead",
          #xlab = FALSE ,#"??"
          #ylab = FALSE ,#"??"
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
           label.x = 8, label.y = 10
           , position = "identity")

Footer
© 2022 GitHub, Inc.
Footer navigation
Terms
Privacy
Sec