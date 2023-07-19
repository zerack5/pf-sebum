
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

m<-"male"
f<-"female"

htest$sex <- gsub("男", "male", htest$sex, fixed = TRUE)
htest$sex <- gsub("女", "female", htest$sex, fixed = TRUE)




selectcolum <- c("second-forehead-CL","second_leftcheek_CL","second-rightcheek-CL","second-nose-CL","second-chin-CL","mean-CL")
df_test <- htest[,selectcolum]
colnames(df_test) <- c("Forehead","Left_cheek","Right_cheek","Nose","Chin","Mean")
cor_matrix <- cor(df_test,method = "spearman",use = "pairwise.complete.obs")

# 设置图片的尺寸（长宽相等）
pdf("cor/CL_spearman.pdf", width = 6, height = 6)
pheatmap::pheatmap(cor_matrix, cluster_rows = FALSE, cluster_cols = FALSE,
                   square = TRUE,display_numbers = TRUE, number_format = "%.2f", 
                   legend_width = "auto")

# 关闭绘图设备，保存 PDF
dev.off()
  
selectcolum <- c("foreheadSER","leftcheekSER","rightcheekSER","noseSER","chinSER","mean-SER")  
df_test <- htest[,selectcolum]
colnames(df_test) <- c("Forehead","Left_cheek","Right_cheek","Nose","Chin","Mean")
cor_matrix <- cor(df_test,method = "spearman",use = "pairwise.complete.obs")

# 设置图片的尺寸（长宽相等）
pdf("cor/SER_spearman.pdf", width = 6, height = 6)
pheatmap::pheatmap(cor_matrix,cluster_rows = FALSE, cluster_cols = FALSE,
                   square = TRUE,display_numbers = TRUE, number_format = "%.2f", 
                   legend_width = "auto")

# 关闭绘图设备，保存 PDF
dev.off()




