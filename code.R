  library(ggplot2)
  library(reshape2)
  library(corrplot)

  # 创建数据矩阵
  data_matrix <- matrix(
    c(
      0.007, 0.953, -0.068, -0.002, 0.013,
      0.018, -0.171, 0.815, -0.058, -0.102,
      0.942, -0.022, -0.079, -0.097, 0.016,
      0.078, -0.002, -0.004, 0.855, -0.078,
      -0.250, -0.006, 0.008, 0.549, 0.068,
      0.447, 0.022, -0.734, -0.019, 0.007,
      0.017, -0.014, -0.039, -0.018, 0.964,
      0.881, -0.023, -0.084, -0.067, 0.006,
      0.937, -0.056, 0.015, -0.083, 0.014,
      0.383, 0.124, 0.461, 0.131, 0.261,
      -0.069, 0.943, -0.065, -0.007, -0.013
    ),
    ncol = 11, byrow = FALSE
  )
  
  # 保留两位小数
  data_matrix <- round(data_matrix, 2)
  colnames(data_matrix) <- paste0("λ", 1:NCOL(data_matrix))
  rownames(data_matrix) <- paste0("FAC", 1:NROW(data_matrix))
  corrplot(data_matrix, method="ellipse", addCoef.col="cornsilk3",number.cex=0.75,tl.srt=0,is.corr=FALSE, tl.offset = 1,mar=c(0.5,1,0,1),tl.col="gray1",tl.cex=0.8)
  
  
  
library(readxl);library(ggplot2);library(reshape2);library(dplyr)
# 读取Excel文件
data4_1 <- read_excel("C:\\Users\\daigu\\Desktop\\1701_match.xlsx")

# 找到Novak Djokovic的高光时刻
djokovic_highlights <- data4_1[data4_1$highlight_1 == 1, ]
# 找到Carlos Alcaraz的高光时刻
alcaraz_highlights <- data4_1[data4_1$highlight_2 == 1, ]

# 使用ggplot2绘制折线图，并在折线下方填充颜色
p1 <-ggplot(data4_1, aes(x = point_no)) + 
  geom_line(aes(y = FAC1_1, colour = "Novak Djokovic"), size = 1) + 
  geom_ribbon(aes(ymin = 0, ymax = FAC1_1, fill = "Novak Djokovic"), alpha = 0.3) +
  geom_line(aes(y = FAC1_2, colour = "Carlos Alcaraz"), size = 1) +
  geom_ribbon(aes(ymin = 0, ymax = FAC1_2, fill = "Carlos Alcaraz"), alpha = 0.3) +
  geom_point(data = djokovic_highlights, aes(y = FAC1_1), colour = "cyan4", size = 4, shape = 16) + # 标记Novak Djokovic的高光时刻
  geom_point(data = alcaraz_highlights, aes(y = FAC1_2), colour = "brown4", size = 5, shape = 18) + # 标记Carlos Alcaraz的高光时刻
  scale_colour_manual(values = c("Novak Djokovic" = "cyan4", "Carlos Alcaraz" = "brown4"),
                      name = "Player",
                      labels = c("Novak Djokovic", "Carlos Alcaraz")) +
  scale_fill_manual(values = c("Novak Djokovic" = "cyan4", "Carlos Alcaraz" = "brown4"),
                    name = "Player",
                    labels = c("Novak Djokovic", "Carlos Alcaraz")) +

  labs(x = "Point Number", y = "FAC1 Score") +
  theme_minimal() +
  guides(colour = guide_legend(override.aes = list(size = 2))) # 设置图例中线的粗细

# 使用ggplot2绘制折线图，并在折线下方填充颜色
p2<-ggplot(data4_1, aes(x = point_no)) + 
  geom_line(aes(y = FAC2_1, colour = "Novak Djokovic"), size = 1) + 
  geom_ribbon(aes(ymin = 0, ymax = FAC2_1, fill = "Novak Djokovic"), alpha = 0.3) +
  geom_line(aes(y = FAC2_2, colour = "Carlos Alcaraz"), size = 1) +
  geom_ribbon(aes(ymin = 0, ymax = FAC2_2, fill = "Carlos Alcaraz"), alpha = 0.3) +
  geom_point(data = djokovic_highlights, aes(y = FAC2_1), colour = "cyan4", size = 4, shape = 16) + # 标记Novak Djokovic的高光时刻
  geom_point(data = alcaraz_highlights, aes(y = FAC2_2), colour = "brown4", size = 5, shape = 18) + # 标记Carlos Alcaraz的高光时刻
  scale_colour_manual(values = c("Novak Djokovic" = "cyan4", "Carlos Alcaraz" = "brown4"),
                      name = "Player",
                      labels = c( "Carlos Alcaraz","Novak Djokovic")) +
  scale_fill_manual(values = c("Novak Djokovic" = "cyan4", "Carlos Alcaraz" = "brown4"),
                    name = "Player",
                    labels = c( "Carlos Alcaraz","Novak Djokovic")) +
  labs(x = "Point Number", y = "FAC2 Score") +
  theme_minimal() +
  guides(colour = guide_legend(override.aes = list(size = 2))) # 设置图例中线的粗细
  
# 使用ggplot2绘制折线图，并在折线下方填充颜色
p3<-ggplot(data4_1, aes(x = point_no)) + 
  geom_line(aes(y = FAC3_1, colour = "Novak Djokovic"), size = 1) + 
  geom_ribbon(aes(ymin = 0, ymax = FAC3_1, fill = "Novak Djokovic"), alpha = 0.3) +
  geom_line(aes(y = FAC3_2, colour = "Carlos Alcaraz"), size = 1) +
  geom_ribbon(aes(ymin = 0, ymax = FAC3_2, fill = "Carlos Alcaraz"), alpha = 0.3) +
  geom_point(data = djokovic_highlights, aes(y = FAC3_1), colour = "cyan4", size = 4, shape = 16) + # 标记Novak Djokovic的高光时刻
  geom_point(data = alcaraz_highlights, aes(y = FAC3_2), colour = "brown4", size = 5, shape = 18) + # 标记Carlos Alcaraz的高光时刻
  scale_colour_manual(values = c("Novak Djokovic" = "cyan4", "Carlos Alcaraz" = "brown4"),
                      name = "Player",
                      labels = c( "Carlos Alcaraz","Novak Djokovic")) +
  scale_fill_manual(values = c("Novak Djokovic" = "cyan4", "Carlos Alcaraz" = "brown4"),
                    name = "Player",
                    labels = c( "Carlos Alcaraz","Novak Djokovic")) +
  labs(x = "Point Number", y = "FAC3 Score") +
  theme_minimal() +
  guides(colour = guide_legend(override.aes = list(size = 2))) # 设置图例中线的粗细

# 使用ggplot2绘制折线图，并在折线下方填充颜色
p4<-ggplot(data4_1, aes(x = point_no)) + 
  geom_line(aes(y = FAC4_1, colour = "Novak Djokovic"), size = 1) + 
  geom_ribbon(aes(ymin = 0, ymax = FAC4_1, fill = "Novak Djokovic"), alpha = 0.3) +
  geom_line(aes(y = FAC4_2, colour = "Carlos Alcaraz"), size = 1) +
  geom_ribbon(aes(ymin = 0, ymax = FAC4_2, fill = "Carlos Alcaraz"), alpha = 0.3) +
  geom_point(data = djokovic_highlights, aes(y = FAC4_1), colour = "cyan4", size = 4, shape = 16) + # 标记Novak Djokovic的高光时刻
  geom_point(data = alcaraz_highlights, aes(y = FAC4_2), colour = "brown4", size = 5, shape = 18) + # 标记Carlos Alcaraz的高光时刻
  scale_colour_manual(values = c("Novak Djokovic" = "cyan4", "Carlos Alcaraz" = "brown4"),
                      name = "Player",
                      labels = c( "Carlos Alcaraz","Novak Djokovic")) +
  scale_fill_manual(values = c("Novak Djokovic" = "cyan4", "Carlos Alcaraz" = "brown4"),
                    name = "Player",
                    labels = c( "Carlos Alcaraz","Novak Djokovic")) +
  labs(x = "Point Number", y = "FAC4 Score") +
  theme_minimal() +
  guides(colour = guide_legend(override.aes = list(size = 2))) # 设置图例中线的粗细

# 使用ggplot2绘制折线图，并在折线下方填充颜色
p5<-ggplot(data4_1, aes(x = point_no)) + 
  geom_line(aes(y = FAC5_1, colour = "Novak Djokovic"), size = 1) + 
  geom_ribbon(aes(ymin = 0, ymax = FAC5_1, fill = "Novak Djokovic"), alpha = 0.3) +
  geom_line(aes(y = FAC5_2, colour = "Carlos Alcaraz"), size = 1) +
  geom_ribbon(aes(ymin = 0, ymax = FAC5_2, fill = "Carlos Alcaraz"), alpha = 0.3) +
  geom_point(data = djokovic_highlights, aes(y = FAC5_1), colour = "cyan4", size = 4, shape = 16) + # 标记Novak Djokovic的高光时刻
  geom_point(data = alcaraz_highlights, aes(y = FAC5_2), colour = "brown4", size = 5, shape = 18) + # 标记Carlos Alcaraz的高光时刻
  scale_colour_manual(values = c("Novak Djokovic" = "cyan4", "Carlos Alcaraz" = "brown4"),
                      name = "Player",
                      labels = c( "Carlos Alcaraz","Novak Djokovic")) +
  scale_fill_manual(values = c("Novak Djokovic" = "cyan4", "Carlos Alcaraz" = "brown4"),
                    name = "Player",
                    labels = c( "Carlos Alcaraz","Novak Djokovic")) +
  labs(x = "Point Number", y = "FAC5 Score") +
  theme_minimal() +
  guides(colour = guide_legend(override.aes = list(size = 2))) # 设置图例中线的粗细

  
# 使用ggplot2绘制折线图，并在折线下方填充颜色
p6<-ggplot(data4_1, aes(x = point_no)) + 
  geom_line(aes(y = `Performance Score_1`, colour = "Novak Djokovic"), size = 1) + 
  geom_ribbon(aes(ymin = 0, ymax = `Performance Score_1`, fill = "Novak Djokovic"), alpha = 0.3) +
  geom_line(aes(y = `Performance Score_2`, colour = "Carlos Alcaraz"), size = 1) +
  geom_ribbon(aes(ymin = 0, ymax = `Performance Score_2`, fill = "Carlos Alcaraz"), alpha = 0.3) +
  geom_point(data = djokovic_highlights, aes(y = `Performance Score_1`), colour = "cyan4", size = 4, shape = 16) + # 标记Novak Djokovic的高光时刻
  geom_point(data = alcaraz_highlights, aes(y = `Performance Score_2`), colour = "brown4", size = 5, shape = 18) + # 标记Carlos Alcaraz的高光时刻
  scale_colour_manual(values = c("Novak Djokovic" = "cyan4", "Carlos Alcaraz" = "brown4"),
                      name = "Player",
                      labels = c( "Carlos Alcaraz","Novak Djokovic")) +
  scale_fill_manual(values = c("Novak Djokovic" = "cyan4", "Carlos Alcaraz" = "brown4"),
                    name = "Player",
                    labels = c( "Carlos Alcaraz","Novak Djokovic")) +
  labs(x = "Point Number", y = "Performance Score") +
  theme_minimal() +
  guides(colour = guide_legend(override.aes = list(size = 2))) # 设置图例中线的粗细

# 添加代表不同网球比赛盘数的垂直分割线
p1 <- p1 + geom_vline(xintercept = c(45, 139, 209, 273), linetype = "dashed", colour = "black") 
# 添加注释来标记不同的区域（比赛的每一盘）
p1 <- p1 + annotate("text", x = c(22.5, 92, 174, 241, 316), y = Inf, label = c("Set 1", "Set 2", "Set 3", "Set 4", "Set 5"), vjust = 1,cex=5.5,fontface = "bold")
# 调整图表中所有字体大小
p1 <- p1 + theme(text = element_text(size = 20)) # 可以增加或减少这个数值以满足你的需求
# 添加代表不同网球比赛盘数的垂直分割线
p2 <- p2 + geom_vline(xintercept = c(45, 139, 209, 273), linetype = "dashed", colour = "black") 
# 添加注释来标记不同的区域（比赛的每一盘）
p2 <- p2 + annotate("text", x = c(22.5, 92, 174, 241, 316), y = Inf, label = c("Set 1", "Set 2", "Set 3", "Set 4", "Set 5"), vjust = 1,cex=5.5,fontface = "bold")
# 调整图表中所有字体大小
p2 <- p2 + theme(text = element_text(size = 20)) # 可以增加或减少这个数值以满足你的需求
# 添加代表不同网球比赛盘数的垂直分割线
p3 <- p3 + geom_vline(xintercept = c(45, 139, 209, 273), linetype = "dashed", colour = "black") 
# 添加注释来标记不同的区域（比赛的每一盘）
p3 <- p3 + annotate("text", x = c(22.5, 92, 174, 241, 316), y = Inf, label = c("Set 1", "Set 2", "Set 3", "Set 4", "Set 5"), vjust = 1,cex=5.5,fontface = "bold")
# 调整图表中所有字体大小
p3 <- p3 + theme(text = element_text(size = 20)) # 可以增加或减少这个数值以满足你的需求
# 添加代表不同网球比赛盘数的垂直分割线
p4 <- p4 + geom_vline(xintercept = c(45, 139, 209, 273), linetype = "dashed", colour = "black") 
# 添加注释来标记不同的区域（比赛的每一盘）
p4 <- p4 + annotate("text", x = c(22.5, 92, 174, 241, 316), y = Inf, label = c("Set 1", "Set 2", "Set 3", "Set 4", "Set 5"), vjust = 1,cex=5.5,fontface = "bold")
# 调整图表中所有字体大小
p4 <- p4 + theme(text = element_text(size = 20)) # 可以增加或减少这个数值以满足你的需求
p5 <- p5 + geom_vline(xintercept = c(45, 139, 209, 273), linetype = "dashed", colour = "black") 
# 添加注释来标记不同的区域（比赛的每一盘）
p5 <- p5 + annotate("text", x = c(22.5, 92, 174, 241, 316), y = Inf, label = c("Set 1", "Set 2", "Set 3", "Set 4", "Set 5"), vjust =1,cex=5.5,fontface = "bold")
# 调整图表中所有字体大小
p5 <- p5 + theme(text = element_text(size = 20)) # 可以增加或减少这个数值以满足你的需求
p6 <- p6 + geom_vline(xintercept = c(45, 139, 209, 273), linetype = "dashed", colour = "black") 
# 添加注释来标记不同的区域（比赛的每一盘）
p6 <- p6 + annotate("text", x = c(22.5, 92, 174, 241, 316), y = Inf, label = c("Set 1", "Set 2", "Set 3", "Set 4", "Set 7"), vjust = 1,cex=5,fontface = "bold")
# 调整图表中所有字体大小
p6 <- p6 + theme(text = element_text(size = 20)) # 可以增加或减少这个数值以满足你的需求
# 打印修改后的图表
print(p1)
print(p2)
print(p3)
print(p4)
print(p5)
print(p6)

install.packages("latticeExtra")
library(latticeExtra);library(dplyr)

dt1<-data4_1%>%select(-c(point_no,highlight_1,FAC1_2,FAC2_2,FAC3_2,FAC4_2,FAC5_2,`Performance Score_2`,highlight_2))%>%ts()
# 设置新的列名
colnames(dt1) <- c("FAC1", "FAC2", "FAC3", "FAC4", "FAC5", "CPS")
p_1<-horizonplot(dt1,layout=c(1,6),origin=0,colorkey=TRUE,par.settings=list(par.main.text=list(cex=1,font=1)))

dt2<-data4_1%>%select(-c(point_no,highlight_2,FAC1_1,FAC2_1,FAC3_1,FAC4_1,FAC5_1,`Performance Score_1`,highlight_1))%>%ts()
# 设置新的列名
colnames(dt2) <- c("FAC1", "FAC2", "FAC3", "FAC4", "FAC5", "CPS")
p_2<-horizonplot(dt2,layout=c(1,6),origin=0,colorkey=TRUE,
                 par.settings = list(par.main.text = list(cex = 1.5, font = 2)))
print(p_1)
print(p_2)


install.packages("ggiraphExtra")
library(ggiraphExtra)
library(readxl);library(ggplot2);library(reshape2);library(dplyr);
# 读取Excel文件
dataa_1 <- read_excel("C:\\Users\\daigu\\Desktop\\副本噪音分析.xlsx")
myangle<-seq(-20,-340,length.out=5) # 设置标签角度，使之垂直于坐标轴
mytheme<-theme_bw()+ # 使用黑白主题
  theme(legend.position="bottom", # 设置图例位置
        axis.text.x=element_text(size=9,color="darkblue",angle=myangle))
# 设置坐标轴标签字体大小、颜色和和角度
# 图（a）使用原始数据
df<-as.data.frame(dataa_1)
ggRadar(data=df,aes(group= "Types of Interference"),rescale=FALSE, ylim=c(-2,2),alpha=0.3, size=2)+xlab("FAC")+ylab("Score")                                                                                        

install.packages("fmsb")
# 载入fmsb包
# 加载fmsb包
# 加载所需的包
library(fmsb)
library(ggplot2)

data <- read_excel("C:\\Users\\daigu\\Desktop\\model_performance.xlsx")
# 使用之前的数据框定义
# 注意：这里不再重复定义data数据框

# 数据需要包括雷达图的最大值和最小值，这里我们手动添加（根据你的数据范围调整）
data_norm <- rbind(rep(-2, 2), rep(2, 5), data[, -1])

# 画雷达图，填充颜色并设置透明度
radarchart(data_norm, axistype = 4,
           # 设置标签
           legend = data$Type,
           legend.x=0.2,
           legend.x=0.2,
           # 设置线条颜色
           col = c("red", "darkblue", "green"),
           # 设置填充颜色，并调整透明度
           pfcol = c(alpha("red", 0.3), alpha("darkblue", 0.3), alpha("green", 0.3)),
           # 设置填充颜色的透明度
           pcol = c(alpha("red", 0.3), alpha("darkblue", 0.3), alpha("green", 0.3)))


# 然后添加图例
legend("topright", 
       legend = c("undisturbed", "Interference 1", "Interference 2"), 
       fill = c(alpha("red", 0.3), alpha("darkblue", 0.3), alpha("green", 0.3)))

