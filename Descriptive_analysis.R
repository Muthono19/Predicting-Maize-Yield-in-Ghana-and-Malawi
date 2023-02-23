# Load the necessary libraries
library(ggplot2)
library(dplyr)
library(grid)
library(gridExtra)
library(Cairo)


### Set the working directory
setwd("C:\\Users\\sgachoki\\Desktop\\IITA")

# Load the data set
d1 <- read.csv("dBaseGhana2013.csv")
d1$Clusters <- as.factor(d1$Clusters)
dim(d1)

d2 <- read.csv("dBaseGhana2019.csv")
d2$Clusters <- as.factor(d2$Clusters)
dim(d2)

d3 <- read.csv("dBaseMalawi2013.csv")
d3$Clusters <- as.factor(d3$Clusters)
dim(d3)

d4 <- read.csv("dBaseMalawi2019.csv")
d4$Clusters <- as.factor(d4$Clusters)
dim(d4)

####Data set without outliers
d1_out <- d1 %>%
  group_by(Clusters) %>%
  mutate(Yield_C = ifelse(Yield>quantile(Yield,0.75,na.rm=T)+1.5*IQR(Yield,na.rm = T)|
                            Yield<quantile(Yield,0.25,na.rm=T)-1.5*IQR(Yield, na.rm = T), NA, Yield))
d2_out <- d2 %>%
  group_by(Clusters) %>%
  mutate(Yield_C = ifelse(Yield>quantile(Yield,0.75,na.rm=T)+1.5*IQR(Yield,na.rm = T)|
                            Yield<quantile(Yield,0.25,na.rm=T)-1.5*IQR(Yield, na.rm = T), NA, Yield))

d3_out <- d3 %>%
  group_by(Clusters) %>%
  mutate(Yield_C = ifelse(Yield>quantile(Yield,0.75,na.rm=T)+1.5*IQR(Yield,na.rm = T)|
                            Yield<quantile(Yield,0.25,na.rm=T)-1.5*IQR(Yield, na.rm = T), NA, Yield))
d4_out <- d4 %>%
  group_by(Clusters) %>%
  mutate(Yield_C = ifelse(Yield>quantile(Yield,0.75,na.rm=T)+1.5*IQR(Yield,na.rm = T)|
                            Yield<quantile(Yield,0.25,na.rm=T)-1.5*IQR(Yield, na.rm = T), NA, Yield))

### Write the database without Outliers
write.csv(d1_out,"dBaseGhana2013_Out.csv") ### Ghana 2013
write.csv(d2_out,"dBaseGhana2019_Out.csv") ### Ghana 2019
write.csv(d3_out,"dBaseMalawi2013_Out.csv")  ### Malawi 2013
write.csv(d4_out,"dBaseMalawi2019_Out.csv")  ### Malawi 2019


### Make plots with and without outliers
p1 <- ggplot(d1, aes(x =Clusters,y = Yield, group = Clusters)) +
  geom_boxplot(outlier.shape = 16, outlier.color = "red") + xlab("") + ylab("With outliers") +
  ggtitle("Ghana 2013")+
  theme(text = element_text(size = 8), axis.title = element_text(size = 8), 
        plot.title = element_text(size = 8, hjust = 0.5))

p1_out <- ggplot(d1_out, aes(x =Clusters,y = Yield_C, group = Clusters)) +
  geom_boxplot(outlier.shape = 16, outlier.color = "red") + xlab("Clusters") + ylab("Without outliers") +
  ggtitle("")+
  theme(text = element_text(size = 8), axis.title = element_text(size = 8), 
        plot.title = element_text(size = 8, hjust = 0.5))

p2 <- ggplot(d2, aes(x =Clusters,y = Yield, group = Clusters)) +
  geom_boxplot(outlier.shape = 16, outlier.color = "red") + xlab("") + ylab("") +
  ggtitle("Ghana 2019")+
  theme(text = element_text(size = 8), axis.title = element_text(size = 8), 
        plot.title = element_text(size = 8, hjust = 0.5))

p2_out <- ggplot(d2_out, aes(x =Clusters,y = Yield_C, group = Clusters)) +
  geom_boxplot(outlier.shape = 16, outlier.color = "red") + xlab("Clusters") + ylab("") +
  ggtitle("")+
  theme(text = element_text(size = 8), axis.title = element_text(size = 8), 
        plot.title = element_text(size = 8, hjust = 0.5))

p3 <- ggplot(d3, aes(x =Clusters,y = Yield, group = Clusters)) +
  geom_boxplot(outlier.shape = 16, outlier.color = "red") + xlab("") + ylab("") +
  ggtitle("Malawi 2013")+
  theme(text = element_text(size = 8), axis.title = element_text(size = 8), 
        plot.title = element_text(size = 8, hjust = 0.5))

p3_out <- ggplot(d3_out, aes(x =Clusters,y = Yield_C, group = Clusters)) +
  geom_boxplot(outlier.shape = 16, outlier.color = "red") + xlab("Clusters") + ylab("") +
  ggtitle("")+
  theme(text = element_text(size = 8), axis.title = element_text(size = 8), 
        plot.title = element_text(size = 8, hjust = 0.5))

p4 <- ggplot(d4, aes(x =Clusters,y = Yield, group = Clusters)) +
  geom_boxplot(outlier.shape = 16, outlier.color = "red") + xlab("") + ylab("") +
  ggtitle("Malawi 2019")+
  theme(text = element_text(size = 8), axis.title = element_text(size = 8), 
        plot.title = element_text(size = 8, hjust = 0.5))

p4_out <- ggplot(d4_out, aes(x =Clusters,y = Yield_C, group = Clusters)) +
  geom_boxplot(outlier.shape = 16, outlier.color = "red") + xlab("Clusters") + ylab("") +
  ggtitle("")+
  theme(text = element_text(size = 8), axis.title = element_text(size = 8), 
        plot.title = element_text(size = 8, hjust = 0.5))


png(file = "BoxplotsALL.png", width = 2400, height = 1400, units = "px", res = 300, type = "cairo")
grid.arrange(p1,p2,p3,p4,p1_out,p2_out,p3_out,p4_out, ncol=4, nrow=2)
dev.off()

####%%%%%%%%%%%%$$$$$$$$$$$$ THE END %%^^^^^^^^^^^^^^&&&&&&&&&&&####



