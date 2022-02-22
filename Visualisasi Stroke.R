data <- read.csv("E:/KULIAH/MAGISTER/BISNIS ANALITIK/Data KMMI.csv",sep = ",")
head(data)
summary(data)

# Cek Missing Value
sum(is.na(data))
library(ggplot2)
ggplot(data,aes(x=BMI))+
  geom_histogram(aes(y=..density..),colour="black", fill="white")+
  geom_density(alpha=0.1,fill="red")

# Mengisi Missing Value
data$BMI[is.na(data$BMI)] <- median(data$BMI, na.rm = TRUE)
sum(is.na(data))

# Statistika Deskriptif
summary(data)

# 1. Jumlah stroke vs tidak
ggplot(data, aes(x=Stroke, fill=Stroke )) + geom_bar( )
help(ggplot)

# 2. Persentase stroke vs tidak
library(plotly)
plot_ly(data, labels=~Stroke, type="pie")
help(plot_ly)

# 3. Barchart jumlah stroke by gender
ggplot(data, aes(x=Gender, fill = Stroke)) + geom_bar(position = "dodge")

# 4. Barchart jumlah stroke by smoking status
ggplot(data, aes(x=Smoking_Status, fill = Stroke)) + geom_bar(position = "dodge")

# 5. Barchart jumlah stroke by hipertension
ggplot(data, aes(x=Hypertension, fill = Stroke)) + geom_bar(position = "dodge")

# 6. Barchart jumlah stroke by heart disease
ggplot(data, aes(x=Heart_Disease, fill = Stroke)) + geom_bar(position = "dodge")

# 7. Barchart jumlah stroke by ever married
ggplot(data, aes(x=Ever_Married, fill = Stroke)) + geom_bar(position = "dodge")

# 8. Barchart jumlah stroke by residence type
ggplot(data, aes(x=Residence_Type, fill = Stroke)) + geom_bar(position = "dodge")

# 9. Barchart jumlah stroke by work type
ggplot(data, aes(x=Work_Type, fill = Stroke)) + geom_bar(position = "dodge")

# 10. Barchart jumlah stroke by work type posisi stack
ggplot(data, aes(x=Work_Type, fill = Stroke)) + geom_bar(position = "stack")
help(geom_bar)

# 11. Horizontal Bar Chart
data%>%
  group_by(Smoking_Status)%>%
  summarise(mean_run=mean(Age))%>%
  ggplot(aes(x=Smoking_Status, y=sort(mean_run)))+
  geom_bar(stat="identity", fill="red", alpha=.6, width=.4)+
  coord_flip()+
  xlab("Status Merokok")+
  ylab("Rata-Rata Umur")+
  theme_bw()

# 12. Bubble Plot
library(lessR)
ggplot(data, aes(x=BMI, y=Age, size=Avg_Glucose_Level, color=Gender))+
  geom_point(alpha=.5)+scale_size(name="Average Glucose Level")+
  scale_fill_viridis(discrete=TRUE, guide='none', option="A")+
  theme(legend.position="right")+ylab("Umur")+xlab("BMI")

# 13. Lollipop Chart
data%>%
  group_by(Smoking_Status)%>%
  Summarise(mean_run=mean(Age))%>%
  ggplot(aes(x=Smoking_Status, y=sort(mean_run)))+
  geom_point(size=3)+
  geom_segment(aes(x=Smoking_Status, xend=Smoking_Status, y=0, yend=sort(mean_run)))+
  labs(title="Lollipop Chart", x="Status Merokok", y="Rata-Rata Umur")+
  coord_flip()+
  theme(axis.text.x=element_text(angle=65, vjust=0.6))+
  theme_bw()

# 14. Histogram & density plot dari BMI
ggplot(data,aes(x=BMI))+
  geom_histogram(aes(y=..density..),colour="black", fill="white")+
  geom_density(alpha=0.1,fill="blue")

# 15. Histogram & density plot dari age
ggplot(data,aes(x=Age))+
  geom_histogram(aes(y=..density..),colour="black", fill="white")+
  geom_density(alpha=0.1,fill="green")

# 14. Histogram & density plot dari Glucose Level
ggplot(data,aes(x=Avg_Glucose_Level))+
  geom_histogram(aes(y=..density..),colour="black", fill="white")+
  geom_density(alpha=0.1,fill="red")


#Boxplot
boxplot(data$Age,horizontal = FALSE, ylab="Age")
boxplot(data$Age~data$Stroke, horizontal = FALSE, ylab = "Age", xlab = "Stroke")
#b$stats
#b$out
#ggplot(data = data, aes(y = Age, x = Stroke, fill = Stroke)) + geom_boxplot()

boxplot(data$Avg_Glucose_Level, horizontal = FALSE, ylab = "Avg_Glucose_Level", col = "lightblue")
boxplot(data$Avg_Glucose_Level~data$Stroke, horizontal = FALSE, ylab = "Avg_Glucose_Level", xlab = "Stroke", col = "lightblue")

boxplot(data$BMI, horizontal = FALSE, col = "maroon", ylab = "BMI")
boxplot(data$BMI~data$Stroke, horizontal = FALSE, ylab = "BMI", xlab = "Stroke", col = c("maroon","yellow"))

#Boxplot with ggplot2
ggplot(data = data, aes(y = Age, x = Stroke, fill = Stroke)) + geom_boxplot() +
  xlab("Stroke") + ylab("Age") + theme(legend.position="none")

#Boxplot with mean
p <- ggplot(data, aes(x=Stroke, y=Age, fill=Stroke)) +
  geom_boxplot(alpha=.7) + stat_summary(fun=mean, geom="point", shape=20, size=14, color="red", fill="red") +
  theme(legend.position="none") + scale_fill_brewer(palette="Set1")
p

#Korelasi
data_num <- data.frame(data$Age,data$Avg_Glucose_Level,data$BMI)
#names(data_num)[names(data_num)=="data.Age"] <- "Age"
#names(data_num)[names(data_num)=="data.Avg_Glucose_Level"] <- "Avg_Glucose_Level"
#names(data_num)[names(data_num)=="data.BMI"] <- "BMI"
head(data_num)
data_num_cor <- cor(data_num)
data_num_cor
#Correlation Heatmap
library(reshape2)
data_num_cor2 <- melt(data_num_cor)
head(data_num_cor2)

ggplot(data = data_num_cor2, aes(x=Var1, y=Var2, fill=value)) + geom_tile()

#Korelasi Tipe Lain
library(ggcorrplot)
ggcorrplot(data_num_cor, hc.order = TRUE, lab = TRUE, lab_size = 4, method = "circle",
           colors = c("green", "tomato2"), title="correlogram", ggtheme=theme_bw)