library(readxl)
Sales <- read_excel("C:/Users/bonny/Desktop/Data Visualisation/Sales.xlsx")
#View(Sales)
# replace n/a with NA
Sales[Sales[,] == "n/a"] <- 'NA'
# drop those values we cannot calculate from other given variables
library(tidyr)
summary_Sales <- Sales %>% drop_na()
x
summary_Sales
Sales$`Total Revenue`<- as.character(as.numeric(Sales$`Unit Price`)*as.numeric(Sales$`Units Sold`))
Sales$`Total Cost`<- as.character(as.numeric(Sales$`Unit Cost`)*as.numeric(Sales$`Units Sold`))
Sales$`Total Profit`<- as.character(as.numeric(Sales$`Total Revenue`)*as.numeric(Sales$`Total Cost`))

#1b)
#from raw data
hist(as.numeric(Sales[1:20,]$`Total Revenue`),main = "Raw data Total Revenue",xlab="Total Revenue",col = "blue")
#from cleaned data
hist(as.numeric(summary_Sales[1:20,]$`Total Revenue`),main = "Cleaned data Total Revenue",col = "pink")

#plots
#plot for raw data
plot(Sales[1:30,]$`Total Revenue`,Sales[1:30,]$`Total Profit`,xlab = "Total Revenue",
     ylab = "Total Profit",col="red",main = "Total Revenue vs Total profit Raw Data")
#plots for cleaned
plot(summary_Sales[1:30,]$`Total Revenue`,summary_Sales[1:30,]$`Total Profit`,xlab = "Total Revenue",
     ylab = "Total Profit",col="green",main = "Total Revenue vs Total profit Raw Data")
#c
library(ggplot2)
ggplot(summary_Sales[1:13,])+geom_point(aes(y =`Total Cost`, x = `Units Sold`), col = 'gold', size = 3) + ggtitle(" Unit Sold vs Total Cost") + theme(plot.title = element_text(hjust = 0.5))
ggplot(summary_Sales[1:10,])+geom_point(aes(y =`Unit Price`, x = `Unit Cost`), col = 'gold', size = 3) + ggtitle(" Unit Price vs Unit Cost") + theme(plot.title = element_text(hjust = 0.5))
ggplot(summary_Sales[1:10,])+geom_point(aes(y =`Total Cost`, x = `Total Revenue`), col = 'gold', size = 3) + ggtitle(" Total Cost vs Total Revenue") + theme(plot.title = element_text(hjust = 0.5))

#2d)
summary_Sales <- summary_Sales %>% arrange(desc(as.numeric(summary_Sales$`Total Cost`)))
summary_Sales$`Total Cost`

#2e)
#TYPES OF EXPLORATORY DATA ANALYSIS:
  
# Univariate Non-graphical
#Multivariate Non-graphical
#Univariate graphical
#Multivariate graphical
#2f)
summary_Sales$Country[1] <- "Angola"
summary_Sales$`Item Type`[1] <- "Chicken Slice"
summary_Sales$`Units Sold`[1] <- "233"
#1g
summary_Sales$Paid[summary_Sales$Paid == 'Orange St'] <- 'Portola Pkwy'

#2h
summary_Sales[, 6:11] <- sapply(summary_Sales[,6:11], as.numeric)
summary(summary_Sales[,as.double(summary_Sales$`Unit Price`)])
class(Sales$`Unit Cost`)
mean(as.numeric(summary_Sales$`Unit Cost`))
mean(as.numeric(summary_Sales$`Unit Price`))
mean(as.numeric(summary_Sales$`Total Cost`))
mean(as.numeric(summary_Sales$`Total Profit`))
mean(as.numeric(summary_Sales$`Total Revenue`))
#quantile
x <- quantile(as.numeric(summary_Sales$`Unit Cost`),0.25)
x
y <- quantile(as.numeric(summary_Sales$`Unit Cost`),0.75)
y
inter_quantile <- y-x
inter_quantile
#quantile
x <- quantile(as.numeric(summary_Sales$`Unit Price`),0.25)
x
y <- quantile(as.numeric(summary_Sales$`Unit Price`),0.75)
y
inter_quantile <- y-x
inter_quantile
#quantile
x <- quantile(as.numeric(summary_Sales$`Total Cost`),0.25)
x
y <- quantile(as.numeric(summary_Sales$`Total Cost`),0.75)
y
inter_quantile <- y-x
inter_quantile
#quantile
x <- quantile(as.numeric(summary_Sales$`Total revenue`),0.25)
x
y <- quantile(as.numeric(summary_Sales$`Total revenue`),0.75)
y
inter_quantile <- y-x
inter_quantile
#quantile
x <- quantile(as.numeric(summary_Sales$`Total Profit`),0.25)
x
y <- quantile(as.numeric(summary_Sales$`Total Profit`),0.75)
y
inter_quantile <- y-x
inter_quantile
#quantile
x <- quantile(as.numeric(summary_Sales$`Units Sold`),0.25)
x
y <- quantile(as.numeric(summary_Sales$`Unit Sold`),0.75)
y
inter_quantile <- y-x
inter_quantile
#2i)
# dataset before
summary_Sales[1,]
summary_Sales[1,2] <- "Paid_Type"
#after
summary_Sales[1,]