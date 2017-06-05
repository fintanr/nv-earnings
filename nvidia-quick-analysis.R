# 
# quick script to plot NVIDIA quarterly revenue growth across a number of segments
#
# @fintanr, 5th June 2017
#

library(ggplot2)
library(reshape2)

df <- read.csv("NVIDIA-DataCenter-Auto.csv")

df$DC.Percent <- round((df$Datacenter/df$Total.Revenue) * 100, 1)
df$Auto.Percent <- round((df$Auto/df$Total.Revenue) * 100, 1)
df$R_and_D.Percent <- round((df$R.D.Spend/df$Total.Revenue) * 100, 1)
df$Date <- as.Date(df$Date)

DC_Auto_Df <- melt(subset(df, select = c(Date,Datacenter,Auto)), id="Date")
colnames(DC_Auto_Df) <- c("Date", "Category", "value")

R_and_D_Df <- melt(subset(df, select = c(Date,R.D.Spend,Total.Revenue)), id="Date")
colnames(R_and_D_Df) <- c("Date", "Category", "value")

Percents_Df <- melt(subset(df, select = c(Date,DC.Percent, Auto.Percent, R_and_D.Percent)), id="Date") 
colnames(Percents_Df) <- c("Date", "Category", "value")

levels(Percents_Df$Category)[levels(Percents_Df$Category)=="R_and_D.Percent"] <- "R & D"
levels(Percents_Df$Category)[levels(Percents_Df$Category)=="Auto.Percent"] <- "Auto"
levels(Percents_Df$Category)[levels(Percents_Df$Category)=="DC.Percent"] <- "Datacenter"

g1<- ggplot(data = DC_Auto_Df, aes(x=Date, y=value, color=Category)) 
g1 <- g1 + geom_line()
g1 <- g1 + theme(text = element_text(size=15), axis.text.x = element_text(angle=45, hjust=1))
g1 <- g1 + xlab("Time") + ylab("Revenue ($M)")
g1 <- g1 + theme(legend.title=element_blank())
g1 <- g1 + ggtitle("NVIDIA Datacenter & Auto Sales\nQ1FY15 to Q1FY18")

ggsave("nvidia-dc-auto-sales-q1fy18.png", g1, width=10)

g2 <- ggplot(data = Percents_Df, aes(x=Date, y=value, color=Category)) 
g2 <- g2 + geom_line()
g2 <- g2 + theme(text = element_text(size=15), axis.text.x = element_text(angle=45, hjust=1))
g2 <- g2 + xlab("Time") + ylab("%")
g2 <- g2 + theme(legend.title=element_blank())
g2 <- g2 + ggtitle("NVIDIA Datacenter and Auto Segments\nPerentage of Total Revenue\nQ1FY15 to Q1FY18")

ggsave("nvidia-dc-auto-rd-percent-q1fy18.png", g2, width=10)





