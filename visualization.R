## SETTING A WORKING DIRECTORY
setwd("*your working directory here")

## LOADING THE NECESSARY PACKAGES
library(dplyr)
library(ggplot2)

## Note: PISA Reading Proficiency scores are divided into LEVEL BELOW 1B, LEVEL 1B, LEVEL 1A, LEVEL 2, LEVEL 3, LEVEL 4, LEVEL 5, AND LEVEL 6
## The data are already aggregated in the form of percentage point (share of the 15 year olds population by reading proficiency score level and country)

## CLEANING AND PREPARING THE DATA ON LEVEL BELOW 1B SCORE

read_below1b <- read.csv("Reading Below Level 1.csv")
colnames(read_below1b)[colnames(read_below1b) == "X2015..YR2015."] <- "level_below1b"  #renaming the column
read_below1b <- read_below1b[,c("Country.Name","level_below1b")] 
read_below1b <- filter(read_below1b, Country.Name != "") #excluding the rows which have blank country name data
read_below1b <- filter(read_below1b, level_below1b != "..") #excluding the rows whose "level_below1b" data are filled with ".."
str(read_below1b) #checking the data type of each variable
# turns out that the "level_below1b" is still in the form of Factor, hence needs to be transformed into Numeric to allow for further aggregation
read_below1b$level_below1b <- as.numeric(paste(read_below1b$level_below1b))
is.numeric(read_below1b$level_below1b) #checking whether it's correctly transformed into Numeric

## CLEANING AND PREPARING THE DATA ON LEVEL 1B SCORE

read_1b <- read.csv("Reading Level 1.csv")
colnames(read_1b)[colnames(read_1b) == "X2015..YR2015."] <- "level_1b"  #renaming the column
read_1b <- read_1b[,c("Country.Name","level_1b")] 
read_1b <- filter(read_1b, Country.Name != "") #excluding the rows which have blank country name data
read_1b <- filter(read_1b, level_1b != "..") #excluding the rows whose "level_1b" data are filled with ".."
str(read_1b) #checking the data type of each variable
# turns out that the "level_1b" is still in the form of Factor, hence needs to be transformed into Numeric to allow for further aggregation
read_1b$level_1b <- as.numeric(paste(read_1b$level_1b))
is.numeric(read_1b$level_1b) #checking whether it's correctly transformed into Numeric

## MERGING LEVEL BELOW 1B AND LEVEL 1B
read <- merge(read_below1b,read_1b,by="Country.Name")

## CLEANING AND PREPARING THE DATA ON LEVEL 1A SCORE

read_1a <- read.csv("Reading Level 1A.csv")
colnames(read_1a)[colnames(read_1a) == "X2015..YR2015."] <- "level_1a"  #renaming the column
read_1a <- read_1a[,c("Country.Name","level_1a")] 
read_1a <- filter(read_1a, Country.Name != "") #excluding the rows which have blank country name data
read_1a <- filter(read_1a, level_1a != "..") #excluding the rows whose "level_1a" data are filled with ".."
str(read_1a) #checking the data type of each variable
# turns out that the "level_1a" is still in the form of Factor, hence needs to be transformed into Numeric to allow for further aggregation
read_1a$level_1a <- as.numeric(paste(read_1a$level_1a))
is.numeric(read_1a$level_1a) #checking whether it's correctly transformed into Numeric

## MERGING THE DATA
read <- merge(read,read_1a,by="Country.Name")

## CLEANING AND PREPARING THE DATA ON LEVEL 2 SCORE

read_2 <- read.csv("Reading Level 2.csv")
colnames(read_2)[colnames(read_2) == "X2015..YR2015."] <- "level_2"  #renaming the column
read_2 <- read_2[,c("Country.Name","level_2")] 
read_2 <- filter(read_2, Country.Name != "") #excluding the rows which have blank country name data
read_2 <- filter(read_2, level_2 != "..") #excluding the rows whose "level_2" data are filled with ".."
str(read_2) #checking the data type of each variable
# turns out that the "level_2" is still in the form of Factor, hence needs to be transformed into Numeric to allow for further aggregation
read_2$level_2 <- as.numeric(paste(read_2$level_2))
is.numeric(read_2$level_2) #checking whether it's correctly transformed into Numeric

## AGGREGATING LEVEL BELOW 1, LEVEL 1A, AND LEVEL 1B INTO ONE VARIABLE
read$level_1 <- rowSums(read[,-1])
read[,c("level_below1b","level_1b","level_1a")] <- NULL

## MERGING WITH THE LEVEL 2 DATA
read <- merge(read,read_2,by="Country.Name")

## CLEANING AND PREPARING THE DATA ON LEVEL 3 SCORE

read_3 <- read.csv("Reading Level 3.csv")
colnames(read_3)[colnames(read_3) == "X2015..YR2015."] <- "level_3"  #renaming the column
read_3 <- read_3[,c("Country.Name","level_3")] 
read_3 <- filter(read_3, Country.Name != "") #excluding the rows which have blank country name data
read_3 <- filter(read_3, level_3 != "..") #excluding the rows whose "level_3" data are filled with ".."
str(read_3) #checking the data type of each variable
# turns out that the "level_3" is still in the form of Factor, hence needs to be transformed into Numeric to allow for further aggregation
read_3$level_3 <- as.numeric(paste(read_3$level_3))
is.numeric(read_3$level_3) #checking whether it's correctly transformed into Numeric

## MERGING WITH THE LEVEL 3 DATA
read <- merge(read,read_3,by="Country.Name")

## CLEANING AND PREPARING THE DATA ON LEVEL 4 SCORE

read_4 <- read.csv("Reading Level 4.csv")
colnames(read_4)[colnames(read_4) == "X2015..YR2015."] <- "level_4"  #renaming the column
read_4 <- read_4[,c("Country.Name","level_4")] 
read_4 <- filter(read_4, Country.Name != "") #excluding the rows which have blank country name data
read_4 <- filter(read_4, level_4 != "..") #excluding the rows whose "level_4" data are filled with ".."
str(read_4) #checking the data type of each variable
# turns out that the "level_4" is still in the form of Factor, hence needs to be transformed into Numeric to allow for further aggregation
read_4$level_4 <- as.numeric(paste(read_4$level_4))
is.numeric(read_4$level_4) #checking whether it's correctly transformed into Numeric

## MERGING WITH THE LEVEL 4 DATA
read <- merge(read,read_4,by="Country.Name")

## CLEANING AND PREPARING THE DATA ON LEVEL 5 SCORE

read_5 <- read.csv("Reading Level 5.csv")
colnames(read_5)[colnames(read_5) == "X2015..YR2015."] <- "level_5"  #renaming the column
read_5 <- read_5[,c("Country.Name","level_5")] 
read_5 <- filter(read_5, Country.Name != "") #excluding the rows which have blank country name data
read_5 <- filter(read_5, level_5 != "..") #excluding the rows whose "level_5" data are filled with ".."
str(read_5) #checking the data type of each variable
# turns out that the "level_5" is still in the form of Factor, hence needs to be transformed into Numeric to allow for further aggregation
read_5$level_5 <- as.numeric(paste(read_5$level_5))
is.numeric(read_5$level_5) #checking whether it's correctly transformed into Numeric

## MERGING WITH THE LEVEL 5 DATA
read <- merge(read,read_5,by="Country.Name")

## CLEANING AND PREPARING THE DATA ON LEVEL 6 SCORE

read_6 <- read.csv("Reading Level 6.csv")
colnames(read_6)[colnames(read_6) == "X2015..YR2015."] <- "level_6"  #renaming the column
read_6 <- read_6[,c("Country.Name","level_6")] 
read_6 <- filter(read_6, Country.Name != "") #excluding the rows which have blank country name data
read_6 <- filter(read_6, level_6 != "..") #excluding the rows whose "level_6" data are filled with ".."
str(read_6) #checking the data type of each variable
# turns out that the "level_6" is still in the form of Factor, hence needs to be transformed into Numeric to allow for further aggregation
read_6$level_6 <- as.numeric(paste(read_6$level_6))
is.numeric(read_6$level_6) #checking whether it's correctly transformed into Numeric

## MERGING WITH THE LEVEL 6 DATA
read <- merge(read,read_6,by="Country.Name")

## DATA VISUALIZATION ##

# Retrieving Indonesia data 
read_ind <- filter(read,Country.Name == "Indonesia")
read_ind <- reshape(read_ind,varying=c("level_1","level_2","level_3","level_4","level_5","level_6"),direction="long",idvar="Country.Name",sep="_")
read_ind
colnames(read_ind)[colnames(read_ind) == "level"] <- "percent"
colnames(read_ind)[colnames(read_ind) == "time"] <- "level"
str(read_ind)
read_ind$level <- as.numeric(read_ind$level)

# Creating Pyramid Chart
ggplot(data=read_ind) + geom_bar(aes(level,percent), stat = "identity") + geom_bar(aes(level,-percent), stat = "identity") + scale_y_continuous(breaks=seq(-50,50,25),labels=abs(seq(-50,50,25)))+ scale_x_continuous(breaks=seq(1,6,by=1)) + coord_flip() + labs(title="INDONESIA")

# Retrieving Vietnam data
read_vn <- filter(read,Country.Name == "Vietnam")
read_vn <- reshape(read_vn,varying=c("level_1","level_2","level_3","level_4","level_5","level_6"),direction="long",idvar="Country.Name",sep="_")
read_vn
colnames(read_vn)[colnames(read_vn) == "level"] <- "percent"
colnames(read_vn)[colnames(read_vn) == "time"] <- "level"
str(read_vn)
read_vn$level <- as.numeric(read_vn$level)

# Creating Pyramid Chart
ggplot(data=read_vn) + geom_bar(aes(level,percent), stat = "identity") + geom_bar(aes(level,-percent), stat = "identity") + scale_y_continuous(breaks=seq(-50,50,25),labels=abs(seq(-50,50,25)))+ scale_x_continuous(breaks=seq(1,6,by=1)) + coord_flip() + labs(title="INDONESIA")

## COMPARING INDONESIA AND VIETNAM
read_ind_vn <- rbind(read_ind, read_vn)
ggplot(data=read_ind_vn) + geom_bar(aes(level,percent,group=Country.Name,fill=Country.Name), stat = "identity",subset(read_ind_vn,read_ind_vn$Country.Name=="Indonesia")) + geom_bar(aes(level,-percent,group=Country.Name,fill=Country.Name), stat = "identity",subset(read_ind_vn,read_ind_vn$Country.Name=="Vietnam")) + scale_y_continuous(breaks=seq(-50,50,25),labels=abs(seq(-50,50,25)))+ scale_x_continuous(breaks=seq(1,6,by=1)) + coord_flip() + labs(title="Comparison of Share of Population By Score Level Between Indonesia and Vietnam")

## COMPARING INDONESIA AND OECD
read_OECD <- data.frame("Country.Name" = "OECD","level_1" = mean(read$level_1), "level_2" = mean(read$level_2), "level_3" = mean(read$level_3),"level_4" = mean(read$level_4),"level_5" = mean(read$level_5),"level_6" = mean(read$level_6))
read_OECD <- reshape(read_OECD,varying=c("level_1","level_2","level_3","level_4","level_5","level_6"),direction="long",idvar="Country.Name",sep="_")
colnames(read_OECD)[colnames(read_OECD) == "level"] <- "percent"
colnames(read_OECD)[colnames(read_OECD) == "time"] <- "level"

read_ind_oecd <- rbind(read_ind,read_OECD)
ggplot(data=read_ind_oecd) + geom_bar(aes(level,percent,group=Country.Name,fill=Country.Name), stat = "identity",subset(read_ind_oecd,read_ind_oecd$Country.Name=="Indonesia")) + geom_bar(aes(level,-percent,group=Country.Name,fill=Country.Name), stat = "identity",subset(read_ind_oecd,read_ind_oecd$Country.Name=="OECD")) + scale_y_continuous(breaks=seq(-50,50,25),labels=abs(seq(-50,50,25)))+ scale_x_continuous(breaks=seq(1,6,by=1)) + coord_flip() + labs(title="Comparison of Share of Population By Score Level Between Indonesia and OECD Countries")
