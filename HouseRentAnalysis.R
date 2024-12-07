# 2.0 Data Import

# Import data
dataset_url <- "https://raw.githubusercontent.com/ChristineFJT/HouseRentAnalysis/refs/heads/main/House_Rent_Dataset.csv"
house_rental_data <- read.csv(dataset_url, header=TRUE)

# install packages
install.packages("plyr")
install.packages("dplyr")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("tidyverse")
install.packages("scales")
install.packages("ggrepel")
install.packages("lubridate")


# call packages
library(plyr)
library(dplyr)
library(tidyr)     # used for function separate()
library(ggplot2)   # used for Visualization
library(tidyverse)
library(scales)
library(ggrepel)
library(lubridate)


# 3.0 Data Cleaning
# 3.1 Remove Unnecessary Column
# Step 1: Count Unique Values of Each Column
count_unique <- sapply(house_rental_data,function(x) n_distinct(x))
count_unique
# Step 2: Identifying Area.Locality as Unnecessary Column - Many Unique Values
# Step 3: Drop Area.Locality using %in% operator
house_rental_data <- house_rental_data[,!names(house_rental_data) %in% c("Area.Locality")]
names(house_rental_data)

# 3.2 Rename Columns
# Names of Column Before Modification
names(house_rental_data)
# rename columns' name
colnames(house_rental_data) <- c("Date_Posted","Bedroom_Hall_Kitchen","Rental_Fee","House_Size",
                                 "Floor","Area_Type","City","Furnishing_Status",
                                 "Tenant_Type","Number_of_Bathroom","Point_of_Contact")
# Names of Column After Modification
names(house_rental_data)

# 3.3 Format Date_Posted Column
house_rental_data$Date_Posted <- as.Date(house_rental_data$Date_Posted,"%m/%d/%Y")
View(head(house_rental_data))

# 3.4 Extract Month from Date_Posted
house_rental_data$Month <- with(house_rental_data,month(ymd(Date_Posted)))

# 3.5 Split Floor Column to Floor_Preference and Total_Floor_Numbers
# Step 1: Show Top 6 Data Before Splitting
View(head(house_rental_data))
# Step 2: Split Floor using separate()
house_rental_data <- house_rental_data %>% separate(Floor, c("Floor_Preference","Total_Floor_Numbers")," out of ")
# Step 3: Show First 6 Data After Splitting
View(head(house_rental_data))

# 3.6 Check for missing value
colSums(is.na(house_rental_data)) 


# 3.7 Handling Missing Values
# Step 1: Check which rows have missing values
View(house_rental_data[is.na(house_rental_data$Total_Floor_Numbers),])
# Step 2: Calculate Mean to Perform Mean Imputation
floor_mean <-round(mean(as.integer(house_rental_data$Total_Floor_Numbers),na.rm=TRUE),0)
floor_mean
# Step 3: Mean Imputation
house_rental_data$Total_Floor_Numbers[is.na(house_rental_data$Total_Floor_Numbers)] <- floor_mean
# Step 4: Check Missing Values Again
colSums(is.na(house_rental_data)) 
# Step 5: Check row number 2554,2884,4491 & 4561
View(house_rental_data[c(2554,2884,4491,4561),])


# 3.8 Replace Values which are Character in Floor_Preference
# Step 1: show categories
levels(factor(house_rental_data$Floor_Preference))
# Step 2: Represent the Categories with Numbers
# represent Ground as 0
house_rental_data$Floor_Preference[which(house_rental_data$Floor_Preference == "Ground")] <- 0

# represent Upper Basement as -1
house_rental_data$Floor_Preference[which(house_rental_data$Floor_Preference == "Upper Basement")] <- -1

# represent Lower Basement as -2
house_rental_data$Floor_Preference[which(house_rental_data$Floor_Preference == "Lower Basement")] <- -2

# Result 
levels(factor(house_rental_data$Floor_Preference))


# 3.9 Swap Values if Floor_Preference is Greater than Total_Floor_Numbers
# Step 1: Checking Class of Columns before Compare Values between Two Columns
class(house_rental_data$Floor_Preference)
class(house_rental_data$Total_Floor_Numbers)
# Step 2: Change the Class of Floor_Preference and Total_Floor_Numbers to Numeric
house_rental_data$Floor_Preference <- as.numeric(house_rental_data$Floor_Preference)
house_rental_data$Total_Floor_Numbers <- as.numeric(house_rental_data$Total_Floor_Numbers)
# Step 3: Check whether the class of the column has changed to numeric
class(house_rental_data$Floor_Preference)
class(house_rental_data$Total_Floor_Numbers)
# Step 4: Show the rows with greater Floor_Preference than Total_Floor_Numbers
View(subset(house_rental_data,Floor_Preference>Total_Floor_Numbers))
# Step 5: Swap Floor_Preference and Total_Floor Numbers
temp_max = pmax(house_rental_data$Floor_Preference,house_rental_data$Total_Floor_Numbers)
house_rental_data$Floor_Preference = pmin(house_rental_data$Floor_Preference,house_rental_data$Total_Floor_Numbers)
house_rental_data$Total_Floor_Numbers = temp_max
View(house_rental_data[c(106,162),])

# 3,10 New Column Rental_Fee_Status
house_rental_data$Rental_Fee_Status <- with(house_rental_data,
                                            ifelse(Rental_Fee > mean(Rental_Fee),"High","Low"))
View(head(house_rental_data))

# 3.11 Rearrange Column Order
house_rental_data <- select(house_rental_data,Date_Posted,Month,Point_of_Contact,Tenant_Type,Bedroom_Hall_Kitchen,
                            Number_of_Bathroom,Floor_Preference,Total_Floor_Numbers,House_Size,Rental_Fee,
                            Rental_Fee_Status,Furnishing_Status,Area_Type,City)
View(head(house_rental_data))

# 3.12 Removing Outliers by Creating Function
outliers <- function(x) {
  Q1 <- quantile(x, probs=.25)
  Q3 <- quantile(x, probs=.75)
  iqr = Q3-Q1
  
  upper_limit = Q3 + (iqr*1.5)
  lower_limit = Q1 - (iqr*1.5)
  
  x > upper_limit | x < lower_limit
}

remove_outliers <- function(house_rental_data, cols = names(house_rental_data)) {
  for (col in cols) {
    house_rental_data <- house_rental_data[!outliers(house_rental_data[[col]]),]
  }
  house_rental_data
}


# 4.0 Data Exploration

# 4.1 show structure of the data
str(house_rental_data)

# 4.2 show number of rows and columns
dim(house_rental_data)

# 4.3 show top 10 data
View(head(house_rental_data,10))

# 4.4 show last 10 data
View(tail(house_rental_data,10))

# 4.5 Show all columns name
names(house_rental_data)

# 4.6 Show Number of Rows with the category of BHK
house_rental_data %>% count(Bedroom_Hall_Kitchen)

# 4.7 Show Number of Houses with the category of area
house_rental_data %>% count(Area_Type)

# 4.8 Show Number of Houses in each area with the category of City
house_rental_data %>% count(City,Area_Type)

# 4.9 show Number of Houses in each city with the category of Furnishing_Status
house_rental_data %>% count(City,Furnishing_Status)

# 4.10 show the details of each column
summary(house_rental_data)


# 5.0 Data Manipulation
# Remove Outliers in Rental_Fee
rf_no_outliers <- remove_outliers(house_rental_data,c('Rental_Fee'))



# 6.0 Data Visualization
# Question 1: How do Tenant Based on to Choose Their Houses?
# Analysis 1-1: Find the Count of Tenant Choose House Based on Date_Posted
# Frequency Polygon
ggplot(house_rental_data,aes(x=Date_Posted)) +
  geom_freqpoly(bins=100) +
  xlab("Date Posted") +
  ylab("Number of House Based on Date Posted") +
  ggtitle("Count of Tenant CHoose House Based on Date_Posted") +
  theme_bw() +
  facet_wrap(~Tenant_Type)


# Analysis 1-2: Find the Percentage of Tenant Choose House Based on Point_of_Contact
# Calculate Percentage Grouped by Point_of_Contact
group_poc <- house_rental_data %>% group_by(Point_of_Contact) %>% 
  count() %>% ungroup() %>% mutate(perc=n/sum(n)) %>% arrange(perc) %>%
  mutate(labels=scales::percent(perc))
# Pie Chart
ggplot(group_poc,aes(x="",y=perc,fill=Point_of_Contact)) +
  geom_bar(stat="identity") +
  guides(fill=guide_legend(title="Point of Contact")) +
  geom_text(aes(label=labels),position=position_stack(vjust=0.5)) +
  coord_polar(theta="y") +
  theme_void() +
  theme(legend.position = "bottom") +
  scale_fill_brewer() +
  ggtitle("Percentage of Tenant Choose House Based on Point of Contact")


# Analysis 1-3: Find the Percentage of Tenant Choose House Based on Number of Bedroom_Hall_Kitchen
# Bar Chart
ggplot(house_rental_data,aes(x=Bedroom_Hall_Kitchen)) +
  geom_bar(aes(y=after_stat(prop),fill=factor(after_stat(x))),stat="count") +
  geom_text(aes(label=scales::percent(after_stat(prop)),
                y=after_stat(prop)),stat="count",vjust=-0.5) +
  labs(x="Number of Bedroom, Hall and Kitchen",y="Percentage",
       fill="Number of Bedroom, Hall and Kitchen") +
  scale_x_continuous(breaks=seq(1,6,1)) +
  scale_y_continuous(labels=scales::percent) +
  theme(legend.position="bottom",
        legend.background = element_rect(fill="antiquewhite")) +
  ggtitle("Percentage of Tenant Choose House Based on Number of Bedroom, Hall and Kitchen")


# Analysis 1-4: Find the Percentage of Tenant Choose House Based on Number_of_Bathroom
# Bar Chart
ggplot(house_rental_data,aes(x=Number_of_Bathroom)) +
  geom_bar(aes(y=after_stat(prop),fill=factor(after_stat(x))),stat="count",width=0.5) +
  geom_text(aes(label=scales::percent(after_stat(prop)),
                y=after_stat(prop)),stat="count",vjust=-0.4,size=4) +
  labs(x="Number of Bathroom",y="Percentage",fill="Number of Bathroom") +  
  scale_x_continuous(breaks=seq(1,10,1)) +
  scale_y_continuous(labels=scales::percent) +
  theme(legend.position="bottom",
        legend.background = element_rect(fill="lightyellow")) +
  ggtitle("Percentage of Tenants Choose House Based on Number of Bathroom")


# Analysis 1-5: Find the Percentage of Tenant Choose House Based on Floor Preference
# Calculate Percentage Grouped by Floor_Preference
group_fp <- house_rental_data %>% 
  group_by(Floor_Preference) %>%
  count() %>% ungroup() %>% mutate(perc=n/sum(n)) %>% 
  arrange(perc) %>% mutate(labels=scales::percent(perc))
# Bar Chart
ggplot(group_fp,aes(x=Floor_Preference,y=perc,fill=Floor_Preference,label=labels)) +
  geom_bar(stat="identity",width=0.7) +
  scale_x_continuous(breaks=seq(-2,80,5)) +
  scale_y_continuous(labels=scales::percent) +
  scale_fill_gradient(low="pink",high="purple") +
  labs(x="Floor Preference",y="Percentage",
       title="Percentage of Tenant Choose House Based on Floor Preference")


# Analysis 1-6: Find the Percentage of Tenant Choose House Based on House Size
# Calculate Percentage Grouped by House_Size
group_hs <- house_rental_data %>% group_by(House_Size) %>% count() %>% ungroup() %>%
  mutate(perc=n/sum(n)) %>%
  arrange(perc) %>%
  mutate(labels=scales::percent(perc))
# Scatter Plot
ggplot(group_hs,aes(x=House_Size,y=perc,label=labels)) +
  geom_point(aes(x=House_Size,y=perc)) +
  scale_x_continuous(breaks=seq(0,8000,500)) +
  scale_y_continuous(labels=scales::percent) +
  geom_text_repel(max.overlaps = 20) +
  ggtitle("Percentage of Tenant Choose House Based on Size of House") +
  labs(x="Size of House",y="Percentage")


# Analysis 1-7: Find the Percentage of Tenant Choose House Based on Rental Fee
# Calculate Percentage Grouped by Rental_Fee_Status
group_rfs <- rf_no_outliers %>% group_by(Rental_Fee_Status) %>% 
  count() %>% ungroup() %>% mutate(perc=n/sum(n)) %>%
  arrange(perc) %>% mutate(labels=scales::percent(perc))
# Pie Chart
ggplot(group_rfs,aes(x="",y=perc,fill=as.factor(Rental_Fee_Status))) + 
  geom_bar(stat="identity") +
  geom_text(aes(x=1.8,label=labels),position=position_stack(vjust=0.5)) +
  coord_polar(theta="y") +
  theme_void() +
  ggtitle("Percentage of Tenant Choose House Based on Rental Fee") +
  scale_fill_discrete(labels=c("High = Rental Fee > Average Rental Fee",
                               "Low = Rental Fee < Average Rental Fee")) +
  guides(fill=guide_legend(title="Rental Fee Status")) +
  theme(plot.title=element_text(hjust=0.5)) 


# Analysis 1-8: Find the Percentage of Tenant Choose House Based on Furnishing_Status
# Calculate Percentage Grouped by Furnishing_Status
group_fs <- house_rental_data %>% group_by(Furnishing_Status) %>% 
  count() %>% ungroup() %>% mutate(perc=n/sum(n)) %>%
  arrange(perc) %>%
  mutate(labels=scales::percent(perc))
# Pie Chart
ggplot(group_fs,aes(x="",y=perc,fill=Furnishing_Status)) +
  geom_bar(stat="identity") +
  guides(fill=guide_legend(title="Furnishing Status")) +
  geom_text(aes(label=labels),position=position_stack(vjust=0.5)) +
  coord_polar(theta="y") +
  theme_void() +
  theme(legend.position = "bottom") +
  scale_fill_brewer(palette = "PiYG") +
  ggtitle("Percentage of Tenant Choose House Based on Furnishing Status")


# Analysis 1-9: Find the Percentage of Tenant Choose House Based on Area Type
# Calculate Percentage Grouped by Area_Type
group_at <- house_rental_data %>% group_by(Area_Type) %>%
  count() %>% ungroup() %>% mutate(perc=n/sum(n)) %>%
  arrange(perc) %>% mutate(labels=scales::percent(perc))
# Pie Chart
ggplot(group_at,aes(x="",y=perc,fill=Area_Type)) +
  geom_bar(stat="identity") +
  guides(fill=guide_legend(title="Area Type")) +
  geom_text(aes(x=1.6,label=labels),position=position_stack(vjust=0.5)) +
  coord_polar(theta="y") +
  theme_void() +
  ggtitle("Percentage of Tenant Choose House Based on Area Type")


# Analysis 1-10: Find the Percentage of Tenant Choose House Based on City
# Calculate Percentage Grouped by City
group_city <- house_rental_data %>% 
  group_by(City) %>%
  count() %>% ungroup() %>% mutate(perc=n/sum(n)) %>% 
  arrange(perc) %>% mutate(labels=scales::percent(perc))
# Bar Chart
ggplot(group_city,aes(x=City,y=perc,fill=City,label=labels)) +
  geom_bar(position="stack",stat="identity",width=0.7) +
  geom_text(aes(label=labels),vjust=-0.5) +
  scale_y_continuous(labels=scales::percent) +
  labs(x="City",y="Percentage",
       title="Percentage of Tenant Choose House Based on City")



# Question 2: What are the Factors influencing Tenants to Choose Their Houses with respect to Date_Posted/Month?
# Convert Numeric to month names
house_rental_data$Month <- month.abb[house_rental_data$Month]

# Analysis 2-1: Find the Relationship between Date_Posted and Number of Bedroom_Hall_Kitchen
ggplot(house_rental_data,aes(x=Bedroom_Hall_Kitchen,fill=Month)) +
  geom_bar(aes(y=after_stat(count)),stat="count") +
  labs(x="Number of Bedroom, Hall and Kitchen",y="Count",
       title="Relationship between Date_Posted and Number of Bedroom_Hall_Kitchen") +
  geom_text(aes(label=after_stat(count),
                y=after_stat(count)),stat="count",vjust=-0.4,size=4) +
  scale_fill_discrete(breaks=c('Apr','May','Jun','Jul')) +
  ylim(0,800) +
  theme_bw() +
  theme(legend.position = "bottom") +
  facet_grid(factor(Month,levels=c('Apr','May','Jun','Jul'))~Tenant_Type) 


# Analysis 2-2: Find the Relationship between Date_Posted and Number_of_Bathroom
ggplot(house_rental_data,aes(x=Number_of_Bathroom,fill=Month)) +
  geom_bar(aes(y=after_stat(count)),stat="count") +
  labs(x="Number of Bathroom",y="Count",
       title="Relationship between Month and Number_of_Bathroom") +
  geom_text(aes(label=after_stat(count),
                y=after_stat(count)),stat="count",vjust=-0.4,size=4) +
  scale_fill_discrete(breaks=c('Apr','May','Jun','Jul')) +
  ylim(0,800) +
  theme_bw() +
  theme(legend.position = "bottom") +
  facet_grid(factor(Month,levels=c('Apr','May','Jun','Jul'))~Tenant_Type) 


# Analysis 2-3: Find the Relationship between Month and Floor_Preference
# Bar Chart
ggplot(house_rental_data,aes(x=Floor_Preference,fill=Month)) +
  geom_bar(stat = "count") +
  labs(x="Floor Preference",y="Count",
       title="Relationship between Month and Floor_Preference") +
  scale_fill_discrete(breaks=c('Apr','May','Jun','Jul')) +
  theme_bw() +
  theme(legend.position = "bottom") +
  facet_grid(factor(Month,levels=c('Apr','May','Jun','Jul'))~Tenant_Type)


# Analysis 2-4: Find the Relationship between Month and House_Size
# Bar Chart
ggplot(house_rental_data,
       aes(x=Month,y=House_Size,fill=Month)) +
  geom_bar(aes(factor(Month,levels=c('Apr','May','Jun','Jul')),House_Size),
           position="dodge",stat="summary",fun="mean") +
  stat_summary(aes(label=round(after_stat(y),2)),fun="mean",geom="text",vjust=-0.5,size=3) +
  labs(x="Month",y="House Size",
       title="Relationship between Month and House_Size") +
  scale_fill_discrete(breaks=c('Apr','May','Jun','Jul')) +
  scale_x_discrete(breaks=c('Apr','May','Jun','Jul')) +
  theme_bw() +
  theme(legend.position = "bottom") +
  facet_wrap(~Tenant_Type) 


# Analysis 2-5: Find the Relationship between Month and Rental_Fee
# Bar Chart
ggplot(house_rental_data,aes(x=Month,y=Rental_Fee,fill=Month)) +
  geom_bar(aes(factor(Month,levels=c('Apr','May','Jun','Jul')),Rental_Fee),
           position="dodge",stat="summary",fun="mean") +
  stat_summary(aes(label=round(after_stat(y),2)),fun="mean",geom="text",vjust=-0.5,size=3) +
  labs(x="Month",y="Average Monthly Rental Fee (RM)",
       title="Relationship between Month and Rental_Fee") +
  scale_y_continuous(labels=scales::comma) +
  theme_bw() +
  scale_fill_manual(values=c('#f6e8c3','#5ab4ac','#c7eae5','#d8b365')) +
  facet_wrap(~Tenant_Type)


# Analysis 2-6: Find the Relationship between Date_Posted and Furnishing_Status
# Bar Chart
ggplot(house_rental_data,aes(x=Date_Posted)) +
  geom_bar(aes(y=after_stat(count)),stat="bin",binwidth=0.5) +
  geom_text(aes(label=after_stat(count)),stat="count",vjust=-0.5,size=2) +
  labs(x="Date Posted",y="Count",
       title="Relationship between Date_Posted and Furnishing_Status") +
  scale_x_date(breaks=date_breaks("2 day"),labels=date_format("%d %b")) +
  theme_bw() +
  theme(axis.line=element_line(),axis.text.x=element_text(angle=90)) +
  facet_grid(Furnishing_Status~Tenant_Type,scales="free") 


# Analysis 2-7: Find the Relationship between Date_Posted and Area_Type
ggplot(house_rental_data,aes(x=Date_Posted)) +
  geom_bar(aes(y=after_stat(count)),stat="bin",binwidth=0.5) +
  geom_text(aes(label=after_stat(count)),stat="count",vjust=-0.5,size=2) +
  labs(x="Date Posted",y="Count",
       title="Relationship between Date_Posted and Area_Type") +
  scale_x_date(breaks=date_breaks("2 day"),labels=date_format("%d %b")) +
  theme_bw() +
  theme(axis.line=element_line(),axis.text.x=element_text(angle=90)) +
  facet_grid(Area_Type~Tenant_Type,scales="free") 


# Analysis 2-8: Find the Relationship between Month and City
# Count of Tenants Group by Tenant_Type and City
print(house_rental_data %>% group_by(Tenant_Type,City) %>% count(),n=25)
# Bar Chart
ggplot(house_rental_data,aes(x=City,fill=City)) +
  geom_bar(stat="count",position="dodge") +
  geom_text(aes(label=after_stat(count)),stat="count",vjust=-0.5,size=2) +
  labs(x="City",y="Count",
       title="Relationship between Month and City") +
  scale_fill_discrete(breaks=c('Apr','May','Jun','Jul')) +
  theme_bw() +
  theme(legend.position = "bottom") +
  facet_grid(factor(Month,levels=c('Apr','May','Jun','Jul'))~Tenant_Type) 



# Question 3: What are the Factors influencing Tenants to Choose Their Houses with respect to Bedroom_Hall_Kitchen?
# Analysis 3-1: Find the Relationship between Number of Bedroom, Hall, Kitchen and Number of Bathroom
# Bedroom_Hall_Kitchen and Number_of_Bathroom for Bachelors
house_rental_data %>% 
  filter(Tenant_Type=="Bachelors") %>%
  group_by(Bedroom_Hall_Kitchen,Number_of_Bathroom) %>%
  summarise(number_cases=n())
# Bedroom_Hall_Kitchen and Number_of_Bathroom for Bachelors/Family
print(house_rental_data %>% 
        filter(Tenant_Type=="Bachelors/Family") %>%
        group_by(Bedroom_Hall_Kitchen,Number_of_Bathroom) %>%
        summarise(number_cases=n()),n=30)
# Bedroom_Hall_Kitchen and Number_of_Bathroom for Family
house_rental_data %>% 
  filter(Tenant_Type=="Family") %>%
  group_by(Bedroom_Hall_Kitchen,Number_of_Bathroom) %>%
  summarise(number_cases=n())

# calculate Percentage Grouped by Tenant_Type, Bedroom_Hall_Kitchen and Number_of_Bathroom
group_tt_bhk_nob <- house_rental_data %>%
  group_by(Tenant_Type,Bedroom_Hall_Kitchen,Number_of_Bathroom) %>%
  summarise(number_cases=n()) 
# Scatter Plot
ggplot(group_tt_bhk_nob,aes(x=Bedroom_Hall_Kitchen,y=Number_of_Bathroom)) +
  geom_point(aes(size=number_cases,color=number_cases)) +
  labs(x="Number of Bedroom, Hall and Kitchen",y="Number of Bathroom") +
  ggtitle("Relationship between Bedroom_Hall_Kitchen and Number_of_Bathroom") +
  scale_x_continuous(breaks=seq(1,6,1)) +
  scale_y_continuous(breaks=seq(1,10,1)) +
  scale_color_gradient(low="#2c7fb8",high="pink") +
  facet_wrap(~Tenant_Type)


# Analysis 3-2: Find the Relationship between Number of Bedroom, Hall, Kitchen and Floor Preference
# Box Plot
ggplot(house_rental_data,aes(x=Bedroom_Hall_Kitchen,y=Floor_Preference)) +
  geom_boxplot(aes(x=factor(Bedroom_Hall_Kitchen),
                   y=Floor_Preference,color=factor(Bedroom_Hall_Kitchen)))+
  labs(x= "Number of Bedroom, Hall and Kitchen",y="Floor Preferance",
       color="Number of Bedroom, Hall and Kitchen",
       title="Relationship between Number of Bedroom, Hall, Kitchen and Floor Preference") +
  scale_fill_discrete(name="Number of Bedroom, Hall and Kitchen") +
  facet_wrap(~Tenant_Type) +
  theme(legend.position = "bottom")


# Analysis 3-3: Find the Relationship between Number of Bedroom, Hall, Kitchen and House Size
# Count Number of Tenants Choose House Based on Bedroom_Hall_Kitchen
house_rental_data %>% 
  group_by(Tenant_Type,Bedroom_Hall_Kitchen) %>% 
  count()
# Violin Plot with Box Plot
ggplot(house_rental_data,aes(x=House_Size,
                             y=factor(Bedroom_Hall_Kitchen),
                             color=factor(Bedroom_Hall_Kitchen))) +
  geom_violin() +
  geom_boxplot(width=0.1) +
  scale_x_continuous(breaks=seq(0,8000,500)) +
  scale_y_discrete(breaks=seq(1,6,1)) +
  facet_wrap(~Tenant_Type) +
  theme(legend.position = "bottom") +
  labs(x="House Size",y="Number of Bedroom, Hall and Kitchen",
       color="Number of Bedroom, Hall and Kitchen",
       title="Relationship between Bedroom_Hall_Kitchen and House_Size")


# Analysis 3-4: Find the Relationship between Number of Bedroom, Hall, Kitchen and Rental Fee

# Violin Plot with Box Plot
ggplot(rf_no_outliers,aes(x=Bedroom_Hall_Kitchen,
                          y=Rental_Fee,
                          color=factor(Bedroom_Hall_Kitchen))) +
  geom_violin() +
  geom_boxplot(width=0.1) +
  scale_x_continuous(breaks=seq(1,6,1)) +
  scale_y_continuous(labels=scales::comma) +
  facet_wrap(~Tenant_Type) +
  theme(legend.position = "bottom") +
  labs(x="Number of Bedroom, Hall and Kitchen",y="Rental Fee",
       color="Number of Bedroom, Hall and Kitchen",
       title="Relationship between Number of Bedroom_Hall_Kitchen and Rental_Fee")


# Analysis 3-5: Find the Relationship between Number of Bedroom, Hall, Kitchen and Furnishing Status
# Calculate Percentage Grouped by Tenant_Type, Bedroom_Hall_Kitchen and Furnishing_Status
group_tt_bhk_fs <- house_rental_data %>% 
  group_by(Tenant_Type,Bedroom_Hall_Kitchen,Furnishing_Status) %>% 
  count() %>% ungroup() %>% mutate(perc=n/sum(n)) %>% arrange(perc) %>% 
  mutate(labels=scales::percent(perc))
# Bar Chart
group_tt_bhk_fs %>% ggplot(aes(Bedroom_Hall_Kitchen,perc,fill=Furnishing_Status)) +
  geom_bar(stat="identity",position="dodge") +
  facet_wrap(~Tenant_Type) +
  geom_text(aes(label=labels),size=2,vjust=-0.3,position=position_dodge(width=1)) +
  labs(x="Number of Bedroom, Hall and Kitchen",y="Percentage",
       title="Relationship between Number of Bedroom_Hall_Kitchen and Furnishing Status") +
  scale_x_continuous(breaks=seq(1,6,1)) +
  scale_y_continuous(labels=scales::percent) +
  theme(legend.position="bottom",
        legend.background = element_rect(fill="antiquewhite")) +
  scale_fill_discrete(name="Furnishing Status")


# Analysis 3-6: Find the Relationship between Number of Bedroom, Hall, Kitchen and Area Type
# Calculate Percentage Grouped by Bedroom_Hall_Kitchen and Area_Type
group_tt_bhk_at <- house_rental_data %>% 
  group_by(Tenant_Type,Bedroom_Hall_Kitchen,Area_Type) %>% 
  count() %>% ungroup() %>% mutate(perc=n/sum(n)) %>% 
  arrange(perc) %>% mutate(labels=scales::percent(perc))
# Bar Chart
ggplot(group_tt_bhk_at,aes(x=Bedroom_Hall_Kitchen,y=perc,fill=Area_Type)) +
  geom_bar(stat="identity",position="dodge") +
  geom_text_repel(aes(label=labels),
                  position=position_dodge(width=0.1),size=3,max.overlaps = 20) +
  labs(x="Number of Bedroom, Hall and Kitchen",y="Percentage",
       title="Relationship between Number of Bedroom_Hall_Kitchen and Area Type") +
  theme(legend.position="bottom",
        legend.background = element_rect(fill="antiquewhite")) +
  scale_fill_discrete(name="Area Type") +
  scale_x_continuous(breaks=seq(1,6,1)) + 
  scale_y_continuous(labels=scales::percent) +
  facet_wrap(~Tenant_Type)


# Analysis 3-7: Find the Relationship between Number of Bedroom, Hall, Kitchen and City
# Calculate Percentage Grouped by Tenant_Type, Bedroom_Hall_Kitchen and City
group_tt_bhk_c <- house_rental_data %>% 
  group_by(Bedroom_Hall_Kitchen,City,Tenant_Type) %>% 
  count()  %>%
  mutate(number_cases=n)
# Bar Chart
group_tt_bhk_c %>% ggplot(aes(Bedroom_Hall_Kitchen,number_cases,fill=City)) +
  geom_bar(stat="identity",position="dodge") +
  geom_text(aes(label=number_cases),size=3,position=position_dodge(width=0.5),vjust=-0.2) +
  labs(x="Number of Bedroom, Hall and Kitchen",y="Count",
       title="Relationship between Number of Bedroom_Hall_Kitchen and City") +
  scale_x_continuous(breaks=seq(1,6,1)) +
  scale_fill_discrete(name="City") +
  theme(legend.position="bottom",
        legend.background = element_rect(fill="antiquewhite")) +
  facet_grid(Tenant_Type~City)


# Analysis 3-8: Find the Relationship between Number of Bedroom, Hall, Kitchen and Point of Contact
# Calculate Percentage Grouped by Bedroom_Hall_Kitchen, Point_of_Contact and Tenant_Type
group_tt_bhk_poc <- house_rental_data %>% 
  group_by(Tenant_Type,Bedroom_Hall_Kitchen,Point_of_Contact) %>% 
  count() %>% ungroup() %>% mutate(perc=n/sum(n)) %>% arrange(perc) %>%
  mutate(labels=scales::percent(perc))
# Bar Chart
ggplot(group_tt_bhk_poc,aes(x=Bedroom_Hall_Kitchen,y=perc,fill=factor(Point_of_Contact))) +
  geom_bar(stat="identity",position="dodge") +
  geom_text(aes(label=labels),size=3,vjust=-0.3,position=position_dodge(width=1)) +
  labs(x="Number of Bedroom, Hall and Kitchen",y="Percentage",
       title="Relationship between Number of Bedroom_Hall_Kitchen and Point_of_Contact") +
  scale_y_continuous(labels=scales::percent) +
  scale_fill_discrete(name="Point of Contact") +
  theme(legend.position="bottom",
        legend.background = element_rect(fill="antiquewhite")) +
  facet_grid(Point_of_Contact~Tenant_Type)


# Analysis 3-9: Find the Relationship between Number of Bedroom_Hall_Kitchen, Number_of_Bathroom and City
# Calculate Percentage Grouped by Tenant_Type, Bedroom_Hall_Kitchen, Number_of_Bathroom and City
group_tt_bhk_nob_c <- house_rental_data %>%
  group_by(Tenant_Type,Bedroom_Hall_Kitchen,Number_of_Bathroom,City) %>%
  summarise(number_cases=n())
# Bubble Plot
ggplot(group_tt_bhk_nob_c,aes(x=Bedroom_Hall_Kitchen,y=Number_of_Bathroom,color=number_cases)) +
  geom_point(aes(size=number_cases),alpha=0.5) +
  labs(x="Number of Bedroom, Hall and Kitchen",y="Number of Bathroom",
       title="Relationship between Number of Bedroom, Hall, Kitchen, Number of Bathroom and City") +
  scale_x_continuous(breaks=seq(1,6,1)) +
  scale_y_continuous(breaks=seq(1,10,1)) +
  facet_grid(Tenant_Type~City) +
  theme(legend.position = "bottom")



# Question 4: What are the Factors influencing Tenants to Choose Their Houses with respect to Number of Bathroom?
# Analysis 4-1: Find the Relationship between Number of Bathroom and Floor Preference
# Box Plot
ggplot(house_rental_data,aes(x=Number_of_Bathroom,y=Floor_Preference,color=factor(Number_of_Bathroom))) +
  geom_boxplot(aes(x=Number_of_Bathroom,y=Floor_Preference,group=Number_of_Bathroom)) +
  labs(x="Number of Bathroom",y="Floor Preference") +
  scale_x_continuous(breaks=seq(1,10,1)) +
  scale_y_continuous(breaks=seq(-2,80,2)) +
  guides(color=guide_legend("Number of Bathroom",nrow=1)) +
  ggtitle("Relationship between Number of Bathroom and Floor Preference") +
  theme(legend.position = "bottom") +
  facet_wrap(~Tenant_Type)


# Analysis 4-2: Find the Relationship between Number of Bathroom and House Size
# Calculate Mean of House_Size Grouped by Tenant_Type and Number_of_Bathroom
group_tt_nob_hs <- house_rental_data %>% group_by(Tenant_Type,Number_of_Bathroom) %>%
  summarise(avg_house_size = mean(House_Size))
# Lollipop Graph
ggplot(group_tt_nob_hs,aes(x=Number_of_Bathroom,y=avg_house_size)) +
  geom_point(size=3,colour="black") +
  geom_segment(aes(x=Number_of_Bathroom,xend=Number_of_Bathroom,y=0,yend=avg_house_size)) +
  geom_text(aes(Number_of_Bathroom,avg_house_size,label=signif(avg_house_size,2),vjust=-0.6)) +
  scale_x_continuous(breaks=seq(1,10,1)) +
  labs(x="Number of Bathroom",y="Size of House",
       title="Relationship between Number of Bathroom and House Size") +
  facet_wrap(~Tenant_Type)


# Analysis 4-3: Find the Relationship between Number of Bathroom and Rental Fee
# Box Plot
ggplot(rf_no_outliers,aes(x=Number_of_Bathroom,y=Rental_Fee)) +
  geom_boxplot(aes(x=factor(Number_of_Bathroom),y=Rental_Fee,color=factor(Number_of_Bathroom))) +
  facet_wrap(~Tenant_Type) +
  labs(x= "Number of Bathroom",y="Rental Fee",
       title="Relationship between Number of Bathroom and Rental Fee") +
  scale_y_continuous(labels=scales::comma) +
  theme(legend.position="bottom") +
  scale_color_discrete(name="Number of Bathroom")


# Analysis 4-4: Find the Relationship between Number of Bathroom and Furnishing Status
# Calculate Percentage Grouped By Tenant_Type, Number_of_Bathroom and Furnishing_Status
group_tt_nob_fs <- house_rental_data %>% 
  group_by(Tenant_Type,Number_of_Bathroom,Furnishing_Status) %>% 
  count() %>% ungroup() %>% mutate(perc=n/sum(n)) %>% arrange(perc) %>% 
  mutate(labels=scales::percent(perc))
# Bar Chart
ggplot(group_tt_nob_fs,aes(factor(Number_of_Bathroom),perc,fill=factor(Number_of_Bathroom))) +
  geom_bar(stat="identity",position="dodge") +
  geom_text(aes(label=labels),size=3,vjust=-0.2,
            position=position_dodge(width=0.9)) +
  labs(x="Number of Bathroom",y="Percentage",
       title="Relationship between Number of Bathroom and Furnishing Status") +
  scale_y_continuous(labels=scales::percent) +
  scale_fill_discrete(name="Number of Bathroom") +
  theme(legend.position="bottom",
        legend.background = element_rect(fill="antiquewhite")) +
  facet_grid(Tenant_Type~Furnishing_Status)


# Analysis 4-5: Find the Relationship between Number of Bathroom and Area Type
# Calculate Percentage Grouped by Tenant_Type, Number_of_Bathroom and Area_Type
group_tt_nob_at <- house_rental_data %>% 
  group_by(Tenant_Type,Number_of_Bathroom,Area_Type) %>% 
  count() %>% ungroup() %>% mutate(perc=n/sum(n)) %>% arrange(perc) %>% 
  mutate(labels=scales::percent(perc))
# Bar Chart
ggplot(group_tt_nob_at,aes(Number_of_Bathroom,perc,fill=Area_Type)) +
  geom_bar(stat="identity",position="dodge") +
  geom_text_repel(aes(label=labels,group=Area_Type),
                  size=3,position=position_dodge(width=0.9),
                  max.overlaps = 30) +
  labs(x="Number of Bathroom",y="Percentage",
       title="Relationship between Number of Bathroom and Area Type") +
  scale_x_continuous(breaks=seq(1,10,1)) +
  scale_y_continuous(labels=scales::percent) +
  scale_fill_discrete(name="Area Type") +
  theme(legend.position="bottom",
        legend.background = element_rect(fill="antiquewhite")) +
  facet_wrap(~Tenant_Type)


# Analysis 4-6: Find the Relationship between Number of Bathroom and City
# Calculate Percentage Grouped by Tenant_Type, Number_of_Bathroom and City
group_tt_nob_c <- house_rental_data %>% 
  group_by(Tenant_Type,Number_of_Bathroom,City) %>% 
  count() %>% ungroup() %>% mutate(perc=n/sum(n)) %>% arrange(perc) %>% 
  mutate(labels=scales::percent(perc))
# Bar Chart
ggplot(group_tt_nob_c,aes(Number_of_Bathroom,perc,fill=Tenant_Type)) +
  geom_bar(stat="identity",position="dodge") +
  geom_text_repel(aes(label=labels,group=City),
                  size=3, position=position_dodge(width=0.9),
                  max.overlaps=30) +
  labs(x="Number of Bathroom",y="Percentage",
       title="Relationship between Number of Bathroom and City") +
  scale_x_continuous(breaks=seq(1,10,1)) +
  scale_y_continuous(labels=scales::percent) +
  scale_fill_discrete(name="Tenant Type") +
  theme(legend.position="bottom",
        legend.background = element_rect(fill="antiquewhite")) +
  facet_wrap(~City)


# Analysis 4-7: Find the Relationship between Number of Bathroom and Point of Contact
# calculate Percentage Grouped By Tenant_Type, Number_of_Bathroom and Point_of_Contact
group_tt_nob_poc <- house_rental_data %>% 
  group_by(Tenant_Type,Number_of_Bathroom,Point_of_Contact) %>% 
  count() %>% ungroup() %>% mutate(perc=n/sum(n)) %>% arrange(perc) %>%
  mutate(labels=scales::percent(perc))
# Bar Chart
ggplot(group_tt_nob_poc,aes(x=Number_of_Bathroom,y=perc,fill=Point_of_Contact)) +
  geom_bar(stat="identity",position="dodge") +
  geom_text(aes(label=labels),size=3,vjust=-0.3,position=position_dodge(width=0.9)) +
  labs(x="Number of Bathroom",y="Percentage",
       title="Relationship between Number of Bathroom and Point_of_Contact") +
  scale_x_continuous(breaks=seq(1,10,1)) +
  scale_y_continuous(labels=scales::percent) +
  scale_fill_discrete(name="Point of Contact") +
  theme(legend.position="bottom",
        legend.background = element_rect(fill="antiquewhite")) +
  facet_grid(Point_of_Contact~Tenant_Type)


# Analysis 4-8: Find the Relationship between Number_of_Bathroom, Floor_Preference and City
# Box Plot
ggplot(house_rental_data,aes(x=factor(Number_of_Bathroom),y=Floor_Preference,
                             color=factor(Number_of_Bathroom))) +
  geom_boxplot(aes(x=factor(Number_of_Bathroom),y=Floor_Preference)) +
  labs(x="Number of Bathroom",y="Floor Preference", color="Number of Bathroom",
       title="Relationship between Number_of_Bathroom, Floor_Preference and City") +
  facet_grid(Tenant_Type~City)



# Question 5: How does Floor Preference Influence Tenants to Choose Their Houses?
# Analysis 5-1: Find the Relationship between Floor Preference and House Size
# Bar Chart
ggplot(house_rental_data,aes(x=Floor_Preference,y=House_Size,fill=Floor_Preference)) +
  geom_bar(stat="summary") +
  labs(x="Floor Preference",y="Average House Size (sqft)",color="Floor Preference") +
  ggtitle("Relationship between Floor Preference and House Size") +
  scale_x_continuous(breaks = seq(-2,80,4)) +
  scale_y_continuous(labels=scales::comma) +
  facet_wrap(~Tenant_Type)


# Analysis 5-2: Find the Relationship between Floor Preference and Rental Fee
# Combination of Scatter and Line Graph
ggplot(rf_no_outliers,aes(x=Floor_Preference,y=Rental_Fee,color=Tenant_Type)) + 
  geom_line(stat="summary") +
  geom_point(stat="summary") +
  facet_wrap(~Tenant_Type) +
  scale_x_continuous(breaks=seq(-2,80,2)) +
  scale_y_continuous(labels = scales::comma) +
  labs(x="Floor Preference",y="Average Rental Fee (RM)") +
  ggtitle("Relationship between Rental_Fee and Floor_Preference")


# Analysis 5-3: Find the Relationship between Floor Preference and Furnishing Status
# Box Plot
ggplot(house_rental_data,aes(x=Floor_Preference,y=Furnishing_Status)) +
  geom_boxplot(aes(x=Floor_Preference,y=Furnishing_Status,color=Furnishing_Status))+
  labs(x= "Floor Preference",y="Furnishing Status",
       color="Furnishing Status",
       title="Relationship between Floor Preference and Furnishing Status") +
  scale_x_continuous(breaks=seq(-2,80,4)) +
  theme(legend.position = "bottom") +
  facet_wrap(~Tenant_Type)


# Analysis 5-4: Find the Relationship between Floor Preference and Area Type
# Box Plot
ggplot(house_rental_data,aes(x=Floor_Preference,y=Area_Type)) +
  geom_boxplot(aes(x=Floor_Preference,y=Area_Type,color=Area_Type))+
  labs(x= "Floor Preference",y="Area Type",
       color="Area Type",
       title="Relationship between Floor Preference and Number of Bedroom, Hall, Kitchen") +
  scale_x_continuous(breaks=seq(-2,80,4)) +
  theme(legend.position = "bottom") +
  facet_wrap(~Tenant_Type)


# Analysis 5-5: Find the Relationship between Floor Preference and City
# Count Number of Tenants in Each City
house_rental_data %>%
  group_by(Tenant_Type,City) %>%
  count()
# Violin Plot with Box Plot
ggplot(house_rental_data,aes(x=Floor_Preference,y=City)) +
  geom_violin(aes(x=Floor_Preference,y=City,color=City))+
  geom_boxplot(width=0.05) +
  labs(x= "Floor Preference",y="City",
       color="City",
       title="Relationship between Floor Preference and City") +
  scale_x_continuous(breaks=seq(-2,80,4)) +
  theme(legend.position = "bottom") +
  facet_wrap(~Tenant_Type)


# Analysis 5-6: Find the Relationship between Floor Preference and Point of Contact
# Count Number of Tenants Group by Point of Contact
house_rental_data %>%
  group_by(Tenant_Type,Point_of_Contact) %>%
  count()
# Violin Plot with Box Plot
ggplot(house_rental_data,aes(x=Floor_Preference,y=Point_of_Contact)) +
  geom_violin(aes(x=Floor_Preference,y=Point_of_Contact,color=Point_of_Contact)) +
  geom_boxplot(width=0.05) +
  labs(x= "Floor Preference",y="Point of Contact",
       color="Point of Contact",
       title="Relationship between Floor Preference and Point of Contact") +
  scale_x_continuous(breaks=seq(-2,80,4)) +
  theme(legend.position = "bottom") +
  facet_wrap(~Tenant_Type)


# Analysis 5-7: Find the Relationship between Floor_Preference, Number of Bedroom_Hall_Kitchen and Furnishing_Status
# Count Number of Tenants choose house based on Bedroom_Hall_Kitchen and Furnishing_Status
# By Bachelors
house_rental_data %>% filter(Tenant_Type=="Bachelors") %>%
  group_by(Tenant_Type,Bedroom_Hall_Kitchen,Furnishing_Status) %>% 
  count()
# By Bachelors/Family
house_rental_data %>% filter(Tenant_Type=="Bachelors/Family") %>%
  group_by(Tenant_Type,Bedroom_Hall_Kitchen,Furnishing_Status) %>% 
  count()
# By Family
house_rental_data %>% filter(Tenant_Type=="Family") %>%
  group_by(Tenant_Type,Bedroom_Hall_Kitchen,Furnishing_Status) %>% 
  count()

# Box Plot
ggplot(house_rental_data,aes(x=Floor_Preference,y=Bedroom_Hall_Kitchen)) +
  geom_boxplot(aes(x=Floor_Preference,y=factor(Bedroom_Hall_Kitchen),color=Furnishing_Status))+
  labs(x= "Floor Preferance",y="Number of Bedroom, Hall and Kitchen",
       color="Furnishing Status",
       title="Relationship between Floor_Preference, Number of Bedroom_Hall_Kitchen and Furnishing_Status") +
  scale_x_continuous(breaks=seq(-2,80,4)) +
  scale_fill_discrete(name="Number of Bedroom, Hall and Kitchen") +
  theme(legend.position = "bottom") +
  facet_grid(Tenant_Type~Furnishing_Status)


# Analysis 5-8: Find the Relationship between Floor_Preference, Number_of_Bathroom and Area_Type
# Count Number of Tenants choose house based on Number_of_Bathroom and Area_Type
# By Bachelors
house_rental_data %>% filter(Tenant_Type=="Bachelors") %>%
  group_by(Tenant_Type,Number_of_Bathroom,Area_Type) %>% 
  count()
# By Bachelors/Family
house_rental_data %>% filter(Tenant_Type=="Bachelors/Family") %>%
  group_by(Tenant_Type,Number_of_Bathroom,Area_Type) %>% 
  count()
# By Family
house_rental_data %>% filter(Tenant_Type=="Family") %>%
  group_by(Tenant_Type,Number_of_Bathroom,Area_Type) %>% 
  count()

# Box Plot
ggplot(house_rental_data,aes(x=Floor_Preference,y=Number_of_Bathroom)) +
  geom_boxplot(aes(x=Floor_Preference,y=factor(Number_of_Bathroom),color=Area_Type))+
  labs(x= "Floor Preferance",y="Number of Bathroom",
       color="Area Type",
       title="Relationship between Floor_Preference, Number_of_Bathroom and Area_Type") +
  scale_x_continuous(breaks=seq(-2,80,4)) +
  scale_y_discrete(breaks=seq(1,10,1)) +
  theme(legend.position = "bottom") +
  facet_grid(Tenant_Type~Area_Type)



# Question 6: How Does House Size Influence Tenants to Choose Their Houses?
# Analysis 6-1: Find the Relationship between House Size and Rental Fee
# Point Graph
ggplot(rf_no_outliers,aes(x=House_Size,y=Rental_Fee)) + 
  geom_point(stat="summary",fun="mean") +
  geom_smooth(method="lm") +
  facet_wrap(~Tenant_Type) +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) +
  labs(x="Size of House",y="Rental Fee (RM)") +
  ggtitle("Relationship between House_Size and Rental_Fee")


# Analysis 6-2: Find the Relationship between House Size and Furnishing Status
# Box Plot
ggplot(house_rental_data,aes(x=House_Size,y=Furnishing_Status)) +
  geom_violin(aes(x=House_Size,y=Furnishing_Status,color=Furnishing_Status)) +
  geom_boxplot(width=0.05,aes(color=Furnishing_Status)) +
  labs(x= "House Size",y="Furnishing Status",
       color="Furnishing Status",
       title="Relationship between House Size and Furnishing Status") +
  scale_x_continuous(breaks=seq(0,8000,500))+
  facet_wrap(~Tenant_Type) +
  theme(legend.position = "bottom")


# Analysis 6-3: Find the Relationship between House Size and Area Type
# Calculate Number of Tenants Choose Houses Based on Area_Type
house_rental_data %>% 
  group_by(Tenant_Type,Area_Type) %>% count()
# Box Plot
ggplot(house_rental_data,aes(x=House_Size,y=Area_Type)) +
  geom_boxplot(aes(x=House_Size,y=Area_Type,color=Area_Type)) +
  labs(x= "House Size",y="Area Type",
       color="Area Type",
       title="Relationship between House Size and Area Type") +
  scale_x_continuous(breaks=seq(0,8000,500))+
  facet_wrap(~Tenant_Type) +
  theme(legend.position = "bottom")


# Analysis 6-4: Find the Relationship between House Size and City
# Calculate Number of Tenants Choose Houses Based on City
house_rental_data %>% 
  group_by(Tenant_Type,City) %>% count()
# Violin Plot with Box Plot
ggplot(house_rental_data,aes(x=House_Size,y=City,color=City)) +
  geom_violin() +
  geom_boxplot(width=0.05) +
  labs(x= "House Size",y="City",
       color="City",
       title="Relationship between House Size and City") +
  scale_x_continuous(breaks=seq(0,8000,500)) +
  theme(legend.position = "bottom") + 
  facet_wrap(~Tenant_Type)


# Analysis 6-5: Find the Relationship between House Size and Point of Contact
# Count Number of Tenants Grouped by Point_Of_Contact
house_rental_data %>% 
  group_by(Tenant_Type,Point_of_Contact) %>%
  count()
# Box Plot
ggplot(house_rental_data,aes(x=House_Size,y=Point_of_Contact,color=Point_of_Contact)) +
  geom_violin() +
  geom_boxplot(width=0.05)+
  labs(x= "Average House Size",y="Point of Contact",
       color="Point of Contact",
       title="Relationship between House Size and Point of Contact") +
  scale_x_continuous(breaks=seq(0,8000,500)) +
  facet_wrap(~Tenant_Type) +
  theme(legend.position = "bottom")


# Analysis 6-6: Find the Relationship between House_Size, Number of Bedroom_Hall_Kitchen and Furnishing_Status
# Violin Plot with Box Plot
ggplot(house_rental_data,aes(x=House_Size,y=factor(Bedroom_Hall_Kitchen),
                             color=factor(Bedroom_Hall_Kitchen))) +
  geom_violin() +
  geom_boxplot(width=0.05) +
  labs(x= "House Size",y="Number of Bedroom, Hall and Kitchen",
       color="Number of Bedroom, Hall and Kitchen",
       title="Relationship between House_Size, Number of Bedroom_Hall_Kitchen and Furnishing_Status") +
  scale_x_continuous(breaks=seq(0,8000,500)) +
  theme(legend.position = "bottom") + 
  facet_grid(Tenant_Type~Furnishing_Status)


# Analysis 6-7: Find the Relationship between House_Size, Number of Bedroom_Hall_Kitchen and Area_Type
# Violin Plot with Box Plot
ggplot(house_rental_data,aes(x=House_Size,y=factor(Bedroom_Hall_Kitchen),
                             color=factor(Bedroom_Hall_Kitchen))) +
  geom_violin() +
  geom_boxplot(width=0.05) +
  labs(x= "House Size",y="Number of Bedroom, Hall and Kitchen",
       color="Number of Bedroom, Hall and Kitchen",
       title="Relationship between House_Size, Number of Bedroom_Hall_Kitchen and Area_Type") +
  scale_x_continuous(breaks=seq(0,8000,500)) +
  theme(legend.position = "bottom") + 
  facet_grid(Tenant_Type~Area_Type)


# Analysis 6-8: Find the Relationship between House_Size, Number_of_Bathroom and Furnishing_Status
# Violin Plot with Box Plot
ggplot(house_rental_data,aes(x=House_Size,y=factor(Number_of_Bathroom),
                             color=factor(Number_of_Bathroom))) +
  geom_violin() +
  geom_boxplot(width=0.05) +
  labs(x= "House Size",y="Number of Bathroom",
       color="Number of Bathroom",
       title="Relationship between House_Size, Number_of_Bathroom and Furnishing_Status") +
  scale_x_continuous(breaks=seq(0,8000,500)) +
  theme(legend.position = "bottom") + 
  facet_grid(Tenant_Type~Furnishing_Status)



# Question 7: How does Rental Fee Influence Tenants to Choose Their Houses??
# Analysis 7-1: Find the Relationship between Rental Fee and Furnishing Status
# Count Number of Tenants Based on Furnishing_Status
house_rental_data %>% 
  group_by(Tenant_Type,Furnishing_Status) %>% 
  count()
# Violin Plot with Box Plot to Show Tenant Preference
ggplot(rf_no_outliers,aes(x=Rental_Fee,y=Furnishing_Status,
                          color=Furnishing_Status)) +
  geom_violin() +
  geom_boxplot(width=0.05) +
  labs(x= "Rental Fee",y="Furnishing Status",
       color="Furnishing Status",
       title="Relationship between Rental Fee and Furnishing Status") +
  scale_x_continuous(breaks=seq(0,70000,5000),
                     labels = label_number(suffix = "k", scale = 1e-3)) +
  theme(legend.position = "bottom") + 
  facet_wrap(~Tenant_Type)


# Analysis 7-2: Find the Relationship between Rental Fee and Area Type
# Count Number of Tenants by Tenant_Type 
# Based on Area Type
house_rental_data %>%
  group_by(Tenant_Type,Area_Type)%>%
  count()
# Bar Chart
ggplot(rf_no_outliers,aes(factor(Area_Type),Rental_Fee)) +
  geom_bar(aes(Area_Type,Rental_Fee,fill=as.factor(Area_Type)),
           position="dodge",stat="summary",fun="mean",width=0.5) +
  stat_summary(aes(label=round(..y..,2)),fun="mean",geom="text",vjust=-0.3,size=4) +
  facet_wrap(~Tenant_Type) +
  scale_y_continuous(labels=scales::comma) +
  labs(x="Area Type",y="Average Rental Fee (RM)") +
  scale_fill_discrete(name="Area Type") +
  theme(legend.position="bottom") +
  ggtitle("Relationship between Rental_Fee and Area_Type")


# Analysis 7-3: Find the Relationship between Rental Fee and City
# Count Number of Tenants by Tenant_Type Based on City
house_rental_data %>% group_by(Tenant_Type,City) %>%
  count()
# Bar Chart
ggplot(rf_no_outliers,aes(factor(City),Rental_Fee)) +
  geom_bar(aes(City,Rental_Fee,fill=as.factor(City)),
           position="dodge",stat="summary",fun="mean") +
  stat_summary(aes(label=round(..y..,2)),fun.y="mean",geom="text",vjust=-0.5,size=3) +
  scale_y_continuous(labels=scales::comma) +
  labs(x="City",y="Average Rental Fee (RM)") +
  scale_fill_discrete(name="City") +
  facet_wrap(~Tenant_Type) +
  theme(legend.position = "bottom") +
  ggtitle("Relationship between Rental_Fee and City")


# Analysis 7-4: Find the Relationship between Rental Fee and Point of Contact
# Count Number of Tenants by Tenant_Type Based on Point_of_Contact
house_rental_data%>%group_by(Tenant_Type,Point_of_Contact)%>%count()
# Violin Plot with Box Plot
ggplot(rf_no_outliers,aes(x=Point_of_Contact,y=Rental_Fee)) +
  geom_violin(aes(x=Point_of_Contact,y=Rental_Fee,color=Point_of_Contact)) +
  geom_boxplot(width=0.05,aes(color=Point_of_Contact)) +
  labs(x= "Point of Contact",y="Rental Fee",
       color="Point of Contact",
       title="Relationship between Rental Fee and Point of Contact") +
  scale_y_continuous(breaks=seq(0,70000,5000),
                     labels = label_number(suffix = "k", scale = 1e-3)) +
  theme(legend.position = "bottom") + 
  facet_wrap(~Tenant_Type)


# Analysis 7-5: Find the Relationship between Rental Fee, Number of Bedroom_Hall_Kitchen and Furnishing_Status
# Bar Chart
ggplot(rf_no_outliers,aes(x=Bedroom_Hall_Kitchen,y=Rental_Fee,fill=Furnishing_Status)) +
  geom_bar(stat="summary") +
  stat_summary(aes(label=round(..y..,2)),size=3,fun.y="mean",geom="text",vjust=2) +
  scale_x_continuous(breaks=seq(1,6,1)) +
  scale_y_continuous(labels=scales::comma) +
  facet_grid(Tenant_Type~Furnishing_Status) +
  theme(legend.position = "bottom") +
  labs(x="Number of Bedroom, Hall and Kitchen",y="Average Rental Fee",
       color="Number of Bedroom, Hall and Kitchen",
       title="Relationship between Average Rental Fee, Number of Bedroom_Hall_Kitchen and Furnishing_Status")


# Analysis 7-6: Find the Relationship between Rental Fee, Number_of_Bathroom and Area_Type
# Bar Chart
ggplot(rf_no_outliers,aes(x=Number_of_Bathroom,y=Rental_Fee,fill=Area_Type)) +
  geom_bar(stat="summary") +
  stat_summary(aes(label=round(after_stat(y),2)),size=3,fun="mean",geom="text",vjust=2) +
  scale_x_continuous(breaks=seq(1,7,1)) +
  scale_y_continuous(labels=scales::comma) +
  facet_grid(Tenant_Type~Area_Type) +
  theme(legend.position = "bottom") +
  labs(x="Number of Bathroom",y="Average Rental Fee",
       color="Area Type",
       title="Relationship between Average Rental Fee, Number_of_Bathroom and Area_Type")


# Analysis 7-7: Find the Relationship between Rental_Fee, Floor_Preference and Area_Type
# Calculate Mean of Rental_Fee Grouped by Tenant_Type, Floor_Preference and Area_Type
group_tt_fp_at <- rf_no_outliers %>% group_by(Tenant_Type,Floor_Preference,Area_Type) %>%
  summarise(avg_rental_fee = mean(Rental_Fee))
# Lollipop Graph
ggplot(group_tt_fp_at,aes(x=avg_rental_fee,y=Floor_Preference,color=Area_Type)) +
  geom_point(size=2,colour="black") +
  geom_segment(aes(x=0,xend=avg_rental_fee,y=Floor_Preference,yend=Floor_Preference)) +
  labs(x="Average Rental Fee (RM)",y="Floor_Preference", color="Area Type",
       title="Relationship between Rental_Fee, Floor_Preference and Area_Type") +
  scale_x_continuous(breaks=seq(0,70000,5000),
                     labels = label_number(suffix = "k", scale = 1e-3)) +
  facet_grid(Tenant_Type~Area_Type) 


# 7.0 Extra Feature
# 7.1 Count Number of Unique Values in Each Column
# From 3.1 Step 1
count_unique <- sapply(house_rental_data,function(x) n_distinct(x))
count_unique


# 7.2 Drop Area.Locality Column
# From 3.1 Step 3
house_rental_data <- house_rental_data[,!names(house_rental_data) %in% c("Area.Locality")]
names(house_rental_data)

# 7.3 Format Date_Posted Column
# From 3.3 
house_rental_data$Date_Posted <- as.Date(house_rental_data$Date_Posted,"%m/%d/%Y")
View(head(house_rental_data))

# 7.4 Extract Month from Date_Posted
# From 3.4 
house_rental_data$Month <- with(house_rental_data,month(ymd(Date_Posted)))
View(head(select(house_rental_data,Date_Posted,Month)))

# 7.5 Split Floor using separate()
# From 3.5 Step 2
house_rental_data <- house_rental_data %>% separate(Floor, c("Floor_Preference","Total_Floor_Numbers")," out of ")
View(head(house_rental_data))

# 7.6 Check for Missing Values
# From 3.7 Step 4 
colSums(is.na(house_rental_data)) 


# 7.7 User Defined Function (outliers and remove_outliers)
# From 3.12 
outliers <- function(x) {
  Q1 <- quantile(x, probs=.25)
  Q3 <- quantile(x, probs=.75)
  iqr = Q3-Q1
  
  upper_limit = Q3 + (iqr*1.5)
  lower_limit = Q1 - (iqr*1.5)
  
  x > upper_limit | x < lower_limit
}

remove_outliers <- function(house_rental_data, cols = names(house_rental_data)) {
  for (col in cols) {
    house_rental_data <- house_rental_data[!outliers(house_rental_data[[col]]),]
  }
  house_rental_data
}
# Usage
rf_no_outliers <- remove_outliers(house_rental_data,c('Rental_Fee'))


# 7.8 geom_freqpoly()
# From Analysis 1.1
ggplot(house_rental_data,aes(x=Date_Posted)) +
  geom_freqpoly(bins=100) +
  xlab("Date Posted") +
  ylab("Number of House Based on Date Posted") +
  ggtitle("Count of Tenant CHoose House Based on Date_Posted") +
  theme_bw() +
  facet_wrap(~Tenant_Type)


# 7.9 coord_polar()
# From Analysis 1-2
ggplot(group_poc,aes(x="",y=perc,fill=Point_of_Contact)) +
  geom_bar(stat="identity") +
  guides(fill=guide_legend(title="Point of Contact")) +
  geom_text(aes(label=labels),position=position_stack(vjust=0.5)) +
  coord_polar(theta="y") +
  theme_void() +
  theme(legend.position = "bottom") +
  scale_fill_brewer() +
  ggtitle("Percentage of Tenant Choose House Based on Point of Contact")


# 7.10 scale_fill_brewer()
# From Analysis 1-2
ggplot(group_poc,aes(x="",y=perc,fill=Point_of_Contact)) +
  geom_bar(stat="identity") +
  guides(fill=guide_legend(title="Furnishing Status")) +
  geom_text(aes(label=labels),position=position_stack(vjust=0.5)) +
  coord_polar(theta="y") +
  theme_void() +
  theme(legend.position = "bottom") +
  scale_fill_brewer() +
  ggtitle("Percentage of Tenant Choose House Based on Point of Contact")

# 7.11 after_stat(prop)
# From Analysis 1-3
ggplot(house_rental_data,aes(x=Bedroom_Hall_Kitchen)) +
  geom_bar(aes(y=after_stat(prop),fill=factor(after_stat(x))),stat="count") +
  geom_text(aes(label=scales::percent(after_stat(prop)),
                y=after_stat(prop)),stat="count",vjust=-0.5) +
  labs(x="Number of Bedroom, Hall and Kitchen",y="Percentage",
       fill="Number of Bedroom, Hall and Kitchen") +
  scale_x_continuous(breaks=seq(1,6,1)) +
  scale_y_continuous(labels=scales::percent) +
  theme(legend.position="bottom",
        legend.background = element_rect(fill="antiquewhite")) +
  ggtitle("Percentage of Tenant Choose House Based on Number of Bedroom, Hall and Kitchen")


# 7.12 labels=scales::percent
# From Analysis 1-3
ggplot(house_rental_data,aes(x=Bedroom_Hall_Kitchen)) +
  geom_bar(aes(y=after_stat(prop),fill=factor(after_stat(x))),stat="count") +
  geom_text(aes(label=scales::percent(after_stat(prop)),
                y=after_stat(prop)),stat="count",vjust=-0.5) +
  labs(x="Number of Bedroom, Hall and Kitchen",y="Percentage",
       fill="Number of Bedroom, Hall and Kitchen") +
  scale_x_continuous(breaks=seq(1,6,1)) +
  scale_y_continuous(labels=scales::percent) +
  theme(legend.position="bottom",
        legend.background = element_rect(fill="antiquewhite")) +
  ggtitle("Percentage of Tenant Choose House Based on Number of Bedroom, Hall and Kitchen")


# 7.13 scale_x_continuous(breaks)
# From Analysis 1-3
ggplot(house_rental_data,aes(x=Bedroom_Hall_Kitchen)) +
  geom_bar(aes(y=after_stat(prop),fill=factor(after_stat(x))),stat="count") +
  geom_text(aes(label=scales::percent(after_stat(prop)),
                y=after_stat(prop)),stat="count",vjust=-0.5) +
  labs(x="Number of Bedroom, Hall and Kitchen",y="Percentage",
       fill="Number of Bedroom, Hall and Kitchen") +
  scale_x_continuous(breaks=seq(1,6,1)) +
  scale_y_continuous(labels=scales::percent) +
  theme(legend.position="bottom",
        legend.background = element_rect(fill="antiquewhite")) +
  ggtitle("Percentage of Tenant Choose House Based on Number of Bedroom, Hall and Kitchen")


# 7.14 scale_y_continuous(labels)
# From Analysis 1-3
ggplot(house_rental_data,aes(x=Bedroom_Hall_Kitchen)) +
  geom_bar(aes(y=after_stat(prop),fill=factor(after_stat(x))),stat="count") +
  geom_text(aes(label=scales::percent(after_stat(prop)),
                y=after_stat(prop)),stat="count",vjust=-0.5) +
  labs(x="Number of Bedroom, Hall and Kitchen",y="Percentage",
       fill="Number of Bedroom, Hall and Kitchen") +
  scale_x_continuous(breaks=seq(1,6,1)) +
  scale_y_continuous(labels=scales::percent) +
  theme(legend.position="bottom",
        legend.background = element_rect(fill="antiquewhite")) +
  ggtitle("Percentage of Tenant Choose House Based on Number of Bedroom, Hall and Kitchen")


# 7.15 theme()
# From Analysis 1-4
ggplot(house_rental_data,aes(x=Number_of_Bathroom)) +
  geom_bar(aes(y=after_stat(prop),fill=factor(after_stat(x))),stat="count",width=0.5) +
  geom_text(aes(label=scales::percent(after_stat(prop)),
                y=after_stat(prop)),stat="count",vjust=-0.4,size=4) +
  labs(x="Number of Bathroom",y="Percentage",fill="Number of Bathroom") +  
  scale_x_continuous(breaks=seq(1,10,1)) +
  scale_y_continuous(labels=scales::percent) +
  theme(legend.position="bottom",
        legend.background = element_rect(fill="lightyellow")) +
  ggtitle("Percentage of Tenants Choose House Based on Number of Bathroom")


# 7.16 scale_fill_gradient()
# From Analysis 1-5
# Calculate Percentage Grouped by Floor_Preference
group_fp <- house_rental_data %>% 
  group_by(Floor_Preference) %>%
  count() %>% ungroup() %>% mutate(perc=n/sum(n)) %>% 
  arrange(perc) %>% mutate(labels=scales::percent(perc))
# Bar Chart
ggplot(group_fp,aes(x=Floor_Preference,y=perc,fill=Floor_Preference,label=labels)) +
  geom_bar(stat="identity",width=0.7) +
  scale_x_continuous(breaks=seq(-2,80,5)) +
  scale_y_continuous(labels=scales::percent) +
  scale_fill_gradient(low="pink",high="purple") +
  labs(x="Floor Preference",y="Percentage",
       title="Percentage of Tenant Choose House Based on Floor Preference")


# 7.17 geom_text_repel(max.overlaps)
# From Analysis 1-6
# Calculate Percentage Grouped by House_Size
group_hs <- house_rental_data %>% group_by(House_Size) %>% count() %>% ungroup() %>%
  mutate(perc=n/sum(n)) %>%
  arrange(perc) %>%
  mutate(labels=scales::percent(perc))
# Scatter Plot
ggplot(group_hs,aes(x=House_Size,y=perc,label=labels)) +
  geom_point(aes(x=House_Size,y=perc)) +
  scale_x_continuous(breaks=seq(0,8000,500)) +
  scale_y_continuous(labels=scales::percent) +
  geom_text_repel(max.overlaps = 20) +
  ggtitle("Percentage of Tenant Choose House Based on Size of House") +
  labs(x="Size of House",y="Percentage")


# 7.18 scale_fill_discrete(labels)
# From Analysis 1-7
# Calculate Percentage Grouped by Rental_Fee_Status
group_rfs <- rf_no_outliers %>% group_by(Rental_Fee_Status) %>% 
  count() %>% ungroup() %>% mutate(perc=n/sum(n)) %>%
  arrange(perc) %>% mutate(labels=scales::percent(perc))
# Pie Chart
ggplot(group_rfs,aes(x="",y=perc,fill=as.factor(Rental_Fee_Status))) + 
  geom_bar(stat="identity") +
  geom_text(aes(x=1.8,label=labels),position=position_stack(vjust=0.5)) +
  coord_polar(theta="y") +
  theme_void() +
  ggtitle("Percentage of Tenant Choose House Based on Rental Fee") +
  scale_fill_discrete(labels=c("High = Rental Fee > Average Rental Fee",
                               "Low = Rental Fee < Average Rental Fee")) +
  guides(fill=guide_legend(title="Rental Fee Status")) +
  theme(plot.title=element_text(hjust=0.5)) 


# 7.19 theme(plot.title)
# From Analysis 1-7
# Calculate Percentage Grouped by Rental_Fee_Status
group_rfs <- rf_no_outliers %>% group_by(Rental_Fee_Status) %>% 
  count() %>% ungroup() %>% mutate(perc=n/sum(n)) %>%
  arrange(perc) %>% mutate(labels=scales::percent(perc))
# Pie Chart
ggplot(group_rfs,aes(x="",y=perc,fill=as.factor(Rental_Fee_Status))) + 
  geom_bar(stat="identity") +
  geom_text(aes(x=1.8,label=labels),position=position_stack(vjust=0.5)) +
  coord_polar(theta="y") +
  theme_void() +
  ggtitle("Percentage of Tenant Choose House Based on Rental Fee") +
  scale_fill_discrete(labels=c("High = Rental Fee > Average Rental Fee",
                               "Low = Rental Fee < Average Rental Fee")) +
  guides(fill=guide_legend(title="Rental Fee Status")) +
  theme(plot.title=element_text(hjust=0.5)) 


# 7.20 guides()
# From Analysis 1-8
# Calculate Percentage Grouped by Furnishing_Status
group_fs <- house_rental_data %>% group_by(Furnishing_Status) %>% 
  count() %>% ungroup() %>% mutate(perc=n/sum(n)) %>%
  arrange(perc) %>%
  mutate(labels=scales::percent(perc))
# Pie Chart
ggplot(group_fs,aes(x="",y=perc,fill=Furnishing_Status)) +
  geom_bar(stat="identity") +
  guides(fill=guide_legend(title="Furnishing Status")) +
  geom_text(aes(label=labels),position=position_stack(vjust=0.5)) +
  coord_polar(theta="y") +
  theme_void() +
  theme(legend.position = "bottom") +
  scale_fill_brewer(palette = "PiYG") +
  ggtitle("Percentage of Tenant Choose House Based on Furnishing Status")


# 7.21 theme_void()
# From Analysis 1-9
# Calculate Percentage Grouped by Area_Type
group_at <- house_rental_data %>% group_by(Area_Type) %>%
  count() %>% ungroup() %>% mutate(perc=n/sum(n)) %>%
  arrange(perc) %>% mutate(labels=scales::percent(perc))
# Pie Chart
ggplot(group_at,aes(x="",y=perc,fill=Area_Type)) +
  geom_bar(stat="identity") +
  guides(fill=guide_legend(title="Area Type")) +
  geom_text(aes(x=1.6,label=labels),position=position_stack(vjust=0.5)) +
  coord_polar(theta="y") +
  theme_void() +
  ggtitle("Percentage of Tenant Choose House Based on Area Type")


# 7.22 ggtitle()
# From Analysis 1-9
# Calculate Percentage Grouped by Area_Type
group_at <- house_rental_data %>% group_by(Area_Type) %>%
  count() %>% ungroup() %>% mutate(perc=n/sum(n)) %>%
  arrange(perc) %>% mutate(labels=scales::percent(perc))
# Pie Chart
ggplot(group_at,aes(x="",y=perc,fill=Area_Type)) +
  geom_bar(stat="identity") +
  guides(fill=guide_legend(title="Area Type")) +
  geom_text(aes(x=1.6,label=labels),position=position_stack(vjust=0.5)) +
  coord_polar(theta="y") +
  theme_void() +
  ggtitle("Percentage of Tenant Choose House Based on Area Type")


# 7.23 Convert Numeric to month names
# From Question 2
house_rental_data$Month <- month.abb[house_rental_data$Month]
View(head(house_rental_data))

# 7.24 stat = "count"
# From Analysis 2-1
ggplot(house_rental_data,aes(x=Bedroom_Hall_Kitchen,fill=Month)) +
  geom_bar(aes(y=after_stat(count)),stat="count") +
  labs(x="Number of Bedroom, Hall and Kitchen",y="Count",
       title="Relationship between Date_Posted and Number of Bedroom_Hall_Kitchen") +
  geom_text(aes(label=after_stat(count),
                y=after_stat(count)),stat="count",vjust=-0.4,size=4) +
  scale_fill_discrete(breaks=c('Apr','May','Jun','Jul')) +
  ylim(0,800) +
  theme_bw() +
  theme(legend.position = "bottom") +
  facet_grid(factor(Month,levels=c('Apr','May','Jun','Jul'))~Tenant_Type) 


# 7.25 ylim()
# From Analysis 2-2
ggplot(house_rental_data,aes(x=Number_of_Bathroom,fill=Month)) +
  geom_bar(aes(y=after_stat(count)),stat="count") +
  labs(x="Number of Bathroom",y="Count",
       title="Relationship between Month and Number_of_Bathroom") +
  geom_text(aes(label=after_stat(count),
                y=after_stat(count)),stat="count",vjust=-0.4,size=4) +
  scale_fill_discrete(breaks=c('Apr','May','Jun','Jul')) +
  ylim(0,800) +
  theme_bw() +
  theme(legend.position = "bottom") +
  facet_grid(factor(Month,levels=c('Apr','May','Jun','Jul'))~Tenant_Type)


# 7.26 facet_grid()
# From Analysis 2-3
ggplot(house_rental_data,aes(x=Floor_Preference,fill=Month)) +
  geom_bar(stat = "count") +
  labs(x="Floor Preference",y="Count",
       title="Relationship between Month and Floor_Preference") +
  scale_fill_discrete(breaks=c('Apr','May','Jun','Jul')) +
  theme_bw() +
  theme(legend.position = "bottom") +
  facet_grid(factor(Month,levels=c('Apr','May','Jun','Jul'))~Tenant_Type)


# 7.27 stat_summary()
# From Analysis 2-4
ggplot(house_rental_data,
       aes(x=Month,y=House_Size,fill=Month)) +
  geom_bar(aes(factor(Month,levels=c('Apr','May','Jun','Jul')),House_Size),
           position="dodge",stat="summary",fun="mean") +
  stat_summary(aes(label=round(after_stat(y),2)),fun="mean",geom="text",vjust=-0.5,size=3) +
  labs(x="Month",y="House Size",
       title="Relationship between Month and House_Size") +
  scale_fill_discrete(breaks=c('Apr','May','Jun','Jul')) +
  scale_x_discrete(breaks=c('Apr','May','Jun','Jul')) +
  theme_bw() +
  theme(legend.position = "bottom") +
  facet_wrap(~Tenant_Type) 

# 7.28 scale_fill_manual()
# From Analysis 2-5
ggplot(house_rental_data,aes(x=Month,y=Rental_Fee,fill=Month)) +
  geom_bar(aes(factor(Month,levels=c('Apr','May','Jun','Jul')),Rental_Fee),
           position="dodge",stat="summary",fun="mean") +
  stat_summary(aes(label=round(after_stat(y),2)),fun="mean",geom="text",vjust=-0.5,size=3) +
  labs(x="Month",y="Average Monthly Rental Fee (RM)",
       title="Relationship between Month and Rental_Fee") +
  scale_y_continuous(labels=scales::comma) +
  theme_bw() +
  scale_fill_manual(values=c('#f6e8c3','#5ab4ac','#c7eae5','#d8b365')) +
  facet_wrap(~Tenant_Type)


# 7.29 theme(axis.text)
# From Analysis 2-6
ggplot(house_rental_data,aes(x=Date_Posted)) +
  geom_bar(aes(y=after_stat(count)),stat="bin",binwidth=0.5) +
  geom_text(aes(label=after_stat(count)),stat="count",vjust=-0.5,size=2) +
  labs(x="Date Posted",y="Count",
       title="Relationship between Date_Posted and Furnishing_Status") +
  scale_x_date(breaks=date_breaks("2 day"),labels=date_format("%d %b")) +
  theme_bw() +
  theme(axis.line=element_line(),axis.text.x=element_text(angle=90)) +
  facet_grid(Furnishing_Status~Tenant_Type,scales="free") 


# 7.30 scale_x_date()
# From Analysis 2-7
ggplot(house_rental_data,aes(x=Date_Posted)) +
  geom_bar(aes(y=after_stat(count)),stat="bin",binwidth=0.5) +
  geom_text(aes(label=after_stat(count)),stat="count",vjust=-0.5,size=2) +
  labs(x="Date Posted",y="Count",
       title="Relationship between Date_Posted and Area_Type") +
  scale_x_date(breaks=date_breaks("2 day"),labels=date_format("%d %b")) +
  theme_bw() +
  theme(axis.line=element_line(),axis.text.x=element_text(angle=90)) +
  facet_grid(Area_Type~Tenant_Type,scales="free") 

# 7.31 Bubble Plot
# From Analysis 3-1
# calculate Percentage Grouped by Tenant_Type, Bedroom_Hall_Kitchen and Number_of_Bathroom
group_tt_bhk_nob <- house_rental_data %>%
  group_by(Tenant_Type,Bedroom_Hall_Kitchen,Number_of_Bathroom) %>%
  summarise(number_cases=n()) 
# Scatter Plot
ggplot(group_tt_bhk_nob,aes(x=Bedroom_Hall_Kitchen,y=Number_of_Bathroom)) +
  geom_point(aes(size=number_cases,color=number_cases)) +
  labs(x="Number of Bedroom, Hall and Kitchen",y="Number of Bathroom") +
  ggtitle("Relationship between Bedroom_Hall_Kitchen and Number_of_Bathroom") +
  scale_x_continuous(breaks=seq(1,6,1)) +
  scale_y_continuous(breaks=seq(1,10,1)) +
  scale_color_gradient(low="#2c7fb8",high="pink") +
  facet_wrap(~Tenant_Type)


# 7.32 geom_violin()
# From Analysis 3-3
ggplot(house_rental_data,aes(x=House_Size,
                             y=factor(Bedroom_Hall_Kitchen),
                             color=factor(Bedroom_Hall_Kitchen))) +
  geom_violin() +
  geom_boxplot(width=0.1) +
  scale_x_continuous(breaks=seq(0,8000,500)) +
  scale_y_discrete(breaks=seq(1,6,1)) +
  facet_wrap(~Tenant_Type) +
  theme(legend.position = "bottom") +
  labs(x="House Size",y="Number of Bedroom, Hall and Kitchen",
       color="Number of Bedroom, Hall and Kitchen",
       title="Relationship between Bedroom_Hall_Kitchen and House_Size")


# 7.33 position_dodge(width)
# From Analysis 3-5
# Calculate Percentage Grouped by Tenant_Type, Bedroom_Hall_Kitchen and Furnishing_Status
group_tt_bhk_fs <- house_rental_data %>% 
  group_by(Tenant_Type,Bedroom_Hall_Kitchen,Furnishing_Status) %>% 
  count() %>% ungroup() %>% mutate(perc=n/sum(n)) %>% arrange(perc) %>% 
  mutate(labels=scales::percent(perc))
# Bar Chart
group_tt_bhk_fs %>% ggplot(aes(Bedroom_Hall_Kitchen,perc,fill=Furnishing_Status)) +
  geom_bar(stat="identity",position="dodge") +
  facet_wrap(~Tenant_Type) +
  labs(title="Relationship between Number of Bedroom_Hall_Kitchen and Furnishing Status") +
  geom_text(aes(label=labels),size=2,vjust=-0.3,position=position_dodge(width=1)) +
  labs(x="Number of Bedroom, Hall and Kitchen",y="Percentage") +
  scale_x_continuous(breaks=seq(1,6,1)) +
  scale_y_continuous(labels=scales::percent) +
  theme(legend.position="bottom",
        legend.background = element_rect(fill="antiquewhite")) +
  scale_fill_discrete(name="Number of Bedroom, Hall and Kitchen")


# 7.34 geom_segment() - Lollipop Graph
# From Analysis 4-2
# Calculate Mean of House_Size Grouped by Tenant_Type and Number_of_Bathroom
group_tt_nob_hs <- house_rental_data %>% group_by(Tenant_Type,Number_of_Bathroom) %>%
  summarise(avg_house_size = mean(House_Size))
# Lollipop Graph
ggplot(group_tt_nob_hs,aes(x=Number_of_Bathroom,y=avg_house_size)) +
  geom_point(size=3,colour="black") +
  geom_segment(aes(x=Number_of_Bathroom,xend=Number_of_Bathroom,y=0,yend=avg_house_size)) +
  geom_text(aes(Number_of_Bathroom,avg_house_size,label=signif(avg_house_size,2),vjust=-0.6)) +
  scale_x_continuous(breaks=seq(1,10,1)) +
  labs(x="Number of Bathroom",y="Size of House",
       title="Relationship between Number of Bathroom and House Size") +
  facet_wrap(~Tenant_Type)


# 7.35 geom_smooth(method)
# From Analysis 6-1
# Point Graph
ggplot(rf_no_outliers,aes(x=House_Size,y=Rental_Fee)) + 
  geom_point(stat="summary",fun="mean") +
  geom_smooth(method="lm") +
  facet_wrap(~Tenant_Type) +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) +
  labs(x="Size of House",y="Rental Fee (RM)") +
  ggtitle("Relationship between House_Size and Rental_Fee")


