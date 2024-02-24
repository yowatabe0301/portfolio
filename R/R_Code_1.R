#' Title: A1 Retail Stores EDA
#' Name: Yoshiyuki Watabe
#' Date: Jan 25, 2024

# WD
setwd("~/Desktop/+/Hult/MBAN/Visualizing_and_Analyzing_Data_with_R/A1_Retail_EDA")

# Libraries
library(data.table)

# Name the case file you are creating
fileName <- 'Yoshiyuki_Watabe_A1_Individual_Assignment.csv'

# Set this to your Cases/A1_Retail_EDA_data folder
caseData <- '~/Desktop/+/Hult/MBAN/Visualizing_and_Analyzing_Data_with_R/r_studio/hult_class/Cases/A1_Retail_EDA/data'

# Let's get all files from the folders
tmp <- list.files(path       = caseData,
                  pattern    = '*.csv',
                  full.names = T,
                  recursive  = T)

# Do you want to get all transactions or a sample?
sampleTransactions   <- F
nTransactionsPerFile <- 10000

caseData <- list()
for(i in 1:length(tmp)){
  print(paste('Reading in file',i, ':',tmp[i]))
  tmpTransactions <- fread(tmp[i])
  
  if(sampleTransactions==T){
    print(paste('Sampling data to',nTransactionsPerFile,'rows.'))
    tmpTransactions <- tmpTransactions[sample(1:nTransactionsPerFile),]
  } else {
    print('Ignoring nTransactionsPerFile & reading all data')
  }
  caseData[[i]] <- tmpTransactions
}

# Organize into a single data frame
caseData <- do.call(rbind, caseData)

# Save into a single file
if(sampleTransactions==T){
  nam <- paste0(Sys.Date(),'_sampled_', nrow(caseData),'_rows_',fileName)
} else {
    nam <- nam <- paste0(Sys.Date(),'_complete_data_', fileName)
  }

fwrite(caseData, nam)

# Bring in data
diageoDF <- read.csv('Yoshiyuki_Watabe_A1_Individual_Assignment.csv')

# Review the top 6 records of diageoDF
head(diageoDF)

# Basic statistics
summary(diageoDF) 

# Organize column names
colnames(diageoDF) <- make.names(colnames(diageoDF))

# Use the names function to review the names of diageoDF
names(diageoDF)

# Checking and processing missing values
sum(is.na(diageoDF))

# 1. identifying sales trends through time
# Libraries
library(ggplot2)
library(dplyr)
library(lubridate)

# Convert Date to Date type and extract year, month
diageoDF$Date <- as.Date(diageoDF$Date, format = "%Y-%m-%d")
diageoDF$Year <- factor(year(diageoDF$Date))
diageoDF$Month <- factor(month(diageoDF$Date, label = TRUE), levels = month.abb)

# Aggregate sales by month and year
monthly_sales_by_year <- diageoDF %>%
  group_by(Year, Month) %>%
  summarise(TotalSales = sum(Sale..Dollars.), .groups = 'drop') %>%
  arrange(Year, Month)

# Create the plot with line graph
ggplot(monthly_sales_by_year, aes(x = Month, y = TotalSales, group = Year, color = Year)) +
  geom_line() +
  geom_point() + # Add points to the line graph
  scale_color_manual(values = rainbow(length(unique(monthly_sales_by_year$Year)))) +
  labs(title = "Monthly Sales Trend by Year", x = "Month", y = "Total Sales (Dollars)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0)) # Set angle of x-axis text to 0

# 2. analysis of superior sales regions and their characteristics
# Calculate the total sales by city
sales_by_city <- diageoDF %>%
  group_by(City) %>%
  summarise(TotalSales = sum(Sale..Dollars.)) %>%
  arrange(desc(TotalSales)) %>%
  top_n(10, TotalSales)

# Plot the top cities with their total sales using a bar chart
ggplot(sales_by_city, aes(x=reorder(City, TotalSales), y=TotalSales)) +
  geom_bar(stat="identity", fill="deepskyblue") +
  coord_flip() +
  labs(title="Top 10 Cities by Total Sales", x="City", y="Total Sales") +
  theme_light() +
  theme(axis.title.y=element_blank())

# Calculate the total sales by county
sales_by_county <- diageoDF %>%
  group_by(County) %>%
  summarise(TotalSales = sum(Sale..Dollars.)) %>%
  arrange(desc(TotalSales)) %>%
  top_n(10, TotalSales)

# Plot the top counties with their total sales using a bar chart
ggplot(sales_by_county, aes(x=reorder(County, TotalSales), y=TotalSales)) +
  geom_bar(stat="identity", fill="dodgerblue") +
  coord_flip() +
  labs(title="Top 10 Counties by Total Sales", x="County", y="Total Sales") +
  theme_light() +
  theme(axis.title.y=element_blank())

# 3. analysis of important alcoholic beverage categories and their sales composition
# Calculate the total sales by liquor category
sales_by_category <- diageoDF %>%
  group_by(Category.Name) %>%
  summarise(TotalSales = sum(Sale..Dollars.)) %>%
  arrange(desc(TotalSales)) %>%
  top_n(10, TotalSales)

# Plot the top liquor categories with their total sales using a horizontal bar chart
ggplot(sales_by_category, aes(x=reorder(Category.Name, TotalSales), y=TotalSales)) +
  geom_bar(stat="identity", fill="darkgreen") +
  coord_flip() +
  labs(title="Top 10 Liquor Categories by Total Sales", x="Category", y="Total Sales") +
  theme_light() +
  theme(axis.title.x=element_blank())

# 4. analysis of consumer preferences regarding product characteristics such as bottle capacity, price range, etc.
# Analyzing consumer preferences by bottle volume and price
diageoDF %>%
  group_by(Bottle.Volume..ml., State.Bottle.Retail) %>%
  summarise(TotalSales = sum(Sale..Dollars.), TotalBottlesSold = sum(Bottles.Sold)) %>%
  arrange(desc(TotalSales)) %>%
  head()

# First, calculate the total sales for each combination of bottle volume and state bottle retail price
sales_by_volume_price <- diageoDF %>%
  group_by(Bottle.Volume..ml., State.Bottle.Retail) %>%
  summarise(TotalSales = sum(Sale..Dollars.), TotalBottles = sum(Bottles.Sold), .groups = 'drop')

# Create a scatter plot with a logarithmic scale for the y-axis
ggplot(sales_by_volume_price, aes(x = Bottle.Volume..ml., y = State.Bottle.Retail, size = TotalBottles, color = TotalSales)) +
  geom_point(alpha = 0.7) + # Set transparency to see overlapping points
  scale_y_log10() + # Apply a logarithmic scale to the y-axis
  scale_color_gradient(low = "blue", high = "red") + # Use a color gradient for total sales
  labs(title = "Bottle Volume vs Price Preference",
       x = "Bottle Volume (ml)",
       y = "State Bottle Retail Price (log scale)",
       color = "Total Sales",
       size = "Total Bottles Sold") +
  theme_minimal() +
  theme(legend.position = "right") # Adjust legend position

# 5. analysis of vendor performance and its contribution to total sales and liters
# Load the 'forcats' package
library(forcats)

# Order the vendors by Total Sales in descending order
top_vendors <- diageoDF %>%
  group_by(`Vendor.Name`) %>%
  summarise(TotalSales = sum(`Sale..Dollars.`), TotalLiters = sum(`Bottles.Sold` * `Bottle.Volume..ml.` / 1000)) %>%
  arrange(desc(TotalSales)) %>%
  top_n(10)

# Create a bar chart for the top vendors
ggplot(top_vendors, aes(x = fct_reorder(`Vendor.Name`, -TotalSales), y = TotalSales, fill = TotalLiters)) +
  geom_bar(stat = "identity") +
  labs(title = "Top Vendors by Total Sales and Total Liters",
       x = "Vendor Name",
       y = "Total Sales") +
  theme_minimal() +
  theme(legend.position = "right",
        axis.text.x = element_text(angle = 90, hjust = 1),
        axis.text.y = element_text(),
        axis.title.y = element_text()) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  guides(fill = guide_legend(title = "Total Liters"))

# End