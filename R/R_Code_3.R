#` Author: Yoshiyuki Watabe
#` Feb 7 2024
#` A3 Individual Assignment

# Import Libraries
library(ggplot2)
library(ggthemes)
library(tm)
library(qdapRegex)
library(wordcloud)
library(syuzhet)
library(topicmodels)

# Set Working Directory
setwd("~/Desktop/+/Hult/MBAN/Visualizing_and_Analyzing_Data_with_R/A3_NLP")

# Custom Functions
Sys.setlocale('LC_ALL','C')

tryTolower <- function(x){
  # return NA when there is an error
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x), error = function(e) e)
  # if not an error
  if (!inherits(try_error, 'error'))
    y = tolower(x)
  return(y)
}

cleanCorpus<-function(corpus, customStopwords){
  corpus <- tm_map(corpus, content_transformer(qdapRegex::rm_url)) 
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, content_transformer(tryTolower))
  corpus <- tm_map(corpus, removeWords, customStopwords)
  return(corpus)
}

# Add Stop Words
customStopwords <- c(stopwords('english'), 'hult', 'university', 'business')

# Data
studentBios <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing_Analyzing_Data_with_R/main/Cases/A3_NLP/Student%20Ambassador%20Bios/final_student_data.csv')



### Exploratory Data Analysis
# Aggregate the number of students by program title
student_counts <- table(studentBios$programTitle)

# Convert the aggregated results into a dataframe
student_counts_df <- as.data.frame(student_counts)

# Rename the columns
colnames(student_counts_df) <- c("programTitle", "Count")

# Visualize the aggregated results
ggplot(student_counts_df, aes(x=reorder(programTitle, Count), y=Count)) +
  geom_bar(stat="identity", fill="darkgreen") +
  theme_minimal() +
  coord_flip() + # Flip the bar chart to horizontal
  labs(title="Number of Ambassador Student per Program Title",
       x="Program Title",
       y="Count") +
  theme(plot.title = element_text(hjust = 0.5)) # Center align the title

# Count the number of students by campus location
campus_counts <- table(studentBios$campus)

# Convert the count results into a dataframe
campus_counts_df <- as.data.frame(campus_counts)

# Rename the columns
colnames(campus_counts_df) <- c("Campus", "Count")

# Visualize the count results
ggplot(campus_counts_df, aes(x=reorder(Campus, Count), y=Count)) +
  geom_bar(stat="identity", fill="darkblue") +
  theme_minimal() +
  coord_flip() + # Flip the bar chart to horizontal
  labs(title="Number of Ambassador Student per Campus",
       x="Campus",
       y="Count") +
  theme(plot.title = element_text(hjust = 0.5)) # Center align the title

# Count the number of students by gender
gender_counts <- table(studentBios$namSorGender.likelyGender)

# Convert the count results into a dataframe
gender_counts_df <- as.data.frame(gender_counts)

# Rename the columns
colnames(gender_counts_df) <- c("Gender", "Count")

# Visualize the count results
ggplot(gender_counts_df, aes(x=Gender, y=Count)) +
  geom_bar(stat="identity", fill="orange") +
  theme_minimal() +
  labs(title="Gender Distribution among Ambassador Students",
       x="Gender",
       y="Count") +
  theme(plot.title = element_text(hjust = 0.5)) # Center align the title

# Count the number of students by their region of origin
region_counts <- table(studentBios$namSorCountry.region)

# Convert the count results into a dataframe for easier handling
region_counts_df <- as.data.frame(region_counts)

# Rename the columns to more descriptive names
colnames(region_counts_df) <- c("Region", "Count")

# Visualize the count results using a bar chart with regions ordered by count
ggplot(region_counts_df, aes(x=reorder(Region, -Count), y=Count)) +
  geom_bar(stat="identity", fill="darkred") +
  theme_minimal() +
  labs(title="Number of Ambassador Students by Region of Origin",
       x="Region",
       y="Count") +
  theme(plot.title = element_text(hjust = 0.5)) # Center the title

# Count the number of students by their sub-region of origin
subRegion_counts <- table(studentBios$namSorCountry.subRegion)

# Convert the count results into a dataframe for easier handling
subRegion_counts_df <- as.data.frame(subRegion_counts)

# Rename the columns to more descriptive names
colnames(subRegion_counts_df) <- c("SubRegion", "Count")

# Visualize the count results using a bar chart with sub-regions ordered by count
ggplot(subRegion_counts_df, aes(x=reorder(SubRegion, -Count), y=Count)) +
  geom_bar(stat="identity", fill="coral") +
  theme_minimal() +
  labs(title="Number of Ambassador Students by Sub-Region of Origin",
       x="Sub-Region",
       y="Count") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(plot.title = element_text(hjust = 0.5)) # Center the title

# Count the number of students by their ISO country code of origin
country_counts <- table(studentBios$isoCode.Country)

# Convert the count results into a dataframe for easier handling
country_counts_df <- as.data.frame(country_counts)

# Rename the columns to more descriptive names
colnames(country_counts_df) <- c("CountryISO", "Count")

# Visualize the count results using a bar chart with countries ordered by count
ggplot(country_counts_df, aes(x=reorder(CountryISO, -Count), y=Count)) +
  geom_bar(stat="identity", fill="steelblue") +
  theme_minimal() +
  labs(title="Number of Ambassador Students by Country ISO Code",
       x="Country ISO Code",
       y="Count") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(plot.title = element_text(hjust = 0.5)) # Center the title



### NLP for Column 'Bio'
# Create a corpus
studentCorpus <- VCorpus(VectorSource(studentBios$bio))

# Clean the corpus
studentCorpus <- cleanCorpus(studentCorpus, customStopwords)

# Examine one record
content(studentCorpus[[1]])

# Document term matrix (documents are rows)
studentDTM <- DocumentTermMatrix(studentCorpus)
studentDTMm <- as.matrix(studentDTM)

# Examine the dimensions
dim(studentDTMm) # it means that we have 85 bios and 1668 unique words

# Word frequency matrix
studentFreq <- colSums(studentDTMm)
studentFreq <- data.frame(word=names(studentFreq),
                        frequency=studentFreq, 
                        row.names = NULL)

# Examine a portion of the WFM to make sure we built it correctly
head(studentFreq, 10)

# Simple barplot; values greater than 25 
topWords      <- subset(studentFreq, studentFreq$frequency >= 25) 
topWords      <- topWords[order(topWords$frequency, decreasing=F),]

# Chg to factor for ggplot
topWords$word <- factor(topWords$word, 
                        levels=unique(as.character(topWords$word))) 

ggplot(topWords, aes(x=word, y=frequency)) + 
  geom_bar(stat="identity", fill='darkred') + 
  coord_flip()+ theme_gdocs() +
  geom_text(aes(label=frequency), colour="white",hjust=1.25, size=5.0)

# Choose a color & drop light ones
pal <- brewer.pal(8, "Reds")
pal <- pal[-(1:2)]

# Create a word cloud
wordcloud(topWords$word,
          topWords$frequency,
          max.words = 50,
          random.order = FALSE,
          colors = pal,
          scale = c(2, 0.5))

# Sentiment Analysis
sentiment_scores <- sapply(studentCorpus$content, get_sentiment, method = "bing")
sentiment_category <- ifelse(sentiment_scores > 0, "Positive", ifelse(sentiment_scores < 0, "Negative", "Neutral"))

# Add sentiment analysis results to the dataset
studentBios$sentiment <- sentiment_category

# Visualization of Sentiment Analysis
ggplot(studentBios, aes(x = sentiment, fill = sentiment)) +
  geom_bar(show.legend = FALSE) +
  labs(title = "Sentiment Analysis of Student Bios", x = NULL, y = "Count") +
  theme_minimal() +
  scale_fill_manual(values = c("Positive" = "green3", "Negative" = "red3", "Neutral" = "gray"))

# Comparative Analysis (Program Title vs. Sentiment)
ggplot(studentBios, aes(y = programTitle, fill = sentiment)) +
  geom_bar(stat = "count", position = "fill") +
  labs(title = "Comparative Sentiment Analysis by Program Title", x = "Proportion", y = "Program Title") +
  theme_minimal() +
  scale_fill_manual(values = c("Positive" = "green3", "Negative" = "red3", "Neutral" = "gray"))

# Comparative Analysis (Campus vs. Sentiment)
ggplot(studentBios, aes(y = campus, fill = sentiment)) +
  geom_bar(stat = "count", position = "fill") +
  labs(title = "Comparative Sentiment Analysis by Campus", x = "Proportion", y = "Campus") +
  theme_minimal() +
  scale_fill_manual(values = c("Positive" = "green3", "Negative" = "red3", "Neutral" = "gray"))

# Comparative Analysis (Inferred Gender vs. Sentiment)
ggplot(studentBios, aes(y = namSorGender.likelyGender, fill = sentiment)) +
  geom_bar(stat = "count", position = "fill") +
  labs(title = "Comparative Sentiment Analysis by Inferred Gender", x = "Proportion", y = "Gender") +
  theme_minimal() +
  scale_fill_manual(values = c("Positive" = "green3", "Negative" = "red3", "Neutral" = "gray"))

# Comparative Analysis (Region vs. Sentiment)
ggplot(studentBios, aes(y = namSorCountry.region, fill = sentiment)) +
  geom_bar(stat = "count", position = "fill") +
  labs(title = "Comparative Sentiment Analysis by Region", x = "Proportion", y = "Region") +
  theme_minimal() +
  scale_fill_manual(values = c("Positive" = "green3", "Negative" = "red3", "Neutral" = "gray"))

# Comparative Analysis (subRegion vs. Sentiment)
ggplot(studentBios, aes(y = namSorCountry.subRegion, fill = sentiment)) +
  geom_bar(stat = "count", position = "fill") +
  labs(title = "Comparative Sentiment Analysis by Sub-Region", x = "Proportion", y = "Sub-Region") +
  theme_minimal() +
  scale_fill_manual(values = c("Positive" = "green3", "Negative" = "red3", "Neutral" = "gray"))

# Comparative Analysis (Country vs. Sentiment)
ggplot(studentBios, aes(y = isoCode.Country, fill = sentiment)) +
  geom_bar(stat = "count", position = "fill") +
  labs(title = "Comparative Sentiment Analysis by Country", x = "Proportion", y = "Country") +
  theme_minimal() +
  scale_fill_manual(values = c("Positive" = "green3", "Negative" = "red3", "Neutral" = "gray"))



### NLP for Column 'Interests'
# Create a corpus
studentCorpus2 <- VCorpus(VectorSource(studentBios$interests))

# Clean the corpus
studentCorpus2 <- cleanCorpus(studentCorpus2, customStopwords)

# Examine one record
content(studentCorpus2[[1]])

# Document term matrix (documents are rows)
studentDTM2 <- DocumentTermMatrix(studentCorpus2)
studentDTMm2 <- as.matrix(studentDTM2)

# Examine the dimensions
dim(studentDTMm2) # it means that we have 85 bios and 1668 unique words

# Word frequency matrix
studentFreq2 <- colSums(studentDTMm2)
studentFreq2 <- data.frame(word=names(studentFreq2),
                          frequency=studentFreq2, 
                          row.names = NULL)

# Examine a portion of the WFM to make sure we built it correctly
head(studentFreq2, 10)

# Simple barplot; values greater than 10
topWords2      <- subset(studentFreq2, studentFreq2$frequency >= 10) 
topWords2      <- topWords2[order(topWords2$frequency, decreasing=F),]

# Chg to factor for ggplot
topWords2$word <- factor(topWords2$word, 
                        levels=unique(as.character(topWords2$word))) 

ggplot(topWords2, aes(x=word, y=frequency)) + 
  geom_bar(stat="identity", fill='darkblue') + 
  coord_flip()+ theme_gdocs() +
  geom_text(aes(label=frequency), colour="white",hjust=1.25, size=5.0)

# Choose a color & drop light ones
pal2 <- brewer.pal(8, "Blues")
pal2 <- pal[-(1:2)]

# Create a word cloud
wordcloud(topWords2$word,
          topWords2$frequency,
          max.words = 50,
          random.order = FALSE,
          colors = pal2,
          scale = c(2, 0.5))

# Sentiment Analysis
sentiment_scores2 <- sapply(studentCorpus2$content, get_sentiment, method = "bing")
sentiment_category2 <- ifelse(sentiment_scores > 0, "Positive", ifelse(sentiment_scores < 0, "Negative", "Neutral"))

# Add sentiment analysis results to the dataset
studentBios$sentiment2 <- sentiment_category2

# Visualization of Sentiment Analysis
ggplot(studentBios, aes(x = sentiment2, fill = sentiment2)) +
  geom_bar(show.legend = FALSE) +
  labs(title = "Sentiment Analysis of Student Interests", x = NULL, y = "Count") +
  theme_minimal() +
  scale_fill_manual(values = c("Positive" = "green3", "Negative" = "red3", "Neutral" = "gray"))

# Comparative Analysis (Program Title vs. Sentiment)
ggplot(studentBios, aes(y = programTitle, fill = sentiment2)) +
  geom_bar(stat = "count", position = "fill") +
  labs(title = "Comparative Sentiment Analysis by Program Title", x = "Proportion", y = "Program Title") +
  theme_minimal() +
  scale_fill_manual(values = c("Positive" = "green3", "Negative" = "red3", "Neutral" = "gray"))

# Comparative Analysis (Campus vs. Sentiment)
ggplot(studentBios, aes(y = campus, fill = sentiment2)) +
  geom_bar(stat = "count", position = "fill") +
  labs(title = "Comparative Sentiment Analysis by Campus", x = "Proportion", y = "Campus") +
  theme_minimal() +
  scale_fill_manual(values = c("Positive" = "green3", "Negative" = "red3", "Neutral" = "gray"))

# Comparative Analysis (Inferred Gender vs. Sentiment)
ggplot(studentBios, aes(y = namSorGender.likelyGender, fill = sentiment2)) +
  geom_bar(stat = "count", position = "fill") +
  labs(title = "Comparative Sentiment Analysis by Inferred Gender", x = "Proportion", y = "Gender") +
  theme_minimal() +
  scale_fill_manual(values = c("Positive" = "green3", "Negative" = "red3", "Neutral" = "gray"))

# Comparative Analysis (Region vs. Sentiment)
ggplot(studentBios, aes(y = namSorCountry.region, fill = sentiment2)) +
  geom_bar(stat = "count", position = "fill") +
  labs(title = "Comparative Sentiment Analysis by Region", x = "Proportion", y = "Region") +
  theme_minimal() +
  scale_fill_manual(values = c("Positive" = "green3", "Negative" = "red3", "Neutral" = "gray"))

# Comparative Analysis (subRegion vs. Sentiment)
ggplot(studentBios, aes(y = namSorCountry.subRegion, fill = sentiment2)) +
  geom_bar(stat = "count", position = "fill") +
  labs(title = "Comparative Sentiment Analysis by Sub-Region", x = "Proportion", y = "Sub-Region") +
  theme_minimal() +
  scale_fill_manual(values = c("Positive" = "green3", "Negative" = "red3", "Neutral" = "gray"))

# Comparative Analysis (Country vs. Sentiment)
ggplot(studentBios, aes(y = isoCode.Country, fill = sentiment2)) +
  geom_bar(stat = "count", position = "fill") +
  labs(title = "Comparative Sentiment Analysis by Country", x = "Proportion", y = "Country") +
  theme_minimal() +
  scale_fill_manual(values = c("Positive" = "green3", "Negative" = "red3", "Neutral" = "gray"))



# End