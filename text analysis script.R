require(readr)
require(tidyverse)
require(tidytext)
require(tm)
require(wordcloud)
require(rmarkdown)


##################### Loading the dataframe #############################


#### Load the dataframe (found on Kaggle):

lyrics <- read_csv("~/cogmasterrr/PCBS/lyrics.csv")

#Our dataframe is too heavy to perform standard text analysis. Let's only keep genres of interest: pop and hip-hop
#Moreover, I remove all observations for year < 1970 (data before 1970 is bad quality)

lyrics <- lyrics %>%
  filter(genre == "Pop" | genre == "Hip-Hop") %>%
  filter(year > 1970)

#Check that only "pop" and "hip-hop" remain:

lyrics$genre <- as.factor(lyrics$genre)
levels(lyrics$genre) 


###Let's get a few basic facts about our database: 

#Count songs and artists:

lyrics %>%
  group_by(genre) %>%
  count() %>%
  summarize(count = n, percentage = n/nrow(lyrics)) #count how many songs per genre
#Roughly: the songs are 60% pop and 40% hip-hop. 

count_artist <- length(unique(lyrics$artist)) #count artists: 3757 artists overall
nrow(lyrics) / count_artist #around 22 songs per artist

lyrics_pop <- lyrics %>%
  filter(genre == "Pop")
count_pop_artist <- length(unique(lyrics_pop$artist)) 
count_pop_artist
#count pop artists: 2347
nrow(lyrics_pop) / count_pop_artist #around 21 songs per pop artist

lyrics_hip_hop <- lyrics %>%
  filter(genre == "Hip-Hop")
count_hip_hop_artist <- length(unique(lyrics_hip_hop$artist)) #count hip hop artists: 1410
count_hip_hop_artist
nrow(lyrics_hip_hop) / count_hip_hop_artist #around 24 songs per hip hop artist

#What about the distribution of the variable "year"?
count_year <- lyrics %>% 
  group_by(year, genre) %>%
  summarize(count = n()) #Count number of songs by year
count_year

weighted.mean(count_year$year, count_year$count) #mean year (weighted by count) is approx. 2009

#This information could be made clearer... Let's plot it: 

ggplot(count_year, aes(year, count, fill = genre)) +
  geom_col() +
  labs(title = "Songs in the database for each year", 
       x = "Year", 
       y = "Number of songs", 
       fill = "Genre")

#All in all: most of our songs are post 2005 for both hip hop and pop.







### Handle stopwords (words that are useless for analysis: "and", "or", "I"...)

stopwords_english <- as.data.frame(stopwords(kind = "en")) #create a new dataframe with one column that contains english stopwords from the tm package

names(stopwords_english)[1] <- "word"  #rename that column: it's called "word"

#In my opinion, the stopwords in the tm package are not enough: I create a new vector containing additional stopwords: 

my_stopwords_en <- c("like", "oh", "yeah", "ain't", "la", "que")
my_stopwords_en <- data.frame(word = my_stopwords_en) #transform vector into dataframe

#Tidy data: I divide the lyrics column into one word per row and remove stopwords

lyrics_data <- lyrics %>% 
  unnest_tokens(word, lyrics) %>% #This step could take a litte while: this line divides lyrics into one word per row
  anti_join(stopwords_english) %>% 
  anti_join(my_stopwords_en) #keep only the complementary of the stopwords databases in my lyrics database
#Our dataframe is ready for analysis!



################## Preliminary analysis: what are the most common words in our data? ####################

#Create a dataframe suitable for plotting and wordcloud analysis:

lyrics_wordcloud <- lyrics_data %>%
  count(word) %>% #count the occurence of each word
  arrange(desc(n)) %>% #arrange words from the most frequent to the less frequent
  mutate(frequence = n / nrow(lyrics_data)) %>% #create a new column "frequence"
  arrange(desc(frequence)) %>% #arrange words by descending frequency
  top_n(10, frequence) #keep only the 10 most frequent words 
  

lyrics_wordcloud <- transform(lyrics_wordcloud, word = reorder(word, frequence)) #reorder words by frequency in the dataframe

#Plot:

ggplot(lyrics_wordcloud, aes(word, frequence)) +
  geom_col(fill = "red") +
  coord_flip() +
  labs(title = "Most frequent words in the database", 
       x = "Word", 
       y = "Frequency") #plot as bar plot

wordcloud(words = lyrics_wordcloud$word, freq = lyrics_wordcloud$frequence, max.words = 10) #plot wordcloud for all songs



#Well, as one could expect, "love" is one of the most frequent words in music.


#Now let's perform the same analysis by genre:

lyrics_data_pop <- lyrics_data %>%
  filter(genre == "Pop") #dataframe with only pop songs

lyrics_wordcloud_pop <- lyrics_data_pop %>%
  count(word) %>% #count the occurence of each word
  arrange(desc(n)) %>% #arrange words from the most frequent to the less frequent
  mutate(frequence = n / nrow(lyrics_data_pop)) %>% #create a new column "frequence"
  arrange(desc(frequence)) %>% #arrange words by descending frequence
  top_n(10, frequence) #keep only the 10 most frequent words (by frequency)

lyrics_wordcloud_pop <- transform(lyrics_wordcloud_pop, word = reorder(word, frequence)) #reorder words by frequency in the dataframe

#Plot:

ggplot(lyrics_wordcloud_pop, aes(word, frequence)) +
  geom_col(fill = "red") +
  coord_flip() +
  labs(title = "Most frequent words in pop songs", 
       x = "Word", 
       y = "Frequency") #plot as bar plot

wordcloud(words = lyrics_wordcloud_pop$word, freq = lyrics_wordcloud_pop$frequence, max.words = 10) #plot wordcloud for pop songs



lyrics_data_hip_hop <- lyrics_data %>%
  filter(genre == "Hip-Hop") #dataframe with only hip-hop songs

lyrics_wordcloud_hip_hop <- lyrics_data_hip_hop %>%
  count(word) %>% #count the occurence of each word
  arrange(desc(n)) %>% #arrange words from the most frequent to the less frequent
  mutate(frequence = n / nrow(lyrics_data_hip_hop)) %>% #create a new column "frequence"
  arrange(desc(frequence)) %>% #arrange words by descending frequence
  top_n(10, frequence) #keep only the 10 most frequent words

lyrics_wordcloud_hip_hop <- transform(lyrics_wordcloud_hip_hop, word = reorder(word, frequence)) #reorder words by frequency in the dataframe
  

#Plot: 

ggplot(lyrics_wordcloud_hip_hop, aes(word, frequence)) +
  geom_col(fill = "red") +
  coord_flip() +
  labs(title = "Most frequent words in hip-hop songs", 
       x = "Word", 
       y = "Frequency")

wordcloud(words = lyrics_wordcloud_hip_hop$word, freq = lyrics_wordcloud_hip_hop$frequence, max.words = 10) #plot wordcloud for hip-hop songs




#We now know quite a bit about our database, and even have basic ideas about the differences between hip hop and pop songs. Now, let's dive in more into the data


#################### Sentiment analysis: how negative are our songs? ########################

### Prepare new dataframes that include a sentiment dictionary

afinn_sentiments <- get_sentiments("afinn") #get a sentiment dictionary from the tidytext package and put it in a new object
neg_ref <- mean(afinn_sentiments$value) #Caution: there already is a negativity bias in the dictionary: mean value is -0.6 approx.
neg_ref

lyrics_sentiments <- lyrics_data %>%
  inner_join(afinn_sentiments) #merge our data with the dictionary: each word now has a sentiment value

negativity_score <- neg_ref - mean(lyrics_sentiments$value) #Compute mean negativity score of all songs
negativity_score #Overall score is 0.25 points less negative than the afinn reference

lyrics_sentiments_pop <- lyrics_data_pop %>%
  inner_join(afinn_sentiments) #merge only data for pop songs with the dictionary: each word now has a sentiment value

negativity_score_pop <- neg_ref - mean(lyrics_sentiments_pop$value) #Compute mean neagativity score of pop songs
negativity_score_pop #Overall score is 1.1 point less negative than the afinn reference

lyrics_sentiments_hip_hop <- lyrics_data_hip_hop %>%
  inner_join(afinn_sentiments) #merge only data for hip-hop songs with the dictionary

negativity_score_hip_hop <- neg_ref - mean(lyrics_sentiments_hip_hop$value) #Compute mean negativity score of hip-hop songs
negativity_score_hip_hop #Overall score is 0.41 points more negative than the afinn reference

delta_neg <- negativity_score_pop - negativity_score_hip_hop #Compute the difference of the two means
delta_neg #Display this difference


#There seems to be more negativity in hip hop lyrics than pop lyrics, which confirms our prediction. 
#The difference is approx. 1.4 negativity points. 

###Let's plot this difference to make it clearer:

lyrics_sentiments <- lyrics_sentiments %>%
  mutate(mean_negativity = case_when(genre == "Pop" ~ negativity_score_pop,
                   genre == "Hip-Hop" ~ negativity_score_hip_hop)) #create new column: mean-negativity by genre

#Plot:


ggplot(lyrics_sentiments, aes(x = genre, y = mean_negativity)) +
  geom_col(fill = "red") +
  labs(title = "Mean negativity score by musical genre", 
       x = "Genre",
       y = "Negativity score")



###But is this difference statistically significant? We use a linear model and an analysis of variance to test this hypothesis:


neg_mod <- lm(formula = mean_negativity ~ genre, data = lyrics_sentiments)
summary(neg_mod)


#It seems like the difference in negativity between pop and hip-hop is statistically significant! What if we consider year of release?

#####Plot mean negativity score across years: 

count_neg_year <- lyrics_sentiments %>%
  group_by(year) %>%
  summarize(mean_score = neg_ref - mean(value)) #count negativity score per year

#Plot: 

ggplot(count_neg_year, aes(year, mean_score)) +
  geom_point() +
  geom_smooth() +
  geom_smooth(method = "lm", lty = 2, color = "red", alpha = 0.1, se = FALSE) +
  labs(title = "Negativity score increases over the years", 
       subtitle = "Linear model in red",
       x = "Year", 
       y = "Negativity score")

#It seems that there is an effect of year: songs get more negative as years go by.



#Same plot but by genre: 
count_neg_year2 <- lyrics_sentiments %>%
  group_by(year, genre) %>%
  summarize(mean_score = neg_ref - mean(value)) #count negativity scores by year and genre

#Plot: 

ggplot(count_neg_year2, aes(year, mean_score, color = genre)) +
  geom_point() +
  geom_smooth() +
  labs(title = "Negativity score increases over the years", 
       x = "Year", 
       y = "Negativity score", 
       color = "Genre")

#This plot suggests that the increase in negativity is driven by the increased popularity of hip-hop rather than the effect of time alone.

###Let's test this statistically: 


neg_mod2 <- lm(formula = mean_negativity ~ genre + year, data = lyrics_sentiments)
summary(neg_mod2)


#The impact of year is not significant, and genre remains statistically significant!

###Finally, let's implement a model that controls for year AND the interaction of year and genre:
neg_mod3 <- lm(formula = mean_negativity ~ genre + year + genre:year, data = lyrics_sentiments)
summary(neg_mod3)


#The impact of year and year:genre are not significant, and genre remains statistically significant again!


#######################Further analysis: how exactly is hip-hop negative?#########################

#What are the most common negative words in hip-hop and pop?

lyrics_sentiments_hip_hop %>%
  filter(value < -4) %>%
  count(word) %>%
  arrange(desc(n)) %>%
  top_n(10, word) #count top 10 most common very negative words in hip hop


lyrics_sentiments_pop %>%
  filter(value < -4) %>%
  count(word) %>%
  arrange(desc(n)) %>%
  top_n(10, word) #count top 10 most common very negative words in pop

#Well... as expected, the higher negativity in hip-hop is driven by curse words.
#Curse words are not absent in pop music, but seem much less frequent. Let's dive further into the distribution of negativiry scores in both genres

###Distribution of negativity scores: 

count_negativity <- lyrics_sentiments %>%
  group_by(value, genre) %>%
  summarize(count = n()) #count words by negativity value and genre

ggplot(count_negativity, aes(value, count, fill = genre)) +
  geom_col(position = "dodge") +
  labs(title = "Distribution of negativity scores by genre",
       x = "Negativity Value (based on AFINN dictionnary)",
       y = "Word Count", 
       fill = "Genre")

#As one could suspect, the negativity of hip-hop is driven by extremely negative values (value < -3).
#These are probably mainly constituted of curse words. 
#Interestingly, it is also due to a large difference in occurence of highly positive words (value = 3).

###Refining the analysis: the most common negative word in hp-hop is not necessarily seen as negative in the context of hip-hop culture

lyrics_sentiments_hip_hop2 <- lyrics_sentiments_hip_hop %>%
  filter(word != "niggas" & word != "nigger") #remove these words from data

#Re-compute the negativity score:


negativity_score_hip_hop2 <- neg_ref - mean(lyrics_sentiments_hip_hop2$value) #Compute mean negativity score of hip-hop songs
negativity_score_hip_hop2 #Overall score is 0.25 points more negative than the afinn reference

#Create a new sentiment dataframe without the removed words:

lyrics_sentiments2 <- lyrics_sentiments %>%
  mutate(mean_negativity = case_when(genre == "Pop" ~ negativity_score_pop,
                                     genre == "Hip-Hop" ~ negativity_score_hip_hop2)) #modify negativity score for hip-hop

#Re-compute the linear model: 

neg_mod4 <- lm(formula = mean_negativity ~ genre + year + genre:year, data = lyrics_sentiments2)
summary(neg_mod4)

#The difference between pop and hip-hop remains significant even after removing the n-word. 


#A deeper analysis: which negative sentiment drives hip-hop's higher negativity bias?

nrc_sentiments <- get_sentiments("nrc") #get a more diverse sentiment dictionary from the tidytext package

lyrics_sentiments_nrc <- lyrics_data %>%
  inner_join(nrc_sentiments) %>%
  filter(word != "nigger" & word != "niggas") #merge lyrics data with the nrc dictionary and remove disturbing words

nrc_count <- lyrics_sentiments_nrc %>%
  filter(sentiment != "negative" & sentiment != "positive") %>%
  group_by(sentiment, genre) %>%
  count() #count words by sentiment and genre, while excluding the "positive" and "negative" sentiments

#Plot: 

ggplot(nrc_count, aes(sentiment, n, fill = genre)) +
  geom_col(position = "dodge") +
  labs(title = "Sentiment distribution by genre", 
       x = "Sentiment", 
       y = "Word Count", 
       fill = "Genre")

#Hip-Hop's higher negativity seems indeed particularly salient in anger, but also disgust and fear. 
#The difference is less striking for sadness. This should be further investigated in subsequent research.








