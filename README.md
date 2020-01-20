# Testing an evolutionary approach to Negativity using text analysis on lyrics (in R). 

This repository contains all relevant scripts, documents and information for my PCBS final project. 

## Summary:

The objectives of this project are twofold: 

1) I wish to perform a full-fledge text analysis/sentiment analysis on music lyrics. Lyrics will be accessed using a ready-to-use database from Kaggle while text analysis will require specific R packages like tm and tidytext. 

2) I wish to build a R Shiny App that allows the user to interactively explore lyrics data. Here is a link to the app I am developing: https://aminesoja.shinyapps.io/Genius/ (see below).


Evolutionary theory predicts that not only species adapt to their global environment, but individuals adapt to their local environment. Harsh and unpredictible environments should trigger a different cluster of psychological dispositions and behaviors than affluent ones. Studies have established for several animal species and humans that the harshness of the environment accounts for individual differences in various traits like agressivity, impulsivity and age at first sexual intercourse (Gillian & Pepper, 2017).

Species that evolved in changing environments, the theory goes, have evolved this ability to perceive cues of environment harshness and behave accordingly. Short-term strategies (impulsivity, aggressivity, early reproduction, etc...) are adaptive in harsh environments where extrinsic mortality is high, whereas long-term strategies (low time-disocunting, moderation, cognitive inhibition...) are adaptive in affluent environments where extrinsic mortality is lower. 

Some researchers have argued that this framework can be applied to humans as well (Nettle, 2009). Human individuals living in harsh conditions - for example, in warfare zones or simply in poverty - display different psychological dispositions and behaviors than individuals living in affluent environments. This theory could explain the lenghtily discussed finding that the poor and the well-off display very different behaviors. 

In humans, harsh environments may not only manifest in higher impulsivity and earlier reproduction, but also in lower trust, higher conservatism and higher negativity. The hypothesis I want to test here is the idea that poverty is associated with a higher negativity. 

I am currently testing this hypothesis experimentally during my lab internship. In this project, I wish to test this hypothesis in a more original and ecological way. I wish to perform text analysis - sentiment analysis in particular - to a database of song lyrics. 

Songs are an interesting object, because they express - and trigger - strong emotions. Sentiment analysis allows us to determine the prevalence of various emotions in lyrics: positivity, negativity, joy, anger, etc.... To be more precise, I wish to test the following hypotheses. 

There exists no database for the socio-economic status of artists. Therefore, I will use music genre as a proxy for the artist's status. I chose to compare French rap songs with French "variété" pop songs. Rap is known to be a working class genre while "variété" addresses a wider and relatively wealthier audience (Coulangeon, 2010). If our hypothesis is correct, after sentiment analysis rap songs should display more negative emotions than "variété" songs. 

## Study protocole

We use a song lyrics database scraped from the music website MetroLyrics and made available on Kaggle. This immense database contains data for more than 200,000 songs and 18,000 artists. A column specifies the genre: 6 genres are available but we will only focus on two of them: pop and hip-hop. 

Lyrics are in English only and feature only English-speaking, mostly American artists. For more information, see: https://www.kaggle.com/gyani95/380000-lyrics-from-metrolyrics

We aim to perform sentiment analysis on these lyrics. We will assign a mean negativity score to pop songs and hip-hop songs based on a sentiment dictionary. Our main prediction is that hip-hop lyrics will be significantly more negative than pop songs. 

This analysis will be coded in R 3.5.2.

## Loading data: 

```
require(readr)
require(tidyverse)
require(tidytext)
require(tm)
require(wordcloud)
require(rmarkdown)


##################### Loading the dataframe #############################


#### Load the dataframe (found on Kaggle):

lyrics <- read_csv("lyrics.csv")

#Our dataframe is too heavy to perform standard text analysis. Let's only keep genres of interest: pop and hip-hop
#Moreover, I remove all observations for year < 1970 (data before 1970 is bad quality)

lyrics <- lyrics %>%
  filter(genre == "Pop" | genre == "Hip-Hop") %>%
  filter(year > 1970)

#Check that only "pop" and "hip-hop" remain:

lyrics$genre <- as.factor(lyrics$genre)
levels(lyrics$genre) 

```

##Exploring the database: 

We perform a few analyses to obtain basic information about the structure of our data.

```

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
```

We find that the data contains roughly 40% of hip-hop songs and 60% of pop songs. There are 2347 pop artists (21 songs per artist) and 1410 hip-hop artists (24 songs per artist). The data is thus a bit asymmetrical, but probably representative of the general cultural offer. Importantly, sample size if sufficiently immense to have interesting insights from the analysis for both genres. The average year of release is 2009. Therefore, our study will mostly have implications for the past two decades, but more studies will be needed to generalize our findings. 


##Cleaning the data: 

#Handling stopwords

Stopwords are words that are considered irrelevant for content analysis: "the", "in", "and", etc.... Most stopwords are included in the tm package. We also add our own stopwords.

```
### Handle stopwords (words that are useless for analysis: "and", "or", "I"...)

stopwords_english <- as.data.frame(stopwords(kind = "en")) #create a new dataframe with one column that contains english stopwords from the tm package
names(stopwords_english)[1] <- "word"  #rename that column: it's called "word"

#In my opinion, the stopwords in the tm package are not enough: I create a new vector containing additional stopwords: 

my_stopwords_en <- c("like", "oh", "yeah", "ain't", "la", "que")
my_stopwords_en <- data.frame(word = my_stopwords_en) #transform vector into dataframe
```


#Tidying the dataframe

The most crucial part of text analysis is to have a tidy data frame. We need to have one word per row and not one song per row. The unnest_tokens() function from the tidytext package allows us to do just that. In the same process, we remove stopwords by using the anti_join() function from dplyr. 

```
#Tidy data: I divide the lyrics column into one word per row and remove stopwords

lyrics_data <- lyrics %>% 
  unnest_tokens(word, lyrics) %>% #This step could take a litte while: this line divides lyrics into one word per row
  anti_join(stopwords_english) %>% 
  anti_join(my_stopwords_en) #keep only the complementary of the stopwords databases in my lyrics database
#Our dataframe is ready for analysis!

```

##Preliminary analysis

The first interesting information that can be extracted from our data is basic word counts. What are the most common words in the database? 

```
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

```

The same analysis can be performed for only pop songs...

```
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

```

... and for only hip-hop songs.

```
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

```

At first glance, pop songs seem to address the subject of love much more than hip-hop, as evidenced by the greater prevalence of words like "love" or "baby" in pop lyrics. This already gives us an idea about our prediction. Let us now move forward to sentiment analysis. 


##Sentiment analysis: 

Our protocole for sentiment analysis is fairly simple: we use a sentiment dictionary dataframe and merge it with our data. Subsequently, we will have a dataframe that associates each word with a positivity/negativity value. 

We use the "Afinn" dictionary. "Afinn" associates words with a score ranging from -5 (most negative) to 5 (most positive). Neutral values (0) do not exist. Like most dictionaries, "Afinn" was constituted by surveying individuals and ask them to rate words on a scale. 

Importantly, the "Afinn" dictionary is itself biased towards negativity: the mean value is roughly -0.6. We use the mean value from "Afinn" as the reference for negativity. We define a negativity score such that: 

Negativity score = Afinn mean value - observed mean value

We define negativity bias as the situation where mean values in the database  are below the Afinn mean value (neg_ref in the script). On the other hand, if an observed value is above the Afinn mean value, it should be seen as a case of positivity bias. 

```
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

```
We find that the negativity score for hip-hop lyrics is substantially higher than the negativity score for pop lyrics. In fact, hip-hop demonstrates a negativity bias (< Afinn mean value) while pop demonstrates a positivity bias (> Afinn mean value). The difference in negativity score between hip-hop and pop is approximately 1.4 points. The following plot further illustrates this point: 

```
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

```

##Statistical analysis: 

This difference should be testes statistically. We use a linear model to test our hypothesis: 

```
neg_mod <- lm(formula = mean_negativity ~ genre, data = lyrics_sentiments)
summary(neg_mod)
```
It seems like the difference in negativity between pop and hip-hop is statistically significant! Yet, we have failed to control for an obvious potential confound: time. 

```
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
       
  ```
Interestingly, time seems to have an effect on negativity: songs tend to get more negative over the years. However, things get more nuanced once we plot the trend for pop and hip-hop individually: 

```
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
```
This plot suggests that the increase in negativity is driven by the increased popularity of hip-hop rather than the effect of time alone.


Let's test this statistically: 

```
###Let's test this statistically: 


neg_mod2 <- lm(formula = mean_negativity ~ genre + year, data = lyrics_sentiments)
summary(neg_mod2)
```

The impact of year is not significant, and genre remains statistically significant! Finally, let's implement a model that controls for year AND the interaction of year and genre:

```
neg_mod3 <- lm(formula = mean_negativity ~ genre + year + genre:year, data = lyrics_sentiments)
summary(neg_mod3)
```
The impact of year and year:genre are not significant, and genre remains statistically significant again! At this point, it seems like our prediction has been verified. Let's dive further to investigate how exactly is hip-hop more negative than pop. 



##Further analysis

To better understand the effect of genre on negativity scores, let's observe the most common negative words per genre: 

```
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

```

Well... as expected, the higher negativity in hip-hop is driven by curse words. Curse words are not absent in pop music, but seem much less frequent. Let's dive further into the distribution of negativiry scores in both genres.

```
#Distribution of negativity scores: 

count_negativity <- lyrics_sentiments %>%
  group_by(value, genre) %>%
  summarize(count = n()) #count words by negativity value and genre

ggplot(count_negativity, aes(value, count, fill = genre)) +
  geom_col(position = "dodge") +
  labs(title = "Distribution of negativity scores by genre",
       x = "Negativity Value (based on AFINN dictionnary)",
       y = "Word Count", 
       fill = "Genre")

```
As one could suspect, the negativity of hip-hop is driven by extremely negative values (value < -3). These are probably mainly constituted of curse words. Interestingly, it is also due to a large difference in occurence of highly positive words (value = 3).

Yet, these figures suggest a potential bias in our analysis. The most common negative word in hip-hop songs is the "n-word". It is understandable why the "Afinn" dictionary has identified it as an extremely negative word. Yet, in the context of hip-hop culture, it is not necessarily an insult nor a negative word, but rather a common interjection. Given its prevalence in negative words, would an effect still be detected if we removed it? 

```
#Refining the analysis: the most common negative word in hp-hop is not necessarily seen as negative in the context of hip-hop culture

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

```

The difference between pop and hip-hop remains significant even after removing the n-word, even though the negativity score for hip-hop has substantially decreased. Our prediction remains valid. 

## Exploratory analysis: 

Is the more prevalent negativity of hip-hop lyrics driven by slurs only? If so, we would expect that negativity bias in hip-hop is mainly driven by anger. Another dictionary from the tidytext package, "NRC", classifies words into 6 emotions: surprise, disgust, trust, sadness, anger, anticipation. Let's perform an exploratory analysis to see which emotion is more prevalent in our database: 

```
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
```

Hip-Hop's higher negativity seems indeed particularly salient in anger, but also disgust and fear. The difference is less striking for sadness. This should be further investigated in subsequent research.





