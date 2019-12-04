# Testing life history theory using text analysis on lyrics (in R). 

This repository contains all relevant scripts, documents and information for my PCBS final project. 

Summary (read the paragraphs below to know the full story): the objectives of this project are twofold: 

1) I wish to build a R Shiny App that allows the user to interactively explore lyrics data

2) I wish to perform a full-fledge text analysis/sentiment analysis on music lyrics. Lyrics will be accessed using web scraping methods while text analysis will require specific R packages like tm and tidytext. 




Evolutionary theory predicts that not only species adapt to their global environment, but individuals adapt to their local environment. Harsh and unpredictible environments should trigger a different cluster of psychological dispositions and behaviors than affluent ones. Studies have established for several animal species that the harshness of the environment accounts for individual differences in various traits like agressivity, impulsivity and age at first sexual intercourse.

Species who evolved in changing environments, the theory goes, have evolved this ability to perceive cues of environment harshness and behave accordingly. Short-term strategies (impulsivity, aggressivity, early reproduction, etc...) are adaptive in harsh environments where extrinsic mortality is high, whereas long-term strategies (low time-disocunting, moderation, cognitive inhibition...) are adaptive in affluent environments where extrinsic mortality is lower. 

Some researchers have argued that this framework can be applied to humans as well. Human individuals living in harsh conditions - for example, in warfare zones or simply in poverty - display different psychological dispositions and behaviors than individuals living in affluent environments. This theory could explain the lenghtily discussed finding that the poor and the well-off display very different behaviors. 

In humans, harsh environments may not only manifest in higher impulsivity and earlier reproduction, but also in lower trust, higher conservatism and higher negativity. The hypothesis I want to test here is the idea that poverty is associated with a higher negativity. 

I am currently testing this hypothesis experimentally during my lab internship. In this project, I wish to test this hypothesis in a more original and ecological way. I wish to perform text analysis - sentiment analysis in particular - to a database of song lyrics. 

Songs are an interesting object, because they express - and trigger - strong emotions. Sentiment analysis allows us to determine the prevalence of various emotions in lyrics: positivity, negativity, joy, anger, etc.... To be more precise, I wish to test the following hypotheses. 

There exists no database for the socio-economic status of artists. Therefore, I will use music genre as a proxy for the artist's status. I chose to compare French rap songs with French "variété" pop songs. Rap is known to be a working class genre while "variété" addresses a wider and relatively wealthier audience. If our hypothesis is correct, after sentiment analysis rap songs should display more negative emotions than "variété" songs. 




