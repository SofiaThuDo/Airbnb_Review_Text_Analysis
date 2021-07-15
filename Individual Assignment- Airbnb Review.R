library(tidytext)
library(tidyverse)
library(dplyr)
library(stringr)
library(scales)
library(ggplot2)
library(tm)
library(rtweet)

data("stop_words")
setwd("C:/Users/asus/Desktop/MsBA/Spring semester/Text Analysis/Individual assignment/Dataset")

##############################
##BERLIN REVIEW
##############################
review_1 =read.csv("Berlin.csv", stringsAsFactors = FALSE)

comments_1 <- review_1$comments

berlin <- data.frame(line=1:401963, text=comments_1)


tidy_berlin <- berlin %>%
                    unnest_tokens(word, text) %>%
                    anti_join(stop_words) 
tidy_berlin%>% 
  count(word, sort=TRUE)


#freq_hist_berlin <- berlin %>%
#                      unnest_tokens(word,text) %>%
#                     anti_join(stop_words) %>%
#                      count(word, sort=TRUE) %>%
#                      mutate(word=reorder(word, n)) %>%
#                      filter(n >30000)%>%
#                      ggplot(aes(word, n))+
#                      geom_col()+
#                      xlab(NULL)+
#                      coord_flip()

#print(freq_hist_berlin)


##############################
##TORONTO
##############################
review_2 =read.csv("Toronto.csv", stringsAsFactors = FALSE)

comments_2 <- review_2$comments

toronto <- data.frame(line=1:576806, text=comments_2)


tidy_toronto <- toronto %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

tidy_toronto%>% 
  count(word, sort=TRUE)


############################
##BOSTON
############################
review_3 =read.csv("Boston.csv", stringsAsFactors = FALSE)

comments_3 <- review_3$comments

boston <- data.frame(line=1:68275, text=comments_3)

tidy_boston <- boston %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) 

tidy_boston%>% 
  count(word, sort=TRUE)

##########################
#MELBOURNE
#########################
review_4 =read.csv("Melbourne.csv", stringsAsFactors = FALSE)

comments_4 <- review_4$comments

melbourne <- data.frame(line=1:486920, text=comments_4)


tidy_melbourne <- melbourne %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) 

tidy_melbourne%>% 
  count(word, sort=TRUE)

#####################
#SEATTLE
#####################
review_5 =read.csv("Seattle.csv", stringsAsFactors = FALSE)

comments_5 <- review_5$comments

seattle <- data.frame(line=1:84849, text=comments_5)


tidy_seattle <- seattle %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) 

tidy_seattle%>% 
  count(word, sort=TRUE)

#########################################
#BOSTON IS THE BASELINE
#########################################

##########################################
## FRAMEWORK TO COMPARE DIFFERENT TEXTS ##
##########################################

#prepare data by combining all the datasets and do frequencies 
library(tidyr)
frequency <- bind_rows(mutate(tidy_berlin, city="Berlin"),
                       mutate(tidy_melbourne, city= "Melbourne"),
                       mutate(tidy_toronto, city= "Toronto"),
                       mutate(tidy_boston, city="Boston")
)%>%#closing bind_rows
  mutate(word=str_extract(word, "[a-z']+")) %>%
  count(city, word) %>%
  group_by(city) %>%
  mutate(proportion = n /sum(n))%>%
  select(-n) %>%
  spread(city, proportion) %>%
  gather(city, proportion,`Toronto`, `Melbourne`, `Berlin`)


#### FRAMEWORK 1: .CORR() TEST --> find correlation coefficients 
##########################################

cor.test(data=frequency[frequency$city == "Melbourne",],
         ~proportion + `Boston`)

cor.test(data=frequency[frequency$city == "Berlin",],
         ~proportion + `Boston`)

cor.test(data=frequency[frequency$city == "Toronto",],
         ~proportion + `Boston`)

#### FRAMEWORK 2: CORRELOGRAMS (keywords segmentation)  
##########################################
library(scales)
ggplot(frequency, aes(x=proportion, y=`Boston`, 
                      color = abs(`Boston`- proportion)))+
  geom_abline(color="grey40", lty=2)+
  geom_jitter(alpha=.1, size=2.5, width=0.3, height=0.3)+
  geom_text(aes(label=word), check_overlap = TRUE, vjust=1.5) +
  scale_x_log10(labels = percent_format())+
  scale_y_log10(labels= percent_format())+
  scale_color_gradient(limits = c(0,0.001), low = "darkslategray4", high = "gray75")+
  facet_wrap(~city, ncol=3)+
  theme(legend.position = "none")+
  labs(y= "Boston", x=NULL)


#########################################
#SEATTLE IS THE BASELINE
#########################################

##########################################
## FRAMEWORK TO COMPARE DIFFERENT TEXTS ##
##########################################

#prepare data by combining all the datasets and do frequencies
library(tidyr)
frequency <- bind_rows(mutate(tidy_berlin, city="Berlin"),
                       mutate(tidy_melbourne, city= "Melbourne"),
                       mutate(tidy_toronto, city= "Toronto"),
                       mutate(tidy_seattle, city="Seattle")
)%>%#closing bind_rows
  mutate(word=str_extract(word, "[a-z']+")) %>%
  count(city, word) %>%
  group_by(city) %>%
  mutate(proportion = n /sum(n))%>%
  select(-n) %>%
  spread(city, proportion) %>%
  gather(city, proportion,`Toronto`, `Melbourne`, `Berlin`)

#FRAMEWORK 1- CORRELATION
cor.test(data=frequency[frequency$city == "Melbourne",],
         ~proportion + `Seattle`)

cor.test(data=frequency[frequency$city == "Berlin",],
         ~proportion + `Seattle`)

cor.test(data=frequency[frequency$city == "Toronto",],
         ~proportion + `Seattle`)

#### FRAMEWORK 2: CORRELOGRAMS (keywords segmentation)  
##########################################
library(scales)
ggplot(frequency, aes(x=proportion, y=`Seattle`, 
                      color = abs(`Seattle`- proportion)))+
  geom_abline(color="grey40", lty=2)+
  geom_jitter(alpha=.1, size=2.5, width=0.3, height=0.3)+
  geom_text(aes(label=word), check_overlap = TRUE, vjust=1.5) +
  scale_x_log10(labels = percent_format())+
  scale_y_log10(labels= percent_format())+
  scale_color_gradient(limits = c(0,0.001), low = "darkslategray4", high = "gray75")+
  facet_wrap(~city, ncol=3)+
  theme(legend.position = "none")+
  labs(y= "Seattle", x=NULL)


###########################################################
#FRAMEWORK 3: SENTIMENTS
###########################################################
library(textdata)
library(dplyr)
library(stringr)
library(tidyverse)
library(tidytext)

afinn <- get_sentiments("afinn") #Negative vs positive sentiment
nrc <- get_sentiments("nrc")     #emotions
bing <- get_sentiments("bing")   #binary

sentiments <- bind_rows(mutate(afinn, lexicon="afinn"),
                        mutate(nrc, lexicon= "nrc"),
                        mutate(bing, lexicon="bing")
)

###########################
#BOSTON CITY
############################
#1- SENTIMENT FRAMEWORK with JOY
nrcsurprise <- get_sentiments("nrc") %>%
  filter(sentiment == "joy")

#inner joining the emma book and the surprise sentiments
tidy_boston %>%
  inner_join(nrcsurprise) %>%
  count(word, sort=T)%>%
  filter(n>1100)


#2- SENTIMENT FRAMEWORK with ANGER
nrcsurprise <- get_sentiments("nrc") %>%
  filter(sentiment == "anger")

#inner joining the emma book and the surprise sentiments
tidy_boston %>%
  inner_join(nrcsurprise) %>%
  count(word, sort=T)%>%
  filter(n>120)

###########################
#SEATTLE CITY
############################

#1- SENTIMENT FRAMEWORK with JOY
nrcsurprise <- get_sentiments("nrc") %>%
  filter(sentiment == "joy")

#inner joining the emma book and the surprise sentiments
tidy_seattle %>%
  inner_join(nrcsurprise) %>%
  count(word, sort=T)%>%
  filter(n>2000)


#2- SENTIMENT FRAMEWORK with ANGER
nrcsurprise <- get_sentiments("nrc") %>%
  filter(sentiment == "anger")

#inner joining the emma book and the surprise sentiments
tidy_seattle %>%
  inner_join(nrcsurprise) %>%
  count(word, sort=T)%>%
  filter(n>165)


####################################################
#FRAMEWORK 4: n-gram
####################################################
library(igraph)
library(ggraph)

#1-QUADROGRAM- SEATTLE

quadrogram <- seattle%>%
                unnest_tokens(quadrogram, text, token = "ngrams", n=4) %>%
                filter(!is.na(quadrogram))%>%
                separate(quadrogram, c("word1", "word2", "word3", "word4"), sep=" ") %>%
                filter(!word1 %in% stop_words$word) %>%
                filter(!word2 %in% stop_words$word) %>%
                filter(!word3 %in% stop_words$word) %>%
                filter(!word4 %in% stop_words$word) 


quadrogram_counts <- quadrogram %>%
                        count(word1, word2, word3, word4, sort = TRUE)


#create matrix to draw trigram network
graph <- quadrogram_counts %>%
  filter(n>15) %>%
  graph_from_data_frame()

#visualize trigram network
a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
ggraph(graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

#2- TRIGRAM-BOSTON
trigram <- boston%>%
  unnest_tokens(trigram, text, token = "ngrams", n=3) %>%
  filter(!is.na(trigram))%>%
  separate(trigram, c("word1", "word2", "word3"), sep=" ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word)


trigram_counts <- trigram %>%
  count(word1, word2, word3, sort = TRUE)


#create matrix to draw trigram network
trigram_graph <- trigram_counts %>%
  filter(n>60) %>%
  graph_from_data_frame()

#visualize trigram network
a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
ggraph(trigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 3) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()


